library(dplyr)
library(tidyr)
library(data.table)

# set dictionary
setFixest_dict(c(total_consumption = "Consumption in kWh per period", 
                 daily_consumption = "Consumption in kWh per day",
                 consumption_hh = "Half Hourly Consumption in kWh",
                 share_consumption = "Share of daily consumption",
                 weekly_consumption = "Gas Consumption per Week in kWh",
                 cosy_contract_active = "Cosy Contract Active",
                 daily_avg_heating_degree = "HDD",
                 avg_heating_degree = "HDD",
                 settlement_week = "Week",
                 date = "Day", 
                 hashed_mpan = "Household", 
                 tariff_gsp_group_id = "GSP", 
                 rate_period = "Rate period",
                 is_variable = "Variable Tariff",
                 energy_efficiency = "Energy Efficiency",
                 estimated_annual_consumption = "EAC",
                 eac_mwh = "EAC in mWh",
                 is_hp_installed = "Is HP Installed",
                 predicted_is_installed = "Is HP Installed",
                 elec_consumption = "Monthly Electricity Consumption (kWh)",
                 gas_consumption = "Monthly Gas Consumption (kWh)",
                 month_date = "Month",
                 is_winter = "Winter",
                 previous_is_variable = "Prev Is Variable",
                 epc_letter = "EPC",
                 predicted_heatloss_watts = "Heatloss (W)",
                 total_floor_area = "Floor Area (m sq)",
                 urban = "Urban",
                 floor_area = "Floor area",
                 property_value = "Property value",
                 account_id = "Household",
                 ev_charging = "EV Charging",
                 has_ev = "EV User"))

# propensity score matching
setwd("scripts")

# cosy sample
aggregated_data <- readRDS("../data/scratch/aggregated_data.RDS") 
cosy_hp_details <- fread("../data/input/cosy_-_cosy_details_2024_07_24.csv") %>%
  inner_join(aggregated_data %>% select(hashed_mpan, account_id) %>% distinct()) %>%
  distinct() 
cosy_hp_details <- cosy_hp_details %>% 
  filter(!is.na(property_value), !is.na(floor_area), !is.na(energy_efficiency)) %>%
  select(account_id, energy_efficiency, property_value, floor_area) %>%
  mutate(sample = "Cosy")


# heat pump sample
hp_details <- fread("../data/input/cosy_-_hp_aggregated_up_2024_06_18.csv") %>%
  distinct(account_id) %>%
  inner_join(fread("../data/input/cosy_-_hp_details_2024_06_25.csv") %>% distinct(account_id, .keep_all = TRUE), by=c("account_id")) %>%
  filter(installed_at <= "2024-05-29") %>% 
  filter(!is.na(property_value), !is.na(floor_area), !is.na(energy_efficiency)) %>%
  select(account_id, energy_efficiency, property_value, floor_area)  %>%
  mutate(sample = "HP")

# random sample
random_domus_sample <- fread("../data/input/cosy_-_random_sample_details_2024_08_23.csv") %>% 
  filter(!is.na(property_value), !is.na(floor_area), !is.na(energy_efficiency)) %>%
  filter(!account_id %in% cosy_hp_details$account_id, !account_id %in% hp_details$account_id) %>%
  select(account_id, energy_efficiency, property_value, floor_area) %>%
  mutate(sample = "Random")

# latest eac
latest_eac <- fread("../data/input/latest_eac.csv") %>%
  distinct(account_id, .keep_all = TRUE)
  
# MERGE 
matching_data <-  rbind(latest_eac %>%
  inner_join(random_domus_sample),
  latest_eac %>%
    inner_join(cosy_hp_details),
  latest_eac %>%
    inner_join(hp_details)) %>%
  filter(!is.na(sample), !is.na(estimated_annual_consumption)) %>%
  distinct(account_id, .keep_all = TRUE)

#The MatchIt, lmtest and sandwich libraries are used.
library(MatchIt)
library(lmtest)
library(sandwich)
library(stargazer)

#Using the mathcit function from MatchIt to match each random customers to cosy
match_obj <- matchit(formula = treated ~ property_value + estimated_annual_consumption + 
                       energy_efficiency + floor_area, data = matching_data %>% 
                       filter(sample %in% c("Cosy", "Random")) %>%
                       mutate(treated = as.numeric(!sample=="Random")),   method = "full", estimand = "ATC",
                     caliper = c(estimated_annual_consumption = 0.5, energy_efficiency= .5, property_value = 0.5, floor_area = 0.5))
match_summary <- summary(match_obj)
match_summary

#plotting the balance between samples
plot(match_obj, type = "jitter", interactive = FALSE)
plot(summary(match_obj), abs = FALSE)

# Extract balance for all data
# Replace underscores with spaces and capitalize the first letter of each word
rownames(match_summary$sum.all) <- gsub("_", " ", rownames(match_summary$sum.all))
rownames(match_summary$sum.all) <- tools::toTitleCase(rownames(match_summary$sum.all))
# Replace "Treated" with "Cosy" and "Control" with "Random" in column names
colnames(match_summary$sum.all) <- gsub("Treated", "Cosy", colnames(match_summary$sum.all))
colnames(match_summary$sum.all) <- gsub("Control", "Random", colnames(match_summary$sum.all))
stargazer(match_summary$sum.all[,-dim(match_summary$sum.all)[2]],
          title = "Summary of Balance for All Data (Cosy)",
          rownames = TRUE,
          digits = 2,
          label = "tab:balance-cosy-prematching",
          out = "../tables/balance_cosy_prematching.tex")


# Extract balance for matched data
# Replace underscores with spaces and capitalize the first letter of each word
rownames(match_summary$sum.matched) <- gsub("_", " ", rownames(match_summary$sum.matched))
rownames(match_summary$sum.matched) <- tools::toTitleCase(rownames(match_summary$sum.matched))
# Replace "Treated" with "Cosy" and "Control" with "Random" in column names
colnames(match_summary$sum.matched) <- gsub("Treated", "Cosy", colnames(match_summary$sum.matched))
colnames(match_summary$sum.matched) <- gsub("Control", "Random", colnames(match_summary$sum.matched))
stargazer(match_summary$sum.all[,-dim(match_summary$sum.all)[2]],
          title = "Summary of Balance for Matched Data (Cosy)",
          rownames = TRUE,
          digits = 2,
          label = "tab:balance-cosy-matching",
          out = "../tables/balance_cosy_matching.tex")


# Replace "Treated" with "Cosy" and "Control" with "Random" in column names
colnames(match_summary$nn) <- c("Random", "Cosy")
stargazer(match_summary$nn,
          title = "Sample Size (Cosy)",
          rownames = TRUE,
          label = "tab:balance-cosy-nn",
          out = "../tables/balance_cosy_nn.tex")


#Extract the matched data and save the data into the variable matched_data
matched_data <- match.data(match_obj)

library(fixest)
m1 <- feols(consumption_hh ~ i(cosy_contract_active) | daily_avg_heating_degree + account_id + date, 
            data = aggregated_data, 
            cluster = ~account_id, 
            split = ~ rate_period)

m1_matched <- feols(consumption_hh ~ i(cosy_contract_active) | daily_avg_heating_degree + account_id + date, 
                    weights = ~weights,
                    data = aggregated_data %>% inner_join(matched_data %>% distinct(account_id, weights)), 
                    cluster = ~account_id, 
                    split = ~ rate_period)



# Function to format numbers
format_number <- function(number) {
  return(format(round(number), big.mark = ",", scientific = FALSE))
}

# Function to format numbers with four decimal places
format_decimal <- function(number, decimals = 4) {
  return(format(round(number, decimals), nsmall = decimals, big.mark = ",", scientific = FALSE))
}


# Register the pre-treatment average fit statistic
fitstat_register("pre_avg", function(x) {
  
  # Extract the formula
  formula <- x$fml_all$linear
  
  # Extract the outcome variable from the formula
  outcome_variable <- all.vars(formula)[1]
  
  # Extract the call object and evaluate the data argument
  call_object <- x$call
  data_expr <- call_object$data
  data <- eval(data_expr)
  
  # Get the logical vector of observations used in the model
  obs_used <- obs(x)
  
  # Subset the original dataset using this logical vector
  data_used <- data[obs_used, ]
  
  # Ensure the outcome variable is treated as a column name
  outcome_values <- data_used[[outcome_variable]]
  
  # Create pre-avg for non-HP installed group
  pre_avg <- mean(outcome_values[data_used$cosy_contract_active == 0], na.rm = TRUE)
  
  # Format the pre-avg
  formatted_pre_avg <- format_decimal(pre_avg)
  
  return(formatted_pre_avg)
}, "Half Hourly Consumption")



# Add number of time periods
fitstat_register("t_obs", function(x) {
  
  # time variable
  t_var <- x$fixef_vars[3]
  
  # nbr of unique val for t var
  t_obs <- x$fixef_sizes[t_var]
  
  return(format_number(t_obs))
}, "Number of Time Periods")

CleanPreAverage <- function(file_path) {
  
  # Read the generated LaTeX file
  file_content <- readLines(file_path)
  
  # Find the lines with the pre-treatment average and remove them
  if (length(grep("Half Hourly Consumption", file_content))==1) {
    pre_avg_line_index <- grep("Half Hourly Consumption", file_content)
  } else {
    pre_avg_line_index <- grep("Half Hourly Consumption", file_content)[2]
  }
  
  pre_avg_lines <- file_content[pre_avg_line_index:(pre_avg_line_index)]
  file_content <- file_content[-c(pre_avg_line_index, pre_avg_line_index)]
  
  # Find the position just after the coefficients
  coeff_end_index <- grep("Fixed-effects", file_content) -2
  
  # Insert the pre-treatment average row after the coefficients
  file_content <- append(file_content, pre_avg_lines, after = coeff_end_index)
  file_content <- append(file_content, "\\emph{Pre-Treatment Average}\\\\", after = coeff_end_index)
  
  # Add a \midrule after the pre-treatment average
  file_content <- append(file_content, "\\midrule", after = coeff_end_index)
  
  # Modify the label for "Size of the 'effective' sample" to "Number of Households"
  sample_line <- grep("Size of the 'effective' sample", file_content)
  file_content[sample_line] <- gsub("Size of the 'effective' sample", "Number of Households", file_content[sample_line])
  
  # Write the modified content back to the LaTeX file
  writeLines(file_content, file_path)
}

etable(m1_matched, tex=TRUE, title = "TWFE using Matching Weights",
       headers = list(list("Matching" = 5),
                      list(rep(as.character(sort(unique(aggregated_data$rate_period))), times =1))), 
       fitstat = ~ N + g + pre_avg +t_obs + r2, file = "../tables/matching_did.tex", replace = TRUE, label="tab:did-matching")

CleanPreAverage("../tables/matching_did.tex")


# MERGE 
set.seed(12345678)
matching_data <-  rbind(latest_eac %>%
                          inner_join(random_domus_sample),
                        latest_eac %>%
                          inner_join(hp_details)) %>%
  filter(!is.na(sample), !is.na(estimated_annual_consumption)) %>%
  distinct(account_id, .keep_all = TRUE)

#Using the mathcit function from MatchIt to match each smoker with a non-smoker (1 to 1 matching) based on
#sex, indigeneity status, high school completion, marital status (partnered or not),
#region of residence (major cities, inner regional, outer regional), language background (English speaking Yes/No) 
#and risky alcohol drinking (Yes/No)
match_obj2 <- matchit(treated ~ property_value + estimated_annual_consumption + energy_efficiency + floor_area,
                     data = matching_data %>%
                       filter(sample %in% c("HP", "Random")) %>%
                       mutate(treated = as.numeric(!sample=="Random")), method = "full", estimand = "ATC",
                     caliper = c(estimated_annual_consumption = 0.5, energy_efficiency= .5, property_value = 0.5, floor_area = 0.5))
match_summary <- summary(match_obj2)
match_summary

#plotting the balance between samples
plot(match_obj2, type = "jitter", interactive = FALSE)
plot(summary(match_obj2), abs = FALSE)

# Extract balance for all data
# Replace underscores with spaces and capitalize the first letter of each word
rownames(match_summary$sum.all) <- gsub("_", " ", rownames(match_summary$sum.all))
rownames(match_summary$sum.all) <- tools::toTitleCase(rownames(match_summary$sum.all))
# Replace "Treated" with "Cosy" and "Control" with "Random" in column names
colnames(match_summary$sum.all) <- gsub("Treated", "Heatpump", colnames(match_summary$sum.all))
colnames(match_summary$sum.all) <- gsub("Control", "Random", colnames(match_summary$sum.all))
stargazer(match_summary$sum.all[,-dim(match_summary$sum.all)[2]],
          title = "Summary of Balance for All Data (Heatpump)",
          rownames = TRUE,
          digits = 2,
          label = "tab:balance-hp-prematching",
          out = "../tables/balance_hp_prematching.tex")


# Extract balance for matched data
# Replace underscores with spaces and capitalize the first letter of each word
rownames(match_summary$sum.matched) <- gsub("_", " ", rownames(match_summary$sum.matched))
rownames(match_summary$sum.matched) <- tools::toTitleCase(rownames(match_summary$sum.matched))
# Replace "Treated" with "Cosy" and "Control" with "Random" in column names
colnames(match_summary$sum.matched) <- gsub("Treated", "Heatpump", colnames(match_summary$sum.matched))
colnames(match_summary$sum.matched) <- gsub("Control", "Random", colnames(match_summary$sum.matched))
stargazer(match_summary$sum.all[,-dim(match_summary$sum.all)[2]],
          title = "Summary of Balance for Matched Data (Heatpump)",
          rownames = TRUE,
          digits = 2,
          label = "tab:balance-hp-matching",
          out = "../tables/balance_hp_matching.tex")


# Replace "Treated" with "Cosy" and "Control" with "Random" in column names
colnames(match_summary$nn) <- c("Random", "Heatpump")
stargazer(match_summary$nn,
          title = "Sample Size (Heatpump)",
          rownames = TRUE,
          label = "tab:balance-hp-nn",
          out = "../tables/balance_hpy_nn.tex")


#Extract the matched data and save the data into the variable matched_data
matched_data2 <- match.data(match_obj2)


# get consumption by period
hp_installed_period <- fread("../data/input/cosy_-_hp_aggregated_up_2024_06_18.csv") %>%
  rename(total_consumption=total_read_value,
         date = settlement_date) %>%
  mutate(consumption_hh = ifelse(rate_period == "Other", 
                                 total_consumption/30, total_consumption/6),
         date = as.Date(date))

# aggreate up to get overall
hp_installed_daily <- hp_installed_period %>%
  group_by(account_id, hashed_mpan, date) %>%
  summarise(total_consumption = sum(total_consumption)) %>%
  mutate(consumption_hh = total_consumption / 48,
         rate_period = factor("Overall")) 

# merge together to make regressions easier
hp_installed <- full_join(hp_installed_period,
                          hp_installed_daily) %>%
  inner_join(fread("../data/input/cosy_-_hp_details_2024_06_25.csv") %>%
               distinct(account_id, .keep_all=TRUE), 
             by=c("account_id","hashed_mpan"))

# add daily weather data
weather <- fread("../data/input/Cosy Analysis Weather Mar 26 daily.csv") %>% 
  rename_with(.cols = starts_with("weekly"), 
              .fn = ~ sub("^weekly", "daily", .)) %>%
  rename(tariff_gsp_group_id=gsp_group_id) %>%
  mutate(date_day=as.Date(date_day, format = "%Y-%m-%d")) %>%
  rename(date = date_day)

# merge weather
hp_installed <- hp_installed %>%
  left_join(weather)


rm(weather, hp_installed_daily, hp_installed_period)

# hp_installed <- hp_installed %>%
#   left_join(readRDS("../data/scratch/ev_mpan.RDS")) %>%
#   mutate(has_ev = ifelse(!is.na(ev_start), as.numeric(date >= ev_start), 0))

# clean up
hp_installed <- hp_installed %>%
  mutate(date = as.Date(date),
         is_hp_installed = as.numeric(installed_at <= date)) %>%
  mutate(rate_period = factor(rate_period, levels = c("Morning Cosy",
                                                      "Afternoon Cosy",
                                                      "Peak Rate",
                                                      "Other", 
                                                      "Overall"))) %>%
  group_by(account_id) %>%
  distinct(account_id, date, rate_period, .keep_all = TRUE)


# Check number of accounts
hp_installed %>% ungroup() %>% select(account_id) %>% distinct() %>% dim()

# Load weather data
weather <- fread("../data/input/cosy_-_weather_weekly_2024_06_13.csv")

# Load and preprocess gas consumption data
# previous 2024_06_13.csv
cosy_hp_install_gas_consumption <- fread("../data/input/cosy_-_hp_users_gas_2024_06_13.csv") %>%
  group_by(account_id) %>%
  mutate(is_hp_installed = as.numeric(installed_at <= settlement_week),
         treated = max(is_hp_installed),
         min_settlement_week = min(settlement_week)) %>%
  left_join(weather, by = c("gsp_group_id", "settlement_week" = "week_date")) %>%
  distinct(account_id, settlement_week, .keep_all = TRUE)

# Create a sequence of weeks
min_date <- min(cosy_hp_install_gas_consumption$settlement_week)
max_date <- max(cosy_hp_install_gas_consumption$settlement_week)
all_weeks <- seq(min_date, max_date, by = "week")

# Create a data frame with all combinations of account_id and settlement_week
all_combinations <- expand.grid(
  account_id = unique(cosy_hp_install_gas_consumption$account_id),
  settlement_week = all_weeks
)

# Merge with original data
merged_data <- all_combinations %>%
  left_join(cosy_hp_install_gas_consumption %>% select(account_id, gsp_group_id, installed_at, min_settlement_week) %>% distinct()) %>%
  left_join(cosy_hp_install_gas_consumption %>% select(account_id, settlement_week, weekly_consumption) %>% distinct()) %>%
  filter(min_settlement_week < settlement_week) %>%
  mutate(
    account_closed = ifelse(is.na(weekly_consumption), 1, 0),
    is_hp_installed = as.numeric(installed_at < settlement_week),
    weekly_consumption = ifelse(is.na(weekly_consumption), 0, weekly_consumption)
  ) %>%
  left_join(weather, by = c("gsp_group_id", "settlement_week" = "week_date")) 

# Define overall_weekly by merging with electricity data
library(lubridate)
overall_weekly <- hp_installed %>%
  filter(rate_period == "Overall") %>%
  mutate(settlement_week = floor_date(date, "week") + 1,
         is_hp_installed = as.numeric(installed_at <= settlement_week)) %>%
  group_by(account_id, hashed_mpan, tariff_gsp_group_id, settlement_week, installed_at, is_hp_installed) %>%
  summarise(elec_consumption = sum(total_consumption)) %>%
  rename(gsp_group_id = tariff_gsp_group_id) %>%
  left_join(merged_data %>%
              select(account_id, settlement_week, weekly_consumption) %>%
              rename(gas_consumption = weekly_consumption)) %>%
  left_join(weather %>% rename(settlement_week = week_date)) %>%
  mutate(
    gas_consumption = 52.25 * gas_consumption,
    elec_consumption = 52.25 * elec_consumption,
    total_consumption = gas_consumption + elec_consumption
  ) 

rm(all_combinations, cosy_hp_install_gas_consumption, merged_data, weather, all_weeks, max_date, min_date)

CleanPreAverage <- function(file_path) {
  
  # Read the generated LaTeX file
  file_content <- readLines(file_path)
  
  # Find the lines with the pre-treatment average and remove them
  if (length(grep("Yearly Consumption", file_content))==1) {
    pre_avg_line_index <- grep("Yearly Consumption", file_content)
  } else {
    pre_avg_line_index <- grep("Yearly Consumption", file_content)[2]
  }
  
  # Find the lines with the pre-treatment average and remove them
  pre_avg_lines <- file_content[pre_avg_line_index:(pre_avg_line_index)]
  file_content <- file_content[-c(pre_avg_line_index, pre_avg_line_index)]
  
  # Find the position just after the coefficients
  coeff_end_index <- grep("Is HP Installed", file_content)[length(grep("Is HP Installed", file_content))] + 2
  
  # Insert the pre-treatment average row after the coefficients
  file_content <- append(file_content, pre_avg_lines, after = coeff_end_index)
  file_content <- append(file_content, "\\emph{Pre-Treatment Average}\\\\", after = coeff_end_index)
  
  # Add a \midrule after the pre-treatment average
  file_content <- append(file_content, "\\midrule", after = coeff_end_index + length(pre_avg_lines)+1)
  
  # Modify the label for "Size of the 'effective' sample" to "Number of Households"
  sample_line <- grep("Size of the 'effective' sample", file_content)
  file_content[sample_line] <- gsub("Size of the 'effective' sample", "Number of Households", file_content[sample_line])
  
  # Write the modified content back to the LaTeX file
  writeLines(file_content, file_path)
}


# Function to format numbers
format_number <- function(number) {
  return(format(round(number), big.mark = ",", scientific = FALSE))
}

# Function to format numbers with four decimal places
format_decimal <- function(number, digits = 2) {
  return(format(round(number, digits), nsmall = digits, big.mark = ",", scientific = FALSE))
}


# Register the pre-treatment average fit statistic
fitstat_register("pre_avg", function(x) {
  
  # Directly use the model's data
  model_data <- eval(x$call$data, envir = x$call_env)
  
  # Extract the outcome variable from the formula
  outcome_variable <- all.vars(x$fml_all$linear)[1]
  
  # Get the logical vector of observations used in the model
  obs_used <- obs(x)
  
  # Ensure `obs_used` correctly subsets the data
  if (is.logical(obs_used) && length(obs_used) == nrow(model_data)) {
    data_used <- model_data[obs_used, ]
  } else if (is.numeric(obs_used) && all(obs_used <= nrow(model_data))) {
    data_used <- model_data[obs_used, ]
  } else {
    stop("Unable to correctly subset data. Check the obs_used vector.")
  }
  
  # Extract the outcome values
  outcome_values <- data_used[[outcome_variable]]
  
  # Create pre-avg for non-HP installed group
  pre_avg <- mean(outcome_values[data_used$is_hp_installed == 0], na.rm = TRUE)
  
  # Format the pre-avg
  formatted_pre_avg <- format_decimal(pre_avg)
  
  return(formatted_pre_avg)
}, "Yearly Consumption")




# Add number of time periods
fitstat_register("t_obs", function(x) {
  
  # time variable
  t_var <- x$fixef_vars[3]
  
  # nbr of unique val for t var
  t_obs <- x$fixef_sizes[t_var]
  
  return(format_number(t_obs))
}, "Number of Time Periods")

# Restricting
ids <- fread("../data/scratch/heatpump_ids.csv")

# Fit the model
max_week <- overall_weekly %>% filter(!is.na(gas_consumption))
max_week <- max(max_week$settlement_week)

m1 <- feols(elec_consumption ~ i(is_hp_installed) | avg_heating_degree + account_id + settlement_week, 
            data = overall_weekly %>% filter(account_id %in% ids$account_ids, settlement_week <= max_week) %>% inner_join(matched_data2 %>% distinct(account_id, weights)), 
            cluster = ~account_id)
m2 <- feols(gas_consumption ~ i(is_hp_installed) | avg_heating_degree + account_id + settlement_week, 
            data = overall_weekly %>% ungroup() %>% filter(account_id %in% ids$account_ids) %>% inner_join(matched_data2 %>% distinct(account_id, weights)), 
            cluster = ~account_id)
m3 <- feols(total_consumption ~ i(is_hp_installed) | avg_heating_degree + account_id + settlement_week, 
            data = overall_weekly %>% ungroup() %>% filter(account_id %in% ids$account_ids) %>% inner_join(matched_data2 %>% distinct(account_id, weights)), 
            cluster = ~account_id)


etable(m1,m2,m3, tex=TRUE, title = "TWFE using Matching Weights (Heatpump)",
       headers = list("Electricity", "Gas", "Total"), 
       fitstat = ~ N + g + pre_avg +t_obs + r2, file = "../tables/matching_hp.tex", replace = TRUE, label="tab:hp-matching")

CleanPreAverage("../tables/matching_hp.tex")
