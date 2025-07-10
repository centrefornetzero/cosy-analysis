# ===============================================
# COSY Reproduction Script
# ===============================================
# This script sets up the environment, loads data,
# runs the main analysis and produces outputs for
# the COSY project.
# ===============================================

# # -----------------------------
# # 1. Settings
# # -----------------------------
# # Toggle for faster testing
# random_subsample <- FALSE
# 
# # -----------------------------
# # 2. Package Setup
# # -----------------------------
# 
# # Function to check if a package is installed, and if not, install it
# install_if_needed <- function(package) {
#   if (!require(package, character.only = TRUE)) {
#     install.packages(package, dependencies = TRUE)
#     library(package, character.only = TRUE)
#   }
# }
# 
# # List of packages to load
# packages <- c(
#   "knitr", "kableExtra", "did", "fixest", "data.table", "lubridate", 
#   "dplyr", "ggplot2", "RColorBrewer", "tidyr", "scales", 
#   "forcats", "viridis",  "stringr", "stargazer", "panelView", "readxl", "HonestDiD"
# )
# 
# # Load (and install if needed) each package
# lapply(packages, install_if_needed)
# 

# -----------------------------
# 4. Variable Labels for Output
# -----------------------------

# set dictionary
setFixest_dict(c(total_consumption = "Consumption in kWh per period", 
                 daily_consumption = "Consumption in kWh per day",
                 consumption_hh = "Half Hourly Consumption in kWh",
                 share_consumption = "Share of daily consumption",
                 weekly_consumption = "Gas Consumption per Week in kWh",
                 cosy_contract_active = "Cosy Contract Active",
                 daily_avg_heating_degree = "HDD",
                 avg_heating_degree = "HDD",
                 hdd = "HDD",
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

# # -----------------------------
# # 5. Custom Styling
# # -----------------------------
# 
# # Custom colors from the provided image 
# flexible_color <- "#4C515C"  # Replace with the exact hex code for Flexible
# cosy_color <- "#5F8ED9"      # Replace with the exact hex code for Cosy
# 
# # -----------------------------
# # 6. Define main functions
# # -----------------------------

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
  
  # Function to format numbers with four decimal places
  format_decimal <- function(number, decimals = 4) {
    return(format(round(number, decimals), nsmall = decimals, big.mark = ",", scientific = FALSE))
  }

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
  
  # Function to format numbers
  format_number <- function(number) {
    return(format(round(number), big.mark = ",", scientific = FALSE))
  }
  
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

# fix dates import
base_as_date <- base::as.Date  # save original function

as.Date <- function(x, ...) {
  if (is.numeric(x)) {
    base_as_date(x, origin = "1970-01-01", ...)
  } else {
    base_as_date(x, ...)
  }
}

# -----------------------------
# 7. Data processing
# -----------------------------

source("scripts/02_01_load_data.R")

# List objects in the environment
list_env <- c(ls(), "list_env")

# ## Merging consumption and customers info datasets
# if(!file.exists("data/scratch/aggregated_data.RDS")) {
#   
#   # agreement data
#   # run queries/Cosy - agreement data
#   agreements <- fread("data/input/Cosy_-_agreement_data_2024_07_24.csv") %>%
#     filter(product_display_name == "Cosy Octopus") %>%
#     arrange(hashed_mpan, agreement_valid_from) %>%
#     mutate(
#       from = as.Date(agreement_valid_from),
#       to = as.Date(agreement_valid_to)
#     ) %>%
#     select(hashed_mpan, from, to) %>%
#     group_by(hashed_mpan) %>%
#     summarise(
#       periods = list(data.frame(from, to)),
#       first_adoption = min(from),  # Get the earliest agreement date for 'adoption'
#       first_week = format(min(from), "%Y-%U"),  # Format the first adoption date as year-week
#       .groups = 'drop'
#     )
#   
#   # files are created using 
#   # queries/cosy - cosy electricity readings
#   # queries/cosy - cosy electricity reading part 2 which I ran for different years seperately
#   aggregated_data <- rbind(fread("data/input/cosy_-_cosy_electricity_reading_part_2_2024_07_26.csv"),
#                            fread("data/input/cosy_-_cosy_electricity_reading_part_2_2024_07_26 (1).csv"),
#                            fread("data/input/cosy_-_cosy_electricity_reading_part_2_2024_07_26 (2).csv")) %>%
#     rename(total_consumption = total_read_value,
#            consumption_hh = mean_read_value) %>%
#     mutate(date = as.Date(settlement_date)) %>% 
#     filter(!is.na(date))     %>%
#     select(-c(settlement_date))
#   
#   # Function to check if a date falls within any period
#   check_active_contract <- function(date, periods) {
#     any(sapply(1:nrow(periods[[1]]), function(i) date >= periods[[1]][i, "from"] && (date <= periods[[1]][i, "to"] | is.na(periods[[1]][i, "to"]))))
#   }
#   
#   # Add indicator without heavy merging
#   consumption_with_indicator <- aggregated_data %>%
#     rowwise() %>%
#     mutate(
#       cosy_contract_active = {
#         periods <- agreements$periods[agreements$hashed_mpan == hashed_mpan]
#         if (length(periods) == 0) 0 else as.integer(check_active_contract(date, periods))
#       }
#     ) %>%
#     ungroup()
#   
#   # join with the earliest adoption date
#   aggregated_data <- consumption_with_indicator %>%
#     inner_join(agreements %>% select(-periods))
#   
#   # add overall
#   aggregated_data <- rbind(
#     aggregated_data, 
#     aggregated_data %>% 
#       group_by(account_id, hashed_mpan, date, cosy_contract_active, first_adoption, first_week) %>%
#       summarise(total_consumption = sum(total_consumption)) %>%
#       mutate(rate_period = "Overall",
#              consumption_hh = total_consumption/48)) %>%
#     mutate(rate_period = factor(rate_period, levels = c("Morning Cosy",
#                                                         "Afternoon Cosy",
#                                                         "Peak Rate",
#                                                         "Other", 
#                                                         "Overall")), 
#            weeks_since_cosy = floor(as.numeric(difftime(date, first_adoption, units = "weeks")))) 
#   
#   # Remove the ~ 50 mpans with 2 account id
#   duplicate_mpan <- aggregated_data %>%
#     select(account_id, hashed_mpan) %>%    # Selecting the necessary columns
#     distinct() %>%                         # Removing completely identical rows
#     count(hashed_mpan) %>%                 # Count occurrences of each hashed_mpan
#     filter(n > 1) %>%                      # Keep only those with more than one occurrence
#     left_join(aggregated_data %>% group_by(account_id, hashed_mpan) %>% summarise(min_date = min(date), max_date = max(date)), by = "hashed_mpan") %>%
#     arrange(hashed_mpan, account_id)       # Arrange for better visibility
#   
#   # add customers characteristics
#   # from queries/cosy - cosy details
#   cosy_cosy_details_2024_06_25 <- fread("data/input/cosy_-_cosy_details_2024_06_25.csv") %>%
#     distinct()
#   
#   # Remove moan associated with two accounts !
#   merged_data <- aggregated_data %>%
#     filter(!hashed_mpan %in% duplicate_mpan$hashed_mpan) %>%
#     inner_join(cosy_cosy_details_2024_06_25)
#   
#   # add weather
#   # queries/cosy analysis - weather
#   weather <- fread("data/input/Cosy Analysis Weather Mar 26 daily.csv") %>% 
#     rename_with(.cols = starts_with("weekly"), 
#                 .fn = ~ sub("^weekly", "daily", .)) %>%
#     mutate(date_day=as.Date(date_day, format = "%Y-%m-%d")) %>%
#     rename(date = date_day)
#   
#   aggregated_data <- merged_data %>%
#     left_join(weather, by =c("gsp_group_id", "date"))
#   
#   
#   Prev_contract <- fread("data/input/Cosy_-_agreement_data_2024_07_24.csv") %>%
#     arrange(hashed_mpan, as.Date(agreement_valid_from)) %>%
#     group_by(hashed_mpan) %>%
#     mutate(
#       previous_contract = lag(product_display_name),
#       previous_is_variable = lag(is_variable),
#       previous_is_charged_half_hourly = lag(is_charged_half_hourly),
#       is_cosy = product_display_name == "Cosy Octopus"
#     ) %>%
#     filter(is_cosy) %>%
#     slice_head(n=1)
#   
#   # EPC
#   aggregated_data <- aggregated_data %>%
#     mutate(epc_letter = case_when(
#       energy_efficiency >= 91 ~ "A",
#       energy_efficiency >= 81 & energy_efficiency <= 90 ~ "B",
#       energy_efficiency >= 69 & energy_efficiency <= 80 ~ "C",
#       energy_efficiency >= 55 & energy_efficiency <= 68 ~ "D",
#       energy_efficiency >= 39 & energy_efficiency <= 54 ~ "E",
#       energy_efficiency >= 21 & energy_efficiency <= 38 ~ "F",
#       energy_efficiency <= 20 ~ "G",
#       TRUE ~ NA_character_
#     ),
#     eac_mwh = estimated_annual_consumption/1000) %>%
#     left_join(Prev_contract) 
#   
#   # Temperature
#   aggregated_data <- aggregated_data %>% 
#     mutate(hdd = factor(
#       case_when(
#         daily_avg_air_temperature_celsius < 0 ~ 0,
#         daily_avg_air_temperature_celsius < 15.5 ~ round(daily_avg_air_temperature_celsius),
#         TRUE ~ 15
#       )
#     ))
# 
#   saveRDS(aggregated_data, "data/scratch/aggregated_data.RDS")
#   
#   rm(weather, adoption, consumption_with_indicator, agreements)
# } else {
#   aggregated_data <- readRDS("data/scratch/aggregated_data.RDS") 
# }
# 
# # Run on a subsample of the data for faster processing
# if (random_subsample) {
#   set.seed(123)
#   sampled_accounts <- sample(unique(aggregated_data$account_id), 1000)
#   aggregated_data <- aggregated_data %>% 
#     filter(account_id %in% sampled_accounts)
#   gc()
# }

source("scripts/02_02_rate_graphs.R")
rm(list = setdiff(ls(), list_env))

# ## Rates Graphs 
# 
# ### Figure 2: Cosy Rate by Period
# 
# # Load the prices
# rates <- fread("data/input/cosy_-_rate_analysis_2024_07_15.csv")  %>%
#   mutate(valid_from = as.Date(valid_from),
#          valid_to = as.Date(valid_to),
#          valid_from = ifelse(is.na(valid_from), as.Date("2022-12-13"), valid_from),
#          valid_to = ifelse(is.na(valid_to), as.Date("2024-07-15"), valid_to),
#          valid_from = as.Date(valid_from),
#          valid_to = as.Date(valid_to)
#   ) %>%
#   filter(!(valid_from == as.Date("2022-12-13") & valid_to == as.Date("2023-03-31")), !valid_from == "2024-06-30") 
# 
# # Add the typical marginal price as a reference column
# marginal_price <- rates %>%
#   filter(rate_start_at == "INTERVAL '07:00:00' HOUR TO SECOND") %>%
#   distinct(tariff_gsp_group_id, valid_from, unit_rate) %>%
#   rename(typical_marginal_price=unit_rate)
# 
# # Merge typical marginal price back into the full dataset
# rates <- rates %>%
#   left_join(marginal_price) %>%
#   mutate(share_of_typical = unit_rate / typical_marginal_price * 100)
# 
# plot_selection <- rates %>%
#   filter(tariff_gsp_group_name == "North Western",valid_from == "2022-12-13") 
# 
# # Get the unique valid_from date
# unique_valid_from <- unique(plot_selection$valid_from)
# 
# # Create a sequence of times for the single day in 1-minute intervals
# times <- seq(from = as.POSIXct(paste(unique_valid_from, "00:00:00")), 
#              to = as.POSIXct(paste(unique_valid_from, "23:59:00")), by = "1 min")
# 
# # Initialize rates with NA and group
# rate_data <- data.frame(
#   time = times,
#   rate = NA,
#   group = "Cosy"
# )
# 
# # Function to convert INTERVAL strings to times and apply the rates
# apply_rates <- function(data, rates) {
#   for (i in 1:nrow(data)) {
#     start_time <- as.POSIXct(paste(unique_valid_from, substr(data$rate_start_at[i], 11, 18)), format="%Y-%m-%d %H:%M:%S")
#     end_time <- as.POSIXct(paste(unique_valid_from, substr(data$rate_end_at[i], 11, 18)), format="%Y-%m-%d %H:%M:%S")
#     if (start_time > end_time) {
#       # Handle cases where the interval crosses midnight
#       rates$rate[rates$time >= start_time | rates$time < end_time] <- data$unit_rate[i]
#     } else {
#       rates$rate[rates$time >= start_time & rates$time < end_time] <- data$unit_rate[i]
#     }
#   }
#   return(rates)
# }
# 
# # Apply the rates using the most recent period
# rate_data <- apply_rates(plot_selection, rate_data)
# 
# # Create a data frame for Flexible Octopus with a constant rate
# flexible_octopus <- data.frame(
#   time = times,
#   rate = plot_selection[plot_selection$rate_start_at == "INTERVAL '07:00:00' HOUR TO SECOND",]$unit_rate,
#   group = "Typical Marginal Price"
# )
# 
# # Combine both data frames
# combined_rates <- rbind(rate_data, flexible_octopus)
# 
# # Define the specific rate values for y-axis breaks
# rate_values <- sort(unique(plot_selection$unit_rate))
# 
# # Define the time periods for shading
# shaded_times <- data.frame(
#   xmin = as.POSIXct(paste(unique_valid_from, c("04:00:00", "13:00:00", "16:00:00")), format="%Y-%m-%d %H:%M:%S"),
#   xmax = as.POSIXct(paste(unique_valid_from, c("07:00:00", "16:00:00", "19:00:00")), format="%Y-%m-%d %H:%M:%S"),
#   fill = c("red", "red", "lightblue")
# )
# 
# # Calculate the typical marginal price (you can adjust this based on your data)
# typical_marginal_price <- mean(combined_rates %>% filter(group == "Typical Marginal Price") %>% pull(rate), na.rm = TRUE)
# 
# # Add a new column for the share of the typical marginal price
# combined_rates <- combined_rates %>%
#   mutate(share_of_typical = rate / typical_marginal_price * 100)
# 
# # Plot the line chart with Flexible Octopus in dashed line and specific y-axis breaks
# # Assuming unique_valid_from is the date used in your 'time' sequence
# ggplot(combined_rates, aes(x = time, y = rate, color = group, linetype = group)) +
#   geom_rect(data = shaded_times, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
#             inherit.aes = FALSE, alpha = 0.2) +
#   geom_line(size = 1) +
#   labs(x = "Time of Day",
#        y = "Rate (p/kWh)",
#        color = "Tariff") +
#   scale_x_datetime(date_labels = "%H:%M", 
#                    date_breaks = "2 hour", 
#                    limits = c(as.POSIXct(min(combined_rates$time)), 
#                               as.POSIXct(max(combined_rates$time)- hours(1)))) +  # Set x-axis limits with correct date
#   scale_y_continuous(
#     name = "Rate (p/kWh)",
#     breaks = rate_values,
#     labels = scales::label_number(accuracy = 0.01),  # Format y-axis with 2 decimal places
#     sec.axis = sec_axis(~ . / typical_marginal_price, 
#                         name = "Share of Typical Marginal Price (%)", 
#                         labels = scales::percent_format(accuracy = 1))
#   ) +
#   theme_minimal() + 
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = c("Cosy" = cosy_color, "Typical Marginal Price" = flexible_color)) +
#   scale_linetype_manual(values = c("Cosy" = "solid", "Typical Marginal Price" = "dashed")) +
#   scale_fill_identity() +
#   guides(linetype = "none")
# 
# ggsave("graphs/Cosy Tariff.png", width = 10, height = 4, dpi = 300)
# 
# 
# # Load the prices
# rates <- fread("data/input/cosy_-_rate_analysis_2024_07_15.csv")  %>%
#   mutate(valid_from = as.Date(valid_from),
#          valid_to = as.Date(valid_to),
#          valid_from = ifelse(is.na(valid_from), as.Date("2022-12-13"), valid_from),
#          valid_to = ifelse(is.na(valid_to), as.Date("2024-07-15"), valid_to),
#          valid_from = as.Date(valid_from),
#          valid_to = as.Date(valid_to)
#   ) %>%
#   filter(!(valid_from == as.Date("2022-12-13") & valid_to == as.Date("2023-03-31")), !valid_from == "2024-06-30") 
# 
# # Add the typical marginal price as a reference column
# marginal_price <- rates %>%
#   filter(rate_start_at == "INTERVAL '07:00:00' HOUR TO SECOND") %>%
#   distinct(tariff_gsp_group_id, valid_from, unit_rate) %>%
#   rename(typical_marginal_price=unit_rate)
# 
# # Merge typical marginal price back into the full dataset
# rates <- rates %>%
#   left_join(marginal_price) %>%
#   mutate(share_of_typical = unit_rate / typical_marginal_price * 100)
# 
# plot_selection <- rates %>%
#   filter(tariff_gsp_group_name == "North Western",valid_from == "2024-03-31") 
# 
# # Get the unique valid_from date
# unique_valid_from <- unique(plot_selection$valid_from)
# 
# # Create a sequence of times for the single day in half-hour intervals
# times <- seq(from = as.POSIXct(paste(unique_valid_from, "00:00:00")), 
#              to = as.POSIXct(paste(unique_valid_from, "23:30:00")), by = "30 min")
# 
# # Initialize rates with NA and group
# rate_data <- data.frame(
#   time = times,
#   rate = NA,
#   group = "Cosy"
# )
# 
# # Function to convert INTERVAL strings to times and apply the rates
# apply_rates <- function(data, rates) {
#   for (i in 1:nrow(data)) {
#     start_time <- as.POSIXct(paste(unique_valid_from, substr(data$rate_start_at[i], 11, 18)), format="%Y-%m-%d %H:%M:%S")
#     end_time <- as.POSIXct(paste(unique_valid_from, substr(data$rate_end_at[i], 11, 18)), format="%Y-%m-%d %H:%M:%S")
#     if (start_time > end_time) {
#       # Handle cases where the interval crosses midnight
#       rates$rate[rates$time >= start_time | rates$time < end_time] <- data$unit_rate[i]
#     } else {
#       rates$rate[rates$time >= start_time & rates$time < end_time] <- data$unit_rate[i]
#     }
#   }
#   return(rates)
# }
# 
# # Apply the rates using the most recent period
# rate_data <- apply_rates(plot_selection, rate_data)
# 
# # Create a data frame for Flexible Octopus with a constant rate
# flexible_octopus <- data.frame(
#   time = times,
#   rate = plot_selection[plot_selection$rate_start_at == "INTERVAL '07:00:00' HOUR TO SECOND",]$unit_rate,
#   group = "Typical Marginal Price"
# )
# 
# # Combine both data frames
# combined_rates <- rbind(rate_data, flexible_octopus)
# 
# # Define the specific rate values for y-axis breaks
# rate_values <- sort(unique(plot_selection$unit_rate))
# 
# # Define the time periods for shading
# shaded_times <- data.frame(
#   xmin = as.POSIXct(paste(unique_valid_from, c("04:00:00", "13:00:00", "16:00:00")), format="%Y-%m-%d %H:%M:%S"),
#   xmax = as.POSIXct(paste(unique_valid_from, c("07:00:00", "16:00:00", "19:00:00")), format="%Y-%m-%d %H:%M:%S"),
#   fill = c("lightblue", "lightblue", "red")
# )
# 
# # Calculate the typical marginal price (you can adjust this based on your data)
# typical_marginal_price <- mean(combined_rates %>% filter(group == "Typical Marginal Price") %>% pull(rate), na.rm = TRUE)
# 
# # Add a new column for the share of the typical marginal price
# combined_rates <- combined_rates %>%
#   mutate(share_of_typical = rate / typical_marginal_price * 100)
# 
# # Plot the line chart with Flexible Octopus in dashed line and specific y-axis breaks
# ggplot(combined_rates, aes(x = time, y = rate, color = group, linetype = group)) +
#   geom_rect(data = shaded_times, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
#             inherit.aes = FALSE, alpha = 0.2) +
#   geom_line(size = 1) +
#   labs(x = "Time of Day",
#        y = "Rate (p/kWh)",
#        color = "Tariff") +
#   scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hour") +
#   scale_y_continuous(
#     name = "Rate (p/kWh)",
#     breaks = rate_values,
#     labels = scales::label_number(accuracy = 0.01),  # Format y-axis with 2 decimal places
#     sec.axis = sec_axis(~ . / typical_marginal_price, 
#                         name = "Share of Typical Marginal Price (%)", 
#                         labels = scales::percent_format(accuracy = 1))
#   ) +
#   theme_minimal() + 
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = c("Cosy" = cosy_color, "Typical Marginal Price" = flexible_color)) +
#   scale_linetype_manual(values = c("Cosy" = "solid", "Typical Marginal Price" = "dashed")) +
#   scale_fill_identity() +
#   guides(linetype = "none")
# 
# 
# 
# 
# ### Figure A.16: Rates by Rate Period and GSP Group as of 01 June 2024
# 
# # Create rate_period indicator
# rates <- rates %>%
#   mutate(rate_period = case_when(
#     rate_start_at == "INTERVAL '04:00:00' HOUR TO SECOND" ~ "Morning \n & Afternoon Cosy",
#     rate_start_at == "INTERVAL '07:00:00' HOUR TO SECOND" ~ "Other \n ( ~ Typical Marginal Price)",
#     rate_start_at == "INTERVAL '13:00:00' HOUR TO SECOND" ~ "Morning \n & Afternoon Cosy",
#     rate_start_at == "INTERVAL '16:00:00' HOUR TO SECOND" ~ "Peak Rate",
#     rate_start_at == "INTERVAL '19:00:00' HOUR TO SECOND" ~ "Other \n ( ~ Typical Marginal Price)",
#     TRUE ~ "Other"
#   ),
#   rate_period = factor(rate_period, levels = c("Morning \n & Afternoon Cosy","Other \n ( ~ Typical Marginal Price)", "Peak Rate")))
# 
# # Get the number of unique GSP group names
# num_gsp_groups <- length(unique(rates$tariff_gsp_group_name))
# 
# # Define a color palette using RColorBrewer and colorRampPalette to generate more colors if needed
# palette <- colorRampPalette(brewer.pal(12, "Set3"))(num_gsp_groups)
# 
# # Add the typical marginal price as a reference column
# rates_selection <- rates %>%
#   filter(valid_from == "2024-03-31") 
# 
# 
# # Part 1: Bar graph showing all the rates by rate_period and GSP group name
# ggplot(rates_selection, aes(x = rate_period, y = unit_rate, fill = tariff_gsp_group_name)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Rate Period",
#        y = "Rate (p/kWh)",
#        fill = "GSP Group") +
#   scale_y_continuous(
#     name = "Rate (p/kWh)",
#     labels = scales::label_number(accuracy = 0.01),  # Format y-axis with 2 decimal places
#     sec.axis = sec_axis(~ . / typical_marginal_price, 
#                         name = "Share of Typical Marginal Price (%)", 
#                         labels = scales::percent_format(accuracy = 1))
#   ) +
#   theme_minimal() +
#   scale_fill_manual(values = palette) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# ggsave("graphs/Rates_by_Rate_Period_and_GSP_Group.png", width = 10, height = 6, dpi = 300)
# 
# 
# 
# ### Figure A.17: Rates Over Time
# 
# # Get the global min and max unit_rate
# global_min_rate <- min(rates$unit_rate, na.rm = TRUE)
# global_max_rate <- max(rates$unit_rate, na.rm = TRUE)
# 
# # Part 2: Line graphs showing the change in rates over time for each rate_period, grouped by region
# rates_long <- rates %>%
#   group_by(tariff_gsp_group_name, rate_period) %>%
#   arrange(valid_from)
# 
# # Create annotations data frame
# annotations <- rates_long %>%
#   group_by(rate_period) %>%
#   summarise(
#     valid_from = as.Date("2023-01-01"),
#     share_of_typical = median(share_of_typical, na.rm = TRUE),
#     unit_rate = median(unit_rate, na.rm = TRUE),
#     tariff_gsp_group_name = unique(tariff_gsp_group_name)
#   )
# 
# ggplot(rates_long, aes(x = valid_from, y = unit_rate, color = tariff_gsp_group_name, group = interaction(tariff_gsp_group_name, rate_period))) +
#   geom_line(size = 1) +
#   labs(
#     x = "Date",
#     y = "Rate (p/kWh)",
#     color = "GSP Group") +
#   theme_minimal() +
#   scale_color_manual(values = palette) +
#   ylim(global_min_rate, global_max_rate) +
#   geom_text(data = annotations, aes(label = rate_period), vjust = 0.8, hjust = 0, color = "grey")
# 
# 
# ggsave("graphs/Rate_Changes_by_Period.png", width = 12, height = 8, dpi = 300)
# 
# 
# 
# 
# ### Figure A.18: Rates as Share of Typical Marginal Price Over Time
# 
# # Aggregate the data by rate_period and valid_from to get the average for all GSP groups
# rates_avg <- rates %>%
#   group_by(rate_period, valid_from) %>%
#   summarise(
#     avg_unit_rate = mean(unit_rate, na.rm = TRUE),
#     avg_share_of_typical = mean(share_of_typical, na.rm = TRUE)
#   )
# 
# # Create annotations data frame
# annotations <- rates_avg %>%
#   group_by(rate_period) %>%
#   summarise(
#     valid_from = as.Date("2023-01-01"),
#     avg_unit_rate = median(avg_unit_rate, na.rm = TRUE),
#     avg_share_of_typical = median(avg_share_of_typical, na.rm = TRUE)
#   )
# 
# # Define specific y-axis breaks for share of typical marginal price
# share_breaks <- seq(0, 160, 20)
# 
# # Define a palette of blue colors
# blue_palette <- scales::brewer_pal(palette = "Blues")(length(unique(rates_avg$rate_period)))
# 
# # Plot average share of typical marginal price over time
# ggplot(rates_avg, aes(x = valid_from, y = avg_share_of_typical, color = rate_period, group = rate_period)) +
#   geom_line(size = 1) +
#   labs(
#     x = "Date",
#     y = "Average Share of Typical Marginal Price (%)",
#     color = "Rate Period"
#   ) +
#   theme_minimal() +
#   scale_color_manual(values = blue_palette) +
#   scale_y_continuous(
#     name = "Average Share of Typical Marginal Price (%)",
#     breaks = share_breaks,
#     labels = scales::percent_format(accuracy = 0.1, scale = 1)
#   ) + 
#   theme(legend.position = "bottom") +
#   geom_text(data = annotations, aes(label = rate_period), vjust = 1.2, hjust = 0, color = "grey")
# 
# 
# ggsave("graphs/Average_Share_of_Typical_Marginal_Price_by_Period.png", width = 12, height = 8, dpi = 300)


source("scripts/02_03_summary_graphs.R")
list_env <- c(list_env, "contract_analysis")
rm(list = setdiff(ls(), list_env))


# ## Summary Statistics Tables and Graphs {#sec:sumstats}
# Next_contract <- fread("data/input/Cosy_-_agreement_data_2024_07_24.csv") %>%
#   arrange(hashed_mpan, desc(as.Date(agreement_valid_from))) %>%
#   group_by(hashed_mpan) %>%
#   mutate(na_flag = ifelse(is.na(agreement_valid_to), 1, 0),
#          na_cumsum = cumsum(na_flag)) %>%
#   filter(na_cumsum == 1) %>%
#   select(-na_flag, -na_cumsum) %>%
#   ungroup()%>%
#   group_by(hashed_mpan) %>%
#   slice(1) %>%
#   mutate(is_variable = ifelse(product_display_name %in% 
#                                 c("Co-op Flexible",
#                                   "Flexible Avro",
#                                   "Flexible Octopus",
#                                   "Flexible Octopus Smart Pay as You Go",
#                                   "Loyal Flexible Octopus Smart Pay as You Go"), FALSE, is_variable)) %>%
#   group_by(is_charged_half_hourly) %>%
#   tally() %>%
#   ungroup() %>%
#   mutate(share_is_variable = n/sum(n))
# 
# # Contract before cosy
# first_cosy_contracts <- fread("data/input/Cosy_-_agreement_data_2024_07_24.csv") %>%
#   arrange(hashed_mpan, as.Date(agreement_valid_from)) %>%
#   group_by(hashed_mpan) %>%
#   mutate(
#     previous_contract = lag(product_display_name),
#     previous_is_variable = lag(is_variable),
#     previous_is_charged_hh = lag(is_charged_half_hourly),
#     is_cosy = product_display_name == "Cosy Octopus"
#   ) %>%
#   filter(is_cosy) %>%
#   slice_head(n = 1) %>%
#   mutate(previous_is_variable = ifelse(previous_contract %in% 
#                                          c("Co-op Flexible",
#                                            "Flexible Avro",
#                                            "Flexible Octopus",
#                                            "Flexible Octopus Smart Pay as You Go",
#                                            "Loyal Flexible Octopus Smart Pay as You Go"), 
#                                        FALSE, 
#                                        previous_is_variable)) %>%
#   filter(!is.na(previous_is_variable)) %>%
#   group_by(previous_is_charged_hh) %>%
#   tally() %>%
#   mutate(share = 100*n/sum(n)) %>%
#   arrange(share)
# 
# 
# ### Figure 3: Weekly Adoption of the Cosy tariff
# # Prepare the data
# weekly_adoptions <- aggregated_data %>%
#   ungroup() %>%
#   select(hashed_mpan, first_adoption) %>%
#   distinct() %>%
#   mutate(first_week = floor_date(first_adoption, "week")) %>%
#   group_by(first_week) %>%
#   summarise(adoptions = n())
# 
# # Define the date for the announcement
# announcement_date <- as.Date("2023-08-31")
# 
# # Choose a color from the Brewer palette for the text annotation
# text_color <- brewer.pal(n = 3, name = "Set1")[1]
# 
# # Create the plot with the vertical line and adjusted annotation
# ggplot(weekly_adoptions, aes(x = first_week, y = adoptions)) +
#   geom_line(color = cosy_color) +  # Line plot for trends with a color from the Brewer palette
#   geom_point(color = cosy_color) +  # Points to highlight individual data with the same color
#   geom_vline(xintercept = as.numeric(announcement_date), linetype = "dashed", color = text_color) +  # Vertical line for the announcement
#   annotate("text", x = announcement_date - weeks(1), y = 150,
#            label = "Announcement:\nBoiler Upgrade Scheme\nincrease to £7,500", hjust = 1, color = text_color) +  # Annotate the vertical line
#   labs(
#     x = "Week",
#     y = "Customers switching to Cosy"
#   ) +
#   scale_x_date(
#     labels = scales::date_format("%b %y"),  # Formatting months and years
#     date_breaks = "1 month"  # Adjust this based on your data density
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position="none",
#     axis.text.x = element_text(angle = 45, hjust = 1)  # Improve readability by rotating labels
#   )
# 
# # Save the plot
# ggsave("graphs/weekly_adoptions.png", width = 16, height = 8, units = "cm")
# 
# 
# # Analyze contracts
# contract_analysis_all <-fread("data/input/Cosy_-_agreement_data_2024_07_24.csv") 
# 
# contract_analysis <- contract_analysis_all %>%
#   inner_join(aggregated_data %>% distinct(account_id, hashed_mpan)) %>%
#   filter(product_display_name == "Cosy Octopus") %>%
#   arrange(account_id, hashed_mpan, agreement_valid_from) %>%
#   mutate(
#     from = as.Date(agreement_valid_from),
#     to = as.Date(agreement_valid_to)
#   ) %>%
#   select(account_id, hashed_mpan, from, to) %>%
#   group_by(account_id) %>%
#   summarise(
#     num_contracts = n(),  # Count number of contracts per customer
#     ongoing = sum(is.na(to)),  # Count how many contracts are ongoing
#     ended = sum(!is.na(to))  # Count how many contracts have ended
#   ) %>%
#   mutate(
#     category = case_when(
#       num_contracts == 1 & ongoing == 1 ~ "Stayed on Cosy (ongoing)",
#       num_contracts == 1 & ended == 1 ~ "Tried then switched",
#       num_contracts > 1 ~ "Multiple contracts",
#       TRUE ~ "Other"  # Catch-all for any other cases
#     )
#   )
# 
# # Count each category
# category_counts <- contract_analysis %>%
#   count(category)
# 
# print(category_counts)
# 
# 

source("scripts/02_04_data_availability.R")
rm(list = setdiff(ls(), list_env))

source("scripts/02_05_balance_table.R")
rm(list = setdiff(ls(), list_env))

source("scripts/02_06_lct_ownership_and_leavers.R")
rm(list = setdiff(ls(), list_env))

# # Empirical Analysis {#sec:results}
# 
# ## TWFE Heterogeneity Analysis
# 
# ### Table A.9: Cosy Adoption on Electricity Consumption Controlling for EV Charging
# # ev half hours 
# # Read the CSV file
# ev_charging <- fread("data/input/cosy_-_ev_detection_2024_07_04.csv") %>%
#   mutate(ev_charging = 1,
#          date = as.Date(interval_start),
#          interval_start = as.POSIXct(interval_start, format="%Y-%m-%d %H:%M:%S"),
#          hour = as.integer(format(interval_start, "%H")),
#          rate_period = case_when(
#            hour >= 4 & hour < 7 ~ "Morning Cosy",
#            hour >= 13 & hour < 16 ~ "Afternoon Cosy",
#            hour >= 16 & hour < 19 ~ "Peak Rate",
#            TRUE ~ "Other"
#          )
#   )
# 
# # Aggregate at the account id, mpan, date and rate period level
# ev_charging_agg <- rbind(ev_charging %>%
#                            group_by(account_id,  hashed_mpan, date, rate_period) %>%
#                            tally(),
#                          ev_charging %>%
#                            group_by(account_id,  hashed_mpan, date) %>%
#                            tally() %>% 
#                            mutate(rate_period="Overall")) %>%
#   rename(ev_charging=n)
# 
# # EV users details
# ev_users <- ev_charging %>%
#   group_by(account_id) %>%
#   summarise(is_ev_detected= min(as.Date(interval_start)))
# 
# # Update hp_installed with the new ev_charging values using case_when
# aggregated_data <- aggregated_data %>%
#   left_join(ev_charging_agg) %>%
#   mutate(ev_charging = ifelse(is.na(ev_charging), 0, ev_charging),
#          ev_charging = case_when(
#            rate_period == "Overall" ~ ev_charging / 48,
#            rate_period == "Other" ~ ev_charging / 30,
#            TRUE ~ ev_charging / 6
#          ),
#          rate_period = factor(rate_period, levels = c("Morning Cosy",
#                                                       "Afternoon Cosy",
#                                                       "Peak Rate",
#                                                       "Other", 
#                                                       "Overall"))) %>%
#   left_join(ev_users) %>%
#   mutate(has_ev = as.numeric(is_ev_detected <= date),
#          has_ev = ifelse(is.na(has_ev), 0, has_ev)) %>%
#   distinct(account_id, date, rate_period, .keep_all=TRUE)
# 
# # Fit the model
# m1c <- feols(consumption_hh ~ i(cosy_contract_active, ref=0) + has_ev + i(cosy_contract_active, has_ev, ref=0) | 
#                hdd + account_id + date, 
#              data = aggregated_data, 
#              cluster = ~account_id, 
#              split = ~ rate_period)
# 
# etable( m1c, cluster = ~ account_id + date)
# 
# # Generate the initial LaTeX table
# etable(m1c, tex = TRUE, title = "Cosy Adoption on Electricity Consumption Controlling for EV Charging", 
#        fitstat = ~ N + g + pre_avg + t_obs + r2, 
#        file = "tables/did_ev.tex", replace = TRUE, label = "tab:hp-did-ev")
# CleanPreAverage("tables/did_ev.tex")
# 
# 
# ###  Table 3: Cosy Adoption on Probability of Charging EV by Period
# 
# # Identify the period with the highest EV charging for each mpan and date
# ev_charging_max <- ev_charging %>%
#   group_by(account_id, hashed_mpan, date, rate_period) %>%
#   summarise(ev_charging = sum(ev_charging, na.rm = TRUE)) %>%
#   group_by(account_id, hashed_mpan, date) %>%
#   filter(ev_charging == max(ev_charging)) %>%
#   mutate(highest_ev_charging = 1) %>%
#   ungroup()
# 
# ev_charging_max <- ev_charging_max %>%
#   left_join(aggregated_data %>% select(account_id, hashed_mpan, date, cosy_contract_active) %>% distinct()) 
# 
# # Create dummy variables for rate periods
# ev_charging_max <- ev_charging_max %>%
#   mutate(
#     Morning_Cosy = ifelse(rate_period == "Morning Cosy", 1, 0),
#     Afternoon_Cosy = ifelse(rate_period == "Afternoon Cosy", 1, 0),
#     Peak_Rate = ifelse(rate_period == "Peak Rate", 1, 0),
#     Other = ifelse(rate_period == "Other", 1, 0)
#   )
# 
# # Run the fixed effects models
# m_charging1 <- feols(Morning_Cosy ~ i(cosy_contract_active) | account_id + date, data = ev_charging_max, cluster = ~ account_id)
# m_charging2 <- feols(Afternoon_Cosy ~ i(cosy_contract_active) | account_id + date, data = ev_charging_max, cluster = ~ account_id)
# m_charging3 <- feols(Peak_Rate ~ i(cosy_contract_active) | account_id + date, data = ev_charging_max, cluster = ~ account_id)
# m_charging4 <- feols(Other ~ i(cosy_contract_active) | account_id + date, data = ev_charging_max, cluster = ~ account_id)
# 
# 
# # Generate the LaTeX table with the dependent variable named "Charging EV"
# etable(m_charging1, m_charging2, m_charging3, m_charging4, 
#        tex = TRUE, 
#        title = "Cosy Adoption on Probability of Charging EV by Period", 
#        headers = c("Morning Cosy", "Afternoon Cosy", "Peak Rate", "Other"),
#        fitstat = ~ N + g + pre_avg + r2, 
#        file = "tables/ev_charging.tex", 
#        replace = TRUE, 
#        label = "tab:ev-charging",
#        dict = c(Morning_Cosy = "Charging EV", 
#                 Afternoon_Cosy = "Charging EV", 
#                 Peak_Rate = "Charging EV", 
#                 Other = "Charging EV"))
# 
# file_path <- "tables/ev_charging.tex"
# 
# # Read the generated LaTeX file
# file_content <- readLines(file_path)
# 
# # Find the lines with the pre-treatment average and remove them
# if (length(grep("Charging EV", file_content))==1) {
#   pre_avg_line_index <- grep("Charging EV", file_content)
# } else {
#   pre_avg_line_index <- grep("Charging EV", file_content)[2]
# }
# 
# pre_avg_lines <- file_content[pre_avg_line_index:(pre_avg_line_index)]
# file_content <- file_content[-c(pre_avg_line_index, pre_avg_line_index)]
# 
# # Find the position just after the coefficients
# coeff_end_index <- grep("Fixed-effects", file_content) -2
# 
# # Insert the pre-treatment average row after the coefficients
# file_content <- append(file_content, pre_avg_lines, after = coeff_end_index)
# file_content <- append(file_content, "\\emph{Pre-Treatment Average}\\\\", after = coeff_end_index)
# 
# # Add a \midrule after the pre-treatment average
# file_content <- append(file_content, "\\midrule", after = coeff_end_index)
# 
# # Modify the label for "Size of the 'effective' sample" to "Number of Households"
# sample_line <- grep("Size of the 'effective' sample", file_content)
# file_content[sample_line] <- gsub("Size of the 'effective' sample", "Number of Households", file_content[sample_line])
# 
# # Modify the name of the dependent in pre-treatment averages
# var_line <- grep("Half Hourly Consumption", file_content)
# file_content[sample_line] <- gsub("Half Hourly Consumption", "Charging EV", file_content[var_line])
# 
# # Add note
# note <- "\\floatfoot{\\justifying \\footnotesize \\upshape \\textbf{Note:} We show the results of four OLS models where the dependent variable is whether a charging event occurred in the period of interest – morning \\textit{Cosy} 4am-7am (column 1), afternoon \\textit{Cosy} 1pm-4pm (column 2), peak 4pm-7pm (column 3), and all other hours of the day (column 4). The sample is 127,789 charging events among 1,743 \\textit{Cosy} adopters for whom we detect evidence of EV charging. Where a charging events stretches across multiple periods, we attribute it to the period that comprises the \\textit{majority} of the event (in minutes). We see that among these EV owning \\textit{Cosy} adopters, \\textit{Cosy} adoption is associated with more charging the off-peak period and less in the peak and other periods.}"
# 
# file_content <- append(file_content, note, after = grep("\\centering", file_content)-1)
# 
# # Write the modified content back to the LaTeX file
# writeLines(file_content, file_path)
# 


# ### Table A.10: Impact of Cosy for Leavers
# # Function to create the ggplot for each period
# create_ggplot <- function(period_data, period_name) {
#   # Extract coefficients, standard errors, and event time
#   coefficients <- period_data$att.egt   # Convert to daily values
#   standard_errors <- period_data$se.egt   # Convert to daily values
#   event_time <- period_data$egt
#   
#   # Create a data frame for plotting
#   plot_data <- data.frame(
#     event_time = event_time,
#     coefficient = coefficients,
#     lower_ci = coefficients - 1.96 * standard_errors,
#     upper_ci = coefficients + 1.96 * standard_errors,
#     period = ifelse(event_time < 0, "No", "Yes")
#   )
#   
#   # Reverse the color order
#   plot_data$period <- factor(plot_data$period, levels = c("Yes", "No"))
#   
#   # Create the ggplot
#   p <- ggplot(plot_data, aes(x = event_time, y = coefficient, color = period)) +
#     geom_point() +
#     geom_line() +
#     geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, , alpha = 0.6) +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#     scale_color_manual(
#       name = "Has Adopted Cosy", 
#       labels = c("No" = "No", "Yes" = "Yes"),
#       values = c("No" = flexible_color, "Yes" = cosy_color)  # Custom colors
#     ) +
#     # scale_x_continuous(breaks = seq(-50, 50, 10)) +
#     labs(
#       x = "Weeks since adoption",
#       y = "Dynamic ATT for Half Hourly Consumption im kWh"
#     ) +
#     theme_minimal()
#   
#   return(p)
# }
# 
# # Identify the cases where cosy_contract_active switches from 1 to 0
# aggregated_data <- aggregated_data %>%
#   mutate(leavers = (account_id %in% contract_analysis[!contract_analysis$category == "Stayed on Cosy (ongoing)",]$account_id))
# 
# # Identify when they leave
# leave_date <- aggregated_data  %>%
#   filter(leavers == 1) %>%
#   mutate(no_active_contract = as.numeric(cosy_contract_active==0 & date >= first_adoption)) %>% 
#   ungroup() %>%
#   group_by(account_id) %>%
#   filter(no_active_contract==1) %>%
#   summarise(leave_date = min(date, na.rm = TRUE))
# 
# # Create weeks since leaving
# start_date <- min(leave_date$leave_date)
# 
# # Create the data for CS
# leavers_did <- aggregated_data %>%
#   inner_join(leave_date) %>%
#   ungroup() %>%
#   mutate(
#     # Calculate the difference in weeks from the start_date
#     week = as.numeric(difftime(date, start_date, units = "weeks")) %/% 1 + 1,
#     # Assuming you have a way to determine 'firstweek', adjust similarly if needed
#     firstweek = as.numeric(difftime(leave_date, start_date, units = "weeks")) %/% 1 + 1) %>%
#   group_by(hashed_mpan, firstweek, week, rate_period) %>%
#   summarise(consumption_hh = mean(consumption_hh)) %>%
#   mutate(
#     id = as.numeric(hashed_mpan),
#     firstweek = as.numeric(firstweek),
#     week = as.numeric(week)
#   )
# 
# m_leavers <- feols(consumption_hh ~ i(cosy_contract_active, ref=0) + i(cosy_contract_active, leavers, ref=0, ref2=0)  | hdd + account_id + date, 
#                    data = aggregated_data, 
#                    cluster = ~account_id, 
#                    split = ~ rate_period)
# 
# etable(m_leavers, tex=TRUE, title = "Impact of Cosy for Leavers",
#        fitstat = ~ N + g + pre_avg +t_obs + r2, file = "tables/did_leavers.tex", replace = TRUE, label="tab:did-leavers")
# CleanPreAverage("tables/did_leavers.tex")
# 
# leavers_ev <- aggregated_data %>%
#   distinct(account_id, leavers) %>%
#   left_join(ev_users) %>%
#   mutate(has_ev = (!is.na(is_ev_detected)))
# 
# 
# # Group by leavers and has_ev, then count
# grouped_data <- leavers_ev %>%
#   group_by(leavers, has_ev) %>%
#   summarise(count = n()) %>%
#   ungroup() %>%
#   group_by(leavers) %>%
#   mutate(proportion = count / sum(count))
# 
# # Plot the bar plot
# ggplot(grouped_data, aes(x = as.factor(leavers), y = proportion, fill = as.factor(has_ev))) +
#   geom_bar(stat = "identity", position = "stack") +
#   scale_fill_manual(values = c("skyblue", "orange"), labels = c("No EV", "Has EV")) +
#   scale_y_continuous(labels = scales::percent) +
#   labs(x = "Leavers", y = "Proportion", fill = "EV Status") +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# ggsave(filename= "graphs/leavers_ev.png",width = 10, height = 8, dpi = 300)



#% 0.1731 in survey

# ### Table A.11: Impact of Cosy by LCTs Ownership
# survey_responses <- fread("data/input/cosy_-_smart_tariff_survey_2024_09_12.csv")
# 
# # Step 1: Clean and split 'all_lcts' column without modifying original data
# cleaned_lcts <- survey_responses$all_lcts %>%
#   str_remove_all("[\\[\\]\"]") %>%
#   str_split(",")
# 
# # Step 2: Find unique values across all rows
# unique_values <- cleaned_lcts %>%
#   unlist() %>%
#   str_trim() %>%
#   unique()
# 
# # Step 3: Create new columns for each unique value
# for (val in unique_values) {
#   # Add column with 1 if the value is present in the row, 0 otherwise, without changing original df
#   survey_responses[[val]] <- sapply(cleaned_lcts, function(x) ifelse(val %in% x, 1, 0))
# }
# 
# survey_responses <- survey_responses %>%
#   mutate(`Has EV` = as.numeric(!has_ev == ""),
#          `Has EV Charger` = as.numeric(!has_charger_ev=="")) %>%
#   select(-c(has_ev, has_charger_ev, charging_method))
# 
# # View the resulting dataframe
# summary(survey_responses)
# 
# df <- aggregated_data %>%
#   select(account_id, consumption_hh, cosy_contract_active, date, hdd, rate_period) %>%
#   inner_join(survey_responses %>% 
#                rename(`Has Solar PV` = `Solar panels or other microgeneration`)) %>%
#   filter(account_id %in% unique(survey_responses$account_id))
# 
# 
# # Run the regression model
# m1_filtered <- feols(consumption_hh ~ i(cosy_contract_active, ref=0) |
#                        account_id + date + hdd,
#                      data = df,
#                      split = ~ rate_period,
#                      cluster = ~account_id)
# tempreg <- feols(consumption_hh ~ i(cosy_contract_active) +
#                    i(cosy_contract_active, `Home battery`, ref=0) +
#                    i(cosy_contract_active, `Has Solar PV`, ref=0) +
#                    i(cosy_contract_active, `Has EV`, ref=0) |
#                    account_id + date + hdd,
#                  data = df,
#                  split = ~ rate_period,
#                  cluster = ~account_id)
# etable(m1_filtered)
# 
# etable(tempreg, tex=TRUE, title = "Impact of Cosy by LCTs Ownership",
#        fitstat = ~ N + g + pre_avg +t_obs + r2, file = "tables/did_lcts.tex", replace = TRUE, label="tab:did-lcts")
# 
# CleanPreAverage("tables/did_lcts.tex")
# 
# # Step 2: Create combinations of EV, Solar, and Battery and count the occurrences
# survey_responses_filtered <- survey_responses %>%
#   filter(account_id %in% unique(aggregated_data$account_id)) 
# 
# lct_matrix <-survey_responses_filtered %>%
#   group_by(`Has EV`, `Solar panels or other microgeneration`, `Home battery`) %>%
#   summarise(count = n()) %>%
#   ungroup() %>%
#   mutate(share = count / sum(count))
# 
# # Step 3: Create readable labels for combinations
# lct_matrix_wide <- lct_matrix %>%
#   mutate(
#     # Combine only the values that exist, ignoring any empty or missing LCTs
#     Combination = trimws(paste(
#       ifelse(`Has EV` == 1, "EV", ""),
#       ifelse(`Home battery` == 1, "Battery", ""),
#       ifelse(`Solar panels or other microgeneration` == 1, "Solar", "")
#     )),
#     # Remove any trailing/leading spaces and '+' when no tech is present
#     Combination = gsub("\\s+", " + ", Combination),  # Ensures proper spacing
#     Combination = gsub("^\\s*\\+\\s*", "", Combination),  # Removes leading '+'
#     Combination = gsub("\\s*\\+\\s*$", "", Combination),  # Removes trailing '+'
#     # If nothing is in the combination, label it as "No other LCT"
#     Combination = ifelse(Combination == "", "No other LCT", Combination)
#   ) %>%
#   arrange(desc(share))
# 
# # Step 4: Create a bar plot with ColorBrewer and no borders
# ggplot(lct_matrix_wide, aes(x = reorder(Combination, -share), y = share, fill = Combination)) +
#   geom_bar(stat = "identity") +  # No border around bars
#   coord_flip() +  # Flip coordinates for easier reading
#   scale_fill_brewer(palette = "Set3") +  # Use ColorBrewer scheme
#   scale_y_continuous(labels = scales::percent_format()) +
#   labs(
#     x = " ",
#     y = paste0("Proportion of Sample (%) [N=", dim(survey_responses_filtered)[1], ']')
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "none")  # Remove legend
# 
# ggsave("graphs/lct_combinaison.png",
#        width = 16, height = 8, units = "cm")
# 
# 
# # Step 1: Calculate the share of each LCT
# lct_summary <- survey_responses_filtered %>%
#   summarise(
#     `Home battery (%)` = mean(`Home battery`) * 100,
#     `Solar PV (%)` = mean(`Solar panels or other microgeneration`) * 100,
#     `EV (%)` = mean(`Has EV`) * 100
#   ) %>%
#   pivot_longer(cols = everything(), names_to = "LCT", values_to = "Share")
# 



# 
# ### Pre-trends checks
# honest_did <- function(es,
#                        e          = 0,
#                        type       = c("smoothness", "relative_magnitude"),
#                        gridPoints = 100,
#                        ...) {
#   
#   type <- match.arg(type)
#   
#   # Make sure that user is passing in an event study
#   if (es$type != "dynamic") {
#     stop("need to pass in an event study")
#   }
#   
#   # Check if used universal base period and warn otherwise
#   if (es$DIDparams$base_period != "universal") {
#     stop("Use a universal base period for honest_did")
#   }
#   
#   # Recover influence function for event study estimates
#   es_inf_func <- es$inf.function$dynamic.inf.func.e
#   
#   # Recover variance-covariance matrix
#   n <- nrow(es_inf_func)
#   V <- t(es_inf_func) %*% es_inf_func / n / n
#   
#   # Check time vector is consecutive with referencePeriod = -1
#   referencePeriod <- -1
#   consecutivePre  <- !all(diff(es$egt[es$egt <= referencePeriod]) == 1)
#   consecutivePost <- !all(diff(es$egt[es$egt >= referencePeriod]) == 1)
#   if ( consecutivePre | consecutivePost ) {
#     msg <- "honest_did expects a time vector with consecutive time periods;"
#     msg <- paste(msg, "please re-code your event study and interpret the results accordingly.", sep="\n")
#     stop(msg)
#   }
#   
#   # Remove the coefficient normalized to zero
#   hasReference <- any(es$egt == referencePeriod)
#   if ( hasReference ) {
#     referencePeriodIndex <- which(es$egt == referencePeriod)
#     V    <- V[-referencePeriodIndex,-referencePeriodIndex]
#     beta <- es$att.egt[-referencePeriodIndex]
#   } else {
#     beta <- es$att.egt
#   }
#   
#   nperiods <- nrow(V)
#   npre     <- sum(1*(es$egt < referencePeriod))
#   npost    <- nperiods - npre
#   if ( !hasReference & (min(c(npost, npre)) <= 0) ) {
#     if ( npost <= 0 ) {
#       msg <- "not enough post-periods"
#     } else {
#       msg <- "not enough pre-periods"
#     }
#     msg <- paste0(msg, " (check your time vector; note honest_did takes -1 as the reference period)")
#     stop(msg)
#   }
#   
#   baseVec1 <- basisVector(index=(e+1),size=npost)
#   orig_ci  <- constructOriginalCS(betahat        = beta,
#                                   sigma          = V,
#                                   numPrePeriods  = npre,
#                                   numPostPeriods = npost,
#                                   l_vec          = baseVec1)
#   
#   if (type=="relative_magnitude") {
#     robust_ci <- createSensitivityResults_relativeMagnitudes(betahat        = beta,
#                                                              sigma          = V,
#                                                              numPrePeriods  = npre,
#                                                              numPostPeriods = npost,
#                                                              l_vec          = baseVec1,
#                                                              gridPoints     = gridPoints,
#                                                              ...)
#     
#   } else if (type == "smoothness") {
#     robust_ci <- createSensitivityResults(betahat        = beta,
#                                           sigma          = V,
#                                           numPrePeriods  = npre,
#                                           numPostPeriods = npost,
#                                           l_vec          = baseVec1,
#                                           ...)
#   }
#   
#   return(list(robust_ci=robust_ci, orig_ci=orig_ci, type=type))
# }
# 
# # start date
# start_date <- aggregated_data %>% ungroup() %>% select(date) %>% distinct() %>% summarise(date = min(date))
# start_date <- start_date$date
# 
# # Adjust your existing code to calculate 'week' and 'firstweek' as the number of weeks from the start_date
# did_data <- aggregated_data %>%
#   ungroup() %>%
#   filter(rate_period == "Overall") %>%
#   mutate(
#     # Calculate the difference in weeks from the start_date
#     week = as.numeric(difftime(date, start_date, units = "weeks")) %/% 1 + 1,
#     # Assuming you have a way to determine 'firstweek', adjust similarly if needed
#     firstweek = as.numeric(difftime(first_adoption, start_date, units = "weeks")) %/% 1 + 1) %>%
#   group_by(hashed_mpan, firstweek, week) %>%
#   summarise(consumption_hh = mean(consumption_hh)) %>%
#   mutate(
#     id = as.numeric(hashed_mpan),
#     firstweek = as.numeric(firstweek),
#     week = as.numeric(week)
#   )

# est_cs <- att_gt(yname = "consumption_hh",
#                  tname = "week",
#                  idname = "id",
#                  gname = "firstweek",
#                  data = did_data,
#                  clustervars = "id",
#                  control_group=c("notyettreated"),
#                  base_period = "universal",
#                  allow_unbalanced_panel = TRUE,
#                  print_details = FALSE)
# 
# es <- did::aggte(est_cs, type = "dynamic",
#                  min_e = -49, max_e = 49, na.rm = TRUE)

# #Run sensitivity analysis for relative magnitudes
# sensitivity_results <- honest_did(es, e=0,type="relative_magnitude",Mbarvec=seq(from = 0.5, to = 2, by = 0.5))
# 
# HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results$robust_ci,
#                                                     sensitivity_results$orig_ci)
# 
# ggsave("graphs/sensitivity_results.png")

source("scripts/02_07_heterogeneity_analysis.R")
rm(list = setdiff(ls(), list_env))
gc()   

# # Specify the objects you want to keep
# all_objects <- ls()
# keep_objects <- c("aggregated_data", "m1", "m1_share", "cosy_color", "flexible_color", "CleanPreAverage", "format_decimal", "format_number")
# 
# # Remove all objects except the ones you want to keep
# rm(list = setdiff(all_objects, keep_objects))
# gc()
# 
# ### Figure 11: Impact of Cosy by Outside Temperature
# # Fit the model
# m1 <- feols(consumption_hh ~ i(cosy_contract_active, ref=0)  | 
#               hdd, 
#             data = aggregated_data, 
#             cluster = ~account_id, 
#             split = ~ rate_period)
# 
# m1_cold <- feols(consumption_hh ~ i(cosy_contract_active, ref=0)  | 
#                    daily_avg_air_temperature_celsius , 
#                  data = aggregated_data %>% filter(daily_avg_air_temperature_celsius <0), 
#                  cluster = ~account_id, 
#                  split = ~ rate_period)
# m2_cold <- feols(consumption_hh ~ i(cosy_contract_active, ref=0)  | 
#                    daily_avg_air_temperature_celsius , 
#                  data = aggregated_data %>% filter(daily_avg_air_temperature_celsius >= 0, 
#                                                    daily_avg_air_temperature_celsius < 5), 
#                  cluster = ~account_id, 
#                  split = ~ rate_period)
# m3_cold <- feols(consumption_hh ~ i(cosy_contract_active, ref=0)  | 
#                    daily_avg_air_temperature_celsius, 
#                  data = aggregated_data %>% filter(daily_avg_air_temperature_celsius >= 5, 
#                                                    daily_avg_air_temperature_celsius < 10), 
#                  cluster = ~account_id, 
#                  split = ~ rate_period)
# m4_cold <- feols(consumption_hh ~ i(cosy_contract_active, ref=0)  | 
#                    daily_avg_air_temperature_celsius , 
#                  data = aggregated_data %>% filter(daily_avg_air_temperature_celsius >= 10), 
#                  cluster = ~account_id, 
#                  split = ~ rate_period)
# 
# # Extract coefficients and standard errors
# coefs <- rbind(
#   coeftable(m1_cold) %>%
#     data.frame() %>%
#     mutate(model = "< 0°C"),
#   coeftable(m2_cold) %>%
#     data.frame() %>%
#     mutate(model = "0-5°C"),
#   coeftable(m3_cold) %>%
#     data.frame() %>%
#     mutate(model = "5-10°C"),
#   coeftable(m4_cold) %>%
#     data.frame() %>%
#     mutate(model = ">10°C")) %>%
#   mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
#          upper_ci = Estimate + 1.96 * `Std..Error`,
#          `Average Daily Temperature` = factor(model, levels = c("< 0°C",
#                                                                 "0-5°C",
#                                                                 "5-10°C",
#                                                                 ">10°C")),
#          rate_period = factor(sample, levels = c("Morning Cosy",
#                                                  "Afternoon Cosy",
#                                                  "Peak Rate",
#                                                  "Other", 
#                                                  "Overall")))
# 
# 
# # Define colors with increasing darkness
# colors <- c("< 0°C" = "#AFCBE3",  # Lightest blue
#             "0-5°C" = "#8AAFD4",  # Slightly darker
#             "5-10°C" = "#6694C6", # Medium blue
#             ">10°C" = "#466CA8")  # Darkest blue
# 
# # Create the ggplot
# ggplot(coefs %>% filter(rate_period != "Overall"), aes(x = `Average Daily Temperature`, 
#                                                        y = Estimate, fill = `Average Daily Temperature`)) +
#   geom_col() +  # Use geom_col for pre-computed y values
#   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6, color = "grey") + 
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#   scale_y_continuous(name = "Estimate (kWh)") +  # Set y-axis label
#   scale_fill_manual(values = colors) +  # Assign colors with increasing darkness
#   labs(fill = "Average Daily Temperature") +  # Remove x-axis label, keep legend title
#   theme_minimal() +  # Apply minimal theme
#   facet_wrap(~ rate_period, scales = "free_y", ncol = 2) +  # Facet with 2 columns and free y-axis scales
#   theme(
#     axis.title.x = element_blank(),  # Remove x-axis title
#     axis.text.x = element_blank(),   # Remove x-axis text
#     axis.ticks.x = element_blank(),  # Remove x-axis ticks
#     legend.position = "bottom"       # Place legend at the bottom
#   )
# 
# ggsave("graphs/cosy_temperature_binned.png",
#        width = 16, height = 8, units = "cm")
# 
# # Unique periods 
# periods <- unique(aggregated_data$rate_period)
# 
# rm(list = ls(pattern = "^m_"))
# gc()
# 
# 
# # Run the regression model
# tempreg <- feols(consumption_hh ~ i(cosy_contract_active, temp_degree, ref=0) |
#                    temp_degree,
#                  data = aggregated_data %>% 
#                    mutate(temp_degree = factor(
#                      case_when(
#                        daily_avg_air_temperature_celsius < 0 ~ 0,
#                        daily_avg_air_temperature_celsius < 25.5 ~ round(daily_avg_air_temperature_celsius),
#                        TRUE ~ 25
#                      )
#                    )),
#                  split = ~ rate_period,
#                  cluster = ~account_id)
# 
# # Loop through each model in tempreg to create plots
# for (i in 1:length(tempreg)) {
#   
#   # Find model
#   val <- tempreg[[i]]$model_info$sample$value
#   j <- which(sapply(1:length(m1), function(j) m1[[j]]$model_info$sample$value) == val)
#   
#   # Extract coefficients and standard errors
#   coefs <- coeftable(tempreg[[i]]) %>%
#     data.frame() %>%
#     tibble::rownames_to_column("term") %>%
#     as_tibble() %>%
#     separate(term, into = c("cosy_contract_active", "remove1", "daily_avg_air_temperature_celsius", "remove2"), sep = "::") %>%
#     mutate(daily_avg_air_temperature_celsius = as.numeric(daily_avg_air_temperature_celsius),
#            lower_ci = Estimate - 1.96 * `Std..Error`,
#            upper_ci = Estimate + 1.96 * `Std..Error`
#     ) %>%
#     mutate(`/% ATE` = Estimate / abs(m1[[j]]$coefficients) * 100,     
#            lower_ci_ATE = `/% ATE` - 1.96 * (`Std..Error` / abs(m1[[j]]$coefficients) * 100),
#            upper_ci_ATE = `/% ATE` + 1.96 * (`Std..Error` / abs(m1[[j]]$coefficients) * 100)
#     )
#   
#   # Create the ggplot
#   ggplot(coefs, aes(x = daily_avg_air_temperature_celsius, y = Estimate)) +
#     geom_point(color = cosy_color) +  # Use your desired color
#     geom_line(color = cosy_color) +   # Use your desired color
#     geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6, color = cosy_color) +  # Use your desired color
#     geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", alpha = 0.6, color = cosy_color) +  # Add horizontal line at 100% ATE
#     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#     scale_y_continuous(
#       name = "Estimate (kWh)", 
#       sec.axis = sec_axis(~ ./m1[[j]]$coefficients, name = "% of ATE", labels = scales::percent_format())
#     ) +
#     labs(
#       x = "Average Temperature in Degrees",
#     ) +
#     theme_minimal()
#   
#   # Print the plot
#   ggsave(paste0("graphs/cosy_temperature_", tolower(gsub(" ", "_", val)), ".png"),
#          width = 16, height = 8, units = "cm")
# }
# 
# 
# # Remove the overall model if it exists
# all_coefs <- coeftable(tempreg) %>%
#   inner_join(coeftable(m1) %>% select(sample, Estimate) %>% rename(average = Estimate)) %>%
#   data.frame() %>%
#   filter(sample != "Overall") %>%
#   separate(coefficient, into = c("cosy_contract_active", "remove1", "daily_avg_air_temperature_celsius", "remove2"), sep = "::") %>%
#   mutate(daily_avg_air_temperature_celsius = as.numeric(daily_avg_air_temperature_celsius),
#          lower_ci = Estimate - 1.96 * `Std..Error`,
#          upper_ci = Estimate + 1.96 * `Std..Error`
#   ) %>%
#   mutate(`/% ATE` = Estimate / abs(average) * 100,     
#          lower_ci_ATE = `/% ATE` - 1.96 * (`Std..Error` / average * 100),
#          upper_ci_ATE = `/% ATE` + 1.96 * (`Std..Error` / average * 100)
#   ) %>%
#   mutate(sample = factor(sample, levels = c("Morning Cosy",
#                                             "Afternoon Cosy",
#                                             "Peak Rate",
#                                             "Other", 
#                                             "Overall")))
# 
# # Create the ggplot
# ggplot(all_coefs, aes(x = daily_avg_air_temperature_celsius, y = Estimate)) +
#   geom_point(color = cosy_color) +  # Use your desired color
#   geom_line(color = cosy_color) +   # Use your desired color
#   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6, color = cosy_color) +  # Use your desired color
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#   geom_hline(aes(yintercept = average), linetype = "dashed", alpha = 0.6, color = cosy_color) +  # Add horizontal line for the average
#   scale_y_continuous(
#     name = "Estimate (kWh)"  ) +
#   labs(
#     x = "Average Temperature in Degrees (°C)",
#   ) +
#   theme_minimal() +
#   facet_wrap(~sample)
# 
# # Print the plot
# ggsave("graphs/cosy_temperature_all.png", width = 16, height = 8, units = "cm")
# 
# rm(list = ls(pattern = "^m[0-9]_"))
# gc()
# 

# 
# ## Moderators
# 
# ### Table A.12: Cosy Adoption by Previous Tariff Type
# # Register the pre-treatment average fit statistic
# fitstat_register("pre_avg_tou", function(x) {
#   
#   # Extract the formula
#   formula <- x$fml_all$linear
#   
#   # Extract the outcome variable from the formula
#   outcome_variable <- all.vars(formula)[1]
#   
#   # Extract the call object and evaluate the data argument
#   call_object <- x$call
#   data_expr <- call_object$data
#   data <- eval(data_expr)
#   
#   # Get the logical vector of observations used in the model
#   obs_used <- obs(x)
#   
#   # Subset the original dataset using this logical vector
#   data_used <- data[obs_used, ]
#   
#   # Ensure the outcome variable is treated as a column name
#   outcome_values <- data_used[[outcome_variable]]
#   
#   # Create pre-avg for non-HP installed group
#   pre_avg <- mean(outcome_values[data_used$previous_is_charged_half_hourly == 1 & data_used$date < data_used$first_adoption], na.rm = TRUE)
#   
#   # Format the pre-avg
#   formatted_pre_avg <- format_decimal(pre_avg)
#   
#   return(formatted_pre_avg)
# }, "Half Hourly Consumption ToU")
# 
# 
# # Register the pre-treatment average fit statistic
# fitstat_register("pre_avg_nontou", function(x) {
#   
#   # Extract the formula
#   formula <- x$fml_all$linear
#   
#   # Extract the outcome variable from the formula
#   outcome_variable <- all.vars(formula)[1]
#   
#   # Extract the call object and evaluate the data argument
#   call_object <- x$call
#   data_expr <- call_object$data
#   data <- eval(data_expr)
#   
#   # Get the logical vector of observations used in the model
#   obs_used <- obs(x)
#   
#   # Subset the original dataset using this logical vector
#   data_used <- data[obs_used, ]
#   
#   # Ensure the outcome variable is treated as a column name
#   outcome_values <- data_used[[outcome_variable]]
#   
#   # Create pre-avg for non-HP installed group
#   pre_avg <- mean(outcome_values[data_used$previous_is_charged_half_hourly == 0 & data_used$date < data_used$first_adoption], na.rm = TRUE)
#   
#   # Format the pre-avg
#   formatted_pre_avg <- format_decimal(pre_avg)
#   
#   return(formatted_pre_avg)
# }, "Half Hourly Consumption Non-ToU")
# 
# 
# m3 <- feols(consumption_hh ~ i(cosy_contract_active) +  i(cosy_contract_active, previous_is_charged_half_hourly, ref=0) | 
#               hdd + account_id + date, 
#             data = aggregated_data, 
#             cluster = ~account_id,
#             split = ~ rate_period)
# 
# etable(m3, tex = TRUE, title = "Cosy Adoption by Previous Tariff Type",
#        label = "tab:prevrav",
#        fitstat = ~ N + g + pre_avg_nontou + pre_avg_tou +t_obs + r2,
#        dict = c(previous_is_charged_half_hourly = "Prev is ToU"),
#        file = "tables/did_prevar.tex", replace = TRUE)
# 
# # Read the generated LaTeX file
# file_content <- readLines("tables/did_prevar.tex")
# 
# # Find the lines with the pre-treatment average and remove them
# pre_avg_line_index <- grep("Half Hourly Consumption", file_content)[2]
# pre_avg_line_index2 <- grep("Half Hourly Consumption", file_content)[3]
# pre_avg_lines <- file_content[pre_avg_line_index:(pre_avg_line_index2)]
# file_content <- file_content[-c(pre_avg_line_index, pre_avg_line_index2)]
# 
# # Find the position just after the coefficients
# coeff_end_index <- grep("Cosy Contract Active", file_content)[2] + 2
# if (length(coeff_end_index) > 1) {
#   coeff_end_index <- coeff_end_index[-1]
# }
# 
# # Insert the pre-treatment average row after the coefficients
# file_content <- append(file_content, pre_avg_lines, after = coeff_end_index)
# 
# file_content <- append(file_content,"\\emph{Pre-Treatment Average}\\", after = coeff_end_index)
# 
# # Add a \midrule after the pre-treatment average
# file_content <- append(file_content, "\\midrule", after = coeff_end_index+3)
# 
# # Write the modified content back to the LaTeX file
# writeLines(file_content, "tables/did_prevar.tex")  
# 
# 
# 
# 
# 
# ### Figure A.29: Impact of Cosy Adoption by EAC on Consumption and Figure A.30: Impact of Cosy Adoption by EAC on Share of Consumption
# rm(tempreg)
# 
# # Create unique breaks for eac_mwh
# breaks <- unique(quantile(aggregated_data[!is.na(aggregated_data$eac_mwh),]$eac_mwh, probs = seq(0, 1, by = 0.1)))
# 
# # Create pretty labels for the categories
# labels <- sapply(1:(length(breaks)-1), function(i) paste0(round(breaks[i]), "MWh to ", round(breaks[i+1]), "MWh"))
# 
# # Create the categories for eac_mwh
# aggregated_data <- aggregated_data %>%
#   mutate(eac_mwh_category = cut(eac_mwh, 
#                                 breaks = breaks, 
#                                 include.lowest = TRUE,
#                                 labels = labels))
# 
# # Run the regression models
# tempreg_total <- feols(consumption_hh ~ i(cosy_contract_active, eac_mwh_category, ref =0) 
#                        | date +  account_id + hdd,
#                        data = aggregated_data %>% filter(!is.na(eac_mwh)),
#                        split = ~ rate_period,
#                        cluster = ~account_id)
# 
# tempreg_share <- feols(share_consumption ~ i(cosy_contract_active, eac_mwh_category, ref =0) 
#                        | date +  account_id + hdd,
#                        data = aggregated_data %>% 
#                          filter(!is.na(eac_mwh), !rate_period=="Overall") %>%
#                          group_by(account_id, date) %>%
#                          mutate(share_consumption = total_consumption/sum(total_consumption)),
#                        split = ~ rate_period,
#                        cluster = ~account_id)
# 
# # share
# m1_share <- feols(share_consumption ~ i(cosy_contract_active) | hdd + account_id + date, 
#                   data = aggregated_data %>% 
#                     filter(!is.na(eac_mwh), !rate_period=="Overall") %>%
#                     group_by(account_id, date) %>%
#                     mutate(share_consumption = total_consumption/sum(total_consumption)), 
#                   cluster = ~account_id, 
#                   split = ~ rate_period)
# 
# for (i in 1:5) {
#   
#   # Find model
#   val <- tempreg_total[[i]]$model_info$sample$value
#   j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
#   
#   # Extract coefficients and standard errors for total_consumption
#   coefs_total <- coeftable(tempreg_total[i]) %>%
#     data.frame() %>%
#     separate(coefficient, into = c("cosy_contract_active", "remove1", "EAC MWh Category", "remove2"), sep = "::") %>%
#     mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
#            upper_ci = Estimate + 1.96 * `Std..Error`,
#            outcome = "Total Consumption") %>%
#     mutate(`EAC MWh Category` = factor(`EAC MWh Category`, levels = labels))
#   if (i<5) {
#     # Extract coefficients and standard errors for share_consumption
#     coefs_share <- coeftable(tempreg_share[i]) %>%
#       data.frame() %>%
#       separate(coefficient, into = c("cosy_contract_active", "remove1", "EAC MWh Category", "remove2"), sep = "::") %>%
#       mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
#              upper_ci = Estimate + 1.96 * `Std..Error`,
#              outcome = "Share Consumption") %>%
#       mutate(`EAC MWh Category` = factor(`EAC MWh Category`, levels = labels))
#   }
#   # Choose a color palette that can handle more than 9 categories
#   brewer_colors <- scales::hue_pal()(length(unique(coefs_share$`EAC MWh Category`)))
#   
#   # Create the ggplot
#   ggplot(coefs_total, aes(x = `EAC MWh Category`, y = Estimate , fill = `EAC MWh Category`)) +
#     geom_bar(stat = "identity", show.legend = FALSE) +
#     geom_errorbar(aes(ymin = lower_ci , ymax = upper_ci), width = 0.2, color = "grey") +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#     geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha=0.6) +  # Add horizontal line at ATE
#     scale_fill_brewer(palette = "Spectral") +  # Use the chosen palette
#     labs(
#       x = "EAC MWh Decile",
#     ) +
#     scale_y_continuous(
#       name = "Estimate (kWh)", 
#       sec.axis = sec_axis(~ ./m1[[j]]$coefficients, name = "% of ATE", labels = scales::percent_format())
#     ) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#       legend.position = "none"  # Remove legend
#     )
#   
#   # Print the plot
#   ggsave(paste0("graphs/eac_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
#          width = 16, height = 8, units = "cm")
#   
#   if (i<5) {
#     # Create the ggplot
#     ggplot(coefs_share, aes(x = `EAC MWh Category`, y = Estimate, fill = `EAC MWh Category`)) +
#       geom_bar(stat = "identity", show.legend = FALSE) +
#       geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
#       geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#       geom_hline(yintercept = m1_share[[i]]$coefficients, linetype = "dashed", color = cosy_color, alpha=0.6) +  # Add horizontal line at ATE
#       scale_fill_brewer(palette = "Spectral") +  # Use the chosen palette
#       labs(
#         x = "EAC MWh Decile",
#       ) +
#       scale_y_continuous(
#         name = "% of Daily Consumption",
#         labels = scales::percent_format(),
#         sec.axis = sec_axis(~ ./m1_share[[i]]$coefficients, name = "% of ATE", labels = scales::percent_format())
#       ) +
#       theme_minimal() +
#       theme(
#         axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#         legend.position = "none"  # Remove legend
#       )
#     
#     # Print the plot
#     ggsave(paste0("graphs/eac_share_", tempreg_total[[i]]$model_info$sample$value %>% tolower() %>% str_replace(" ", "_"), ".png"),
#            width = 16, height = 8, units = "cm")
#   }
# }
# 
# 
# # Extract coefficients and standard errors for each model and convert to % of ATE
# extract_and_combine_coefs <- function(tempreg_model, m1_model, labels) {
#   all_coefs <- data.frame()
#   
#   for (i in 1:length(tempreg_model)) {
#     val <- tempreg_model[[i]]$model_info$sample$value
#     j <- which(sapply(1:length(m1_model), function(j) m1_model[[j]]$model_info$sample$value) == val)
#     
#     coefs <- coeftable(tempreg_model[[i]]) %>%
#       data.frame() %>%
#       tibble::rownames_to_column("term") %>%
#       as_tibble() %>%
#       separate(term, into = c("cosy_contract_active", "remove1", "EAC_MWh_Category", "remove2"), sep = "::") %>%
#       mutate(
#         average = m1_model[[j]]$coefficients,
#         EAC_MWh_Category = factor(EAC_MWh_Category, levels = labels),
#         lower_ci = Estimate - 1.96 * Std..Error,
#         upper_ci = Estimate + 1.96 * Std..Error,
#         `%_ATE` = Estimate / abs(m1_model[[j]]$coefficients) * 100,
#         lower_ci_ATE = `%_ATE` - 1.96 * (Std..Error / m1_model[[j]]$coefficients * 100),
#         upper_ci_ATE = `%_ATE` + 1.96 * (Std..Error / m1_model[[j]]$coefficients * 100),
#         outcome = ifelse(grepl("share", deparse(substitute(tempreg_model))), "Share Consumption", "Total Consumption"),
#         period = factor(val, levels = c("Morning Cosy",
#                                         "Afternoon Cosy",
#                                         "Peak Rate",
#                                         "Other", 
#                                         "Overall")))
#     
#     all_coefs <- bind_rows(all_coefs, coefs)
#   }
#   
#   return(all_coefs)
# }
# 
# # Combine all coefficients
# all_coefs_total <- extract_and_combine_coefs(tempreg_total, m1, labels)
# all_coefs_share <- extract_and_combine_coefs(tempreg_share, m1_share, labels)
# all_coefs <- bind_rows(all_coefs_total, all_coefs_share)
# 
# # Create the combined ggplot using facet_wrap
# ggplot(all_coefs_share %>% filter(period!="Overall"), aes(x = EAC_MWh_Category, y = Estimate, fill = EAC_MWh_Category)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
#   geom_hline(aes(yintercept = average), linetype = "dashed", alpha = 0.6, color = cosy_color) +  # Add horizontal line for the average
#   scale_fill_brewer(palette = "Spectral") +  # Use the chosen palette
#   labs(
#     x = "EAC MWh Decile",
#   ) +
#   scale_y_continuous(
#     name = "Estimate (kWh)", 
#     labels = scales::percent_format()
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#     legend.position = "none"  # Remove legend
#   ) +
#   facet_wrap(~ period)
# 
# # Save the combined plot
# ggsave("graphs/eac_share_combined.png", device = "png", width = 16, height = 12, dpi = 300)
# 
# 
# # Create the combined ggplot using facet_wrap
# ggplot(all_coefs_total %>% filter(period!="Overall"), aes(x = EAC_MWh_Category, y = Estimate, fill = EAC_MWh_Category)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
#   geom_hline(aes(yintercept = average), linetype = "dashed", alpha = 0.6, color = cosy_color) +  # Add horizontal line for the average
#   scale_fill_brewer(palette = "Spectral") +  # Use the chosen palette
#   labs(
#     x = "EAC MWh Decile",
#   ) +
#   scale_y_continuous(
#     name = "Estimate (kWh)", 
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#     legend.position = "none"  # Remove legend
#   ) +
#   facet_wrap(~ period)
# 
# # Save the combined plot
# ggsave("graphs/eac_combined.png", device = "png", width = 16, height = 12, dpi = 300)
# 
# # List all objects in the environment
# rm(list = ls(pattern = "^m_"))
# gc()
# 
# ### Figure A.28: Impact of Cosy Adoption by EPC Score on Consumption
# m2a <- feols(consumption_hh ~ i(cosy_contract_active, epc_letter, ref=0)  | 
#                hdd + date + account_id,
#              data = aggregated_data, 
#              split = ~ rate_period,
#              cluster = ~account_id)
# 
# # share
# m1_share <- feols(share_consumption ~ i(cosy_contract_active) | hdd + account_id + date, 
#                   data = aggregated_data %>% 
#                     filter(!is.na(epc_letter), !rate_period=="Overall") %>%
#                     group_by(account_id, date) %>%
#                     mutate(share_consumption = total_consumption/sum(total_consumption)), 
#                   cluster = ~account_id, 
#                   split = ~ rate_period)
# 
# # Define colors to match the Energy Efficiency Rating chart
# rating_colors <- c(
#   "A" = "#00CC00",  # Green
#   "B" = "#66FF33",  # Light Green
#   "C" = "#FFFF00",  # Yellow
#   "D" = "#FF9900",  # Orange
#   "E" = "#FF6600",  # Dark Orange
#   "F" = "#FF0000",  # Red
#   "G" = "#990000"   # Dark Red
# )
# 
# for (i in 1:5) {
#   # Find model
#   val <- m2a[[i]]$model_info$sample$value
#   j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
#   
#   # Extract coefficients and standard errors
#   coefs <- coeftable(m2a[[i]]) %>%
#     data.frame() %>%
#     tibble::rownames_to_column("term") %>%
#     separate(term, into = c("cosy_contract_active", "remove1", "EPC letter", "remove2"), sep = "::") %>%
#     mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
#            upper_ci = Estimate + 1.96 * `Std..Error`
#     )
#   # Assuming coefs is your data frame and rating_colors is your color vector
#   coefs <- coefs %>%
#     mutate(EPC_label_position = max(Estimate) * 0.1)  # Position for EPC labels on the right
#   
#   # Create the ggplot
#   ggplot(coefs, aes(y = rev(factor(`EPC letter`)), x = Estimate, fill = factor(`EPC letter`))) +
#     geom_bar(stat = "identity", show.legend = FALSE) +
#     geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.2, color = "grey") +
#     geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
#     geom_vline(xintercept = m1[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha=0.6) +  # Add vertical line at ATE
#     geom_text(aes(x = EPC_label_position, label = `EPC letter`), hjust = 0, color = "white") +  # Add EPC letters on the right
#     scale_fill_manual(values = rating_colors, name = "EPC Letter") +  # Use the defined colors+
#     labs(
#       x = "Average Half Hourly Consumption",
#       y = "EPC Letter"
#     ) +
#     theme_minimal() +
#     theme(
#       axis.text.y = element_blank(),  # Hide original y-axis text
#       axis.ticks.y = element_blank(),  # Hide original y-axis ticks
#       axis.text.y.right = element_text(hjust = 0.5),  # Center the text on the right-hand side
#       axis.title.y.right = element_text(margin = margin(l = 10)),  # Add margin to right y-axis title
#       legend.position = "none"  # Remove legendx
#     )
#   
#   
#   # Print the plot
#   ggsave(paste0("graphs/epc_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
#          width = 16, height = 8, units = "cm")
# }
# 
# # Assuming m2a is your model list, m1 contains the ATE values, and rating_colors is your color vector
# all_coefs <- data.frame()
# 
# for (i in 1:5) {
#   # Find model
#   val <- m2a[[i]]$model_info$sample$value
#   j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
#   
#   # Extract coefficients and standard errors
#   coefs <- coeftable(m2a[[i]]) %>%
#     data.frame() %>%
#     tibble::rownames_to_column("term") %>%
#     separate(term, into = c("cosy_contract_active", "remove1", "EPC_letter", "remove2"), sep = "::") %>%
#     mutate(
#       average = m1[[j]]$coefficients,
#       lower_ci = Estimate - 1.96 * Std..Error,
#       upper_ci = Estimate + 1.96 * Std..Error,
#       `%_ATE` = Estimate / abs(m1[[j]]$coefficients) * 100,
#       lower_ci_ATE = lower_ci / abs(m1[[j]]$coefficients) * 100,
#       upper_ci_ATE = upper_ci / abs(m1[[j]]$coefficients) * 100,
#       period = val
#     )
#   
#   # Combine all coefficients
#   all_coefs <- bind_rows(all_coefs, coefs)
# }
# 
# # Assuming coefs is your data frame and rating_colors is your color vector
# all_coefs <- all_coefs %>%
#   mutate(EPC_label_position = max(`Estimate`) * 0.1)  # Position for EPC labels on the right
# 
# # Create the ggplot
# ggplot(all_coefs %>% filter(period!="Overall"), aes(y = factor(EPC_letter, levels = rev(unique(EPC_letter))), x = Estimate, fill = factor(EPC_letter))) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.2, color="grey") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
#   geom_vline(aes(xintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add vertical line at average ATE
#   geom_text(aes(x = EPC_label_position, label = EPC_letter), color = "black") +  # Add EPC letters on the right
#   scale_fill_manual(values = rating_colors, name = "EPC Letter") +  # Use the defined colors
#   labs(
#     x = "Estimate (kWh)",
#     y = "EPC Letter"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.y = element_blank(),  # Hide original y-axis text
#     axis.ticks.y = element_blank(),  # Hide original y-axis ticks
#     axis.text.y.right = element_text(hjust = 0.5),  # Center the text on the right-hand side
#     axis.title.y.right = element_text(margin = margin(l = 10)),  # Add margin to right y-axis title
#     legend.position = "none"  # Remove legend
#   ) +
#   facet_wrap(~ period, scales = "free_y")
# 
# # Print the plot
# ggsave("graphs/cosy_epc_combined.png", width = 16, height = 8, units = "cm")
# 
# rm(m2a)
# 
# 
# ### Figure A.33: Impact of Cosy Adoption by Heat Loss on Consumption and Figure A.34: Impact of Cosy Adoption by Heat Loss on Share of Consumption
# # Create unique breaks for predicted_heatloss_watts
# breaks <- unique(quantile(aggregated_data[!is.na(aggregated_data$predicted_heatloss_watts),]$predicted_heatloss_watts/1000, probs = seq(0, 1, by = 0.1)))
# 
# # Create pretty labels for the categories
# labels <- sapply(1:(length(breaks)-1), function(i) paste0(round(breaks[i], 1), " kW to ", round(breaks[i+1], 1), " kW"))
# 
# # Create the categories for predicted_heatloss_watts
# aggregated_data <- aggregated_data %>%
#   mutate(predicted_heatloss_watts_category = cut(predicted_heatloss_watts/1000, 
#                                                  breaks = breaks, 
#                                                  include.lowest = TRUE,
#                                                  labels = labels))
# 
# 
# m_heatloss <- feols(consumption_hh ~ i(cosy_contract_active, predicted_heatloss_watts_category, ref =0) 
#                     | date +  account_id + hdd,
#                     data = aggregated_data %>% filter(!is.na(predicted_heatloss_watts_category)),
#                     split = ~ rate_period,
#                     cluster = ~account_id)
# 
# for (i in 1:5) {
#   
#   # Find model
#   val <- m_heatloss[[i]]$model_info$sample$value
#   j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
#   
#   # Extract coefficients and standard errors for total_consumption
#   coefs_total <- coeftable(m_heatloss[i]) %>%
#     data.frame() %>%
#     separate(coefficient, into = c("cosy_contract_active", "remove1", "Heatloss MW Category", "remove2"), sep = "::") %>%
#     mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
#            upper_ci = Estimate + 1.96 * `Std..Error`,
#            outcome = "Total Consumption") %>%
#     mutate(`Heatloss MW Category` = factor(`Heatloss MW Category`, levels = labels))
#   
#   # Define the shades of reds
#   red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
#   
#   # Create the ggplot
#   ggplot(coefs_total, aes(x = `Heatloss MW Category`, y = Estimate, fill = `Heatloss MW Category`)) +
#     geom_bar(stat = "identity", show.legend = FALSE) +
#     geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#     geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
#     scale_fill_manual(values = red_palette) +
#     labs(
#       x = "Heatloss MW Decile",
#     ) +
#     scale_y_continuous(
#       name = "Estimate (kWh)", 
#       sec.axis = sec_axis(~ ./m1[[j]]$coefficients, name = "% of ATE", labels = scales::percent_format())
#       
#     ) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#       legend.position = "none"  # Remove legend
#     )
#   
#   # Print the plot
#   ggsave(paste0("graphs/heatloss_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
#          width = 16, height = 8, units = "cm")
#   
# }
# 
# 
# m_heatloss2 <- feols(share_consumption ~ i(cosy_contract_active, predicted_heatloss_watts_category, ref =0)
#                      | date +  account_id + hdd,
#                      data = aggregated_data %>% 
#                        filter(!is.na(predicted_heatloss_watts_category), !rate_period=="Overall") %>%
#                        group_by(account_id, date) %>%
#                        mutate(share_consumption = total_consumption/sum(total_consumption)),
#                      split = ~ rate_period,
#                      cluster = ~account_id)
# 
# for (i in 1:4) {
#   
#   # Find model
#   val <- m_heatloss2[[i]]$model_info$sample$value
#   j <- which(sapply(1:4, function(j) m1_share[[j]]$model_info$sample$value) == val)
#   
#   # Extract coefficients and standard errors for total_consumption
#   coefs_total <- coeftable(m_heatloss2[i]) %>%
#     data.frame() %>%
#     separate(coefficient, into = c("cosy_contract_active", "remove1", "Heatloss MW Category", "remove2"), sep = "::") %>%
#     mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
#            upper_ci = Estimate + 1.96 * `Std..Error`,
#            outcome = "Total Consumption") %>%
#     mutate(`Heatloss MW Category` = factor(`Heatloss MW Category`, levels = labels))
#   
#   # Define the shades of reds
#   red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
#   
#   # Create the ggplot
#   ggplot(coefs_total, aes(x = `Heatloss MW Category`, y = Estimate, fill = `Heatloss MW Category`)) +
#     geom_bar(stat = "identity", show.legend = FALSE) +
#     geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#     geom_hline(yintercept = m1_share[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
#     scale_fill_manual(values = red_palette) +
#     labs(
#       x = "Heatloss MW Decile",
#     ) +
#     scale_y_continuous(
#       name = "% of Daily Consumption",
#       labels = scales::percent_format(),
#       sec.axis = sec_axis(~ ./m1_share[[j]]$coefficients, name = "% of ATE", labels = scales::percent_format())
#     ) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#       legend.position = "none"  # Remove legend
#     )
#   
#   # Print the plot
#   ggsave(paste0("graphs/share_heatloss_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
#          width = 16, height = 8, units = "cm")
#   
# }
# 
# 
# # Extract coefficients and standard errors for each model and convert to % of ATE
# extract_and_combine_coefs <- function(tempreg_model, m1_model, labels) {
#   all_coefs <- data.frame()
#   
#   for (i in 1:length(tempreg_model)) {
#     val <- tempreg_model[[i]]$model_info$sample$value
#     j <- which(sapply(1:length(m1_model), function(j) m1_model[[j]]$model_info$sample$value) == val)
#     
#     coefs <- coeftable(tempreg_model[[i]]) %>%
#       data.frame() %>%
#       tibble::rownames_to_column("term") %>%
#       as_tibble() %>%
#       separate(term, into = c("cosy_contract_active", "remove1", "EAC_MWh_Category", "remove2"), sep = "::") %>%
#       mutate(
#         average = m1_model[[j]]$coefficients,
#         EAC_MWh_Category = factor(EAC_MWh_Category, levels = labels),
#         lower_ci = Estimate - 1.96 * Std..Error,
#         upper_ci = Estimate + 1.96 * Std..Error,
#         `%_ATE` = Estimate / abs(m1_model[[j]]$coefficients) * 100,
#         lower_ci_ATE = `%_ATE` - 1.96 * (Std..Error / m1_model[[j]]$coefficients * 100),
#         upper_ci_ATE = `%_ATE` + 1.96 * (Std..Error / m1_model[[j]]$coefficients * 100),
#         outcome = ifelse(grepl("share", deparse(substitute(tempreg_model))), "Share Consumption", "Total Consumption"),
#         period = factor(val, levels = c("Morning Cosy",
#                                         "Afternoon Cosy",
#                                         "Peak Rate",
#                                         "Other", 
#                                         "Overall")))
#     
#     all_coefs <- bind_rows(all_coefs, coefs)
#   }
#   
#   return(all_coefs)
# }
# 
# 
# # Combine all coefficients
# all_coefs_total <- extract_and_combine_coefs(m_heatloss, m1, labels)
# all_coefs_share <- extract_and_combine_coefs(m_heatloss2, m1_share, labels)
# all_coefs <- bind_rows(all_coefs_total, all_coefs_share)
# 
# # Create the combined ggplot using facet_wrap
# ggplot(all_coefs_share %>% filter(period!="Overall"), aes(x = EAC_MWh_Category, y = Estimate, fill = EAC_MWh_Category)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#   geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
#   scale_fill_manual(values = red_palette) +
#   labs(
#     x = "Heatloss MW Decile",
#   ) +
#   scale_y_continuous(
#     name = "% of Daily Consumption",
#     labels = scales::percent_format(),
#   ) +  
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#     legend.position = "none"  # Remove legend
#   ) +
#   facet_wrap(~ period)
# 
# # Save the combined plot
# ggsave("graphs/heatloss_share_combined.png", device = "png", width = 16, height = 12, dpi = 300)
# 
# 
# # Create the combined ggplot using facet_wrap
# ggplot(all_coefs_total %>% filter(period!="Overall"), aes(x = EAC_MWh_Category, y = Estimate, fill = EAC_MWh_Category)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#   geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
#   scale_fill_manual(values = red_palette) +
#   labs(
#     x = "Heatloss MW Decile",
#   ) +
#   scale_y_continuous(
#     name = "Estimate (kWh)"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#     legend.position = "none"  # Remove legend
#   ) +
#   facet_wrap(~ period)
# 
# # Save the combined plot
# ggsave("graphs/heatloss_combined.png", device = "png", width = 16, height = 12, dpi = 300)
# 
# # List all objects in the environment
# rm(list = ls(pattern = "^m_"))
# gc()
# 
# # Specify the objects you want to keep
# all_objects <- ls()
# keep_objects <- c("aggregated_data", "m1", "m1_share", "cosy_color", "flexible_color", "CleanPreAverage", "format_decimal", "format_number")
# 
# # Remove all objects except the ones you want to keep
# rm(list = setdiff(all_objects, keep_objects))
# 
# 
# ### Figure A.31: Impact of Cosy Adoption by Floor Area on Consumption and Figure A.32: Impact of Cosy Adoption by Floor Area on Share of Consumptio
# gc()
# 
# # Create unique breaks for total_floor_area
# breaks <- unique(quantile(aggregated_data[!is.na(aggregated_data$total_floor_area),]$total_floor_area, probs = seq(0, 1, by = 0.1)))
# 
# # Create pretty labels for the categories
# labels <- sapply(1:(length(breaks)-1), function(i) paste0(round(breaks[i], 0), " to ", round(breaks[i+1], 0), " m sq."))
# 
# # Create the categories for total_floor_area
# aggregated_data <- aggregated_data %>%
#   mutate(total_floor_area_category = cut(total_floor_area, 
#                                          breaks = breaks, 
#                                          include.lowest = TRUE,
#                                          labels = labels))
# 
# 
# m_floor <- feols(consumption_hh ~ i(cosy_contract_active, total_floor_area_category, ref =0) 
#                  | date +  account_id + hdd,
#                  data = aggregated_data %>% filter(!is.na(total_floor_area_category)),
#                  split = ~ rate_period,
#                  cluster = ~account_id)
# 
# for (i in 1:5) {
#   
#   # Find model
#   val <- m_floor[[i]]$model_info$sample$value
#   j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
#   
#   # Extract coefficients and standard errors for total_consumption
#   coefs_total <- coeftable(m_floor[i]) %>%
#     data.frame() %>%
#     separate(coefficient, into = c("cosy_contract_active", "remove1", "Floor Area", "remove2"), sep = "::") %>%
#     mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
#            upper_ci = Estimate + 1.96 * `Std..Error`,
#            outcome = "Total Consumption") %>%
#     mutate(`Floor Area` = factor(`Floor Area`, levels = labels))
#   
#   # Define the shades of reds
#   red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
#   
#   # Create the ggplot
#   ggplot(coefs_total, aes(x = `Floor Area`, y = Estimate, fill = `Floor Area`)) +
#     geom_bar(stat = "identity", show.legend = FALSE) +
#     geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#     geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
#     scale_fill_manual(values = red_palette) +
#     labs(
#       x = "Floor Area Decile",
#       y = "Half Hourly Consumption in kWh"
#     ) +
#     scale_y_continuous(
#       name = "Estimate (kWh)", 
#     ) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#       legend.position = "none"  # Remove legend
#     )
#   
#   # Print the plot
#   ggsave(paste0("graphs/floor_area_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
#          width = 16, height = 8, units = "cm")
#   
# }
# 
# 
# m_floor_share <- feols(share_consumption ~ i(cosy_contract_active, total_floor_area_category, ref =0)
#                        | date +  account_id + hdd,
#                        data = aggregated_data %>% 
#                          filter(!is.na(total_floor_area_category), !rate_period=="Overall") %>%
#                          group_by(account_id, date) %>%
#                          mutate(share_consumption = total_consumption/sum(total_consumption)),
#                        split = ~ rate_period,
#                        cluster = ~account_id)
# 
# for (i in 1:4) {
#   
#   # Find model
#   val <- m_floor_share[[i]]$model_info$sample$value
#   j <- which(sapply(1:4, function(j) m1_share[[j]]$model_info$sample$value) == val)
#   
#   # Extract coefficients and standard errors for total_consumption
#   coefs_total <- coeftable(m_floor_share[i]) %>%
#     data.frame() %>%
#     separate(coefficient, into = c("cosy_contract_active", "remove1", "Floor Area", "remove2"), sep = "::") %>%
#     mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
#            upper_ci = Estimate + 1.96 * `Std..Error`,
#            outcome = "Total Consumption") %>%
#     mutate(`Floor Area` = factor(`Floor Area`, levels = labels))
#   
#   # Define the shades of reds
#   red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
#   
#   # Create the ggplot
#   ggplot(coefs_total, aes(x = `Floor Area`, y = Estimate, fill = `Floor Area`)) +
#     geom_bar(stat = "identity", show.legend = FALSE) +
#     geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#     geom_hline(yintercept = m1_share[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
#     scale_fill_manual(values = red_palette) +
#     labs(
#       x = "Floor Area Decile",
#       y = "Half Hourly Consumption in kWh"
#     ) +
#     scale_y_continuous(
#       name = "% of Daily Consumption",
#       labels = scales::percent_format(),
#       sec.axis = sec_axis(~ ./m1_share[[j]]$coefficients, name = "% of ATE", labels = scales::percent_format())
#     ) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#       legend.position = "none"  # Remove legend
#     )
#   
#   # Print the plot
#   ggsave(paste0("graphs/share_floor_area_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
#          width = 16, height = 8, units = "cm")
#   
# }
# 
# # Initialize an empty data frame to store all coefficients
# all_coefs <- data.frame()
# 
# # Extract coefficients for total consumption
# for (i in 1:5) {
#   val <- m_floor[[i]]$model_info$sample$value
#   j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
#   
#   coefs_total <- coeftable(m_floor[[i]]) %>%
#     data.frame() %>%
#     tibble::rownames_to_column("term") %>%
#     separate(term, into = c("cosy_contract_active", "remove1", "Floor_Area", "remove2"), sep = "::") %>%
#     mutate(
#       average = m1[[j]]$coefficients,
#       lower_ci = Estimate - 1.96 * `Std..Error`,
#       upper_ci = Estimate + 1.96 * `Std..Error`,
#       `%_ATE` = Estimate / abs(m1[[j]]$coefficients) * 100,
#       lower_ci_ATE = lower_ci / abs(m1[[j]]$coefficients) * 100,
#       upper_ci_ATE = upper_ci / abs(m1[[j]]$coefficients) * 100,
#       outcome = "Total Consumption",
#       `Floor_Area` = factor(`Floor_Area`, levels = labels),
#       period = factor(val, levels = c("Morning Cosy",
#                                       "Afternoon Cosy",
#                                       "Peak Rate",
#                                       "Other", 
#                                       "Overall")))
#   
#   all_coefs <- bind_rows(all_coefs, coefs_total)
# }
# 
# # Extract coefficients for share consumption
# for (i in 1:4) {
#   val <- m_floor_share[[i]]$model_info$sample$value
#   j <- which(sapply(1:4, function(j) m1_share[[j]]$model_info$sample$value) == val)
#   
#   coefs_share <- coeftable(m_floor_share[[i]]) %>%
#     data.frame() %>%
#     tibble::rownames_to_column("term") %>%
#     separate(term, into = c("cosy_contract_active", "remove1", "Floor_Area", "remove2"), sep = "::") %>%
#     mutate(
#       average = m1_share[[j]]$coefficients,
#       lower_ci = Estimate - 1.96 * `Std..Error`,
#       upper_ci = Estimate + 1.96 * `Std..Error`,
#       `%_ATE` = Estimate / abs(m1_share[[j]]$coefficients) * 100,
#       lower_ci_ATE = lower_ci / abs(m1_share[[j]]$coefficients) * 100,
#       upper_ci_ATE = upper_ci / abs(m1_share[[j]]$coefficients) * 100,
#       period = val,
#       outcome = "Share Consumption",
#       `Floor_Area` = factor(`Floor_Area`, levels = labels),
#       period = factor(val, levels = c("Morning Cosy",
#                                       "Afternoon Cosy",
#                                       "Peak Rate",
#                                       "Other", 
#                                       "Overall")))
#   
#   all_coefs <- bind_rows(all_coefs, coefs_share)
# }
# 
# # Define the shades of reds
# red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
# 
# # Create the combined ggplot using facet_wrap
# ggplot(all_coefs %>% filter(outcome == "Share Consumption"), aes(x = `Floor_Area`, y = Estimate, fill = `Floor_Area`)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#   geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add vertical line at average ATE
#   scale_fill_manual(values = red_palette) +
#   labs(
#     x = "Floor Area Decile"
#   ) +
#   scale_y_continuous(
#     name = "Share of Daily Comsumption (%)", 
#     labels = scales::percent_format(),
#   ) +  theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#     legend.position = "none"  # Remove legend
#   ) +
#   facet_wrap(~ period, scales = "free_y")
# 
# 
# # Save the combined plot
# ggsave("graphs/floor_area_share_combined.png", device = "png", width = 16, height = 12, units = "cm")
# 
# 
# 
# # Create the combined ggplot using facet_wrap
# ggplot(all_coefs %>% filter(outcome == "Total Consumption", period != "Overall"), 
#        aes(x = `Floor_Area`, y = Estimate, fill = `Floor_Area`)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color="grey") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#   geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add vertical line at average ATE
#   scale_fill_manual(values = red_palette) +
#   labs(
#     x = "Floor Area Decile",
#   ) +
#   scale_y_continuous(
#     name = "Estimate (kWh)"  ) +  
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#     legend.position = "none"  # Remove legend
#   ) +
#   facet_wrap(~ period)
# 
# 
# # Save the combined plot
# ggsave("graphs/floor_area_combined.png", device = "png", width = 16, height = 12, units = "cm")
# 
# 
# rm(list = ls(pattern = "^m_"))
# gc()
# 
# 
# ### Property values
# 
# # Create unique breaks for property_value
# breaks <- unique(quantile(aggregated_data[!is.na(aggregated_data$property_value),]$property_value, 
#                           probs = seq(0, 1, by = 0.1)))
# 
# # Create pretty labels for the categories
# labels <- sapply(1:(length(breaks)-1), function(i) paste0("£", round(breaks[i]/1000, 0), "k to £", 
#                                                           round(breaks[i+1]/1000, 0), "k"))
# labels[length(labels)] <- paste0("£", round(breaks[length(breaks)-1]/1000), "+")
# 
# # Create the categories for property_value
# aggregated_data <- aggregated_data %>%
#   mutate(property_value_category = cut(property_value, 
#                                        breaks = breaks, 
#                                        include.lowest = TRUE,
#                                        labels = labels))
# 
# 
# m_property_value <- feols(consumption_hh ~ i(cosy_contract_active, property_value_category, ref =0) 
#                           | date +  account_id + hdd,
#                           data = aggregated_data %>% filter(!is.na(property_value_category)),
#                           split = ~ rate_period,
#                           cluster = ~account_id)
# 
# m_property_value_share <- feols(share_consumption ~ i(cosy_contract_active, property_value_category, ref =0)
#                                 | date +  account_id + hdd,
#                                 data = aggregated_data %>% 
#                                   filter(!is.na(property_value_category), !rate_period=="Overall") %>%
#                                   group_by(account_id, date) %>%
#                                   mutate(share_consumption = total_consumption/sum(total_consumption)),
#                                 split = ~ rate_period,
#                                 cluster = ~account_id)
# 
# # Initialize an empty data frame to store all coefficients
# all_coefs <- data.frame()
# 
# for (i in 1:5) {
#   
#   # Find model
#   val <- m_property_value[[i]]$model_info$sample$value
#   j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
#   
#   # Extract coefficients and standard errors for total_consumption
#   coefs_total <- coeftable(m_property_value[i]) %>%
#     data.frame() %>%
#     separate(coefficient, into = c("cosy_contract_active", "remove1", "Property Value", "remove2"), 
#              sep = "::") %>%
#     mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
#            upper_ci = Estimate + 1.96 * `Std..Error`,
#            outcome = "Total Consumption",
#            period = factor(sample, levels = c("Morning Cosy",
#                                               "Afternoon Cosy",
#                                               "Peak Rate",
#                                               "Other", 
#                                               "Overall")),
#            `Property Value` = factor(`Property Value`, levels = labels),
#            average = m1[[j]]$coefficients)
#   
#   all_coefs <- bind_rows(all_coefs, coefs_total)
#   
#   # Define the shades of reds
#   red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
#   
#   # Create the ggplot
#   ggplot(coefs_total, aes(x = `Property Value`, y = Estimate, fill = `Property Value`)) +
#     geom_bar(stat = "identity", show.legend = FALSE) +
#     geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#     geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
#     scale_fill_manual(values = red_palette) +
#     labs(
#       x = "Property Value Decile",
#       y = "Half Hourly Consumption in kWh"
#     ) +
#     scale_y_continuous(
#       name = "Estimate (kWh)", 
#     ) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#       legend.position = "none"  # Remove legend
#     )
#   
#   # Print the plot
#   ggsave(paste0("graphs/property_value_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
#          width = 16, height = 8, units = "cm")
#   
# }
# 
# 
# for (i in 1:4) {
#   
#   # Find model
#   val <- m_property_value_share[[i]]$model_info$sample$value
#   j <- which(sapply(1:4, function(j) m1_share[[j]]$model_info$sample$value) == val)
#   
#   # Extract coefficients and standard errors for total_consumption
#   coefs_total <- coeftable(m_property_value_share[i]) %>%
#     data.frame() %>%
#     separate(coefficient, into = c("cosy_contract_active", "remove1", "Property Value", "remove2"), sep = "::") %>%
#     mutate(
#       average = m1_share[[j]]$coefficients,
#       lower_ci = Estimate - 1.96 * `Std..Error`,
#       upper_ci = Estimate + 1.96 * `Std..Error`,
#       `%_ATE` = Estimate / abs(m1_share[[j]]$coefficients) * 100,
#       lower_ci_ATE = lower_ci / abs(m1_share[[j]]$coefficients) * 100,
#       upper_ci_ATE = upper_ci / abs(m1_share[[j]]$coefficients) * 100,
#       period = val,
#       outcome = "Share Consumption",
#       `Property Value` = factor(`Property Value`, levels = labels),
#       period = factor(val, levels = c("Morning Cosy",
#                                       "Afternoon Cosy",
#                                       "Peak Rate",
#                                       "Other", 
#                                       "Overall")))
#   
#   all_coefs <- bind_rows(all_coefs, coefs_total)
#   
#   # Define the shades of reds
#   red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
#   
#   # Create the ggplot
#   ggplot(coefs_total, aes(x = `Property Value`, y = Estimate, fill = `Property Value`)) +
#     geom_bar(stat = "identity", show.legend = FALSE) +
#     geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#     geom_hline(yintercept = m1_share[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
#     scale_fill_manual(values = red_palette) +
#     labs(
#       x = "Property Value Decile",
#       y = "Half Hourly Consumption in kWh"
#     ) +
#     scale_y_continuous(
#       name = "% of Daily Consumption",
#       labels = scales::percent_format(),
#       sec.axis = sec_axis(~ ./m1_share[[j]]$coefficients, name = "% of ATE", labels = scales::percent_format())
#     ) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#       legend.position = "none"  # Remove legend
#     )
#   
#   # Print the plot
#   ggsave(paste0("graphs/share_property_value_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
#          width = 16, height = 8, units = "cm")
#   
# }
# 
# 
# # Define the shades of reds
# red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
# 
# # Create the combined ggplot using facet_wrap
# ggplot(all_coefs %>% filter(outcome == "Share Consumption"), 
#        aes(x = `Property Value`, y = Estimate, fill = `Property Value`)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#   geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add vertical line at average ATE
#   scale_fill_manual(values = red_palette) +
#   labs(
#     x = "Property Value Decile"
#   ) +
#   scale_y_continuous(
#     name = "Share of Daily Comsumption (%)", 
#     labels = scales::percent_format(),
#   ) +  theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#     legend.position = "none"  # Remove legend
#   ) +
#   facet_wrap(~ period, scales = "free_y")
# 
# 
# # Save the combined plot
# ggsave("graphs/property_value_share_combined.png", device = "png", width = 16, height = 12, units = "cm")
# 
# 
# 
# # Create the combined ggplot using facet_wrap
# ggplot(all_coefs %>% filter(outcome == "Total Consumption", period != "Overall"), 
#        aes(x = `Property Value`, y = Estimate, fill = `Property Value`)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color="grey") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#   geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add vertical line at average ATE
#   scale_fill_manual(values = red_palette) +
#   labs(
#     x = "Property Value Decile",
#   ) +
#   scale_y_continuous(
#     name = "Estimate (kWh)"  ) +  
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#     legend.position = "none"  # Remove legend
#   ) +
#   facet_wrap(~ period)
# 
# 
# # Save the combined plot
# ggsave("graphs/property_value_combined.png", device = "png", width = 16, height = 12, units = "cm")
# 
# 
# 
# 
# 
# ### Figure A.36: Impact of Cosy by Region
# m_region <- feols(consumption_hh ~ i(cosy_contract_active, region, ref =0) 
#                   | date +  account_id + hdd,
#                   data = aggregated_data %>% filter(!is.na(region), !region==""),
#                   split = ~ rate_period,
#                   cluster = ~account_id)
# etable(m_region, tex = TRUE, title = "Cosy Adoption by Region",
#        label = "tab:regionaldid",
#        fitstat = ~ N + g + pre_avg +t_obs + r2, 
#        file = "tables/did_region.tex", replace = TRUE)
# 
# 
# # Extract coefficients for total consumption
# all_coefs <- data.frame()
# for (i in 1:5) {
#   val <- m_region[[i]]$model_info$sample$value
#   j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
#   
#   coefs_total <- coeftable(m_region[[i]]) %>%
#     data.frame() %>%
#     tibble::rownames_to_column("term") %>%
#     separate(term, into = c("cosy_contract_active", "remove1", "Region", "remove2"), sep = "::") %>%
#     mutate(
#       average = m1[[j]]$coefficients,
#       lower_ci = Estimate - 1.96 * `Std..Error`,
#       upper_ci = Estimate + 1.96 * `Std..Error`,
#       `%_ATE` = Estimate / abs(m1[[j]]$coefficients) * 100,
#       lower_ci_ATE = lower_ci / abs(m1[[j]]$coefficients) * 100,
#       upper_ci_ATE = upper_ci / abs(m1[[j]]$coefficients) * 100,
#       period = factor(val, levels = c("Morning Cosy",
#                                       "Afternoon Cosy",
#                                       "Peak Rate",
#                                       "Other", 
#                                       "Overall")))
#   
#   
#   all_coefs <- bind_rows(all_coefs, coefs_total)
# }
# 
# # Select a color palette from RColorBrewer
# region_colors <- brewer.pal(n = length(unique(all_coefs$Region)), name = "Set3")
# 
# # Order regions by their estimate size for period == "Morning Cosy"
# morning_cosy_order <- all_coefs %>%
#   filter(period == "Morning Cosy") %>%
#   arrange(desc(Estimate)) %>%
#   pull(Region)
# 
# # Reorder the Region factor based on the estimate size in "Morning Cosy"
# all_coefs <- all_coefs %>%
#   mutate(Region = factor(Region, levels = morning_cosy_order))
# 
# # Create the ggplot
# ggplot(all_coefs %>% filter(period != "Overall"), aes(x = Region, y = Estimate, fill = Region)) +
#   geom_bar(stat = "identity") +
#   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#   geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add vertical line at average ATE
#   scale_fill_manual(values = region_colors) +  # Apply random colors to regions
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#     legend.position = "none"  # Remove legend
#   ) +
#   facet_wrap(~ period)
# 
# # Save the plot
# ggsave("graphs/region_combined.png", device = "png", width = 16, height = 12, units = "cm")
# 
# 
# rm(list = ls(pattern = "^m_"))
# gc()
# 
# ### Figure A.35: Impact of Cosy Adoption by MSOA Income on Consumption
# # Load and preprocess the cosy_hp_details data
# cosy_hp_details <- fread("data/input/cosy_-_cosy_details_2024_07_24.csv") %>%
#   inner_join(aggregated_data %>% select(hashed_mpan) %>% distinct(), by = "hashed_mpan") %>%
#   filter(!is.na(postcode)) %>%
#   distinct(hashed_mpan, postcode)
# 
# 
# postcode_msoa <- fread("data/input/PCD_OA21_LSOA21_MSOA21_LAD_AUG23_UK_LU.csv") %>%
#   left_join(cosy_hp_details, by = c("pcds" = "postcode")) %>%
#   mutate(n = ifelse(is.na(n), 0, 1)) %>%
#   select(msoa21cd, pcds) %>%
#   distinct(pcds, .keep_all = TRUE)
# 
# income <- readxl::read_excel("data/input/saiefy1920finalqaddownload280923.xlsx", sheet = "Total annual income", skip = 4) %>%
#   select(`MSOA code`, `Total annual income (£)`) %>%
#   distinct() %>%
#   inner_join(postcode_msoa, by=c("MSOA code"="msoa21cd")) %>%
#   distinct(pcds, .keep_all = TRUE)
# 
# 
# # Create unique breaks for predicted_heatloss_watts
# income_dist <- readxl::read_excel("data/input/saiefy1920finalqaddownload280923.xlsx", sheet = "Total annual income", skip = 4) %>%
#   select(`MSOA code`, `Total annual income (£)`) 
# 
# 
# breaks <- unique(quantile(income_dist$`Total annual income (£)`/1000, probs = seq(0, 1, by = 0.1)))
# 
# # Create pretty labels for the categories
# labels <- sapply(1:(length(breaks)-1), function(i) paste0("£", round(breaks[i]), "k-", round(breaks[i+1]), "k"))
# 
# aggregated_data <- aggregated_data[,1:53] %>%
#   left_join(cosy_hp_details) %>%
#   left_join(income, by=c("postcode"="pcds")) %>%
#   mutate(income_category = cut(`Total annual income (£)`/1000, 
#                                breaks = breaks, 
#                                include.lowest = TRUE,
#                                labels = labels))
# # income check
# m_income <- feols(consumption_hh ~ i(cosy_contract_active, income_category, ref =0) 
#                   | date +  account_id + hdd,
#                   data = aggregated_data,
#                   split = ~ rate_period,
#                   cluster = ~account_id)
# 
# 
# m_income_share <- feols(share_consumption ~ i(cosy_contract_active, income_category, ref =0) 
#                         | date +  account_id + hdd,
#                         data = aggregated_data %>% 
#                           filter(!is.na(income_category), !rate_period=="Overall") %>%
#                           group_by(account_id, date) %>%
#                           mutate(share_consumption = total_consumption/sum(total_consumption)),
#                         split = ~ rate_period,
#                         cluster = ~account_id)
# 
# 
# for (i in 1:5) {
#   
#   # Find model
#   val <- m_income[[i]]$model_info$sample$value
#   j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
#   
#   # Extract coefficients and standard errors for total_consumption
#   coefs_total <- coeftable(m_income[i]) %>%
#     data.frame() %>%
#     separate(coefficient, into = c("cosy_contract_active", "remove1", "Income Category", "remove2"), sep = "::") %>%
#     mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
#            upper_ci = Estimate + 1.96 * `Std..Error`,
#            outcome = "Total Consumption") %>%
#     mutate(`Income Category` = factor(`Income Category`, levels = labels))
#   
#   # Define the shades of reds
#   red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
#   
#   # Create the ggplot
#   ggplot(coefs_total, aes(x = `Income Category`, y = Estimate, fill = `Income Category`)) +
#     geom_bar(stat = "identity", show.legend = FALSE) +
#     geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#     geom_hline(yintercept = m1[[j]]$coefficients, linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add horizontal line at ATE
#     scale_fill_manual(values = red_palette) +
#     labs(
#       x = "Income Decile",
#       y = "Half Hourly Consumption in kWh"
#     ) +
#     scale_y_continuous(
#       name = "Estimate (kWh)", 
#       sec.axis = sec_axis(~ ./m1[[j]]$coefficients, name = "% of ATE", labels = scales::percent_format())
#       
#     ) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#       legend.position = "none"  # Remove legend
#     )
#   
#   # Print the plot
#   ggsave(paste0("graphs/income_", val %>% tolower() %>% str_replace(" ", "_"), ".png"),
#          width = 16, height = 8, units = "cm")
#   
# }
# 
# 
# # Initialize an empty data frame to store all coefficients
# all_coefs <- data.frame()
# 
# # Extract coefficients for total consumption
# for (i in 1:5) {
#   val <- m_income[[i]]$model_info$sample$value
#   j <- which(sapply(1:5, function(j) m1[[j]]$model_info$sample$value) == val)
#   
#   coefs_total <- coeftable(m_income[[i]]) %>%
#     data.frame() %>%
#     tibble::rownames_to_column("term") %>%
#     separate(term, into = c("cosy_contract_active", "remove1", "Income Category", "remove2"), sep = "::") %>%
#     mutate(
#       average = m1[[j]]$coefficients,
#       lower_ci = Estimate - 1.96 * `Std..Error`,
#       upper_ci = Estimate + 1.96 * `Std..Error`,
#       `%_ATE` = Estimate / abs(m1[[j]]$coefficients) * 100,
#       lower_ci_ATE = lower_ci / abs(m1[[j]]$coefficients) * 100,
#       upper_ci_ATE = upper_ci / abs(m1[[j]]$coefficients) * 100,
#       outcome = "Total Consumption",
#       `Income Category` = factor(`Income Category`, levels = labels),
#       period = factor(val, levels = c("Morning Cosy",
#                                       "Afternoon Cosy",
#                                       "Peak Rate",
#                                       "Other", 
#                                       "Overall")))
#   
#   all_coefs <- bind_rows(all_coefs, coefs_total)
# }
# 
# 
# # Define the shades of reds
# red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
# 
# 
# # Create the combined ggplot using facet_wrap
# ggplot(all_coefs %>% filter(outcome == "Total Consumption", period != "Overall"), 
#        aes(x =`Income Category`, y = Estimate, fill = `Income Category`)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#   geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add vertical line at average ATE
#   scale_fill_manual(values = red_palette) +
#   labs(
#     x = "MSOA Income Decile",
#   ) +
#   scale_y_continuous(
#     name = "Estimate (kWh)", 
#   ) +  
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#     legend.position = "none"  # Remove legend
#   ) +
#   facet_wrap(~ period, scales = "free_y")
# 
# 
# # Save the combined plot
# ggsave("graphs/income_category_combined.png", device = "png", width = 16, height = 12, units = "cm")
# 
# 
# all_coefs <- list()
# # Extract coefficients for share consumption
# for (i in 1:4) {
#   val <- m_income_share[[i]]$model_info$sample$value
#   j <- which(sapply(1:4, function(j) m1_share[[j]]$model_info$sample$value) == val)
#   
#   coefs_share <- coeftable(m_income_share[[i]]) %>%
#     data.frame() %>%
#     tibble::rownames_to_column("term") %>%
#     separate(term, into = c("cosy_contract_active", "remove1", "income_decile", "remove2"), sep = "::") %>%
#     mutate(
#       average = m1_share[[j]]$coefficients,
#       lower_ci = Estimate - 1.96 * `Std..Error`,
#       upper_ci = Estimate + 1.96 * `Std..Error`,
#       `%_ATE` = Estimate / abs(m1_share[[j]]$coefficients) * 100,
#       lower_ci_ATE = lower_ci / abs(m1_share[[j]]$coefficients) * 100,
#       upper_ci_ATE = upper_ci / abs(m1_share[[j]]$coefficients) * 100,
#       period = val,
#       outcome = "Share Consumption",
#       `income_decile` = factor(`income_decile`, levels = labels),
#       period = factor(val, levels = c("Morning Cosy",
#                                       "Afternoon Cosy",
#                                       "Peak Rate",
#                                       "Other", 
#                                       "Overall")))
#   
#   all_coefs <- bind_rows(all_coefs, coefs_share)
# }
# 
# # Define the shades of reds
# red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")
# 
# # Create the combined ggplot using facet_wrap
# ggplot(all_coefs %>% filter(outcome == "Share Consumption"), 
#        aes(x = `income_decile`, y = Estimate, fill = `income_decile`)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "grey") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
#   geom_hline(aes(yintercept = average), linetype = "dashed", color = cosy_color, alpha = 0.6) +  # Add vertical line at average ATE
#   scale_fill_manual(values = red_palette) +
#   labs(
#     x = "MSOA Income Decile"
#   ) +
#   scale_y_continuous(
#     name = "Share of Daily Comsumption (%)", 
#     labels = scales::percent_format(),
#   ) +  theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
#     legend.position = "none"  # Remove legend
#   ) +
#   facet_wrap(~ period, scales = "free_y")
# 
# 
# # Save the combined plot
# ggsave("graphs/income_share_combined.png", device = "png", width = 16, height = 12, units = "cm")

source("scripts/02_08_cosy_and_hp_coadoption.R")

# ### Figure A.21: Main Results Coefficients With and Without Controlling for Heat Pump
# # Register the pre-treatment average fit statistic
# fitstat_register("pre_avg3", function(x) {
#   
#   # Extract the formula
#   formula <- x$fml_all$linear
#   
#   # Extract the outcome variable from the formula
#   outcome_variable <- all.vars(formula)[1]
#   
#   # Extract the call object and evaluate the data argument
#   call_object <- x$call
#   data_expr <- call_object$data
#   data <- eval(data_expr)
#   
#   # Get the logical vector of observations used in the model
#   obs_used <- obs(x)
#   
#   # Subset the original dataset using this logical vector
#   data_used <- data[obs_used, ]
#   
#   # Ensure the outcome variable is treated as a column name
#   outcome_values <- data_used[[outcome_variable]]
#   
#   # Create pre-avg for non-HP installed group
#   pre_avg <- mean(outcome_values[data_used$is_hp_installed == 1 & data_used$cosy_contract_active == 1], na.rm = TRUE)
#   
#   # Format the pre-avg
#   formatted_pre_avg <- format_decimal(pre_avg)
#   
#   return(formatted_pre_avg)
# }, "Half Hourly Consumption Pre-Cosy")
# 
# 
# # Register the pre-treatment average fit statistic
# fitstat_register("pre_avg2", function(x) {
#   
#   # Extract the formula
#   formula <- x$fml_all$linear
#   
#   # Extract the outcome variable from the formula
#   outcome_variable <- all.vars(formula)[1]
#   
#   # Extract the call object and evaluate the data argument
#   call_object <- x$call
#   data_expr <- call_object$data
#   data <- eval(data_expr)
#   
#   # Get the logical vector of observations used in the model
#   obs_used <- obs(x)
#   
#   # Subset the original dataset using this logical vector
#   data_used <- data[obs_used, ]
#   
#   # Ensure the outcome variable is treated as a column name
#   outcome_values <- data_used[[outcome_variable]]
#   
#   # Create pre-avg for non-HP installed group
#   pre_avg <- mean(outcome_values[data_used$is_hp_installed == 0], na.rm = TRUE)
#   
#   # Format the pre-avg
#   formatted_pre_avg <- format_decimal(pre_avg)
#   
#   return(formatted_pre_avg)
# }, "Half Hourly Consumption Pre-Heatpump")
# 
# 
# # get the subsample with HP installation by OE
# # get install date
# hp_install_date <- fread("data/input/cosy_-_hp_details_2024_06_25.csv") %>%
#   select(account_id, installed_at) %>%
#   mutate(installed_at = as.Date(installed_at))
# 
# # get people from the survey responders
# survey_responses <- fread("data/input/responses.csv") %>%
#   select(-Other) %>% 
#   rename(account_number = kid) %>%
#   inner_join(fread("data/input/survey_ids.csv")) %>%
#   mutate(installed_at_2 = as.Date(`When was your heat pump installed?`),
#          `Electric vehicle(s)` = as.numeric(`Electric vehicle(s)`=="Electric vehicle(s)"))
# 
# aggregated_data <- aggregated_data %>%
#   left_join(hp_install_date)  %>%
#   left_join(survey_responses)  %>%
#   mutate(is_hp_installed = ifelse(!is.na(installed_at), 
#                                   as.numeric(installed_at <= date),
#                                   ifelse(!is.na(installed_at_2),  as.numeric(installed_at_2 <= date),
#                                          NA)))
# 
# 
# m_hp_cosy <- feols(consumption_hh ~ i(is_hp_installed) + i(cosy_contract_active) | hdd + account_id + date, 
#                    data = aggregated_data, 
#                    cluster = ~account_id, 
#                    split = ~ rate_period)
# 
# etable(m_hp_cosy, tex=TRUE, title = "HP and Cosy Adoption",
#        fitstat = ~ N + g + pre_avg2 + pre_avg3  +t_obs + r2, file = "tables/did_hp_install.tex", replace = TRUE, label="tab:did-hp-install")
# 
# 
# 
# # Read the generated LaTeX file
# file_content <- readLines("tables/did_hp_install.tex")
# 
# # Find the lines with the pre-treatment average and remove them
# pre_avg_line_index <- grep("Half Hourly Consumption Pre-Heatpump", file_content)
# pre_avg_line_index2 <- grep("Half Hourly Consumption Pre-Cosy", file_content)
# pre_avg_lines <- file_content[pre_avg_line_index]
# pre_avg_lines2 <- file_content[pre_avg_line_index2]
# 
# file_content <- file_content[-c(pre_avg_line_index, pre_avg_line_index2)]
# 
# # Find the position just after the coefficients
# coeff_end_index <- grep("Cosy Contract Active", file_content) + 2
# if (length(coeff_end_index) > 1) {
#   coeff_end_index <- coeff_end_index[-1]
# }
# 
# # Insert the pre-treatment average row after the coefficients
# file_content <- append(file_content, pre_avg_lines, after = coeff_end_index)
# file_content <- append(file_content, pre_avg_lines2, after = coeff_end_index)
# file_content <- append(file_content,"\\emph{Pre-Treatment Average}\\", after = coeff_end_index)
# # Add a \midrule after the pre-treatment average
# file_content <- append(file_content, "\\midrule", after = coeff_end_index + length(pre_avg_lines))
# 
# # Modify the label for "Size of the 'effective' sample" to "Number of Households"
# sample_line <- grep("Size of the 'effective' sample", file_content)
# file_content[sample_line] <- gsub("Size of the 'effective' sample", "Number of Households", file_content[sample_line])
# 
# 
# # Write the modified content back to the LaTeX file
# writeLines(file_content, "tables/did_hp_install.tex")
# 
# rm(list = ls(pattern = "^m_"))
# gc()
# 
# 
# # Write the modified content back to the LaTeX file
# # plot the coefficients with and without controls
# 
# m_without_control <- feols(consumption_hh ~ i(cosy_contract_active) | hdd + account_id + date, 
#                            data = aggregated_data %>% filter(!is.na(installed_at_2)), 
#                            split = ~ rate_period,
#                            cluster = ~account_id)
# m_with_control <- feols(consumption_hh ~ i(cosy_contract_active) | hdd + account_id + date + is_hp_installed, 
#                         data = aggregated_data %>% filter(!is.na(installed_at_2)), 
#                         split = ~ rate_period,
#                         cluster = ~account_id) 
# 
# # Extract coefficients and standard errors for total_consumption
# coefs_total <- rbind(coeftable(m_without_control)  %>%
#                        data.frame() %>%
#                        mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
#                               upper_ci = Estimate + 1.96 * `Std..Error`,
#                               model = "Without HP Installation Date"),
#                      coeftable(m_with_control)  %>%
#                        data.frame() %>%
#                        mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
#                               upper_ci = Estimate + 1.96 * `Std..Error`,
#                               model = "With HP Installation Date")) %>%
#   mutate(rate_period = factor(sample, levels = c("Morning Cosy",
#                                                  "Afternoon Cosy",
#                                                  "Peak Rate",
#                                                  "Other", 
#                                                  "Overall")))
# 
# # Define custom colors
# custom_colors <- c("Without HP Installation Date" = "#E8F5FF", "With HP Installation Date" = cosy_color)
# 
# # Create the ggplot
# ggplot(coefs_total, aes(x = factor(rate_period), y = Estimate, fill = model)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 1), show.legend = TRUE) +
#   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(width = 1), alpha=0.6) +
#   scale_fill_manual(values = custom_colors) +
#   labs(x = "Rate Period",
#        y = "Coefficient Estimate",
#        fill = "Model") +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# # Print the plot
# ggsave(paste0("graphs/did_controlling_hp_installation.png"),
#        width = 16, height = 8, units = "cm")
# 
# 
# etable(m_with_control, m_without_control, tex=TRUE, title = "HP and Cosy Adoption",
#        fitstat = ~ N + g  + pre_avg2 + pre_avg3 +t_obs + r2, file = "tables/did_hp_install_overall.tex", replace = TRUE, label="tab:did-hp-install-overall")
# 
# 
# # Read the generated LaTeX file
# file_content <- readLines("tables/did_hp_install_overall.tex")
# 
# # F# Find the lines with the pre-treatment average and remove them
# pre_avg_line_index <- grep("Half Hourly Consumption Pre-Heatpump", file_content)
# pre_avg_line_index2 <- grep("Half Hourly Consumption Pre-Cosy", file_content)
# pre_avg_lines <- file_content[pre_avg_line_index]
# pre_avg_lines2 <- file_content[pre_avg_line_index2]
# 
# file_content <- file_content[-c(pre_avg_line_index, pre_avg_line_index2)]
# 
# # Find the position just after the coefficients
# coeff_end_index <- grep("Cosy Contract Active", file_content) + 2
# if (length(coeff_end_index) > 1) {
#   coeff_end_index <- coeff_end_index[-1]
# }
# 
# # Insert the pre-treatment average row after the coefficients
# file_content <- append(file_content, pre_avg_lines, after = coeff_end_index)
# file_content <- append(file_content, pre_avg_lines2, after = coeff_end_index)
# file_content <- append(file_content,"\\emph{Pre-Treatment Average}\\", after = coeff_end_index)
# # Add a \midrule after the pre-treatment average
# file_content <- append(file_content, "\\midrule", after = coeff_end_index + length(pre_avg_lines))
# 
# # Modify the label for "Size of the 'effective' sample" to "Number of Households"
# sample_line <- grep("Size of the 'effective' sample", file_content)
# file_content[sample_line] <- gsub("Size of the 'effective' sample", "Number of Households", file_content[sample_line])
# 
# # Write the modified content back to the LaTeX file
# writeLines(file_content, "tables/did_hp_install_overall.tex")

source("scripts/02_09_structural_winner.R")

source("scripts/02_10_DiD_analysis.R")

