# Load data 

# get consumption by period
hp_installed <- fread("data/input/cosy_-_hp_aggregated_up_2024_06_18.csv") %>%
  group_by(account_id, settlement_date) %>%
  summarise(total_consumption = sum(total_read_value)) %>%
  mutate(consumption_hh = total_consumption / 48) %>%
  inner_join(fread("data/input/cosy_-_hp_details_2024_06_25.csv") %>%
               distinct(account_id, .keep_all = TRUE),
             by=c("account_id")) %>%
  mutate(date = as.Date(settlement_date),
         is_hp_installed = as.numeric(installed_at <= date))%>%
  group_by(account_id) %>%
  mutate(treated = max(is_hp_installed))  # identify treated versus not yet treated

# add daily weather data
weather <- fread("data/input/Cosy Analysis Weather Mar 26 daily.csv") %>% 
  rename_with(.cols = starts_with("weekly"), 
              .fn = ~ sub("^weekly", "daily", .)) %>%
  rename(tariff_gsp_group_id=gsp_group_id) %>%
  mutate(date_day=as.Date(date_day, format = "%Y-%m-%d")) %>%
  rename(date = date_day)

# merge weather
hp_installed <- hp_installed %>%
  left_join(weather)

# Round degrees Celsius 
hp_installed <- hp_installed %>% mutate(hdd = factor(
  case_when(
    daily_avg_air_temperature_celsius < 0 ~ 0,
    daily_avg_air_temperature_celsius < 15.5 ~ round(daily_avg_air_temperature_celsius),
    TRUE ~ 15
  )
))


# Check number of accounts
hp_installed %>% ungroup() %>% filter(treated==1) %>% select(account_id) %>% distinct() %>% dim()

# Load and preprocess gas consumption data
# previous 2024_06_13.csv
cosy_hp_install_gas_consumption <- fread("data/input/cosy_-_hp_users_gas_2024_06_13.csv") %>%
  group_by(account_id) %>%
  mutate(is_hp_installed = as.numeric(installed_at <= settlement_week),
         treated = max(is_hp_installed),
         min_settlement_week = min(settlement_week)) %>%
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
  left_join(cosy_hp_install_gas_consumption %>% 
              distinct(account_id, settlement_week, weekly_consumption, 
                       min_settlement_week, installed_at)) %>%
  filter(min_settlement_week < settlement_week) %>%
  mutate(
    is_hp_installed = as.numeric(installed_at < settlement_week),
    weekly_consumption = ifelse(is.na(weekly_consumption), 0, weekly_consumption)
  ) 

# Define overall_weekly by merging with electricity data
overall_weekly <- hp_installed %>%
  mutate(settlement_week = floor_date(date, "week") + 1,
         is_hp_installed = as.numeric(installed_at <= settlement_week)) %>%
  group_by(account_id, hashed_mpan, tariff_gsp_group_id, settlement_week, treated, installed_at, is_hp_installed) %>%
  summarise(elec_consumption = sum(total_consumption)) %>%
  left_join(merged_data %>%
              select(account_id, settlement_week, weekly_consumption) %>%
              rename(gas_consumption = weekly_consumption)) %>%
  mutate(
    gas_consumption = 52.25 * gas_consumption,
    elec_consumption = 52.25 * elec_consumption,
    total_consumption = gas_consumption + elec_consumption
  ) 



# Create CS main results 
start_date <- min(overall_weekly$settlement_week)
did_data <- overall_weekly %>%
  ungroup() %>%
  mutate(
    week = as.numeric(difftime(settlement_week, start_date, units = "weeks")) %/% 1 + 1,
    firstweek = as.numeric(difftime(installed_at, start_date, units = "weeks")) %/% 1 + 1
  ) %>%
  group_by(account_id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  select(id, firstweek, week, total_consumption, elec_consumption, gas_consumption) %>%
  filter(week <= 129, firstweek <= 129) 

# Step 2: Estimate CS models and save results

# Define the output filenames for the main analysis
output_filenames <- c(
  "data/scratch/est_cs_total_weekly.RDS",
  "data/scratch/est_cs_elec_weekly.RDS",
  "data/scratch/est_cs_gas_weekly.RDS"
)

# Define the corresponding variable names
yname_vars <- c("total_consumption", "elec_consumption", "gas_consumption")

# Estimate and save the results for the main analysis (not yet treated control group)
for (i in seq_along(yname_vars)) {
  yname <- yname_vars[i]
  filename <- output_filenames[i]
  
  message("Estimating treatment effect for ", yname, " and saving to ", filename)
  
  est_cs <- att_gt(yname = yname,
                   tname = "week",
                   idname = "id",
                   gname = "firstweek",
                   data = did_data,
                   anticipation = 1,
                   clustervars = "id",
                   control_group = c("notyettreated"),
                   allow_unbalanced_panel = TRUE,
                   base_period = "varying")
  
  saveRDS(est_cs, filename)
  message("Saved: ", filename)
}


# Robustness check with base_period = "universal"
robust_output_filenames <- gsub("\\.RDS$", "_robust_universal.RDS", output_filenames)

for (i in seq_along(yname_vars)) {
  yname <- yname_vars[i]
  filename <- robust_output_filenames[i]
  
  message("Estimating treatment effect for ", yname, " with universal base period and saving to ", filename)
  
  est_cs <- att_gt(yname = yname,
                   tname = "week",
                   idname = "id",
                   gname = "firstweek",
                   data = did_data,
                   anticipation = 1,
                   clustervars = "id",
                   control_group = c("notyettreated"),
                   allow_unbalanced_panel = TRUE,
                   base_period = "universal")
  
  saveRDS(est_cs, filename)
  message("Saved: ", filename)
}

# Step 3: Estimate CS models for the "never treated" control group

never_treated_output_filenames <- gsub("est_cs_", "est_cs_never_treated_", output_filenames)

did_data <- overall_weekly %>%
  ungroup() %>%
  mutate(
    week = as.numeric(difftime(settlement_week, start_date, units = "weeks")) %/% 1 + 1,
    firstweek = as.numeric(difftime(installed_at, start_date, units = "weeks")) %/% 1 + 1
  ) %>%
  group_by(account_id) %>%
  mutate(id = cur_group_id(),
         firstweek = ifelse(firstweek>129, 0, firstweek)) %>%
  ungroup() %>%
  select(id, firstweek, week, total_consumption, elec_consumption, gas_consumption) %>%
  filter(week <= 129) 

for (i in seq_along(yname_vars)) {
  yname <- yname_vars[i]
  filename <- never_treated_output_filenames[i]
  
  message("Estimating treatment effect for ", yname, " with 'never treated' control group and saving to ", filename)
  
  est_cs <- att_gt(yname = yname,
                   tname = "week",
                   idname = "id",
                   gname = "firstweek",
                   data = did_data,
                   anticipation = 1,
                   clustervars = "id",
                   control_group = c("notyettreated", "nevertreated"),
                   allow_unbalanced_panel = TRUE,
                   base_period = "varying")
  
  saveRDS(est_cs, filename)
  message("Saved: ", filename)
}

# Step 4: Estimate CS models for the gas-only subset
start_date <- min(overall_weekly$settlement_week)
start_date <- min(overall_weekly$settlement_week)
did_data <- overall_weekly %>%
  filter(account_id %in% merged_data$account_id) %>%
  ungroup() %>%
  mutate(
    week = as.numeric(difftime(settlement_week, start_date, units = "weeks")) %/% 1 + 1,
    firstweek = as.numeric(difftime(installed_at, start_date, units = "weeks")) %/% 1 + 1
  ) %>%
  group_by(account_id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  select(id, firstweek, week, total_consumption, elec_consumption, gas_consumption) %>%
  filter(week <= 129, firstweek <= 129)  

gas_only_output_filename <- "data/scratch/est_cs_elec_weekly_gas_only.RDS"

message("Estimating treatment effect for electricity consumption in gas-only sample and saving to ", gas_only_output_filename)

est_cs_gas_only <- att_gt(yname = "elec_consumption",
                          tname = "week",
                          idname = "id",
                          gname = "firstweek",
                          data = did_data,
                          anticipation = 1,
                          clustervars = "id",
                          control_group = c("notyettreated"),
                          allow_unbalanced_panel = TRUE,
                          base_period = "varying")

saveRDS(est_cs_gas_only, gas_only_output_filename)
message("Saved: ", gas_only_output_filename)






