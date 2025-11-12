# ====================================================================
# Create did data by combining elec and gas consumption data
# ====================================================================
# ------------------- Load elec consumption data ----------------
hp_installed <- 
  read_rds("../gcs/cosy2/output/hp_installed.rds") %>%
  filter(rate_period == "Overall") 

hp_installed %>% ungroup() %>% filter(treated==1) %>% select(account_id) %>% distinct() %>% dim()

hp_installed_weekly <- 
  hp_installed %>%
  mutate(settlement_week = floor_date(date, "week") + 1) %>%
  group_by(account_id, hashed_mpan, settlement_week, installed_at, tariff_gsp_group_id) %>%
  summarise(elec_consumption = sum(total_consumption)) %>%
  ungroup


# ------------------- Load gas consumption data ----------------
# previous 2024_06_13.csv
cosy_hp_install_gas_consumption <- fread("../gcs/cosy2/input/cosy_-_hp_users_gas_2024_06_13.csv") %>%
  group_by(account_id) %>%
  distinct(account_id, settlement_week, .keep_all = TRUE) %>%
  mutate(min_settlement_week = min(settlement_week))

# Create a sequence of weeks
min_date <- min(cosy_hp_install_gas_consumption$settlement_week)
max_date <- max(cosy_hp_install_gas_consumption$settlement_week)
all_weeks <- seq(min_date, max_date, by = "week")

# Create a data frame with all combinations of account_id and settlement_week
all_combinations <- expand.grid(
  account_id = unique(cosy_hp_install_gas_consumption$account_id),
  settlement_week = all_weeks
)

# Merge with original unbalanced gas data
merged_data <- all_combinations %>%
  left_join(cosy_hp_install_gas_consumption %>% 
              distinct(account_id, settlement_week, weekly_consumption, 
                       min_settlement_week, installed_at)) %>%
  filter(min_settlement_week < settlement_week) %>%
  mutate(
    weekly_consumption = ifelse(is.na(weekly_consumption), 0, 
                                weekly_consumption)
  ) %>%
  select(account_id, settlement_week, gas_consumption = weekly_consumption)

# ------------------- Merge gas and elec consumption ------------------
# Define overall_weekly by merging with electricity data
overall_weekly <- 
  hp_installed_weekly %>%
  left_join(merged_data, by = c("account_id", "settlement_week")) %>%
  mutate(gas_consumption = 52.25 * gas_consumption,
         elec_consumption = 52.25 * elec_consumption,
         total_consumption = gas_consumption + elec_consumption,
         is_hp_installed = as.numeric(installed_at < settlement_week)) 
     

# add weather 
weather_weekly <- fread("../gcs/cosy2/input/cosy_-_weather_weekly_2024_06_13.csv") %>%
  mutate(settlement_week = as.Date(week_date)) %>%
  distinct(gsp_group_id, settlement_week, .keep_all = TRUE) %>%
  select(gsp_group_id, settlement_week, avg_heating_degree, avg_air_temperature_celsius) %>%
  rename(tariff_gsp_group_id = gsp_group_id)

# merge with consumption data
overall_weekly <- overall_weekly %>%
  left_join(weather_weekly, by = c("tariff_gsp_group_id", "settlement_week")) %>% 
  mutate(hdd = factor(
      case_when(
          avg_air_temperature_celsius < 0 ~ 0,
          avg_air_temperature_celsius < 15.5 ~ round(avg_air_temperature_celsius),
    TRUE ~ 15
  )),
  temp_degree = factor(
    case_when(
      avg_air_temperature_celsius < 0 ~ 0,
      avg_air_temperature_celsius < 25.5 ~ round(avg_air_temperature_celsius),
      TRUE ~ 25
    )))
            
write_rds(overall_weekly, "../gcs/cosy2/output/overall_weekly.rds")

            
# create a dataset that fits the needs of did() function
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


# ---------------------- Create CS main results ---------------------


# Step 2: Estimate CS models and save results

# Define the output filenames for the main analysis
output_filenames <- c(
  "../gcs/cosy2/scratch/est_cs_total_weekly.RDS",
  "../gcs/cosy2/scratch/est_cs_elec_weekly.RDS",
  "../gcs/cosy2/scratch/est_cs_gas_weekly.RDS"
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

gas_only_output_filename <- "../gcs/cosy2/scratch/est_cs_elec_weekly_gas_only.RDS"

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






