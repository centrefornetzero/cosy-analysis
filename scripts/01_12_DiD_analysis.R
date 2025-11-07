# ====================================================================
# Create did data by combining elec and gas consumption data
# ====================================================================
# ------------------- Load gas & elec consumption data ----------------
hp_installed <- read_rds("../gcs/cosy2/output/hp_installed.rds")

hp_installed_weekly <- 
  hp_installed %>%
  filter(rate_period == "Overall") %>%
  mutate(settlement_week = floor_date(date, "week") + 1) %>%
  group_by(account_id, hashed_mpan, tariff_gsp_group_id, settlement_week, installed_at) %>%
  summarise(elec_consumption = sum(total_consumption)) %>%
  ungroup

hp_installed %>% ungroup() %>% filter(treated==1) %>% select(account_id) %>% distinct() %>% dim()


# previous 2024_06_13.csv
cosy_hp_install_gas_consumption <- fread("../gcs/cosy2/input/cosy_-_hp_users_gas_2024_06_13.csv") %>%
  group_by(account_id) %>%
  distinct(account_id, settlement_week, .keep_all = TRUE)


# ---------------------- Balance the datasets ---------------------
# for each account number, take min and max settlement week for which they have
# consumption, whether that's from the elec or gas dataset
full <- 
  select(hp_installed_weekly, account_id, settlement_week) %>%
  bind_rows(select(cosy_hp_install_gas_consumption, account_id, settlement_week)) %>%
  distinct() %>%
  group_by(account_id) %>%
  complete(
    settlement_week = seq(min(settlement_week), max(settlement_week), by = "week")
  )


# Make it balanced: for each account_id, include all weeks from min to max date 
hp_weekly_balanced <- 
  full %>%

  # first only restrict to customers who ever had gas consumption
  inner_join(distinct(hp_installed_weekly, account_id)) %>% 
  full_join(hp_installed_weekly) %>%

  # Fill in covariates 
  group_by(account_id) %>%
  fill(tariff_gsp_group_id, installed_at, .direction = "downup") %>%

  # Choose how to treat missing consumption for added weeks:
  mutate(elec_consumption = tidyr::replace_na(elec_consumption, 0)) %>%  # or keep as NA?
  ungroup()


# Make it balanced: for each account_id, include all weeks from min to max consumption week 
gas_weekly_balanced <- 
  full %>%
  
  # first only restrict to customers who ever had gas consumption
  inner_join(distinct(cosy_hp_install_gas_consumption, account_id)) %>% 
  full_join(cosy_hp_install_gas_consumption) %>% 

  group_by(account_id) %>%
  fill(installed_at, .direction = "downup") %>%

  mutate(weekly_consumption = replace_na(weekly_consumption, 0)) %>% 
  ungroup() %>%
  select(-gsp_group_id) %>%
  rename(gas_consumption = weekly_consumption)



# ---------------------- Merge gas and elec data ---------------------
# Define overall_weekly by merging with electricity data
# panel is balanced - any missing consumption is coded as 0
overall_weekly <- 
  hp_weekly_balanced %>%
  full_join(gas_weekly_balanced) %>%
  group_by(account_id) %>%
  mutate(
      is_hp_installed = as.numeric(installed_at <= settlement_week),
      treated = max(is_hp_installed),
      elec_consumption = replace_na(elec_consumption, 0), 
      gas_consumption = 52.25 * gas_consumption,
      elec_consumption = 52.25 * elec_consumption,
      total_consumption = gas_consumption + elec_consumption
  ) %>%
  ungroup()


# add weather 
weather_weekly <- fread("../gcs/cosy2/input/cosy_-_weather_weekly_2024_06_13.csv") %>%
  mutate(settlement_week = as.Date(week_date)) %>%
  distinct(gsp_group_id, settlement_week, .keep_all = TRUE) %>%
  select(gsp_group_id, settlement_week, avg_heating_degree, avg_air_temperature_celsius) %>%
  rename(tariff_gsp_group_id = gsp_group_id)

# merge with consumption data
overall_weekly <- overall_weekly %>%
  inner_join(weather_weekly) %>% 
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
  select(id, firstweek, week, total_consumption, elec_consumption, gas_consumption)

check = fastdid::fastdid(data = did_data, 
       result_type = "simple", 
       outcomevar = "gas_consumption", 
       timevar = "week", 
       unitvar = "id", 
       cohortvar = "firstweek", 
       base_period = "varying", 
        boot = TRUE,
       clustervar = "id", 
               allow_unbalance_panel = TRUE)
stop()
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






