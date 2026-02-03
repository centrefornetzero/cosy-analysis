# get consumption by period
hp_installed_period <- fread(file.path(datapath, "input/cosy_-_hp_aggregated_up_2024_06_18.csv")) %>%
  rename(total_consumption=total_read_value,
         date = settlement_date) %>%
  mutate(consumption_hh = ifelse(rate_period == "Other", 
                                 total_consumption/30, total_consumption/6),
         date = as.Date(date)) %>%

  # aggregate from mpan level up to account level 
  # install_date is at the account level 
  group_by(account_id, date, rate_period) %>%
  summarise(total_consumption = sum(total_consumption), 
           consumption_hh = sum(consumption_hh)) %>%
  ungroup()

# aggreate up to get overall consumption
hp_installed_daily <- hp_installed_period %>%
  group_by(account_id, date) %>%
  summarise(total_consumption = sum(total_consumption)) %>%
  mutate(consumption_hh = total_consumption / 48,
         rate_period = factor("Overall")) 

# read in household covariates
# for hhs where there are multiple mpans for one account, take covariates associated
# with largest EAC
covariates <- 
  fread(file.path(datapath, "input/cosy_-_hp_details_2024_06_25.csv")) %>%
  group_by(account_id) %>%
  arrange(-estimated_annual_consumption) %>%
  filter(row_number() == 1)


# bind period-level and overall consumption data to make regressions easier
# and join in covariates 
hp_installed <- 
  bind_rows(hp_installed_period, hp_installed_daily) %>%
  inner_join(covariates, by=c("account_id"))

# add daily weather data
weather <- fread(file.path(datapath, "input/Cosy Analysis Weather Mar 26 daily.csv")) %>% 
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

# check there are no duplicated rows
stopifnot(group_by(hp_installed, account_id, date, rate_period) %>% filter(n() > 1) %>% nrow() == 0)

rm(weather, hp_installed_daily, hp_installed_period)

# clean up
hp_installed <- hp_installed %>%
  mutate(date = as.Date(date),
         is_hp_installed = as.numeric(installed_at <= date)) %>%
  mutate(rate_period = recode(
               rate_period,
               "Morning Cosy"   = "Morning Off-peak",
               "Afternoon Cosy" = "Afternoon Off-peak"
             ),
             rate_period = factor(
               rate_period,
               levels = c(
                 "Morning Off-peak",
                 "Afternoon Off-peak",
                 "Peak Rate",
                 "Other",
                 "Overall"
               )
             )
           ) %>%
  group_by(account_id) %>%
  mutate(treated = max(is_hp_installed)) %>% # identify treated versus not yet treated
  ungroup() 

# check that data is unique at the account-date-period level
stopifnot(hp_installed %>% group_by(account_id, date, rate_period) %>% filter(n() > 1) %>% nrow() == 0)

# Save usefull variables into hp_installed
start_date <- min(hp_installed$date)
hp_installed <-  hp_installed %>% 
  mutate(settlement_week = floor_date(date, "week") + 1,
    week      = as.numeric(difftime(settlement_week, start_date, units = "weeks")) %/% 1 + 1,
    firstweek = as.numeric(difftime(installed_at, start_date, units = "weeks")) %/% 1 + 1
  ) 

write_rds(hp_installed, file.path(datapath, "output/hp_installed.rds"))

# summary statistics
summary(hp_installed)

# HP deals and installation
deals_and_installations <- fread(file.path(datapath, "/input/cosy_-_hp_deals_and_installation_2025_06_06.csv")) %>%
  distinct(account_id, .keep_all = TRUE)

# Run on a subsample of the data for faster processing
if (random_subsample) {
  set.seed(123)
  sampled_accounts <- sample(unique(hp_installed$account_id), 1000)
  hp_installed <- hp_installed %>% 
    filter(account_id %in% sampled_accounts)
  gc()
}



# ====================================================================
# Create did data by combining elec and gas consumption data
#====================================================================
# ------------------- Load elec consumption data ----------------
hp_installed <- 
  hp_installed %>%
  filter(rate_period == "Overall") 

hp_installed %>% ungroup() %>% filter(treated==1) %>% select(account_id) %>% distinct() %>% dim()

hp_installed_weekly <- 
  hp_installed %>%
  group_by(account_id, hashed_mpan, settlement_week, installed_at, tariff_gsp_group_id) %>%
  summarise(elec_consumption = sum(total_consumption)) %>%
  ungroup


# ------------------- Load gas consumption data ----------------
# previous 2024_06_13.csv
cosy_hp_install_gas_consumption <- fread(file.path(datapath, "input/cosy_-_hp_users_gas_2024_06_13.csv")) %>%
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
              distinct(account_id,min_settlement_week, installed_at)) %>%
  left_join(cosy_hp_install_gas_consumption %>% 
              distinct(account_id, settlement_week, weekly_consumption)) %>%
  filter(as.Date(min_settlement_week) < as.Date(settlement_week)) %>%
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
weather_weekly <- fread(file.path(datapath, "input/cosy_-_weather_weekly_2024_06_13.csv")) %>%
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
            
write_rds(overall_weekly, file.path(datapath, "output/overall_weekly.rds"))

            