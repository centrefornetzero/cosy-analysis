# get consumption by period
hp_installed_period <- fread("../gcs/cosy2/input/cosy_-_hp_aggregated_up_2024_06_18.csv") %>%
  rename(total_consumption=total_read_value,
         date = settlement_date) %>%
  mutate(consumption_hh = ifelse(rate_period == "Other", 
                                 total_consumption/30, total_consumption/6),
         date = as.Date(date)) %>%

  # aggregate from mpan level up to account level 
  group_by(account_id, date, rate_period) %>%
  summarise(total_consumption = sum(total_consumption), 
           consumption_hh = sum(consumption_hh)) %>%
  ungroup()

# aggreate up to get overall
hp_installed_daily <- hp_installed_period %>%
  group_by(account_id, date) %>%
  summarise(total_consumption = sum(total_consumption)) %>%
  mutate(consumption_hh = total_consumption / 48,
         rate_period = factor("Overall")) 

# merge together to make regressions easier
hp_installed <- 
  bind_rows(hp_installed_period, hp_installed_daily) %>%

  # merge in household covariates 
  inner_join(fread("../gcs/cosy2/input/cosy_-_hp_details_2024_06_25.csv") %>%
               distinct(account_id, hashed_mpan, .keep_all = TRUE),
             by=c("account_id","hashed_mpan"))

# add daily weather data
weather <- fread("../gcs/cosy2/input/Cosy Analysis Weather Mar 26 daily.csv") %>% 
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
stopifnot(group_by(hp_installed, account_id, hashed_mpan, date, rate_period) %>% filter(n() > 1) %>% nrow() == 0)

rm(weather, hp_installed_daily, hp_installed_period)

# hp_installed <- hp_installed %>%
#   left_join(readRDS("../gcs/cosy2/scratch/ev_mpan.RDS")) %>%
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
  mutate(treated = max(is_hp_installed)) %>% # identify treated versus not yet treated
  ungroup() 

stopifnot(hp_installed %>% group_by(account_id, date, rate_period) %>% filter(n() > 1) %>% nrow() == 0)
write_rds(hp_installed, "../gcs/cosy2/output/hp_installed.rds")

# summary statistics
summary(hp_installed)

# HP deals and installation
deals_and_installations <- fread("../gcs/cosy2/input/cosy_-_hp_deals_and_installation_2025_06_06.csv") %>%
  distinct(account_id, .keep_all = TRUE)

# Run on a subsample of the data for faster processing
if (random_subsample) {
  set.seed(123)
  sampled_accounts <- sample(unique(hp_installed$account_id), 1000)
  hp_installed <- hp_installed %>% 
    filter(account_id %in% sampled_accounts)
  gc()
}

