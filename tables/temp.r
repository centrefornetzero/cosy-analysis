# get consumption by period
hp_installed <- 
  read_rds("../gcs/cosy2/output/hp_installed.rds") %>%
  filter(rate_period == "Overall") 

hp_installed %>% ungroup() %>% filter(treated==1) %>% select(account_id) %>% distinct() %>% dim()

hp_installed_weekly <- 
  hp_installed %>%
  mutate(settlement_week = floor_date(date, "week") + 1) %>%
  group_by(account_id, hashed_mpan, settlement_week, installed_at) %>%
  summarise(elec_consumption = sum(total_consumption)) %>%
  ungroup

# Check number of accounts
hp_installed %>% ungroup() %>% filter(treated==1) %>% select(account_id) %>% distinct() %>% dim()



# Load and preprocess gas consumption data
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


# Estimate and save the results for the main analysis (not yet treated control group)
est_cs <- att_gt(yname = "total_consumption",
                   tname = "week",
                   idname = "id",
                   gname = "firstweek",
                   data = did_data,
                   anticipation = 1,
                   clustervars = "id",
                   control_group = c("notyettreated"),
                   allow_unbalanced_panel = TRUE,
                   base_period = "varying")
  
simple <- aggte(est_cs, type = "simple", na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.01) 