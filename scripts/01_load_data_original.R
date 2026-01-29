#  Create the main dataset for Cosy adoption analysis

# This script is very long to run so I create a conditional close checking if the file has already been created

# Delete file to rerun everything
file.remove(file.path(datapath, "scratch/aggregated_data_old.RDS"))

## Merging consumption and customers info datasets
print('hello')
start <- Sys.time()

# Extract earliest Cosy adoption using agreement data for each MPAN
agreements <- fread(file.path(datapath, "input/Cosy_-_agreement_data_2024_07_24.csv")) %>%
filter(product_display_name == "Cosy Octopus") %>%
arrange(hashed_mpan, agreement_valid_from) %>%
mutate(
  from = as.Date(agreement_valid_from),
  to = as.Date(agreement_valid_to)
) %>%
select(hashed_mpan, from, to) %>%
group_by(hashed_mpan) %>%
summarise(
  periods = list(data.frame(from, to)),
  first_adoption = min(from),  # Get the earliest agreement date for 'adoption'
  first_week = format(min(from), "%Y-%U"),  # Format the first adoption date as year-week
  .groups = 'drop'
)

print('checkpoint 1')

# Load smart meter consumption data at the day - rate period level
# queries/cosy - cosy electricity readings
# queries/cosy - cosy electricity reading part 2 which I ran for different years seperately
aggregated_data_old <- 
  rbind(fread(file.path(datapath, "input/cosy_-_cosy_electricity_reading_part_2_2024_07_26.csv")),
        fread(file.path(datapath, "input/cosy_-_cosy_electricity_reading_part_2_2024_07_26 (1).csv")),
        fread(file.path(datapath, "input/cosy_-_cosy_electricity_reading_part_2_2024_07_26 (2).csv"))) %>%
rename(total_consumption = total_read_value,
       consumption_hh = mean_read_value) %>%
mutate(date = as.Date(settlement_date)) %>% 
filter(!is.na(date))     %>%
select(-c(settlement_date))

print('checkpoint 2')

# Function to check if a date falls within any period
check_active_contract <- function(date, periods) {
any(sapply(1:nrow(periods[[1]]), function(i) date >= periods[[1]][i, "from"] && (date <= periods[[1]][i, "to"] | is.na(periods[[1]][i, "to"]))))
}

# Add indicator without heavy merging
consumption_with_indicator <- aggregated_data_old %>%
rowwise() %>%
mutate(
  cosy_contract_active = {
    periods <- agreements$periods[agreements$hashed_mpan == hashed_mpan]
    if (length(periods) == 0) 0 else as.integer(check_active_contract(date, periods))
  }
) %>%
ungroup()

# Join with the earliest adoption date
aggregated_data_old <- consumption_with_indicator %>%
inner_join(agreements %>% select(-periods))

# Caculate overall daily consumption
aggregated_data_old <- rbind(
aggregated_data_old, 
aggregated_data_old %>% 
  group_by(account_id, hashed_mpan, date, cosy_contract_active, first_adoption, first_week) %>%
  summarise(total_consumption = sum(total_consumption)) %>%
  mutate(rate_period = "Overall",
         consumption_hh = total_consumption/48)) %>%
mutate(rate_period = factor(rate_period, levels = c("Morning Cosy",
                                                    "Afternoon Cosy",
                                                    "Peak Rate",
                                                    "Other", 
                                                    "Overall")), 
       weeks_since_cosy = floor(as.numeric(difftime(date, first_adoption, units = "weeks")))) 

# Remove the ~ 50 mpans with 2 account id
duplicate_mpan <- aggregated_data_old %>%
select(account_id, hashed_mpan) %>%    # Selecting the necessary columns
distinct() %>%                         # Removing completely identical rows
count(hashed_mpan) %>%                 # Count occurrences of each hashed_mpan
filter(n > 1) %>%                      # Keep only those with more than one occurrence
left_join(aggregated_data_old %>% group_by(account_id, hashed_mpan) %>% summarise(min_date = min(date), max_date = max(date)), by = "hashed_mpan") %>%
arrange(hashed_mpan, account_id)       # Arrange for better visibility

# add customers characteristics
# from queries/cosy - cosy details
cosy_cosy_details_2024_06_25 <- fread(file.path(datapath, "input/cosy_-_cosy_details_2024_06_25.csv")) %>%
distinct()

# Remove moan associated with two accounts !
merged_data <- aggregated_data_old %>%
filter(!hashed_mpan %in% duplicate_mpan$hashed_mpan) %>%
inner_join(cosy_cosy_details_2024_06_25)

# Add weather by GSP day 
# queries/cosy analysis - weather
weather <- fread(file.path(datapath, "input/Cosy Analysis Weather Mar 26 daily.csv")) %>% 
rename_with(.cols = starts_with("weekly"),  # the data is actually at the day level - just naming error
            .fn = ~ sub("^weekly", "daily", .)) %>%
mutate(date_day=as.Date(date_day, format = "%Y-%m-%d")) %>%
rename(date = date_day)

aggregated_data_old <- merged_data %>%
left_join(weather, by =c("gsp_group_id", "date"))

## HERE!!!!!
Prev_contract <- fread(file.path(datapath, "input/Cosy_-_agreement_data_2024_07_24.csv")) %>%
arrange(hashed_mpan, as.Date(agreement_valid_from)) %>%
group_by(hashed_mpan) %>%
mutate(
  previous_contract = lag(product_display_name),
  previous_is_variable = lag(is_variable),
  previous_is_charged_half_hourly = lag(is_charged_half_hourly),
  is_cosy = product_display_name == "Cosy Octopus"
) %>%
filter(is_cosy) %>%
slice_head(n=1)

# Create EPC letters
aggregated_data_old <- aggregated_data_old %>%
mutate(epc_letter = case_when(
  energy_efficiency >= 91 ~ "A",
  energy_efficiency >= 81 & energy_efficiency <= 90 ~ "B",
  energy_efficiency >= 69 & energy_efficiency <= 80 ~ "C",
  energy_efficiency >= 55 & energy_efficiency <= 68 ~ "D",
  energy_efficiency >= 39 & energy_efficiency <= 54 ~ "E",
  energy_efficiency >= 21 & energy_efficiency <= 38 ~ "F",
  energy_efficiency <= 20 ~ "G",
  TRUE ~ NA_character_
),
eac_mwh = estimated_annual_consumption/1000) %>%
left_join(Prev_contract) 

# Create categorical for average daily temperature ranging from 0 - 15 (confusingly named HDD but it is not)
aggregated_data_old <- aggregated_data_old %>% 
mutate(hdd = factor(
  case_when(
    daily_avg_air_temperature_celsius < 0 ~ 0,
    daily_avg_air_temperature_celsius < 15.5 ~ round(daily_avg_air_temperature_celsius),
    TRUE ~ 15
  )
))

saveRDS(aggregated_data_old, file.path(datapath, "scratch/aggregated_data_old.RDS"))

end <- Sys.time() - start
print(end)