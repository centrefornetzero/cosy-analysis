#  Create the main dataset for Cosy adoption analysis

# This script is very long to run so I create a conditional close checking if the file has already been created

# Delete file to rerun everything
# file.remove(file.path(datapath, "scratch/aggregated_data.RDS"))

## Merging consumption and customers info datasets
#if(!file.exists(file.path(datapath, "scratch/aggregated_data.RDS"))) {
  start <- Sys.time()
    print('hello')

  # Load smart meter consumption data at the day - rate period level
  # queries/cosy - cosy electricity readings
  # queries/cosy - cosy electricity reading part 2 which I ran for different years seperately
  aggregated_data <- 
    rbind(fread(file.path(datapath, "input/cosy_-_cosy_electricity_reading_part_2_2024_07_26.csv")),
          fread(file.path(datapath, "input/cosy_-_cosy_electricity_reading_part_2_2024_07_26 (1).csv")),
          fread(file.path(datapath, "input/cosy_-_cosy_electricity_reading_part_2_2024_07_26 (2).csv"))) %>%
    rename(total_consumption = total_read_value,
           consumption_hh = mean_read_value) %>%
    mutate(date = as.Date(settlement_date)) %>% 
    filter(!is.na(date))     %>%
    select(-c(settlement_date)) 
  
               
  print('checkpoint 1')    

  # ------------ Add indicator if customer is on Cosy -------------------
  # first create panel data that shows which dates cosy is active for each mpan
  # also add in earliest Cosy adoption date 
  agreements_active <- 
    fread(file.path(datapath, "input/Cosy_-_agreement_data_2024_07_24.csv")) %>%
    filter(product_display_name == "Cosy Octopus") %>%
    mutate(to = as_date(replace_na(agreement_valid_to, ymd(20240724))),
           from = as_date(agreement_valid_from)) %>%
    select(hashed_mpan, from, to) %>%
    mutate(id = row_number(), 
           date = map2(from, to, seq, by = "day")) %>%
    unnest(date) %>%
    mutate(cosy_contract_active = 1) %>%
    distinct(hashed_mpan, date, cosy_contract_active) %>%
    group_by(hashed_mpan) %>%
    mutate(first_adoption = min(date)) %>%
    ungroup()
    
    print('checkpoint 2')
    
  # merge panel of active cosy dates to consumption data
  aggregated_data <- 
    aggregated_data %>%
    left_join(agreements_active) %>%
    mutate(cosy_contract_active = replace_na(cosy_contract_active, 0)) %>%
    inner_join(distinct(agreements_active, hashed_mpan))
    
  # -------------------- Caculate overall daily consumption --------------------
  aggregate_daily <- 
    aggregated_data %>% 
      group_by(account_id, hashed_mpan, date, cosy_contract_active, first_adoption) %>%
      summarise(total_consumption = sum(total_consumption)) %>%
      mutate(rate_period = "Overall",
             consumption_hh = total_consumption/48)
    
  aggregated_data <- 
    rbind(aggregated_data, aggregate_daily) %>%
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
            ), 
           weeks_since_cosy = floor(as.numeric(difftime(date, first_adoption, units = "weeks")))) %>%
      # Remove the ~ 50 mpans with 2 account id
    group_by(hashed_mpan) %>%
    filter(n_distinct(account_id) == 1) %>%
    ungroup
    
    print(paste('checkpoint 3', Sys.time() - start))
    start <- Sys.time()
    
  # ------------------------- Add in covariates ----------------------------
  # let's add in covariates 
    # add customers characteristics
  # from queries/cosy - cosy details
  cosy_cosy_details_2024_06_25 <- fread(file.path(datapath, "input/cosy_-_cosy_details_2024_06_25.csv"))  %>%
    distinct()
  
  # Add weather by GSP day 
  # queries/cosy analysis - weather
  weather <- fread(file.path(datapath, "input/Cosy Analysis Weather Mar 26 daily.csv")) %>% 
    rename_with(.cols = starts_with("weekly"),  # the data is actually at the day level - just naming error
                .fn = ~ sub("^weekly", "daily", .)) %>%
    mutate(date_day=as.Date(date_day, format = "%Y-%m-%d")) %>%
    rename(date = date_day)
  
  # details about previous contract
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
    slice_head(n=1) %>%
    select(hashed_mpan, starts_with("previous"))
    
    print(paste('checkpoint 4', Sys.time() - start))
    start <- Sys.time()
  
  # Create EPC letters
  aggregated_data <- 
    aggregated_data <- aggregated_data %>%
    inner_join(cosy_cosy_details_2024_06_25) %>%
    left_join(weather, by =c("gsp_group_id", "date")) %>%
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
  aggregated_data <- aggregated_data %>% 
    mutate(hdd = factor(
      case_when(
        daily_avg_air_temperature_celsius < 0 ~ 0,
        daily_avg_air_temperature_celsius < 15.5 ~ round(daily_avg_air_temperature_celsius),
        TRUE ~ 15
      )
    ))
  
  saveRDS(aggregated_data, file.path(datapath, "scratch/aggregated_data.RDS"))
  
  rm(weather, adoption, consumption_with_indicator, agreements)
#} else {
#  aggregated_data <- readRDS(file.path(datapath, "scratch/aggregated_data.RDS")) 
#}

# Run on a subsample of the data for faster processing
if (random_subsample) {
  set.seed(123)
  sampled_accounts <- sample(unique(aggregated_data$account_id), 1000)
  aggregated_data <- aggregated_data %>% 
    filter(account_id %in% sampled_accounts)
  gc()
}
