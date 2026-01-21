# -*- coding: utf-8 -*-
# Half hourly analysis for HP installation and Cosy (sample non-half-hourly accounts)

library(arrow)
library(dplyr)
library(data.table)
library(stringr)
library(fixest)

################################################################################
## Paths
################################################################################

parquet_base_path <- "../gcs/cosy/hp_adopters_elec/"
parquet_base_path_cosy <- "../gcs/cosy/cosy_elec/"

hh_flag_path   <- file.path(datapath, "input/cosy_-_is_charged_half_hourly_hp_accounts_2025_06_11.csv")
hp_details_path <- file.path(datapath, "input/cosy_-_hp_details_2024_06_25.csv")
ev_path        <- file.path(datapath, "input/cosy_-_ev_detection_2024_07_04.csv")
weather_path   <- file.path(datapath, "input/Cosy Analysis Weather Mar 26 daily.csv")


################################################################################
## Load covariates / joins
################################################################################

ev_charging <- fread(ev_path) %>%
  mutate(ev_charging = 1,
         account_id = as.character(account_id))

weather <- fread(weather_path) %>%
  rename_with(.cols = starts_with("weekly"),
              .fn = ~ sub("^weekly", "daily", .)) %>%
  rename(tariff_gsp_group_id = gsp_group_id) %>%
  mutate(date = as.Date(date_day, format = "%Y-%m-%d"))

hp_details <- fread(hp_details_path) %>%
  distinct(hashed_mpan, .keep_all = TRUE)  %>%
  mutate(installed_at = as.Date(installed_at),
         account_id = as.character(account_id))

################################################################################
## Identify eligible non-half-hourly accounts AND present in parquet
################################################################################

# list account folders in parquet
account_dirs <- list.files(parquet_base_path, pattern = "^account_id=", full.names = TRUE)
account_ids  <- str_replace(basename(account_dirs), "^account_id=", "")

# read HH flag file
hh_flag <- fread(hh_flag_path)

# IMPORTANT: user wants NON half-hourly accounts
non_hh_accounts <- hh_flag[is_charged_half_hourly == FALSE, unique(account_id)]
non_hh_accounts <- as.character(non_hh_accounts)

# keep only those that actually exist in parquet
eligible_accounts <- intersect(account_ids, non_hh_accounts)

set.seed(12345)
sample_accounts <- sample(eligible_accounts, size = min(500, length(eligible_accounts)))

message("Eligible non-HH accounts in parquet: ", length(eligible_accounts))
message("Sampled accounts: ", length(sample_accounts))

################################################################################
## Load the 500 account parquet folders
################################################################################

hp_list <- lapply(sample_accounts, function(aid) {
  path <- file.path(parquet_base_path, paste0("account_id=", aid))

  tryCatch({
    df <- open_dataset(path, format = "parquet") %>% collect()
    if (nrow(df) == 0) return(NULL)

    # If account_id column not in parquet, add it from folder name
    if (!("account_id" %in% names(df))) df$account_id <- aid

    df
  }, error = function(e) {
    message("Failed to read account_id=", aid, " | ", e$message)
    NULL
  })
})

hp <- bind_rows(hp_list)

rm(hp_list); gc()

message("Rows loaded: ", nrow(hp))
message("Unique accounts loaded: ", dplyr::n_distinct(hp$account_id))


################################################################################
## Construct variables used in regressions
################################################################################

# NOTE: your original code expects interval_start to exist in hp
# If your parquet uses a different timestamp column, rename it here.
# e.g. hp <- hp %>% rename(interval_start = adjusted_interval_start)

hp <- hp %>%
  inner_join(hp_details) %>%
  mutate(
    settlement_date = as.Date(interval_start),
    installed_at    = as.Date(installed_at),
    settlement_time = format(as.POSIXct(interval_start, tz = "UTC"),
                         tz = "Europe/London", "%H:%M"),
    is_hp_installed = as.numeric(installed_at <= settlement_date)
  ) %>%
  left_join(weather, by = c("settlement_date" = "date", "tariff_gsp_group_id")) %>%
  arrange(interval_start) %>%
  group_by(settlement_time) %>%
  mutate(settlement_period = cur_group_id()) %>%
  ungroup()

hp <- hp %>%
  left_join(
    ev_charging %>%
      select(account_id, interval_start, ev_charging) %>%
      distinct(account_id, interval_start, .keep_all = TRUE),
    by = c("account_id", "interval_start")
  ) %>%
  mutate(ev_charging = ifelse(is.na(ev_charging), 0, ev_charging))

################################################################################
## Regression
################################################################################

m_hourly_hp <- feols(
  value ~ i(settlement_period, ref = 1) +
    i(settlement_period, is_hp_installed, ref2 = 0) |
    account_id + settlement_date + daily_avg_heating_degree + ev_charging,
  cluster = ~account_id,
  data = hp,
  lean = TRUE,
  mem.clean = TRUE
)

# clean up
rm(hp)
gc()



################################################################################
## Load the 500 account parquet folders for cosy tariff
################################################################################


parquet_base_path_cosy <- "../gcs/cosy/cosy_elec/"

# 1) list + sample
account_dirs <- list.files(parquet_base_path_cosy, pattern = "^account_id=", full.names = TRUE)
account_ids  <- str_replace(basename(account_dirs), "^account_id=", "")

set.seed(123)
sample_accounts <- sample(account_ids, size = min(500, length(account_ids)))

message("Eligible accounts in parquet: ", length(account_ids))
message("Sampled accounts: ", length(sample_accounts))

# HP-style: loop over accounts, read each folder independently
cosy_list <- lapply(sample_accounts, function(aid) {
  path <- file.path(parquet_base_path_cosy, paste0("account_id=", aid))

  tryCatch({
    df <- open_dataset(path, format = "parquet") %>% collect()
    if (nrow(df) == 0) return(NULL)

    # enforce IMPORT only
    df <- df %>%
      filter(import_or_export_product == "IMPORT")

    # standardise column names to match HP pipeline
    df <- df %>%
      transmute(
        account_id     = as.character(aid),
        hashed_mpan    = as.character(hashed_mpan),
        interval_start = as.POSIXct(interval_start, tz = "UTC"),
        read_value     = as.numeric(value)   # <-- align with HP naming
      )

    df
  }, error = function(e) {
    message("Failed to read account_id=", aid, " | ", e$message)
    NULL
  })
})

cosy <- bind_rows(cosy_list)

rm(cosy_list); gc()

message("Rows loaded: ", nrow(cosy))
message("Unique accounts loaded: ", dplyr::n_distinct(cosy$account_id))

# load cosy first adoption
first_adoption <- 
    fread(file.path(datapath, "input/Cosy_-_agreement_data_2024_07_24.csv")) %>%
    filter(product_display_name == "Cosy Octopus") %>%
    mutate(to = as_date(replace_na(agreement_valid_to, ymd(20240724))),
           from = as_date(agreement_valid_from)) %>%
    group_by(hashed_mpan = as.character(hashed_mpan), tariff_gsp_group_id) %>%
    summarise(first_adoption = min(agreement_valid_from)) %>%
    ungroup()

summary(first_adoption)

# Create Treatment dummy and Settlement Categorical
cosy <- cosy %>%
  #filter(account_id %in% sample_selection) %>%
  inner_join(first_adoption) %>%
  mutate(settlement_date = as.Date(interval_start),
         first_adoption = as.Date(first_adoption),
         settlement_time = format(as.POSIXct(interval_start, tz = "UTC"),
                         tz = "Europe/London", "%H:%M"),
         is_cosy = as.numeric(first_adoption<=settlement_date)) %>%
  left_join(weather, by = c("settlement_date" = "date", c("tariff_gsp_group_id"="tariff_gsp_group_id"))) %>%
  arrange(interval_start) %>%
  group_by(settlement_time) %>%
  mutate(settlement_period = cur_group_id())   

# Charging events
cosy <- cosy %>%
  left_join(ev_charging %>% select(account_id, interval_start, ev_charging) %>%
              distinct(account_id, interval_start, ev_charging, .keep_all =TRUE)) %>%
  mutate(ev_charging = ifelse(is.na(ev_charging), 0, ev_charging))



# Cosy analysis
m_hourly_cosy <- feols(read_value ~ i(settlement_period, ref=1) +
                         i(settlement_period,is_cosy, ref2=0) 
                       | account_id + settlement_date + daily_avg_heating_degree + ev_charging,
                       cluster = ~ account_id,
                       data = cosy,
                       lean = TRUE,
                       mem.clean = TRUE)


# Extract coefficients and standard errors
coefs_cosy <- coeftable(m_hourly_cosy) %>%
  data.frame() %>%
  tibble::rownames_to_column("term") %>%
  as_tibble() %>%
  separate(term, into = c("remove", "remove2", "settlement_period", "is_cosy"), sep = ":") %>%
  mutate(settlement_period = as.numeric(settlement_period),
         treatment = "cosy") %>% 
  filter(!is.na(is_cosy)) %>%
  select(-c(remove, remove2, `t.value`, `Pr...t..`, is_cosy))

# Extract coefficients and standard errors
coefs_hp <- coeftable(m_hourly_hp) %>%
  data.frame() %>%
  tibble::rownames_to_column("term") %>%
  as_tibble() %>%
  separate(term, into = c("remove", "remove2", "settlement_period", "has_hp"), sep = ":") %>%
  mutate(settlement_period = as.numeric(settlement_period),
         treatment = "hp") %>% 
  filter(!is.na(has_hp))%>%
  select(-c(remove, remove2, `t.value`, `Pr...t..`, has_hp)) 


# merge together
coefs <- full_join(coefs_cosy, coefs_hp) %>%
  mutate(lower_ci = Estimate - 1.96 * Std..Error,
         upper_ci = Estimate + 1.96 * Std..Error)

# Function to convert settlement period to time
settlement_period_to_time <- function(period) {
  hours <- (period - 1) %/% 2
  minutes <- ifelse((period %% 2) == 1, "00", "30")
  sprintf("%02d:%s", hours, minutes)
}

# Apply the function to create time labels
coefs <- coefs %>%
  mutate(time_label = settlement_period_to_time(settlement_period))


head(coefs)


# Apply the function to create time label
# Select a few representative time labels to display for clarity
selected_periods <- seq(min(coefs$settlement_period), max(coefs$settlement_period), by = 2)
display_labels <- coefs %>%
  filter(settlement_period %in% selected_periods) %>%
  distinct(settlement_period, time_label) %>%
  pull(time_label)


# Define the time periods for shading (settlement periods converted to numeric for xmin, xmax)
shaded_periods <- data.frame(
  xmin = c(9, 27, 33),  # 4-7 AM (periods 8-14), 1-4 PM (periods 26-32), 4-7 PM (periods 34-40)
  xmax = c(15, 33, 39),
  period_type = c("Morning and Afternoon Off-peak", "Morning and Afternoon Off-peak", "Peak Rate")
)

# Create the ggplot
ggplot(coefs, aes(x = settlement_period, y = Estimate, group = treatment, color = treatment)) +
  # Add shaded rectangles using the shaded_periods data
  geom_rect(data = shaded_periods, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period_type),
            inherit.aes = FALSE, alpha = 0.2) +   # Add transparency and disable inherited aesthetics
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6) +
  scale_color_manual(
    name = "Treatment", 
    labels = c("hp" = "Heat Pump", "cosy" = "Heat Pump Tariff"),  # Italicizing Cosy using markdown
    values = c("hp" = hp_color, "cosy" = cosy_color)
  ) +
  scale_fill_manual(
    name = "Rate Period",  # Correct the fill legend
    values = c("Morning and Afternoon Off-peak" = "red", "Peak Rate" = "lightblue"),  # Assign the correct colors
    labels = c("Morning and Afternoon Off-peak" = "Morning and Afternoon Off-peak", "Peak Rate" = "Peak Rate")
  ) +
  labs(
    x = "Time of Day (Settlement Period)",
    y = "Impact on Electricity Consumption (kWh)",
    color = "Treatment",
    fill = "Rate Period"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.text = element_markdown()  # Enable markdown-style formatting for the legend
  ) +
  scale_x_continuous(
    breaks = selected_periods,  # Ensure you define 'selected_periods' correctly
    labels = display_labels,    # Ensure 'display_labels' are defined or generated from 'settlement_period'
    expand = expansion(mult = c(0.05, 0.15))
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")   # Add horizontal line at y = 0

# Save the plot if necessary
ggsave("graphs/combined_impact_hourly_consumption.png", width = 10, height = 6, dpi = 300)


# #################
# stop here
# #################


# Read the files (HP Installation, Cosy Adoption and Daily Weather)
# hp <- rbind(fread("input/cosy_-_half_hourly_data_for_hp_sample_(no_half_hourly_charged_restrictions)_2025_11_12.csv"),
#            fread("input/cosy_-#_half_hourly_data_for_hp_sample_(no_half_hourly_charged_restrictions)_2024_11_12 (1).csv"),
#            fread("input/cosy_-#_half_hourly_data_for_hp_sample_(no_half_hourly_charged_restrictions)_2024_11_12 (2).csv"))


# Read the files (HP Installation, Cosy Adoption and Daily Weather)
hp <- fread(file.path(datapath, "input/cosy_-_half_hourly_data_for_hp_sample_(no_half_hourly_charged_restrictions)_2025_07_07.csv")) %>%
  mutate(interval_start = with_tz(as.POSIXct(interval_start, tz = "UTC")))

# weather data
weather <- fread(file.path(datapath, "input/Cosy Analysis Weather Mar 26 daily.csv")) %>% 
  rename_with(.cols = starts_with("weekly"), 
              .fn = ~ sub("^weekly", "daily", .)) %>%
  rename(tariff_gsp_group_id=gsp_group_id) %>%
  mutate(date=as.Date(date_day, format = "%Y-%m-%d"))

# Select some random mpans
# set.seed(12345)
# sample_selection <- sample(unique(hp$account_id), 350)

# Create Treatment dummy and Settlement Categorical
hp <- hp %>%
  #filter(account_id %in% sample_selection) %>%
  inner_join(fread(file.path(datapath, "input/cosy_-_hp_details_2024_06_25.csv")) %>% 
               distinct(hashed_mpan, .keep_all=TRUE), by=c("hashed_mpan")) %>%
  mutate(settlement_date = as.Date(interval_start),
         installed_at = as.Date(installed_at),
         settlement_time = format(as.POSIXct(interval_start),
                                  format="%H:%M"), #format time
         is_hp_installed = as.numeric(installed_at <= settlement_date)
  ) %>%
  left_join(weather, by = c("settlement_date" = "date", "tariff_gsp_group_id")) %>%
  arrange(interval_start) %>%
  group_by(settlement_time) %>%
  mutate(settlement_period = cur_group_id())   # Read the CSV file for EV charging events

# Join with EV charging 
hp <- hp %>%
  left_join(ev_charging %>% select(account_id, interval_start, ev_charging) %>%
              distinct(account_id, interval_start, ev_charging, .keep_all =TRUE)) %>%
  mutate(ev_charging = ifelse(is.na(ev_charging), 0, ev_charging))

length(unique(hp$hashed_mpan))

# Regression
m_hourly_hp <- feols(read_value ~ i(settlement_period, ref=1) +
                       i(settlement_period,is_hp_installed, ref2=0) 
                     | account_id + settlement_date + daily_avg_heating_degree + ev_charging,
                     cluster = ~ account_id,
                     data =  hp,
                     lean = TRUE,
                     mem.clean = TRUE)

# clean up
rm(hp)
gc()


# add the week day / weekend analysis
cosy <- cosy %>%
  mutate(wday = wday(interval_start, label =TRUE),
         weekend = wday %in% c("Sun", "Sat"))

# Cosy analysis
m_hourly_cosy <- feols(read_value ~ i(settlement_period, ref=1) +
                         i(settlement_period,is_cosy, ref2=0) 
                       | account_id + settlement_date + daily_avg_heating_degree + ev_charging,
                       cluster = ~ account_id,
                       split = ~ weekend,
                       data = cosy,
                       lean = TRUE,
                       mem.clean = TRUE)


# same for the HP analysis

# clean up
rm(cosy)
gc()

# Read the files (HP Installation, Cosy Adoption and Daily Weather)
hp <- fread(file.path(datapath, "input/cosy_-_half_hourly_data_for_hp_sample_2024_09_26.csv")) %>%
  rename(interval_start = adjusted_interval_start)

# weather data
weather <- fread(file.path(datapath, "input/Cosy Analysis Weather Mar 26 daily.csv")) %>% 
  rename_with(.cols = starts_with("weekly"), 
              .fn = ~ sub("^weekly", "daily", .)) %>%
  rename(tariff_gsp_group_id=gsp_group_id) %>%
  mutate(date=as.Date(date_day, format = "%Y-%m-%d"))

# Select some random mpans
# set.seed(12345)
# sample_selection <- sample(unique(hp$account_id), 350)

# Create Treatment dummy and Settlement Categorical
hp <- hp %>%
  #filter(account_id %in% sample_selection) %>%
  inner_join(fread(file.path(datapath, "input/cosy_-_hp_details_2024_06_25.csv")) %>% distinct(hashed_mpan, .keep_all=TRUE), by=c("hashed_mpan")) %>%
  mutate(settlement_date = as.Date(interval_start),
         installed_at = as.Date(installed_at),
         settlement_time = format(as.POSIXct(interval_start),
                                  format="%H:%M"), #format time
         is_hp_installed = as.numeric(installed_at <= settlement_date)
  ) %>%
  left_join(weather, by = c("settlement_date" = "date", "tariff_gsp_group_id")) %>%
  arrange(interval_start) %>%
  group_by(settlement_time) %>%
  mutate(settlement_period = cur_group_id())   # Read the CSV file for EV charging events

# add EV charging, and weekend dummies
hp <- hp %>%
  left_join(ev_charging %>% select(account_id, interval_start, ev_charging) %>%
              distinct(account_id, interval_start, ev_charging, .keep_all =TRUE)) %>%
  mutate(ev_charging = ifelse(is.na(ev_charging), 0, ev_charging),
         wday = wday(interval_start, label =TRUE),
         weekend = wday %in% c("Sun", "Sat"))


# Event study style regressions
# Midnight is the omitted category

m_hourly_hp <- feols(read_value ~ i(settlement_period, ref=1) +
                       i(settlement_period,is_hp_installed, ref2=0) 
                     | account_id + settlement_date + daily_avg_heating_degree + ev_charging,
                     cluster = ~ account_id,
                     split = ~ weekend,
                     data =  hp,
                     lean = TRUE,
                     mem.clean = TRUE)



# Extract coefficients and standard errors
coefs_cosy <- coeftable(m_hourly_cosy) %>%
  data.frame() %>%
  tibble::rownames_to_column("term") %>%
  as_tibble() %>%
  separate(coefficient, into = c("part1", "part2"), sep = "::") %>%
  separate(part2, into = c("settlement_period", "is_cosy"), sep = ":")  %>%
  mutate(settlement_period = as.numeric(settlement_period),
         treatment = "cosy") %>% 
  filter(!is.na(is_cosy)) %>%
  select(-c(part1, `t.value`, `Pr...t..`, is_cosy, term, sample.var, id))

# Extract coefficients and standard errors
coefs_hp <- coeftable(m_hourly_hp) %>%
  data.frame() %>%
  tibble::rownames_to_column("term") %>%
  as_tibble() %>%
  separate(coefficient, into = c("part1", "part2"), sep = "::") %>%
  separate(part2, into = c("settlement_period", "has_hp"), sep = ":")  %>%
  mutate(settlement_period = as.numeric(settlement_period),
         treatment = "hp") %>% 
  filter(!is.na(has_hp)) %>%
  select(-c(part1, `t.value`, `Pr...t..`, has_hp, term, sample.var, id))

# merge together
coefs <- full_join(coefs_cosy, coefs_hp) %>%
  mutate(lower_ci = Estimate - 1.96 * Std..Error,
         upper_ci = Estimate + 1.96 * Std..Error)

# Function to convert settlement period to time
settlement_period_to_time <- function(period) {
  hours <- (period - 1) %/% 2
  minutes <- ifelse((period %% 2) == 1, "00", "30")
  sprintf("%02d:%s", hours, minutes)
}

# Apply the function to create time labels
coefs <- coefs %>%
  mutate(time_label = settlement_period_to_time(settlement_period))


head(coefs)



# Apply the function to create time label
# Select a few representative time labels to display for clarity
selected_periods <- seq(min(coefs$settlement_period), max(coefs$settlement_period), by = 2)
display_labels <- coefs %>%
  filter(settlement_period %in% selected_periods) %>%
  distinct(settlement_period, time_label) %>%
  pull(time_label)

# Define the time periods for shading (settlement periods converted to numeric for xmin, xmax)
shaded_periods <- data.frame(
  xmin = c(9, 27, 33),  # 4-7 AM (periods 8-14), 1-4 PM (periods 26-32), 4-7 PM (periods 34-40)
  xmax = c(15, 33, 39),
  period_type = c("Morning and Afternoon Off-peak", "Morning and Afternoon Off-peak", "Peak Rate")
)

# Create the ggplot
ggplot(coefs, aes(x = settlement_period, y = Estimate, group = treatment, color = treatment)) +
  # Add shaded rectangles using the shaded_periods data
  geom_rect(data = shaded_periods, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period_type),
            inherit.aes = FALSE, alpha = 0.2) +   # Add transparency and disable inherited aesthetics
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6) +
  scale_color_manual(
    name = "Treatment", 
    labels = c("hp" = "Heat Pump", "cosy" = "Heat Pump Tariff"),  # Italicizing Cosy using markdown
    values = c("hp" = hp_color, "cosy" = cosy_color)
  ) +
  scale_fill_manual(
    name = "Rate Period",  # Correct the fill legend
    values = c("Morning and Afternoon Off-peak" = "red", "Peak Rate" = "lightblue"),  # Assign the correct colors
    labels = c("Morning and Afternoon Off-peak" = "Morning and Afternoon Off-peak", "Peak Rate" = "Peak Rate")
  ) +
  labs(
    x = "Time of Day (Settlement Period)",
    y = "Impact on Electricity Consumption (kWh)",
    color = "Treatment",
    fill = "Rate Period"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.text = element_markdown(),  # Enable markdown-style formatting for the legend
    legend.box = "vertical"  # Stack legend items in a vertical box
  ) +
  guides(
    color = guide_legend(nrow = 2),   # Set legend rows for color
    fill = guide_legend(nrow = 2)     # Set legend rows for fill
  ) +
  scale_x_continuous(
    breaks = selected_periods,  # Ensure you define 'selected_periods' correctly
    labels = display_labels,    # Ensure 'display_labels' are defined or generated from 'settlement_period'
    expand = expansion(mult = c(0.05, 0.15))
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  facet_wrap(~ sample, labeller = as_labeller(c(`FALSE` = "Weekdays", `TRUE` = "Weekend")))


# Save the plot if necessary
ggsave("graphs/combined_impact_hourly_consumption_weekend.png", width = 10, height = 6, dpi = 300)




# same thing for temperature
# Read the files (HP Installation, Cosy Adoption and Daily Weather)
hp <- fread(file.path(datapath, "input/cosy_-_half_hourly_data_for_hp_sample_2024_09_26.csv")) %>%
  rename(interval_start = adjusted_interval_start)

# weather data
weather <- fread(file.path(datapath, "input/Cosy Analysis Weather Mar 26 daily.csv")) %>% 
  rename_with(.cols = starts_with("weekly"), 
              .fn = ~ sub("^weekly", "daily", .)) %>%
  rename(tariff_gsp_group_id=gsp_group_id) %>%
  mutate(date=as.Date(date_day, format = "%Y-%m-%d"))

# Select some random mpans
# set.seed(12345)
# sample_selection <- sample(unique(hp$account_id), 350)

# Create Treatment dummy and Settlement Categorical
hp <- hp %>%
  #filter(account_id %in% sample_selection) %>%
  inner_join(fread(file.path(datapath, "input/cosy_-_hp_details_2024_06_25.csv")) %>% distinct(hashed_mpan, .keep_all=TRUE), by=c("hashed_mpan")) %>%
  mutate(settlement_date = as.Date(interval_start),
         installed_at = as.Date(installed_at),
         settlement_time = format(as.POSIXct(interval_start),
                                  format="%H:%M"), #format time
         is_hp_installed = as.numeric(installed_at <= settlement_date)
  ) %>%
  left_join(weather, by = c("settlement_date" = "date", "tariff_gsp_group_id")) %>%
  arrange(interval_start) %>%
  group_by(settlement_time) %>%
  mutate(settlement_period = cur_group_id())   # Read the CSV file for EV charging events

# add EV charging, and weekend dummies
hp <- hp %>%
  left_join(ev_charging %>% select(account_id, interval_start, ev_charging) %>%
              distinct(account_id, interval_start, ev_charging, .keep_all =TRUE)) %>%
  mutate(ev_charging = ifelse(is.na(ev_charging), 0, ev_charging),
         wday = wday(interval_start, label =TRUE),
         weekend = wday %in% c("Sun", "Sat"))


hp <- hp %>% 
  mutate(temp_degree = factor(
    case_when(
      daily_avg_air_temperature_celsius < 5 ~ "Under 5",
      daily_avg_air_temperature_celsius < 15 ~ "Under 15",
      TRUE ~ "Over 15")))

m_hourly_hp <- feols(read_value ~ i(settlement_period, ref=1) +
                       i(settlement_period,is_hp_installed, ref2=0) 
                     | account_id + settlement_date + daily_avg_heating_degree + ev_charging,
                     cluster = ~ account_id,
                     split = ~ temp_degree,
                     data =  hp,
                     lean = TRUE,
                     mem.clean = TRUE)

rm(hp)
gc()


# load cosy first adoption
first_adoption <- readRDS(file.path(datapath, "scratch/aggregated_data.RDS")) %>%
  ungroup() %>%
  select(hashed_mpan, first_adoption, account_id, tariff_gsp_group_id) %>%
  distinct(hashed_mpan, first_adoption, .keep_all=TRUE)

# Read the files (Cosy Adoption)
cosy <- fread(file.path(datapath, "input/cosy_-_half_hourly_data_for_cosy_sample_2024_10_29.csv")) %>%
  rename(interval_start = adjusted_interval_start)

# Select some random mpans
# set.seed(12345)
# sample_selection <- sample(unique(cosy$mpan_hashed), 350)

# Create Treatment dummy and Settlement Categorical
cosy <- cosy %>%
  #filter(account_id %in% sample_selection) %>%
  inner_join(first_adoption, by = "hashed_mpan") %>%
  mutate(settlement_date = as.Date(interval_start),
         first_adoption = as.Date(first_adoption),
         settlement_time = format(as.POSIXct(interval_start),
                                  format="%H:%M"), #format time
         is_cosy = as.numeric(first_adoption<=settlement_date)) %>%
  left_join(weather, by = c("settlement_date" = "date", c("tariff_gsp_group_id"="tariff_gsp_group_id"))) %>%
  arrange(interval_start) %>%
  group_by(settlement_time) %>%
  mutate(settlement_period = cur_group_id())   

# Charging events
cosy <- cosy %>%
  left_join(ev_charging %>% select(account_id, interval_start, ev_charging) %>%
              distinct(account_id, interval_start, ev_charging, .keep_all =TRUE)) %>%
  mutate(ev_charging = ifelse(is.na(ev_charging), 0, ev_charging)) %>% 
  mutate(temp_degree = factor(
    case_when(
      daily_avg_air_temperature_celsius < 5 ~ "Under 5",
      daily_avg_air_temperature_celsius < 15 ~ "Under 15",
      TRUE ~ "Over 15")))


# Cosy analysis
m_hourly_cosy <- feols(read_value ~ i(settlement_period, ref=1) +
                         i(settlement_period,is_cosy, ref2=0) 
                       | account_id + settlement_date + daily_avg_heating_degree + ev_charging,
                       cluster = ~ account_id,
                       split = ~ temp_degree,
                       data = cosy,
                       lean = TRUE,
                       mem.clean = TRUE)


# Extract coefficients and standard errors
coefs_cosy <- coeftable(m_hourly_cosy) %>%
  data.frame() %>%
  tibble::rownames_to_column("term") %>%
  as_tibble() %>%
  separate(coefficient, into = c("part1", "part2"), sep = "::") %>%
  separate(part2, into = c("settlement_period", "is_cosy"), sep = ":")  %>%
  mutate(settlement_period = as.numeric(settlement_period),
         treatment = "cosy") %>% 
  filter(!is.na(is_cosy)) %>%
  select(-c(part1, `t.value`, `Pr...t..`, is_cosy, term, sample.var, id))

# Extract coefficients and standard errors
coefs_hp <- coeftable(m_hourly_hp) %>%
  data.frame() %>%
  tibble::rownames_to_column("term") %>%
  as_tibble() %>%
  separate(coefficient, into = c("part1", "part2"), sep = "::") %>%
  separate(part2, into = c("settlement_period", "has_hp"), sep = ":")  %>%
  mutate(settlement_period = as.numeric(settlement_period),
         treatment = "hp") %>% 
  filter(!is.na(has_hp)) %>%
  select(-c(part1, `t.value`, `Pr...t..`, has_hp, term, sample.var, id))

# merge together
coefs <- full_join(coefs_cosy, coefs_hp) %>%
  mutate(lower_ci = Estimate - 1.96 * Std..Error,
         upper_ci = Estimate + 1.96 * Std..Error)

# Function to convert settlement period to time
settlement_period_to_time <- function(period) {
  hours <- (period - 1) %/% 2
  minutes <- ifelse((period %% 2) == 1, "00", "30")
  sprintf("%02d:%s", hours, minutes)
}

# Apply the function to create time labels
coefs <- coefs %>%
  mutate(time_label = settlement_period_to_time(settlement_period))


head(coefs)


# Apply the function to create time label
# Select a few representative time labels to display for clarity
selected_periods <- seq(min(coefs$settlement_period), max(coefs$settlement_period), by = 4)
display_labels <- coefs %>%
  filter(settlement_period %in% selected_periods) %>%
  distinct(settlement_period, time_label) %>%
  pull(time_label)

# Define the time periods for shading (settlement periods converted to numeric for xmin, xmax)
shaded_periods <- data.frame(
  xmin = c(9, 27, 33),  # 4-7 AM (periods 8-14), 1-4 PM (periods 26-32), 4-7 PM (periods 34-40)
  xmax = c(15, 33, 39),
  period_type = c("Morning and Afternoon Off-peak", "Morning and Afternoon Off-peak", "Peak Rate")
)

# Change the sample order
coefs <- coefs %>%
  mutate(sample = factor(sample, levels = c("Under 5", "Under 15", "Over 15")))

# Create the ggplot
ggplot(coefs, aes(x = settlement_period, y = Estimate, group = treatment, color = treatment)) +
  # Add shaded rectangles using the shaded_periods data
  geom_rect(data = shaded_periods, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period_type),
            inherit.aes = FALSE, alpha = 0.2) +   # Add transparency and disable inherited aesthetics
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6) +
  scale_color_manual(
    name = "Treatment", 
    labels = c("hp" = "Heat Pump", "cosy" = "Heat Pump Tariff"),  # Italicizing Cosy using markdown
    values = c("hp" = hp_color, "cosy" = cosy_color)
  ) +
  scale_fill_manual(
    name = "Rate Period",  # Correct the fill legend
    values = c("Morning and Afternoon Off-peak" = "red", "Peak Rate" = "lightblue"),  # Assign the correct colors
    labels = c("Morning and Afternoon Off-peak" = "Morning and Afternoon Off-peak", "Peak Rate" = "Peak Rate")
  ) +
  labs(
    x = "Time of Day (Settlement Period)",
    y = "Impact on Electricity Consumption (kWh)",
    color = "Treatment",
    fill = "Rate Period"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.text = element_markdown(),  # Enable markdown-style formatting for the legend
    legend.box = "vertical"  # Stack legend items in a vertical box
  ) +
  guides(
    color = guide_legend(nrow = 2),   # Set legend rows for color
    fill = guide_legend(nrow = 2)     # Set legend rows for fill
  ) +
  scale_x_continuous(
    breaks = selected_periods,  # Ensure you define 'selected_periods' correctly
    labels = display_labels,    # Ensure 'display_labels' are defined or generated from 'settlement_period'
    expand = expansion(mult = c(0.05, 0.15))
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  facet_wrap(~ sample, labeller = labeller(
    sample = c("Under 5" = "Under 5°C", "Under 15" = "Under 15°C", "Over 15" = "Over 15°C")
  ))


# Save the plot if necessary
ggsave("graphs/combined_impact_hourly_consumption_degrees.png", width = 10, height = 6, dpi = 300)


# let's do EV owners separetely
cosy <- cosy %>%
  group_by(account_id) %>%
  mutate(ev_owner = max(ev_charging)) 

# Cosy analysis
m_hourly_cosy <- feols(read_value ~ i(settlement_period, ref=1) +
                         i(settlement_period,is_cosy, ref2=0) 
                       | account_id + settlement_date + daily_avg_heating_degree + ev_charging,
                       cluster = ~ account_id,
                       split = ~ ev_owner,
                       data = cosy,
                       lean = TRUE,
                       mem.clean = TRUE)


rm(cosy)
gc()

# Read the files (HP Installation, Cosy Adoption and Daily Weather)
hp <- fread(file.path(datapath, "input/cosy_-_half_hourly_data_for_hp_sample_2024_09_26.csv")) %>%
  rename(interval_start = adjusted_interval_start)

# weather data
weather <- fread(file.path(datapath, "input/Cosy Analysis Weather Mar 26 daily.csv")) %>% 
  rename_with(.cols = starts_with("weekly"), 
              .fn = ~ sub("^weekly", "daily", .)) %>%
  rename(tariff_gsp_group_id=gsp_group_id) %>%
  mutate(date=as.Date(date_day, format = "%Y-%m-%d"))

# Select some random mpans
# set.seed(12345)
# sample_selection <- sample(unique(hp$account_id), 350)

# Create Treatment dummy and Settlement Categorical
hp <- hp %>%
  #filter(account_id %in% sample_selection) %>%
  inner_join(fread(file.path(datapath, "input/cosy_-_hp_details_2024_06_25.csv")) %>% distinct(hashed_mpan, .keep_all=TRUE), by=c("hashed_mpan")) %>%
  mutate(settlement_date = as.Date(interval_start),
         installed_at = as.Date(installed_at),
         settlement_time = format(as.POSIXct(interval_start),
                                  format="%H:%M"), #format time
         is_hp_installed = as.numeric(installed_at <= settlement_date)
  ) %>%
  left_join(weather, by = c("settlement_date" = "date", "tariff_gsp_group_id")) %>%
  arrange(interval_start) %>%
  group_by(settlement_time) %>%
  mutate(settlement_period = cur_group_id())   # Read the CSV file for EV charging events

# add EV charging, and weekend dummies
hp <- hp %>%
  left_join(ev_charging %>% select(account_id, interval_start, ev_charging) %>%
              distinct(account_id, interval_start, ev_charging, .keep_all =TRUE)) %>%
  mutate(ev_charging = ifelse(is.na(ev_charging), 0, ev_charging),
         wday = wday(interval_start, label =TRUE),
         weekend = wday %in% c("Sun", "Sat"))

hp <- hp %>%
  group_by(account_id) %>%
  mutate(ev_owner = max(ev_charging)) 

m_hourly_hp <- feols(read_value ~ i(settlement_period, ref=1) +
                       i(settlement_period,is_hp_installed, ref2=0) 
                     | account_id + settlement_date + daily_avg_heating_degree + ev_charging,
                     cluster = ~ account_id,
                     split = ~ ev_owner,
                     data =  hp,
                     lean = TRUE,
                     mem.clean = TRUE)


# Extract coefficients and standard errors
coefs_cosy <- coeftable(m_hourly_cosy) %>%
  data.frame() %>%
  tibble::rownames_to_column("term") %>%
  as_tibble() %>%
  separate(coefficient, into = c("part1", "part2"), sep = "::") %>%
  separate(part2, into = c("settlement_period", "is_cosy"), sep = ":")  %>%
  mutate(settlement_period = as.numeric(settlement_period),
         treatment = "cosy") %>% 
  filter(!is.na(is_cosy)) %>%
  select(-c(part1, `t.value`, `Pr...t..`, is_cosy, term, sample.var, id))

# Extract coefficients and standard errors
coefs_hp <- coeftable(m_hourly_hp) %>%
  data.frame() %>%
  tibble::rownames_to_column("term") %>%
  as_tibble() %>%
  separate(coefficient, into = c("part1", "part2"), sep = "::") %>%
  separate(part2, into = c("settlement_period", "has_hp"), sep = ":")  %>%
  mutate(settlement_period = as.numeric(settlement_period),
         treatment = "hp") %>% 
  filter(!is.na(has_hp)) %>%
  select(-c(part1, `t.value`, `Pr...t..`, has_hp, term, sample.var, id))

# merge together
coefs <- full_join(coefs_cosy, coefs_hp) %>%
  mutate(lower_ci = Estimate - 1.96 * Std..Error,
         upper_ci = Estimate + 1.96 * Std..Error)

# Function to convert settlement period to time
settlement_period_to_time <- function(period) {
  hours <- (period - 1) %/% 2
  minutes <- ifelse((period %% 2) == 1, "00", "30")
  sprintf("%02d:%s", hours, minutes)
}

# Apply the function to create time labels
coefs <- coefs %>%
  mutate(time_label = settlement_period_to_time(settlement_period))


head(coefs)


# Apply the function to create time label
# Select a few representative time labels to display for clarity
selected_periods <- seq(min(coefs$settlement_period), max(coefs$settlement_period), by = 4)
display_labels <- coefs %>%
  filter(settlement_period %in% selected_periods) %>%
  distinct(settlement_period, time_label) %>%
  pull(time_label)

# Define the time periods for shading (settlement periods converted to numeric for xmin, xmax)
shaded_periods <- data.frame(
  xmin = c(9, 27, 33),  # 4-7 AM (periods 8-14), 1-4 PM (periods 26-32), 4-7 PM (periods 34-40)
  xmax = c(15, 33, 39),
  period_type = c("Morning and Afternoon Off-peak", "Morning and Afternoon Off-peak", "Peak Rate")
)

# Create the ggplot
ggplot(coefs, aes(x = settlement_period, y = Estimate, group = treatment, color = treatment)) +
  # Add shaded rectangles using the shaded_periods data
  geom_rect(data = shaded_periods, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period_type),
            inherit.aes = FALSE, alpha = 0.2) +   # Add transparency and disable inherited aesthetics
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6) +
  scale_color_manual(
    name = "Treatment", 
    labels = c("hp" = "Heat Pump", "cosy" = "Heat Pump Tariff"),  # Italicizing Cosy using markdown
    values = c("hp" = hp_color, "cosy" = cosy_color)
  ) +
  scale_fill_manual(
    name = "Rate Period",  # Correct the fill legend
    values = c("Morning and Afternoon Off-peak" = "red", "Peak Rate" = "lightblue"),  # Assign the correct colors
    labels = c("Morning and Afternoon Off-peak" = "Morning and Afternoon Off-peak", "Peak Rate" = "Peak Rate")
  ) +
  labs(
    x = "Time of Day (Settlement Period)",
    y = "Impact on Electricity Consumption (kWh)",
    color = "Treatment",
    fill = "Rate Period"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.text = element_markdown(),  # Enable markdown-style formatting for the legend
    legend.box = "vertical"  # Stack legend items in a vertical box
  ) +
  guides(
    color = guide_legend(nrow = 2),   # Set legend rows for color
    fill = guide_legend(nrow = 2)     # Set legend rows for fill
  ) +
  scale_x_continuous(
    breaks = selected_periods,  # Ensure you define 'selected_periods' correctly
    labels = display_labels,    # Ensure 'display_labels' are defined or generated from 'settlement_period'
    expand = expansion(mult = c(0.05, 0.15))
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  facet_wrap(~ sample)








