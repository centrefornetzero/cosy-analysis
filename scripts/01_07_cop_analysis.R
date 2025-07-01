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
    gas_consumption = gas_consumption,
    elec_consumption = elec_consumption,
    total_consumption = gas_consumption + elec_consumption
  ) 


# add weather 
weather_weekly <- fread("data/input/cosy_-_weather_weekly_2024_06_13.csv") %>%
  mutate(settlement_week = as.Date(week_date)) %>%
  distinct(gsp_group_id, settlement_week, .keep_all = TRUE) %>%
  select(gsp_group_id, settlement_week, avg_heating_degree, avg_air_temperature_celsius) %>%
  rename(tariff_gsp_group_id = gsp_group_id)

# merge with consumption data
overall_weekly <- overall_weekly %>%
  inner_join(weather_weekly) %>%
  rename(hdd = avg_heating_degree) %>% 
  mutate(temp_degree = factor(
    case_when(
      avg_air_temperature_celsius < 0 ~ 0,
      avg_air_temperature_celsius < 25.5 ~ round(avg_air_temperature_celsius),
      TRUE ~ 25
    )))


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
  filter(week <= 129, firstweek <= 129) 


rm(all_combinations, merged_data, weather_weekly, electricity_daily, cosy_hp_install_gas_consumption)
gc()

       
# Fit the model
m1 <- feols(c(elec_consumption, gas_consumption, total_consumption) ~ i(is_hp_installed) | 
              hdd + account_id + settlement_week, 
            data =  overall_weekly %>% filter(account_id %in% did_data$account_id), 
            cluster = ~account_id)

# Run the regression model
tempreg <- feols(c(elec_consumption, gas_consumption, total_consumption) ~ 
                   i(is_hp_installed, temp_degree, ref=0) |
                   account_id + temp_degree  + settlement_week,
                 data = overall_weekly,
                 cluster = ~account_id)

# Extract coefficients and standard errors
coefs_m1 <- coeftable(m1) %>%
  data.frame() %>%
  select(lhs, Estimate) %>%
  rename(avg_ate = Estimate)

# Main interaction table
coefs <- coeftable(tempreg) %>%
  data.frame() %>%
  separate(coefficient, 
           into = c("is_hp_installed", "remove1", "daily_avg_air_temperature_celsius", "remove2"), sep = "::") %>%
  mutate(daily_avg_air_temperature_celsius = factor(daily_avg_air_temperature_celsius, levels = 0:25),
         lower_ci = Estimate - 1.96 * `Std..Error`,
         upper_ci = Estimate + 1.96 * `Std..Error`
  ) %>%
  inner_join(coefs_m1) %>%
  mutate(`/% ATE` = Estimate / avg_ate * 100,     
         lower_ci_ATE = `/% ATE` - 1.96 * (`Std..Error` / avg_ate * 100),
         upper_ci_ATE = `/% ATE` + 1.96 * (`Std..Error` / avg_ate * 100)
  ) %>%
  filter(lhs != "total_consumption") 
fwrite(coefs, "data/scratch/gas_electricity_by_temperature.csv")


coefs_wider <- coefs  %>%
  filter(lhs != "total_consumption") %>%
  select(lhs, daily_avg_air_temperature_celsius, Estimate) %>%
  pivot_wider(names_from = lhs, values_from = c(Estimate)) %>%
  mutate(quasi_cop = abs(gas_consumption / elec_consumption)) 

# Plot gas and elec
ggplot(coefs %>% filter(as.numeric(daily_avg_air_temperature_celsius) <23), 
       aes(x = daily_avg_air_temperature_celsius, y = Estimate, group = lhs)) +
  geom_point(aes(color = lhs, fill = lhs)) +
  geom_line(aes(color = lhs)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = lhs), 
                width = 0.2, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Average Weekly Temperature in Degrees (°C)",
    y = "Estimate (Weekly kWh)",
    color = NULL,
    fill = NULL
  ) +
  scale_color_manual(
    values = c("elec_consumption" = hp_color, "gas_consumption" = not_hp_color),
    labels = c("elec_consumption" = "Electricity", "gas_consumption" = "Gas")
  ) +
  scale_fill_manual(
    values = c("elec_consumption" = hp_color, "gas_consumption" = not_hp_color),
    labels = c("elec_consumption" = "Electricity", "gas_consumption" = "Gas")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Print the plot
ggsave(paste0("graphs/hp_temperature_gas_elec.png"),
       width = 16, height = 8, units = "cm")

# Calculate the average value for the dashed line
avg_cop <- abs(m1$`lhs: gas_consumption`$coefficients / m1$`lhs: elec_consumption`$coefficients)

set.seed(123)  # for reproducibility
B <- 500  # number of bootstrap samples
temperature_levels <- levels(coefs$daily_avg_air_temperature_celsius)
results <- vector("list", B)
pb <- progress_bar$new(total = B, format = "Bootstrapping [:bar] :percent ETA: :eta")

for (b in 1:B) {
  pb$tick()
  
  # Resample account_ids with replacement
  sampled_ids <- sample(unique(overall_weekly$account_id), replace = TRUE)
  
  # Rebuild bootstrapped sample
  boot_data <- overall_weekly %>%
    filter(treated == 1) %>%
    semi_join(data.frame(account_id = sampled_ids), by = "account_id")
  
  # Refit model
  boot_model <- tryCatch({
    feols(c(elec_consumption, gas_consumption) ~ 
            i(is_hp_installed, temp_degree, ref = 0) |
            account_id + temp_degree + settlement_week,
          data = boot_data, cluster = ~account_id, lean=TRUE)
  }, error = function(e) return(NULL))
  
  # If failed, skip
  if (is.null(boot_model)) next
  
  # Extract estimates
  boot_coefs <- coeftable(boot_model) %>%
    data.frame() %>%
    separate(coefficient, 
             into = c("is_hp_installed", "remove1", "temp", "remove2"), sep = "::") %>%
    select(lhs, Estimate, temp) %>%
    pivot_wider(names_from = lhs, values_from = Estimate) %>%
    mutate(quasi_cop = abs(gas_consumption / elec_consumption)) %>%
    select(temp, quasi_cop)
  
  results[[b]] <- boot_coefs
}

# Combine bootstrap results
cop_boot <- bind_rows(results, .id = "bootstrap") %>%
  mutate(temp = factor(temp, levels = temperature_levels)) %>%
  group_by(temp) %>%
  summarise(
    lower = quantile(quasi_cop, 0.025, na.rm = TRUE),
    upper = quantile(quasi_cop, 0.975, na.rm = TRUE),
    median = median(quasi_cop, na.rm = TRUE),
    .groups = "drop"
  )

# ASHP COP data from the EPRI chart
ashp_cop <- data.frame(
  temp_f = c(-20, -10, 0, 10, 20, 30, 40, 50, 60),
  cop = c(1.8, 1.8, 1.9, 2.1, 2.4, 2.7, 3.1, 3.5, 3.9)
) %>% 
  mutate(temp_c = (temp_f - 32) * 5 / 9) 

# Create target Celsius values from 0 to 15°C
target_temps <- tibble(temp_c = seq(0, 15, by = 1))

# Interpolate using base R's approx, wrapped in a tidyverse style
ashp_interp_df <- target_temps %>%
  mutate(cop = approx(x = ashp_cop$temp_c, y = ashp_cop$cop, xout = temp_c)$y)

# Define the linear model from The Brattle Group chart
brattle_cop <- tibble(temp_c = seq(0, 15, by = 1)) %>%
  mutate(
    temp_f = temp_c * 9 / 5 + 32,
    cop = 1.2 + 0.05 * temp_f,
    temp = as.character(temp_c)  # to match factor format in cop_boot if needed
  )

ashp_interp_df <- ashp_interp_df %>%
  mutate(source = "EPRI")

brattle_cop <- brattle_cop %>%
  select(temp_c, cop) %>%
  mutate(source = "Brattle")

cop_reference_lines <- bind_rows(ashp_interp_df, brattle_cop)

# Your existing ggplot + ASHP COP overlay
ggplot(cop_boot %>% filter(as.numeric(temp) < 17), 
       aes(x = as.numeric(as.character(temp)), y = median)) +
  geom_bar(stat = "identity", alpha = 0.6, fill = hp_color) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = hp_color) +
  geom_hline(yintercept = 3.49, linetype = "dashed", color = hp_color) +
  annotate("text", 
           x = 2.5,
           y = avg_cop + 1,
           label = paste0("italic('Sample average ≈", 3.49, "')"),
           parse = TRUE,
           color = hp_color,
           size = 4) +
  # Overlay both COP reference lines with legend
  geom_line(data = cop_reference_lines, 
            aes(x = temp_c, y = cop, linetype = source), 
            color = flexible_color) +
  scale_linetype_manual(values = c("EPRI" = "solid", "Brattle" = "dashed")) +
  labs(
    x = "Average Temperature in Degrees (°C)",
    y = "Estimated ratio of heat output \nto energy input",
    linetype = "Engineering Models of COP"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") 

# Print the plot
ggsave(paste0("graphs/quasi_cop.png"),
       width = 16, height = 8, units = "cm")
