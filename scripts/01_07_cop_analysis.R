# Load and preprocess gas consumption data
# previous 2024_06_13.csv
cosy_hp_install_gas_consumption <- fread("data/input/cosy_-_hp_gas_consumption_daily_2024_09_10.csv") %>%
  arrange(account_id, settlement_date)%>%
  distinct(account_id, settlement_date, .keep_all = TRUE) %>%
  filter(installed_at < "2024-05-27") %>%
  mutate(is_hp_installed = as.numeric(installed_at <= settlement_date),
         treated = max(is_hp_installed),
         min_settlement_date = min(settlement_date),
         settlement_date = as.Date(settlement_date))

# Create a sequence of weeks
min_date <- min(cosy_hp_install_gas_consumption$settlement_date)
max_date <- max(cosy_hp_install_gas_consumption$settlement_date)
all_weeks <- seq(min_date, max_date, by = "week")

# Create a data frame with all combinations of account_id and settlement_week
all_combinations <- expand.grid(
  account_id = unique(cosy_hp_install_gas_consumption$account_id),
  settlement_date = as.Date(all_weeks)
)

# Merge with original data
merged_data <- all_combinations %>%
  left_join(cosy_hp_install_gas_consumption %>% 
              distinct(account_id, settlement_date, consumption_chargedcurrent_daily, 
                       min_settlement_date, installed_at))%>%
  filter(min_settlement_date < settlement_date) %>%
  mutate(
    is_hp_installed = as.numeric(installed_at < settlement_date),
    gas_consumption = ifelse(is.na(consumption_chargedcurrent_daily), 0, consumption_chargedcurrent_daily)
  ) 

# Define overall_weekly by merging with electricity data
overall_daily <- hp_installed %>%
  filter(rate_period== "Overall") %>%
  filter(account_id %in% merged_data$account_id) %>%
  select(account_id, date, total_consumption, daily_avg_air_temperature_celsius, is_hp_installed, treated, daily_avg_air_temperature_celsius) %>%
  rename(elec_consumption = total_consumption) %>%
  left_join(merged_data %>%
              rename(date = settlement_date) %>%
              select(account_id, date, gas_consumption)) %>%
  mutate(
    gas_consumption = gas_consumption,
    elec_consumption = elec_consumption,
    total_consumption = gas_consumption + elec_consumption
  ) %>%
  group_by(account_id) %>%
  mutate(hdd = factor(
    case_when(
      daily_avg_air_temperature_celsius < 0 ~ 0,
      daily_avg_air_temperature_celsius < 15.5 ~ round(daily_avg_air_temperature_celsius),
      TRUE ~ 15
    )
  ))

rm(all_combinations, cosy_hp_install_gas_consumption, merged_data, weather_weekly)
gc()

# Fit the model
m1 <- feols(c(elec_consumption, gas_consumption, total_consumption) ~ i(is_hp_installed) | 
              hdd + account_id + date, 
            data = overall_daily %>% filter(treated == 1) %>% ungroup(), 
            cluster = ~account_id)

# Run the regression model
overall_daily <- overall_daily %>% 
  mutate(temp_degree = factor(
    case_when(
      daily_avg_air_temperature_celsius < 0 ~ 0,
      daily_avg_air_temperature_celsius < 25.5 ~ round(daily_avg_air_temperature_celsius),
      TRUE ~ 25
    )))

tempreg <- feols(c(elec_consumption, gas_consumption, total_consumption) ~ 
                   i(is_hp_installed, temp_degree, ref=0) |
                   account_id + temp_degree  + date,
                 data = overall_daily %>% filter(treated == 1) %>% ungroup() ,
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

coefs_wider <- coefs  %>%
  filter(lhs != "total_consumption") %>%
  select(lhs, daily_avg_air_temperature_celsius, Estimate) %>%
  pivot_wider(names_from = lhs, values_from = c(Estimate)) %>%
  mutate(quasi_cop = abs(gas_consumption / elec_consumption)) 

# Plot gas and elec
ggplot(coefs, 
       aes(x = daily_avg_air_temperature_celsius, y = Estimate, group = lhs)) +
  geom_point(aes(color = lhs, fill = lhs)) +
  geom_line(aes(color = lhs)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = lhs), 
                width = 0.2, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Average Temperature in Degrees (°C)",
    y = "Estimate (kWh)",
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
  sampled_ids <- sample(unique(overall_daily$account_id), replace = TRUE)
  
  # Rebuild bootstrapped sample
  boot_data <- overall_daily %>%
    filter(treated == 1) %>%
    semi_join(data.frame(account_id = sampled_ids), by = "account_id")
  
  # Refit model
  boot_model <- tryCatch({
    feols(c(elec_consumption, gas_consumption) ~ 
            i(is_hp_installed, temp_degree, ref = 0) |
            account_id + temp_degree + date,
          data = boot_data, cluster = ~account_id)
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


ggplot(cop_boot %>% filter(as.numeric(temp) < 15), 
       aes(x = temp, y = median)) +
  geom_bar(stat = "identity", alpha = 0.6, fill = hp_color) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "grey") +
  geom_hline(yintercept = avg_cop, linetype = "dashed", color = hp_color) +
  annotate("text", 
           x = 2.5,
           y = avg_cop + 0.4,
           label = paste0("italic('Sample average ≈", round(avg_cop, 2), "')"),
           parse = TRUE,
           color = hp_color,
           size = 4) +
  labs(
    x = "Average Temperature in Degrees (°C)",
    y = "Quasi COP"
  ) +
  theme_minimal()


# Print the plot
ggsave(paste0("graphs/quasi_cop.png"),
       width = 16, height = 8, units = "cm")