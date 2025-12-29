# Load necessary libraries
library(fixest)   # For fixed effects models (feols)
library(dplyr)    # For data manipulation
library(ggplot2)  # For plotting

# Set a seed for reproducibility
set.seed(123)

# Number of customers and days
n_customers <- 100
n_days <- 365

# Simulate daily gas consumption data
daily_gas <- data.frame(
  account_id = rep(1:n_customers, each = n_days),
  settlement_date = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = n_days), times = n_customers),
  temp_degree = runif(n_customers * n_days, 0, 25)  # Random daily temperature
)

# Create random customer and day fixed effects
customer_fe <- rnorm(n_customers, mean = 10, sd = 2)  # Random customer fixed effect
day_fe <- rnorm(n_days, mean = 0, sd = 1)  # Random day fixed effect

# Add customer and day fixed effects to the data
daily_gas$customer_fe <- customer_fe[daily_gas$account_id]
daily_gas$day_fe <- day_fe[as.numeric(daily_gas$settlement_date - as.Date("2023-01-01")) + 1]

# Temperature effect: consumption decreases as temperature increases
daily_gas$temp_effect <- 0.5 * daily_gas$temp_degree  # Decreasing effect with temperature

# Install effect: customers get treated progressively over time, with a different effect than temperature
treatment_start_dates <- sample(seq(as.Date("2023-03-01"), as.Date("2023-06-01"), by = "day"), n_customers, replace = TRUE)
daily_gas$is_hp_installed <- ifelse(daily_gas$settlement_date >= treatment_start_dates[daily_gas$account_id], 1, 0)

# Install effect: Decreases gas consumption, interacts with temperature differently than the temperature effect
daily_gas$install_effect <- daily_gas$is_hp_installed * (0.8 * daily_gas$temp_degree + -15)  # Different temperature-dependent effect

# Add random noise
daily_gas$random_noise <- rnorm(n_customers * n_days, mean = 0, sd = 2)

# Simulate the gas consumption based on the effects
daily_gas$consumption_chargedcurrent_daily <- daily_gas$customer_fe + daily_gas$day_fe +
  daily_gas$temp_effect + daily_gas$install_effect + daily_gas$random_noise

# create temperature factor
daily_gas$temp_degree <- factor(round(daily_gas$temp_degree), levels = 0:25)

# Model 1: Two-way fixed effects (TWFE) difference-in-differences model
# This model includes customer and day fixed effects, and looks at the effect of temperature and installation.
m1_gas <- feols(consumption_chargedcurrent_daily ~ i(is_hp_installed) | 
                  account_id + settlement_date + temp_degree,  # Fixed effects for customer and date
                data = daily_gas, cluster = ~ account_id)

# Model 2: Heterogeneity analysis with interaction between treatment (is_hp_installed) and temperature
# This model allows for a varying impact of heat pump installation based on outside temperature.
m_tempreg <- feols(consumption_chargedcurrent_daily ~  i(is_hp_installed, temp_degree, ref = 0) | 
                     account_id + settlement_date + temp_degree,  # Fixed effects for customer and date
                   data = daily_gas, cluster = ~ account_id)

# Compare the two models side by side
etable(m1_gas, m_tempreg)

# Plot the coefficients of the interaction from model 2
all_coefs <- coeftable(m_tempreg) %>%
  as.data.frame() %>%
  rownames_to_column(var = "rownames") %>%
  separate(rownames, into = c("is_hp_installed", "remove1", "daily_avg_air_temperature_celsius"), sep = "::") %>%
  mutate(daily_avg_air_temperature_celsius = as.numeric(daily_avg_air_temperature_celsius),
         lower_ci = Estimate - 1.96 * `Std. Error`,  # Lower confidence interval
         upper_ci = Estimate + 1.96 * `Std. Error`)  # Upper confidence interval

# Plot the effect of temperature on the estimated impact of heat pump installation
ggplot(all_coefs, aes(x = daily_avg_air_temperature_celsius, y = Estimate)) +
  geom_point(color = "blue") +  # Plot the point estimates
  geom_line(color = "blue") +   # Connect the points with a line
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6, color = "blue") +  # Add error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line at y = 0
  scale_y_continuous(name = "Estimated Impact on Gas Consumption (kWh)") +  # Y-axis label
  labs(x = "Average Temperature (Degrees Celsius)") +  # X-axis label
  theme_minimal()  # Apply a minimal theme to the plot

# Save the plot
ggsave("hp_temperature_gas.png", width = 16, height = 8, units = "cm")