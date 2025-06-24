library(readr)
library(dplyr)
library(ggplot2)
library(fixest)

# Load and join data
cosy_hp_users_gas_2024_06_13 <- read_csv("data/input/cosy_-_hp_users_gas_2024_06_13.csv")
hp_gas_consumption_2025_06_17 <- read_csv("data/input/hp_gas_consumption_2025_06_17.csv")

kwh_checks <- hp_gas_consumption_2025_06_17 %>%
  inner_join(cosy_hp_users_gas_2024_06_13, by = c("account_id", "week_starting" = "settlement_week"))

# Fit model
model <- feols(weekly_consumption ~ weekly_kwh, data = kwh_checks)
coefs <- coef(model)
intercept <- round(coefs[1], 2)
slope <- round(coefs[2], 2)
eqn <- paste0("y = ", slope, "x + ", intercept)

# Plot with annotation
ggplot(kwh_checks, aes(x = weekly_kwh, y = weekly_consumption)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # 1:1 reference line
  geom_abline(intercept = intercept, slope = slope, color = "blue") +          # regression line
  annotate("text", x = Inf, y = -Inf, label = eqn,
           hjust = 1.1, vjust = -1.2, size = 4.5, color = "blue", parse = FALSE) +
  labs(title = "Weekly kWh Consumption Comparison",
       y = "Weekly kWh from billing tables",
       x = "Weekly kWh from amphio") +
  theme_minimal()
