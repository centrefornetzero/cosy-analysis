# ====================================================================
# --------- read in elec + gas consumption data ---------
# ====================================================================

# Load main yearly results
eff_df <- fread(file.path(datapath, "output/eff_df.csv")) 

# Load main sample IDs
ids_cs_elec <- readRDS(file.path(datapath, "scratch/ids_cs_elec.RS"))

# Load data for regression
overall_weekly <-
  read_rds(file.path(datapath, "output/overall_weekly.rds")) %>%
  mutate_at(vars(elec_consumption, gas_consumption, total_consumption),
            ~.x / 52.25) %>%
  filter(account_id %in% ids_cs_elec)

# Create CS main results 
start_date <- min(overall_weekly$settlement_week)

# Build week / firstweek and the anticipation=4 treatment indicator
overall_weekly <-  overall_weekly %>%
  ungroup() %>%
  mutate(
    week = as.numeric(difftime(settlement_week, start_date, units = "weeks")) %/% 1 + 1,
    firstweek = as.numeric(difftime(installed_at, start_date, units = "weeks")) %/% 1 + 1
  ) %>%
  group_by(account_id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  filter(week <= 129, firstweek <= 129)  %>%
  filter(week < firstweek - 4 | week >= firstweek)

rm(all_combinations, merged_data, weather_weekly, electricity_daily, cosy_hp_install_gas_consumption)
gc()


# ====================================================================
# --------- HP Impacts by Outside Temperature --------------
# ====================================================================   
# Fit the model
m1 <- feols(c(elec_consumption, gas_consumption) ~ i(is_hp_installed) | 
              hdd + account_id + settlement_week, 
            data =  overall_weekly, 
            cluster = ~account_id)

# Run the regression model
tempreg <- feols(c(elec_consumption, gas_consumption) ~ 
                   i(is_hp_installed, temp_degree, ref=0) |
                   account_id + temp_degree  + settlement_week,
                 data = overall_weekly,
                 cluster = ~account_id)

etable(m1, tempreg, fitstat = ~ g + N)

# Extract coefficients and standard errors
coefs_m1 <- coeftable(m1) %>%
  data.frame() %>%
  select(lhs, Estimate) %>%
  rename(avg_ate = Estimate)

# Build readable labels
temp_levels <- 0:25
temp_labels <- as.character(temp_levels)
temp_labels[temp_levels == 0]  <- "< 0°C"
temp_labels[temp_levels == 25] <- "≥ 25°C"
temp_labels[!(temp_levels %in% c(0, 25))] <-
  paste0(temp_levels[!(temp_levels %in% c(0, 25))], "°C")

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
  filter(lhs != "total_consumption") %>%
    mutate(
      daily_avg_air_temperature_celsius = factor(
      daily_avg_air_temperature_celsius,
      levels = temp_levels,
      labels = temp_labels,
      ordered = TRUE
    ))
fwrite(coefs, file.path(datapath, "scratch/gas_electricity_by_temperature.csv"))


coefs_wider <- coefs  %>%
  filter(lhs != "total_consumption") %>%
  select(lhs, daily_avg_air_temperature_celsius, Estimate) %>%
  pivot_wider(names_from = lhs, values_from = c(Estimate)) %>%
  mutate(quasi_cop = abs(0.9*gas_consumption / elec_consumption)) 

# Human-readable temperature axis breaks and labels
temp_breaks <- c(0, 5, 10, 15, 20, 25)
temp_break_labels <- temp_labels[temp_levels %in% temp_breaks]


# Plot gas and elec
p_outside_temp <- 
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
  scale_x_discrete(breaks = temp_break_labels)+
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
       plot = p_outside_temp, width = 16, height = 8, 
       units = "cm")


# Plot gas and elec
p_outside_temp +
  labs(
    x = "Average Weekly Temperature in Degrees (°C)",
    y = "Estimate (Weekly kWh)",
    color = NULL,
    fill = NULL,
    title = "How heat pumps affect gas & electricity use,  by temperature"
  ) 

# Print the plot
ggsave(paste0("graphs/hp_temperature_gas_elec_blog_version.png"),
       width = 17, height = 8, units = "cm")


# ====================================================================
# --------- COP - Energy Demand Ratio --------------
# ====================================================================  
# Calculate the average value for the dashed line
main_results <- eff_df %>% filter(window == "Last 12 months")

avg_cop <- round(main_results$emp_eff, digits = 2)
print(paste0("Average empirical efficiency ~ ", avg_cop))

# Create rounded tempeture
overall_weekly <- overall_weekly %>%
  mutate(temp_rounded = factor(round(overall_weekly$avg_air_temperature_celsius), -2:23))

# Set up bootstrapping
if (!file.exists(file.path(datapath, "scratch/cop_boot_no_boxing.csv"))) {
    set.seed(123456789)  # for reproducibility
    B <- 1000  # number of bootstrap samples
    results <- vector("list", B)
    pb <- progress_bar$new(total = B, format = "Bootstrapping [:bar] :percent ETA: :eta")


    for (b in 1:B) {

      start <- Sys.time()
      pb$tick()

      # Resample account_ids with replacement
      sampled_ids <- sample(unique(overall_weekly$account_id), replace = TRUE)

      # Rebuild bootstrapped sample
      boot_data <- overall_weekly %>%
        inner_join(data.frame(account_id = sampled_ids), by = "account_id")

      boot_model <- feols(c(elec_consumption, gas_consumption) ~ 
                i(is_hp_installed, temp_rounded, ref = 0) |
                account_id + temp_rounded + settlement_week,
              data = boot_data, cluster = ~account_id, lean=TRUE)

      # Extract estimates
      boot_coefs <- coeftable(boot_model) %>%
        data.frame() %>%
        separate(coefficient, 
                 into = c("is_hp_installed", "remove1", "temp", "remove2"), sep = "::") %>%
        select(lhs, Estimate, temp) %>%
        pivot_wider(names_from = lhs, values_from = Estimate) %>%
        mutate(quasi_cop = abs(0.9 * gas_consumption / elec_consumption)) %>%
        select(temp, quasi_cop)

      results[[b]] <- boot_coefs

      print(paste0(b, ": ", Sys.time() - start))
    }


    # Combine bootstrap results
    cop_boot <- bind_rows(results, .id = "bootstrap") %>%
        mutate(
          daily_avg_air_temperature_celsius = factor(
          temp,
          levels = -2:23,
          ordered = TRUE
        )) %>%
      group_by(temp) %>%
      summarise(
        lower = quantile(quasi_cop, 0.025, na.rm = TRUE),
        upper = quantile(quasi_cop, 0.975, na.rm = TRUE),
        median = median(quasi_cop, na.rm = TRUE),
        .groups = "drop"
      )
    fwrite(cop_boot, file.path(datapath, "scratch/cop_boot_no_boxing.csv"))
} else {
    cop_boot <- fread(file.path(datapath, "scratch/cop_boot_no_boxing.csv")) 
}



ashp_cop <- tibble(
  temp_f = c(-20, -10, 0, 10, 20, 30, 40, 50, 60),
  cop    = c(1.8, 1.8, 1.9, 2.1, 2.4, 2.7, 3.1, 3.5, 3.9)
) %>%
  mutate(temp_c = (temp_f - 32) * 5 / 9)

target_temps <- tibble(temp_c = seq(-2, 15, by = 1))

ashp_interp_df <- target_temps %>%
  mutate(cop = approx(x = ashp_cop$temp_c, y = ashp_cop$cop, xout = temp_c)$y,
         source = "EPRI")

brattle_cop <- tibble(temp_c = seq(-2, 15, by = 1)) %>%
  mutate(
    temp_f = temp_c * 9 / 5 + 32,
    cop = 1.2 + 0.05 * temp_f,
    source = "Brattle"
  ) %>%
  select(temp_c, cop, source)


cop_reference_lines <- bind_rows(ashp_interp_df, brattle_cop)

# Set up temperature axis labels for the plot combining bootstrapped COP estimates with the ASHP COP overlay
temp_breaks <- c(0, 5, 10, 15, 20, 25)
temp_break_labels <- temp_labels[temp_levels %in% temp_breaks]
temp_levels <- -2:23

# keep all bins from -2 to 15
temp_levels <- -2:15
temp_labels <- c("-2°C", "-1°C", "0°C", paste0(1:15, "°C"))

# cap value = top of error bar at 14C
cap_14 <- cop_boot %>%
  mutate(temp_num = as.numeric(as.character(temp))) %>%
  filter(temp_num == 14) %>%
  summarise(cap = mean(upper, na.rm = TRUE)) %>%
  pull(cap)

cop_boot_plot <- cop_boot %>%
  mutate(
    temp_num = as.numeric(as.character(temp)),
    degree_lab = factor(temp_num, levels = temp_levels, labels = temp_labels, ordered = TRUE),
    upper_plot = ifelse(temp_num == 15, cap_14, upper),
    upper_plot = pmax(upper_plot, median)   # keep valid error bar
  ) %>%
  filter(temp_num >= -2, temp_num <= 15)

cop_reference_plot <- cop_reference_lines %>%
  mutate(
    temp_num = round(temp_c),
    degree_lab = factor(temp_num, levels = temp_levels, labels = temp_labels, ordered = TRUE)
  ) %>%
  filter(temp_num >= -2, temp_num <= 15)

p_quasi <- ggplot(cop_boot_plot, aes(x = degree_lab, y = median)) +
  geom_col(alpha = 0.6, fill = hp_color) +
  geom_errorbar(aes(ymin = lower, ymax = upper_plot), width = 0.2, color = hp_color) +
  annotate("text", x = "15°C", y = cap_14 + 0.11, label = "truncated", size = 2, color = hp_color) +
  annotate("text",
           x = "5°C", y = avg_cop + 1.5,
           label = paste0("Sample average ~ ", round(avg_cop, 2)),
           color = hp_color, size = 4) +
  geom_line(
    data = cop_reference_plot,
    aes(x = degree_lab, y = cop, linetype = source, group = source),
    color = flexible_color
  ) +
  labs(
    x = "Average Weekly Temperature in Degrees (°C)",
    y = "Estimated ratio of heat output \nto energy input",
    linetype = "Engineering Models of COP"
  )  +
  scale_x_discrete(
    limits = temp_labels,
    breaks = c("0°C", "5°C", "10°C", "15°C"),
    drop = FALSE
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("graphs/quasi_cop.png", plot = p_quasi, width = 16, height = 8, units = "cm")


                         
