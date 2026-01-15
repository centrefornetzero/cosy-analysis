# read in elec + gas consumption data
overall_weekly <- read_rds(file.path(datapath, "output/overall_weekly.rds"))

# create a dataset that fits the needs of did() function
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

# ---------------------- Create CS main results ---------------------


# Step 2: Estimate CS models and save results

# Define the output filenames for the main analysis
output_filenames <- c(
  "scratch/est_cs_elec_weekly.RDS",
  "scratch/est_cs_gas_weekly.RDS"
)

# Define the corresponding variable names
yname_vars <- c("elec_consumption", "gas_consumption")

# Estimate and save the results for the main analysis (not yet treated control group)
for (i in seq_along(yname_vars)) {
  yname <- yname_vars[i]
  filename <- file.path(datapath, output_filenames[i])
  
  message("Estimating treatment effect for ", yname, " and saving to ", filename)
  
  est_cs <- att_gt(yname = yname,
                   tname = "week",
                   idname = "id",
                   gname = "firstweek",
                   data = did_data,
                   anticipation = 1,
                   clustervars = "id",
                   control_group = c("notyettreated"),
                   est_method = "ipw",   # <— avoids fastglm in most setups
                   faster_mode=FALSE,
                   allow_unbalanced_panel = TRUE,
                   base_period = "varying")
  
  saveRDS(est_cs, filename)
  message("Saved: ", filename)
}


# Robustness check with base_period = "universal"
robust_output_filenames <- gsub("\\.RDS$", "_robust_universal.RDS", output_filenames)

for (i in seq_along(yname_vars)) {
  yname <- yname_vars[i]
  filename <- file.path(datapath, robust_output_filenames[i])
  
  message("Estimating treatment effect for ", yname, " with universal base period and saving to ", filename)
  
  est_cs <- att_gt(yname = yname,
                   tname = "week",
                   idname = "id",
                   gname = "firstweek",
                   data = did_data,
                   anticipation = 1,
                   clustervars = "id",
                   control_group = c("notyettreated"),
                   est_method = "ipw",   # <— avoids fastglm in most setups
                   faster_mode=FALSE,
                   allow_unbalanced_panel = TRUE,
                   base_period = "universal")
  
  saveRDS(est_cs, filename)
  message("Saved: ", filename)
}

# Step 3: Estimate CS models for the "never treated" control group

never_treated_output_filenames <- gsub("est_cs_", "est_cs_never_treated_", output_filenames)

did_data <- overall_weekly %>%
  ungroup() %>%
  mutate(
    week = as.numeric(difftime(settlement_week, start_date, units = "weeks")) %/% 1 + 1,
    firstweek = as.numeric(difftime(installed_at, start_date, units = "weeks")) %/% 1 + 1
  ) %>%
  group_by(account_id) %>%
  mutate(id = cur_group_id(),
         firstweek = ifelse(firstweek>129, 0, firstweek)) %>%
  ungroup() %>%
  select(id, firstweek, week, total_consumption, elec_consumption, gas_consumption) %>%
  filter(week <= 129) 

for (i in seq_along(yname_vars)) {
  yname <- yname_vars[i]
  filename <- file.path(datapath, never_treated_output_filenames[i])
  
  message("Estimating treatment effect for ", yname, " with 'never treated' control group and saving to ", filename)
  
  est_cs <- att_gt(yname = yname,
                   tname = "week",
                   idname = "id",
                   gname = "firstweek",
                   data = did_data,
                   anticipation = 1,
                   clustervars = "id",
                   control_group = c("notyettreated", "nevertreated"),
                   est_method = "ipw",   # <— avoids fastglm in most setups
                   faster_mode=FALSE,
                   allow_unbalanced_panel = TRUE,
                   base_period = "varying")
  
  saveRDS(est_cs, filename)
  message("Saved: ", filename)
}

# Step 4: Estimate CS models for the gas-only subset
start_date <- min(overall_weekly$settlement_week, na.rm = TRUE)

gas_accounts <- overall_weekly %>%
  group_by(account_id) %>%
  summarise(has_gas = any(!is.na(gas_consumption)), .groups = "drop") %>%
  filter(has_gas) %>%
  pull(account_id)

did_data <- overall_weekly %>%
  filter(account_id %in% gas_accounts) %>%
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

gas_only_output_filename <- file.path(datapath, "scratch/est_cs_elec_weekly_gas_only.RDS")

message("Estimating treatment effect for electricity consumption in gas-only sample and saving to ", gas_only_output_filename)

est_cs_gas_only <- att_gt(yname = "elec_consumption",
                          tname = "week",
                          idname = "id",
                          gname = "firstweek",
                          data = did_data,
                          anticipation = 1,
                          clustervars = "id",
                          control_group = c("notyettreated"),
                          est_method = "ipw",   # <— avoids fastglm in most setups
                          faster_mode=FALSE,
                          allow_unbalanced_panel = TRUE,
                          base_period = "varying")

saveRDS(est_cs_gas_only, gas_only_output_filename)
message("Saved: ", gas_only_output_filename)







# ============================================================
# 9) DID IMPUTATION (patched)
# Output: tables/hp_did_never_treated_detailed.tex
# ============================================================
                         
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


# did_imputation command
# Setting up the arguments for did_imputation
imputation_results <- did_imputation(
  data = did_data,
  yname = "gas_consumption",    # Outcome variable (or choose "elec_consumption" or "gas_consumption")
  gname = "firstweek",            # Variable name for unit-specific treatment time
  tname = "week",                 # Calendar period
  idname = "id",                  # Unique unit ID
  first_stage = NULL,             # Default to unit and time fixed effects
  wname = NULL,                   # No weights provided
  wtr = NULL,                     # Treatment weights (for static and event study effects)
  horizon = c(-52, 52),                 # Use all event time horizons
  pretrends = -52:-1,               # Use all pre-trends
  cluster_var = "id"              # Clustering variable
)

# Viewing the results
summary(imputation_results)


# did_imputation command
# Setting up the arguments for did_imputation
imputation_results2 <- did_imputation(
  data = did_data,
  yname = "elec_consumption",    # Outcome variable (or choose "elec_consumption" or "gas_consumption")
  gname = "firstweek",            # Variable name for unit-specific treatment time
  tname = "week",                 # Calendar period
  idname = "id",                  # Unique unit ID
  first_stage = NULL,             # Default to unit and time fixed effects
  wname = NULL,                   # No weights provided
  wtr = NULL,                     # Treatment weights (for static and event study effects)
  horizon = c(-52, 52),                 # Use all event time horizons
  pretrends = -52:-1,               # Use all pre-trends
  cluster_var = "id"              # Clustering variable
)

# Viewing the results
summary(imputation_results2)



# merge the two estimates
plot_data <- rbind(imputation_results %>% mutate(type = "Gas"),
                   imputation_results2 %>% mutate(type = "Electricity")) %>%
  rename(coefficient = estimate,
         event_time = term) %>%
  mutate(event_time = as.numeric(event_time)) %>%
  filter(event_time > -91, event_time < 90) %>%
  mutate(lower_ci =  conf.low,
         upper_ci = conf.high)

#%>%
#mutate(lower_ci = ifelse(event_time < 0, NA, conf.low),
#       upper_ci = ifelse(event_time < 0, NA, conf.high))


# define color
elec_color <- "#AD87CA"
gas_color <- "#2D354A"

# plot
ggplot(plot_data, aes(x = event_time, y = coefficient, group = type)) +
  geom_line(aes(color = type)) +
  geom_point(aes(color = type, shape = type), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = type), width = 0.2, alpha = 0.6) +
  scale_colour_manual(name = "Type",
                      labels = c("Electricity", "Gas"),
                      values = c(Electricity = elec_color, Gas = gas_color)) +   
  scale_shape_manual(name = "Type",
                     labels =  c("Electricity", "Gas"),
                     values = c(16, 17)) + 
  scale_x_continuous(breaks = seq(floor(min(plot_data$event_time) / 10) * 10, ceiling(max(plot_data$event_time) / 10) * 10, 10)) +
  labs(
    x = "Weeks since adoption",
    y = "Dynamic ATT for Weekly Consumption (kWh)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")  # Move the legend to the bottom
ggsave("graphs/hp_dynamic_att_combined_imputation.png", device = "png", width = 16, height = 12, dpi = 300)                         
    