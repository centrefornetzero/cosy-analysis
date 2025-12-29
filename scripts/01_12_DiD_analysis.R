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
  "scratch/est_cs_total_weekly.RDS",
  "scratch/est_cs_elec_weekly.RDS",
  "scratch/est_cs_gas_weekly.RDS"
)

# Define the corresponding variable names
yname_vars <- c("total_consumption", "elec_consumption", "gas_consumption")

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
                   allow_unbalanced_panel = TRUE,
                   base_period = "varying")
  
  saveRDS(est_cs, filename)
  message("Saved: ", filename)
}

# Step 4: Estimate CS models for the gas-only subset
start_date <- min(overall_weekly$settlement_week)
start_date <- min(overall_weekly$settlement_week)
did_data <- overall_weekly %>%
  filter(account_id %in% merged_data$account_id) %>%
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
                          allow_unbalanced_panel = TRUE,
                          base_period = "varying")

saveRDS(est_cs_gas_only, gas_only_output_filename)
message("Saved: ", gas_only_output_filename)






