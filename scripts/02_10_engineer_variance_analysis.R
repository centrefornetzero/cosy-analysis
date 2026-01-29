## Decompose treatment effects into household vs installer engineer effects

# =======================
# 1. Data Preparation & Regression
# =======================
hp_installed <- read_rds(file.path(datapath, "/output/hp_installed.rds"))

# Installer FE
installers <- fread(file.path(datapath, "/input/cosy_-_hp_engineers_2025_03_17.csv")) %>%
  inner_join(distinct(filter(hp_installed, treated == 1), account_id, deal_created_at)) %>%
  filter(hp_engineer != "") %>%
  group_by(hp_engineer) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  select(-n)

# Prepare data: filter, join, engineer features
df <- 
  installers %>%
  inner_join(hp_installed) %>%
  filter(treated == 1, rate_period == "Overall") %>%
  mutate(temperature = round(daily_avg_heating_degree))

# Main model: includes interaction and fixed effects
ref_ing <- unique(df$hp_engineer)[1]   # or choose explicitly, see below

reg <- feols(
  consumption_hh ~ i(is_hp_installed, ref = 0) +
    i(is_hp_installed, hp_engineer, ref = 0, ref2 = ref_ing) |
    account_id + date + hdd,
  cluster = ~account_id,
  data = df
)

etable(reg)

# =====================================================================
# Delete?
# If I train my heat-pump consumption model on half the engineers, can it 
# correctly predict consumption outcomes for customers of other engineers?
# =====================================================================
# stratified randomisation
set.seed(123)
installers$insample <- randomizr::strata_rs(strata = installers$hp_engineer, 
                                            prob = 0.5)

# Regression in sample
m_insample <- feols(
  consumption_hh ~ i(is_hp_installed, ref = 0) +
    i(is_hp_installed, hp_engineer, ref = 0, ref2 = "Steven Wiltshire") |
    date + hdd,
  cluster = ~account_id,
  data = df %>% filter(hp_engineer %in% installers[installers$insample == 1,]$hp_engineer)
)

# Predict out of sample
df$consumption_hh_pred <- predict(m_insample, 
                                  newdata = df)

# Out of sample regression
m_outsample <- feols(
  consumption_hh ~ consumption_hh_pred |
    account_id,
  cluster = ~account_id,
  data = df %>% filter(hp_engineer %in% installers[installers$insample == 0,]$hp_engineer)
)

# Plot data
plot_data <- df %>%
  inner_join(installers) %>%
  mutate(insample_label = ifelse(insample == 1, "In-sample", "Out-of-sample")) 

ggplot(plot_data, aes(x = consumption_hh_pred, 
                      y = consumption_hh, 
                      color = factor(insample_label),
                      group = factor(insample_label))) +
  geom_point(alpha = 0.3) +
  scale_color_manual(values = c("In-sample" = hp_color, 
                                "Out-of-sample" = "grey")) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Predicted Consumption (kWh)",
    y = "Actual Consumption (kWh)",
    color = "Group",
    title = "Predicted vs. Actual Consumption",
    subtitle = "Linear fit for in-sample and out-of-sample households"
  ) +
  theme_minimal()

# plot engineer FE
ggplot(plot_data, 
       aes(x = consumption_hh_pred, y = consumption_hh, 
           color = factor(insample_label))) +
  geom_point(alpha = 0.3) +
  scale_color_manual(values = c("In-sample" = hp_color, 
                                "Out-of-sample" = "grey")) +
  labs(
    x = "Engineer",
    y = "Consumption (kWh)",
    color = "Group",
    title = "Engineer-Specific Consumption",
    subtitle = "In-sample and out-of-sample households"
  ) +
  theme_minimal()



# =====================================================================
## manual decomposition of variance for FE
# =====================================================================
fes <- fixef(reg)
fe_account <- fes$account_id[unique(as.character(df$account_id))]
fe_date <- fes$date[as.character(df$date)]
fe_temp <- fes$hdd[as.character(df$hdd)]

# decomposition of treatment effects
X <- model.matrix(reg)
interaction_cols <- grep("hp_engineer", colnames(X), value = TRUE)
interaction_pred <- as.numeric(X[, interaction_cols] %*% coef(reg)[interaction_cols])
ate_col <- grep("is_hp_installed::1$", colnames(X), value = TRUE)
ate_pred <- as.numeric(X[, ate_col] * coef(reg)[ate_col])

# total var of outcomes
var_total <- var(df$consumption_hh, na.rm = TRUE)

var_decomp <- data.frame(
  Component = c("Account FE", "Date FE", "HDD FE", "ATE", "Engineer Interaction"),
  Share = c(
    var(fe_account, na.rm = TRUE),
    var(fe_date, na.rm = TRUE),
    var(fe_temp, na.rm = TRUE),
    var(ate_pred, na.rm = TRUE),
    var(interaction_pred, na.rm = TRUE)
  ) / var_total
)

stargazer(var_decomp, 
          summary = FALSE, 
          label = "variance-decomp",
          out = "tables/variance_decomp.tex",  
          title = "Decomposition of Variance in Consumption Outcomes",
          type = "latex")





# =====================================================================
# 3. Bias-Corrected FE Covariance (felm)
# DELETE?
# ====================================================================

df2 <- df %>%
  inner_join(data.frame(date = as.Date(names(fes$date)), time_fe = fes$date)) %>%
  inner_join(data.frame(temperature = as.numeric(names(fes$hdd)), temp_fe = fes$hdd))

df2$consumption_hh_resid <- df2$consumption_hh - df2$time_fe - df2$temp_fe

reg_sub <- felm(
  consumption_hh_resid ~ is_hp_installed + is_hp_installed:hp_engineer | account_id,
  data = df2,
  cmethod = "reghdfe"
)

fe_sub <- getfe(reg_sub, se = TRUE)
covar <- fevcov(reg_sub, fe_sub)
covar_biased <- covar + attr(covar, "bias")
total_var <- var(df2$consumption_hh_resid, na.rm = TRUE)

interaction_cols <- grep("^is_hp_installed:hp_engineer", colnames(model.matrix(reg_sub)))
interact_pred <- model.matrix(reg_sub)[, interaction_cols] %*% coef(reg_sub)[interaction_cols]
main_pred <- model.matrix(reg_sub)[, "is_hp_installed"] * coef(reg_sub)["is_hp_installed"]

var_table <- data.frame(
  Component = c("Account FE", "Main Treatment Effect", "Treatment × Engineer"),
  Variance_Share = c(diag(covar / total_var), var(main_pred) / total_var, var(interact_pred) / total_var)
)

stargazer(var_table, 
          summary = FALSE, 
          label = "bias-coorected-variance-decomp",
          out = "tables/bias_corrected_decomp.tex",  
          title = "Decomposition of Variance in Consumption Outcomes",
          type = "latex")


# =====================================================================
# 4. Covariance: Account FE × Interaction Effect
# ====================================================================
account_fe_vec <- fe_sub[fe_sub$fe == "account_id", c("idx", "effect")]
colnames(account_fe_vec) <- c("account_id", "account_fe")
account_fe_vec$account_id <- as.character(account_fe_vec$account_id)
df2$account_id <- as.character(df2$account_id)
df2 <- merge(df2, account_fe_vec, by = "account_id")

df2$interaction_pred <- interact_pred
cov_account_interaction <- cov(df2$account_fe, df2$interaction_pred, use = "complete.obs")
cor_account_interaction <- cor(df2$account_fe, df2$interaction_pred, use = "complete.obs")

cat("Covariance:", cov_account_interaction, "\n")
cat("Correlation:", cor_account_interaction, "\n")

# 5. Plot: Engineer FE vs Avg Household FE

engineer_house_fe <- df2 %>%
  group_by(hp_engineer) %>%
  summarise(avg_account_fe = mean(account_fe, na.rm = TRUE))

interaction_coefs <- coef(summary(reg_sub)) %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  filter(grepl("^is_hp_installed:hp_engineer", term)) %>%
  mutate(hp_engineer = gsub("is_hp_installed:hp_engineer", "", term),
         hp_engineer = gsub("[()]", "", hp_engineer))

plot_data <- left_join(engineer_house_fe, interaction_coefs, by = "hp_engineer")

ggplot(plot_data, aes(x = avg_account_fe, y = Estimate)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    x = "Average Account Fixed Effect",
    y = "Engineer-Specific Treatment Effect"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("graphs/engineer_vs_household_fe.png", width = 8, height = 6)


# coeftable
coefs <- coeftable(reg) %>%
  data.frame() %>%
  tibble::rownames_to_column("term") %>%
  as_tibble() %>%
  separate(term, into = c("is_hp_installed", "remove1", "remove2", "remove3", "remove44" , "hp_engineer"), sep = ":") %>%
  mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
         upper_ci = Estimate + 1.96 * `Std..Error`
  ) %>% filter(!is.na(remove3))


rm(m1c, m_solar, ev_charging, ev_users, ev_charging_agg)
rm(list = ls(pattern = "boot*"))
rm(list = ls(pattern = "coefs*"))
rm(results)


# =====================================================================
# Plot engineer specific coefs
# DELETE?
# ====================================================================
# Installer FE
installers <- fread(file.path(datapath, "/input/cosy_-_hp_engineers_2025_03_17.csv")) 

# Unique periods 
periods <- unique(hp_installed$rate_period)

# Run the regression model
m1 <- feols(consumption_hh ~ i(is_hp_installed, ref=0) |
              account_id + hdd  + date,
            data = hp_installed %>% filter(rate_period == "Overall"),
            cluster = ~account_id) 

# Run the regression model
tempreg <- feols(consumption_hh ~ i(is_hp_installed, hp_engineer, ref=0) |
                   account_id + hdd  + date,
                 data = df,
                 cluster = ~account_id) 

coefs <- coeftable(tempreg) %>%
  data.frame() %>%
  tibble::rownames_to_column("term") %>%
  as_tibble() %>%
  separate(term, into = c("is_hp_installed", "remove1", "remove2", "remove3", "remove4" , "hp_engineer"), sep = ":") %>%
  filter(!is.na(hp_engineer)) %>%
  mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
         upper_ci = Estimate + 1.96 * `Std..Error`
  ) %>%
  mutate(`/% ATE` = Estimate / m1$coefficients * 100,     
         lower_ci_ATE = `/% ATE` - 1.96 * (`Std..Error` / m1$coefficients * 100),
         upper_ci_ATE = `/% ATE` + 1.96 * (`Std..Error` / m1$coefficients * 100)
  ) %>%
  arrange(Estimate) %>%
  mutate(hp_engineer = factor(hp_engineer, levels = unique(hp_engineer)))

# plot the yearly impact
yearly_factor <- 365.25*48
ggplot(coefs, aes(x = hp_engineer, y = Estimate * yearly_factor)) +  # Scale Estimate
  geom_point(color = hp_color) +
  geom_line(color = hp_color) +
  geom_errorbar(aes(ymin = lower_ci * yearly_factor, ymax = upper_ci * yearly_factor), 
                width = 0.2, alpha = 0.6, color = hp_color) +  # Scale CI
  geom_hline(yintercept = m1$coefficients * yearly_factor, 
             linetype = "dashed", alpha = 0.6, color = hp_color) +  # Scale ATE line
  scale_y_continuous(
    name = "Estimate (kWh per Year)",  # Update Y-axis label
    labels = label_comma(),  # Format y-axis with comma separator
    sec.axis = sec_axis(~ ./ (m1$coefficients[1] * yearly_factor), 
                        name = "% of ATE", 
                        labels = scales::percent_format())  # Scale % ATE
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Zero line
  labs(
    x = "Is Installed x Engineer"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

summary(coefs$Estimate)
quantile(coefs$Estimate, 0.75)/quantile(coefs$Estimate, 0.25)

# Print the plot
ggsave(paste0("graphs/hp_engineer.png"),
       width = 16, height = 8, units = "cm")
