## Figure A.13: Event Study - Heat Pump Installation on Daily Average of Customers’

# create df
event_study_df <- hp_installed %>%
  mutate(weeks_since_hp = as.numeric(difftime(date, installed_at, units = "weeks")) %/% 1 + 1,
         weeks_since_hp = case_when(
           weeks_since_hp < -52 ~ -52,
           weeks_since_hp > 52 ~ 52,
           TRUE ~ weeks_since_hp)) %>%
  select(account_id, weeks_since_hp, consumption_hh, hdd, date, rate_period)

rm(hp_installed)
gc()

m_event_study <- feols(consumption_hh ~ i(weeks_since_hp, ref=-1) | account_id + hdd + date, 
                       data = event_study_df %>% filter(rate_period=="Overall") %>% 
                         mutate(),
                       cluster = ~ account_id)

etable(m_event_study)

# plot the coefficients
# Extract coefficients and standard errors
coefs <- coeftable(m_event_study)%>%
  data.frame() %>%
  tibble::rownames_to_column("term")%>%
  as_tibble() %>%
  separate(term, into = c("var",  "weeks_since_hp"), sep = "::") %>%
  mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
         upper_ci = Estimate + 1.96 * `Std..Error`,
         outcome = "Half Hourly Consumption") %>%
  mutate(weeks_since_hp = as.numeric(weeks_since_hp),
         post = (weeks_since_hp >-1))

# Create the plot
ggplot(coefs, aes(x = weeks_since_hp, y = Estimate, color = post)) +
  geom_point(stat = "identity", show.legend = TRUE) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Weeks Since HP Installation",
    y = "Half Hourly Consumption in kWh",
    color = "Is HP Installed"  # Update legend title
  ) +
  scale_color_manual(
    values = c("TRUE" = hp_color, "FALSE" = not_hp_color),  # Set custom colors
    labels = c("No", "Yes")  # Update legend labels
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
    legend.position = "right"  # Show legend
  )


# Print the plot
ggsave(paste0("graphs/hp_event_study_overall.png"),
       width = 16, height = 8, units = "cm")

rm(m_event_study, hp_installed)


m_event_study <- feols(consumption_hh ~ i(weeks_since_hp, ref=-1) | account_id + hdd + date, 
                       data = event_study_df %>% filter(rate_period=="Peak Rate") %>% 
                         mutate(),
                       cluster = ~ account_id)

etable(m_event_study)

# plot the coefficients
# Extract coefficients and standard errors
coefs <- coeftable(m_event_study)%>%
  data.frame() %>%
  tibble::rownames_to_column("term")%>%
  as_tibble() %>%
  separate(term, into = c("var",  "weeks_since_hp"), sep = "::") %>%
  mutate(lower_ci = Estimate - 1.96 * `Std..Error`,
         upper_ci = Estimate + 1.96 * `Std..Error`,
         outcome = "Half Hourly Consumption") %>%
  mutate(weeks_since_hp = as.numeric(weeks_since_hp),
         post = (weeks_since_hp >-1))

# Create the plot
ggplot(coefs, aes(x = weeks_since_hp, y = Estimate, color = post)) +
  geom_point(stat = "identity", show.legend = TRUE) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Weeks Since HP Installation",
    y = "Half Hourly Consumption in kWh",
    color = "Is HP Installed"  # Update legend title
  ) +
  scale_color_manual(
    values = c("TRUE" = hp_color, "FALSE" = not_hp_color),  # Set custom colors
    labels = c("No", "Yes")  # Update legend labels
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
    legend.position = "right"  # Show legend
  )


# Print the plot
ggsave(paste0("graphs/hp_event_study_peak_rate.png"),
       width = 16, height = 8, units = "cm")
