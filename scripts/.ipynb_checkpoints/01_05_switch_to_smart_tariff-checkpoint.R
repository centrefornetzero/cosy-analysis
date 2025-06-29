
Cosy_hp_agreements_data_2024_07_05 <- fread("data/input/Cosy_-_hp_agreements_data_2024_07_05.csv") %>%
  mutate(time_since_hp = round(as.numeric(difftime(agreement_valid_from, installed_at, units = "week"))/(4.43)),
         time_since_hp = case_when(time_since_hp < -15 ~ -15,
                                   time_since_hp > 15 ~ 15,
                                   TRUE ~ time_since_hp),
         smart_tariff = as.numeric(product_display_name %in% c("Agile Octopus","Intelligent Octopus Go", "Cosy Octopus", "Octopus Go", "Octopus Flux Import", "Intelligent Octopus Flux Import", "Octopus Go Faster")),
         year = year(agreement_valid_from))
table(Cosy_hp_agreements_data_2024_07_05$time_since_hp)

m_tariff <- feols(smart_tariff ~ i(time_since_hp, ref= -1) | account_id + year, data = Cosy_hp_agreements_data_2024_07_05, cluster = ~ account_id)

# Extract coefficients and standard errors
coefs <- coeftable(m_tariff) %>%
  data.frame() %>%
  tibble::rownames_to_column("term") %>%
  separate(term, into = c( "remove1", "time_since_hp"), sep = "::") %>%
  mutate(time_since_hp = as.numeric(time_since_hp), 
         lower_ci = Estimate - 1.96 * `Std..Error`,
         upper_ci = Estimate + 1.96 * `Std..Error`,
         post = (time_since_hp>=0)
  )

# Create the ggplot
ggplot(coefs, aes(x = time_since_hp, y = Estimate, color = post)) +
  geom_point(stat = "identity") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Months Since HP Installation",
    y = "Probability of Having a Smart Tariff",
    color = "Is HP Installed"  # Update legend title
  ) +
  scale_color_manual(
    values = c("TRUE" = hp_color, "FALSE" = not_hp_color),  # Set custom colors
    labels = c("No", "Yes")  # Update legend labels
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  theme_minimal()

ggsave("graphs/hp_switch_to_smart_tariff.png")