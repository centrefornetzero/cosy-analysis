# Prepare the data for installations
weekly_installations <- hp_installed %>%
  group_by(account_id) %>%
  mutate(treated = max(is_hp_installed)) %>%
  filter(treated ==1) %>%
  ungroup() %>%
  select(account_id, installed_at) %>%
  distinct() %>%
  mutate(first_week = as.Date(floor_date(installed_at, "week"))) %>%
  group_by(first_week) %>%
  summarise(installations = n())

# Prepare the data for deals
weekly_deals <- deals_and_installations %>%
  filter(deal_created_at >= "2022-02-01",
         deal_created_at < "2024-06-23") %>%
  mutate(first_week = as.Date(floor_date(deal_created_at, "week"))) %>%
  group_by(first_week) %>%
  summarise(deals = n()) 

# Define the date for the announcement
announcement_date <- as.Date("2023-08-31")

# Choose colors from the Brewer palette
color_installations <- hp_color
color_deals <- hp_color

# Create the combined plot
ggplot() +
  # Plot for deals
  geom_line(data = weekly_deals, aes(x = first_week, y = deals, color = "Deals"), linetype = "dashed") +
  geom_vline(xintercept = as.numeric(announcement_date), linetype = "dashed", color = color_deals) +
  # Plot for installations
  geom_line(data = weekly_installations, aes(x = first_week, y = installations, color = "Installations")) +
  geom_point(data = weekly_installations, aes(x = first_week, y = installations, color = "Installations")) +
  annotate("text", x = announcement_date - weeks(1), y = max(weekly_deals$deals) * 0.9,
           label = "Announcement:\nBoiler Upgrade Scheme\nincrease to £7,500", hjust = 1, 
           color = color_deals) +
  labs(
    x = "Week",
    y = "Number of Installations/Deals",
    color = "Metric"
  ) +
  scale_x_date(
    labels = scales::date_format("%b %y"),
    date_breaks = "3 month"
  ) +
  scale_color_manual(values = c("Installations" = color_installations, "Deals" = "grey")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save the combined plot
ggsave("graphs/combined_weekly_installations_deals.png", width = 15, height = 10, units = "cm", dpi = 300)

hp_installed %>%
  distinct(account_id, installed_at) %>%
  mutate(month_installation = month(installed_at, label = TRUE)) %>%
  count(month_installation) %>%
  ggplot(aes(x = month_installation, y = n)) +
  geom_col(fill = hp_color) +
  labs(
    x = "Month of Installation",
    y = "Number of Installations"
  ) +
  theme_minimal()

ggsave("graphs/monthly_installation.png", width = 12, height = 8, dpi = 300)

