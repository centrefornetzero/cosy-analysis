

## Summary Statistics Tables and Graphs {#sec:sumstats}
Next_contract <- fread("data/input/Cosy_-_agreement_data_2024_07_24.csv") %>%
  arrange(hashed_mpan, desc(as.Date(agreement_valid_from))) %>%
  group_by(hashed_mpan) %>%
  mutate(na_flag = ifelse(is.na(agreement_valid_to), 1, 0),
         na_cumsum = cumsum(na_flag)) %>%
  filter(na_cumsum == 1) %>%
  select(-na_flag, -na_cumsum) %>%
  ungroup()%>%
  group_by(hashed_mpan) %>%
  slice(1) %>%
  mutate(is_variable = ifelse(product_display_name %in% 
                                c("Co-op Flexible",
                                  "Flexible Avro",
                                  "Flexible Octopus",
                                  "Flexible Octopus Smart Pay as You Go",
                                  "Loyal Flexible Octopus Smart Pay as You Go"), FALSE, is_variable)) %>%
  group_by(is_charged_half_hourly) %>%
  tally() %>%
  ungroup() %>%
  mutate(share_is_variable = n/sum(n))

# Contract before cosy
first_cosy_contracts <- fread("data/input/Cosy_-_agreement_data_2024_07_24.csv") %>%
  arrange(hashed_mpan, as.Date(agreement_valid_from)) %>%
  group_by(hashed_mpan) %>%
  mutate(
    previous_contract = lag(product_display_name),
    previous_is_variable = lag(is_variable),
    previous_is_charged_hh = lag(is_charged_half_hourly),
    is_cosy = product_display_name == "Cosy Octopus"
  ) %>%
  filter(is_cosy) %>%
  slice_head(n = 1) %>%
  mutate(previous_is_variable = ifelse(previous_contract %in% 
                                         c("Co-op Flexible",
                                           "Flexible Avro",
                                           "Flexible Octopus",
                                           "Flexible Octopus Smart Pay as You Go",
                                           "Loyal Flexible Octopus Smart Pay as You Go"), 
                                       FALSE, 
                                       previous_is_variable)) %>%
  filter(!is.na(previous_is_variable)) %>%
  group_by(previous_is_charged_hh) %>%
  tally() %>%
  mutate(share = 100*n/sum(n)) %>%
  arrange(share)


### Figure 3: Weekly Adoption of the Cosy tariff
# Prepare the data
weekly_adoptions <- aggregated_data %>%
  ungroup() %>%
  select(hashed_mpan, first_adoption) %>%
  distinct() %>%
  mutate(first_week = floor_date(first_adoption, "week")) %>%
  group_by(first_week) %>%
  summarise(adoptions = n())

# Define the date for the announcement
announcement_date <- as.Date("2023-08-31")

# Choose a color from the Brewer palette for the text annotation
text_color <- brewer.pal(n = 3, name = "Set1")[1]

# Create the plot with the vertical line and adjusted annotation
ggplot(weekly_adoptions, aes(x = first_week, y = adoptions)) +
  geom_line(color = cosy_color) +  # Line plot for trends with a color from the Brewer palette
  geom_point(color = cosy_color) +  # Points to highlight individual data with the same color
  geom_vline(xintercept = as.numeric(announcement_date), linetype = "dashed", color = text_color) +  # Vertical line for the announcement
  annotate("text", x = announcement_date - weeks(1), y = 150,
           label = "Announcement:\nBoiler Upgrade Scheme\nincrease to £7,500", hjust = 1, color = text_color) +  # Annotate the vertical line
  labs(
    x = "Week",
    y = "Customers switching to Cosy"
  ) +
  scale_x_date(
    labels = scales::date_format("%b %y"),  # Formatting months and years
    date_breaks = "1 month"  # Adjust this based on your data density
  ) +
  theme_minimal() +
  theme(
    legend.position="none",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Improve readability by rotating labels
  )

# Save the plot
ggsave("graphs/weekly_adoptions.png", width = 16, height = 8, units = "cm")


# Analyze contracts
contract_analysis <- fread("data/input/Cosy_-_agreement_data_2024_07_24.csv") %>%
  inner_join(aggregated_data %>% distinct(account_id, hashed_mpan)) %>%
  filter(product_display_name == "Cosy Octopus") %>%
  arrange(account_id, hashed_mpan, agreement_valid_from) %>%
  mutate(
    from = as.Date(agreement_valid_from),
    to = as.Date(agreement_valid_to)
  ) %>%
  select(account_id, hashed_mpan, from, to) %>%
  group_by(account_id) %>%
  summarise(
    num_contracts = n(),  # Count number of contracts per customer
    ongoing = sum(is.na(to)),  # Count how many contracts are ongoing
    ended = sum(!is.na(to))  # Count how many contracts have ended
  ) %>%
  mutate(
    category = case_when(
      num_contracts == 1 & ongoing == 1 ~ "Stayed on Cosy (ongoing)",
      num_contracts == 1 & ended == 1 ~ "Tried then switched",
      num_contracts > 1 ~ "Multiple contracts",
      TRUE ~ "Other"  # Catch-all for any other cases
    )
  )

# Count each category
category_counts <- contract_analysis %>%
  count(category)

print(category_counts)


