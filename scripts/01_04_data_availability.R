# ============================================================
# Cosy Data Availability Plot
# Builds a panelview treatment-timing plot of household smart-meter
# data, showing each household's Cosy adoption status (before/after)
# and any gaps in smart-meter coverage over time.
#
# Outputs: graphs/data_availability.png
# ============================================================

plot_panel <- panelview(
  consumption_hh ~ cosy_contract_active + hdd, 
  data = aggregated_data %>%
    filter(rate_period == "Overall",
           !is.na(date),
           !is.na(hashed_mpan)) %>%
    mutate(cosy_contract_active = as.numeric(cosy_contract_active)) %>%
    select(consumption_hh, hashed_mpan, date, cosy_contract_active, hdd) %>%
    distinct(),
  index = c("hashed_mpan", "date"),
  xlab = "Time",
  ylab = "Household",
  by.timing = TRUE,
  pre.post = TRUE,
  gridOff = TRUE,
  axis.lab.gap = c(130),
  main = "",
  background = "white",
  color = c(flexible_color, cosy_color, "white"),
  legend.labs = c("Before Adoption", "After Adoption", "No smart meter data"),
  collapse.history = TRUE
)

plot_panel <- plot_panel +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

ggsave(
  "graphs/data_availability.png",
  plot = plot_panel,
  width = 16, height = 8, units = "cm"
)