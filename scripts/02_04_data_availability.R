## Availability of smart meter data

### Figure A.19: Smart Meter Data Availability for Cosy Adopters

# if (!file.exists("graphs/data_availability.png")) {
  plot_panel <- panelview(consumption_hh ~ cosy_contract_active + hdd, 
                          data = aggregated_data %>% filter(rate_period=="Overall", 
                                                            !is.na(date), 
                                                            !is.na(hashed_mpan)) %>% 
                            mutate(cosy_contract_active = as.numeric(cosy_contract_active)) %>%
                            select(consumption_hh, hashed_mpan, date, cosy_contract_active, hdd) %>% distinct(), index = c("hashed_mpan","date"), 
                          xlab = "Time", ylab = "hashed_mpan", by.timing = TRUE, 
                          pre.post = TRUE, gridOff = TRUE, axis.lab.gap = c(100),
                          main = "Smart Meter Data Availability",
                          background = "white",
                          color = c(flexible_color, cosy_color, "white"),
                          legend.labs = c("Before Adoption", "After Adoption", "No smart meter data"), 
                          collapse.history = "TRUE")
  ggsave("graphs/data_availability.png", 
         width = 16, height = 8, units = "cm")
# }