# ============================================================
# Smart Meter Data Availability (Heat Pump Sample)
#
# Loads the CS electricity estimation sample and the weekly
# consumption panel, then uses panelView to visualize which
# households have electricity and gas smart-meter data before and
# after heat pump installation.
#
# Outputs: graphs/hp_data_availability.png,
#          graphs/hp_gas_data_availability.png
# ============================================================

# Load data for regression
overall_weekly <- 
  read_rds(file.path(datapath, "output/overall_weekly.rds")) %>%
  mutate_at(vars(elec_consumption, gas_consumption, total_consumption), 
            ~.x / 52.25) 


## Smart Meter Data Availability for Heat Pump Customers
plot_panel <- panelview(elec_consumption ~ is_hp_installed + hdd, 
                        data = overall_weekly, index = c("account_id","settlement_week"), 
                        xlab = "Time", 
                        ylab = "Household", 
                        main = "Electricity data",
                        by.timing = TRUE, 
                        pre.post = TRUE, 
                        gridOff = TRUE, 
                        axis.lab.gap = c(100),
                        background = "white",
                        color = c("grey", not_hp_color, hp_color, "white"),
                        legend.labs = c("Never Treated (Installation in Future)", 
                                        "Before HP Installation", "After HP Installation", 
                                        "No smart meter data"), collapse.history = "TRUE")

ggsave("graphs/hp_data_availability.png", 
       width = 16, height = 8, units = "cm")




plot_panel <- panelview(gas_consumption ~ is_hp_installed, 
                        main = "Gas data",
                        data = overall_weekly, index = c("account_id","settlement_week"), 
                        xlab = "Time", ylab = "Household", by.timing = TRUE, pre.post = TRUE, gridOff = TRUE,
                        axis.lab.gap = c(40),
                        background = "white",
                        color = c("grey", not_hp_color, hp_color, "white"),
                        legend.labs = c("Never Treated (Installation in Future)", 
                                        "Before HP Installation", "After HP Installation", 
                                        "No smart meter data"), collapse.history = "TRUE")
ggsave("graphs/hp_gas_data_availability.png", 
       width = 16, height = 8, units = "cm")
