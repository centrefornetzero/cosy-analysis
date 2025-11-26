hp_installed <- read_rds(file.path(datapath, "output/hp_installed.rds"))

## Figure A.1: Smart Meter Data Availability for Heat Pump Customers
plot_panel <- panelview(consumption_hh ~ is_hp_installed + hdd, 
                        data = hp_installed %>% filter(rate_period=="Overall") %>% select(consumption_hh, account_id, date, is_hp_installed, hdd) %>% distinct(), index = c("account_id","date"), 
                        xlab = "Time", 
                        ylab = "MPAN", 
                        by.timing = TRUE, 
                        pre.post = TRUE, 
                        gridOff = TRUE, 
                        axis.lab.gap = c(100),
                        main = "Smart Meter Data Availability",
                        background = "white",
                        color = c("grey", not_hp_color, hp_color, "white"),
                        legend.labs = c("Never Treated (Installation in Future)", 
                                        "Before HP Installation", "After HP Installation", 
                                        "No smart meter data"), collapse.history = "TRUE")

ggsave("graphs/hp_data_availability.png", 
       width = 16, height = 8, units = "cm")



# Delete?
cosy_hp_install_gas_consumption <- fread(file.path(datapath, "input/cosy_-_hp_users_gas_2024_06_13.csv")) %>%
  group_by(account_id) %>%
  mutate(is_hp_installed = as.numeric(installed_at <= settlement_week),
         treated = max(is_hp_installed),
         min_settlement_week = min(settlement_week)) %>%
  distinct(account_id, settlement_week, .keep_all = TRUE)


plot_panel <- panelview(weekly_consumption ~ is_hp_installed, 
                        data = cosy_hp_install_gas_consumption, index = c("account_id","settlement_week"), 
                        xlab = "Time", ylab = "MPAN", by.timing = TRUE, pre.post = TRUE, gridOff = TRUE,
                        axis.lab.gap = c(40),
                        main = "Gas Data Availability",
                        background = "white",
                        color = c("grey", not_hp_color, hp_color, "white"),
                        legend.labs = c("Never Treated (Installation in Future)", 
                                        "Before HP Installation", "After HP Installation", 
                                        "No smart meter data"), collapse.history = "TRUE")
ggsave("graphs/hp_gas_data_availability.png", 
       width = 16, height = 8, units = "cm")
