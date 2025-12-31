
# Load the CS estimates for the Heat Pump Adoption (Elec, Gas, Total)
est_cs_elec_weekly <- readRDS(file.path(datapath, "scratch/est_cs_elec_weekly.RDS"))
est_cs_gas_weekly  <- readRDS(file.path(datapath, "scratch/est_cs_gas_weekly.RDS"))
est_cs_total_weekly <- readRDS(file.path(datapath, "scratch/est_cs_total_weekly.RDS"))

# Load the CS estimates for Cosy Tariff Adoption (Elec for different rate periods)
est_cs_overall <- readRDS(file.path(datapath, "scratch/did_cosy_Overall.RDS"))
est_cs_morning <- readRDS(file.path(datapath, "scratch/did_cosy_Morning Cosy.RDS"))
est_cs_afternoon <- readRDS(file.path(datapath, "scratch/did_cosy_Afternoon Cosy.RDS"))
est_cs_peak <- readRDS(file.path(datapath, "scratch/did_cosy_Peak Rate.RDS"))
est_cs_other <- readRDS(file.path(datapath, "scratch/did_cosy_Other.RDS"))

# 1) Generic summary-stat helper ----------------------------------------------

summarise_outcome <- function(df, y, id = "id", scale = 1) {
  y  <- rlang::ensym(y)
  id <- rlang::ensym(id)
  
  df %>%
    mutate(y_scaled = !!y * scale) %>%
    summarise(
      Obs    = scales::comma(sum(!is.na(y_scaled))),
      Mean = round(mean(y_scaled, na.rm = TRUE), 2),
      SD   = round(sd(y_scaled, na.rm = TRUE), 2),
      Min  = round(min(y_scaled, na.rm = TRUE), 2),
      Max  = round(max(y_scaled, na.rm = TRUE), 2),
      .groups = "drop"
    )
}

# 2) Wrapper for CS objects, now split pre/post -------------------------------

cs_summary_prepost <- function(obj, y,
                               id = "id",
                               label = NA_character_,
                               scale = 1,
                               week_var = "week",
                               firstweek_var = "firstweek") {
  
  df <- obj$DIDparams$data
  
  # define pre/post using your exact rule
  df_pre  <- df %>% filter(.data[[week_var]] +1 < .data[[firstweek_var]])
  df_post <- df %>% filter(.data[[week_var]]  >= .data[[firstweek_var]]) 
  
  bind_rows(
    df_pre  %>% summarise_outcome({{ y }}, id = id, scale = scale) %>%
      mutate(Sample = label, Period = "Pre", .before = 1),
    
    df_post %>% summarise_outcome({{ y }}, id = id, scale = scale) %>%
      mutate(Sample = label, Period = "Post", .before = 1)
  )
}

# 3) Apply to all your estimates ---------------------------------------------

summaries <- bind_rows(
  cs_summary_prepost(est_cs_elec_weekly,  elec_consumption, id = "id",
                     label = "Heat Pump: Elec (kWh)", scale = 1),
  cs_summary_prepost(est_cs_gas_weekly,   gas_consumption,  id = "id",
                     label = "Heat Pump: Gas (kWh)",  scale = 1),
  cs_summary_prepost(est_cs_total_weekly, total_consumption, id = "id",
                     label = "Heat Pump: Total (kWh)", scale = 1),
  
  cs_summary_prepost(est_cs_overall,   consumption_hh, id = "id",
                     label = "Tariff: Elec Overall (kWh)", scale = 48*7*52.25),
  cs_summary_prepost(est_cs_morning,   consumption_hh, id = "id",
                     label = "Tariff: Elec Morning Off-peak (kWh)", scale = 6*7*52.25),
  cs_summary_prepost(est_cs_afternoon, consumption_hh, id = "id",
                     label = "Tariff: Elec Afternoon Off-peak (kWh)", scale = 6*7*52.25),
  cs_summary_prepost(est_cs_peak,      consumption_hh, id = "id",
                     label = "Tariff: Elec Peak Rate (kWh)", scale = 6*7*52.25),
  cs_summary_prepost(est_cs_other,     consumption_hh, id = "id",
                     label = "Tariff: Elec Other (kWh)", scale = 30*7*52.25)
)

# 4) Create LaTeX table -------------------------------------------------------

sample_levels <- c(
  "Heat Pump: Elec (kWh)",
  "Heat Pump: Gas (kWh)",
  "Heat Pump: Total (kWh)",
  "Tariff: Elec Overall (kWh)",
  "Tariff: Elec Morning Off-peak (kWh)",
  "Tariff: Elec Afternoon Off-peak (kWh)",
  "Tariff: Elec Peak Rate (kWh)",
  "Tariff: Elec Other (kWh)"
)

tab <- summaries %>%
  mutate(
    Sample = factor(Sample, levels = sample_levels),
    Panel = if_else(grepl("^Heat Pump", as.character(Sample)),
                    "Panel A: Heat Pump Adoption Analysis",
                    "Panel B: Tariff Adoption Analysis"),
    Period = factor(Period, levels = c("Pre", "Post"))
  ) %>%
  arrange(Panel, Sample, Period)

# Row ranges for each panel in the PRINTED TABLE (before any grouping rows are inserted)
panel_ranges <- tab %>%
  count(Panel) %>%
  mutate(
    start = lag(cumsum(n), default = 0) + 1,
    end   = cumsum(n)
  )

# Index for sample groups (pack_rows)
sample_index <- tab %>%
  count(Sample) %>%
  { setNames(.$n, .$Sample) }

latex_table <- tab %>%
  select(Panel, Sample, Period, Obs, Mean, SD, Min, Max) %>%
  mutate(
    Mean = scales::comma(Mean, accuracy = 0.01),
    SD   = scales::comma(SD,   accuracy = 0.01),
    Min  = scales::comma(Min,  accuracy = 0.01),
    Max  = scales::comma(Max,  accuracy = 0.01)
  ) %>%
  select(-Panel) %>%  # keep Sample for pack_rows
  kbl(
    format = "latex",
    booktabs = TRUE,
    align = "llrrrrr",
    caption = "Summary statistics before and after adoption",
    label = "summary_prepost",
    escape = TRUE
  ) %>%
  kable_styling(latex_options = c("hold_position", "double_rule")) %>%
  add_header_above(c(" " = 2, "Outcome" = 5))

# Add PANEL headers (explicit namespace to avoid masking)
for (i in seq_len(nrow(panel_ranges))) {
  latex_table <- kableExtra::group_rows(
    latex_table,
    group_label = panel_ranges$Panel[i],
    start_row   = panel_ranges$start[i],
    end_row     = panel_ranges$end[i],
    bold = TRUE,
    italic = TRUE,
    hline_before = TRUE,
    hline_after  = FALSE
  )
}

# Now add sample grouping (this inserts additional label rows, but we're done with panel indices)
latex_table <- latex_table %>%
  pack_rows(index = sample_index, bold = TRUE)

latex_table
save_kable(latex_table, file = "tables/summary_prepost.tex")
