
# Load the CS estimates for the Heat Pump Adoption (Elec, Gas)
est_cs_elec_weekly <- readRDS(file.path(datapath, "scratch/est_cs_elec_weekly.RDS"))
est_cs_gas_weekly  <- readRDS(file.path(datapath, "scratch/est_cs_gas_weekly.RDS"))

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
      .groups = "drop"
    )
}

# 2) Wrapper for CS objects, split by pre/post period -------------------------
cs_summary_prepost <- function(obj, y,
                               id = "id",
                               label = NA_character_,
                               scale = 1,
                               week_var = "week",
                               firstweek_var = "firstweek") {
  
  df <- obj$DIDparams$data 
  names(df) <- make.unique(names(df))
  anticipation <- obj$DIDparams$anticipation
  
  # Define pre/post periods, excluding weeks within the anticipation window from "pre"
  df_pre  <- df %>% filter(.data[[week_var]]  +anticipation < .data[[firstweek_var]])
  df_post <- df %>% filter(.data[[week_var]]  >= .data[[firstweek_var]]) 
  
  bind_rows(
    df_pre  %>% summarise_outcome({{ y }}, id = id, scale = scale) %>%
      mutate(Sample = label, Period = "Pre", .before = 1),
    
    df_post %>% summarise_outcome({{ y }}, id = id, scale = scale) %>%
      mutate(Sample = label, Period = "Post", .before = 1)
  )
}

# 3) Apply to all estimates ---------------------------------------------------

# Row labels are deliberately self-explanatory: the panel header (group_rows)
# already says whether the row is electricity/gas use (Panel A) or which tariff
# band it refers to (Panel B), so we do NOT repeat "Heat Pump" / "Tariff" in
# each row. Tariff bands carry their clock times so the reader knows what
# "morning", "peak" and "other" mean. All quantities are annual kWh.
summaries <- bind_rows(
  cs_summary_prepost(est_cs_elec_weekly,  elec_consumption, id = "id",
                     label = "Electricity consumption", scale = 1),
  cs_summary_prepost(est_cs_gas_weekly,   gas_consumption,  id = "id",
                     label = "Gas consumption",  scale = 1),

  cs_summary_prepost(est_cs_overall,   consumption_hh, id = "id",
                     label = "Overall (all 24 hours)", scale = 48*7*52.25),
  cs_summary_prepost(est_cs_morning,   consumption_hh, id = "id",
                     label = "Morning off-peak (04:00–07:00)", scale = 6*7*52.25),
  cs_summary_prepost(est_cs_afternoon, consumption_hh, id = "id",
                     label = "Afternoon off-peak (13:00–16:00)", scale = 6*7*52.25),
  cs_summary_prepost(est_cs_peak,      consumption_hh, id = "id",
                     label = "Peak (16:00–19:00)", scale = 6*7*52.25),
  cs_summary_prepost(est_cs_other,     consumption_hh, id = "id",
                     label = "Standard rate, all other hours", scale = 30*7*52.25)
)

# 4) Create LaTeX table -------------------------------------------------------

sample_levels <- c(
  "Electricity consumption",
  "Gas consumption",
  "Overall (all 24 hours)",
  "Morning off-peak (04:00–07:00)",
  "Afternoon off-peak (13:00–16:00)",
  "Peak (16:00–19:00)",
  "Standard rate, all other hours"
)

# Samples that belong to the heat pump (Panel A) vs tariff (Panel B) sample
panel_A_samples <- c("Electricity consumption", "Gas consumption")

tab <- summaries %>%
  mutate(
    Sample = factor(Sample, levels = sample_levels),
    Panel = if_else(as.character(Sample) %in% panel_A_samples,
                    "Panel A: Heat Pump Adoption Sample",
                    "Panel B: Tariff Adoption Sample"),
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

# Layout: the series name lives in the first column (the "sub-header"), shown
# once per series on the "Pre" row and blank on "Post", so the wording is not
# repeated. Period stays in its own column. Only the Panel A/B rows are bold
# (group_rows); the series names are plain text. booktabs rules only, and
# linesep puts a blank line after each "Post" so the series are separated.
display <- tab %>%
  mutate(
    Mean   = scales::comma(Mean, accuracy = 0.01),
    SD     = scales::comma(SD,   accuracy = 0.01),
    Series = if_else(Period == "Pre", as.character(Sample), "")
  ) %>%
  select(Series, Period, Obs, Mean, SD)

latex_table <- display %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    align = "llrrr",
    col.names = c("", "Period", "Obs", "Mean", "SD"),
    caption = "Summary statistics before and after adoption",
    label = "summary_prepost",
    escape = FALSE,
    linesep = c("", "\\addlinespace")   # blank line after each Post row
  ) %>%
  kable_styling(latex_options = "hold_position")

# Panel headers (only these are bold), no indent of children, no \hline
for (i in seq_len(nrow(panel_ranges))) {
  latex_table <- kableExtra::group_rows(
    latex_table,
    group_label = panel_ranges$Panel[i],
    start_row   = panel_ranges$start[i],
    end_row     = panel_ranges$end[i],
    bold = TRUE,
    italic = FALSE,
    indent = FALSE,
    hline_before = FALSE,
    hline_after  = FALSE
  )
}

latex_table
save_kable(latex_table, file = "tables/summary_prepost.tex")

# ---- Add explanatory note inside the float so it always follows the table ----
# Defines Pre/Post, the annualisation, why Panel B is electricity-only, and the
# tariff bands (with clock times) so "morning"/"other" are unambiguous.
prepost_note <- paste0(
  "\\floatfoot{\\justifying \\footnotesize \\upshape \\textbf{Note:} ",
  "This table reports summary statistics for the two estimation samples. ",
  "All figures are annual consumption in kWh, obtained by annualizing the weekly ",
  "consumption (panel A) and half-hourly (panel B) used in the difference-in-differences models. ",
  "``Pre'' is the period before adoption and ``Post'' the period after; weeks within ",
  "the anticipation window are excluded. \\emph{Obs} is the number of household--period ",
  "observations, and Mean and SD are computed across those observations. ",
  "\\emph{Panel A} is the heat pump adoption sample and reports household electricity and gas use. ",
  "\\emph{Panel B} is the heat-pump time-of-use tariff adoption sample: two cheap off-peak windows ",
  "in the morning (04:00--07:00) and the afternoon (13:00--16:00), a peak window (16:00--19:00) ",
  "priced above the standard rate, and the standard rate that applies in all other hours ",
  "(07:00--13:00 and 19:00--04:00); ``Overall'' covers all 24 hours.}"
)

prepost_lines <- readLines("tables/summary_prepost.tex")
caption_idx   <- grep("\\\\caption", prepost_lines)[1]
prepost_lines <- append(prepost_lines, prepost_note, after = caption_idx)
writeLines(prepost_lines, "tables/summary_prepost.tex")
