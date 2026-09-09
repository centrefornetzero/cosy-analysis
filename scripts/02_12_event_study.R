# ==============================================================================
# Event Study — Heat Pump Installation Effects (weeklyised kWh)
# ------------------------------------------------------------------------------
# What this script does:
#  1) Builds an event-study panel at account-day level
#  2) Constructs event time in weeks since installation (binned to [-52, 52])
#  3) Weeklyised half-hourly consumption to "kWh/week" for interpretability
#  4) Estimates TWFE event-study regressions with a common anticipation window
#  5) Produces consistent plots (anticipation shading + thousand separators)
# ==============================================================================

# ------------------------------------------------------------------------------
# Paths / inputs
# ------------------------------------------------------------------------------
cat("\n>>> Event study: loading hp_installed <<<\n")
hp_installed <- read_rds(file.path(datapath, "output/hp_installed.rds"))

# ------------------------------------------------------------------------------
# Global settings for event study + plotting
# ------------------------------------------------------------------------------
WEEK_BIN      <- 52     # bin event time to [-52, 52]
REF_WEEK      <- -4     # reference week for i(weeks_since_hp, ref=...)
ANTIC_XMIN    <- -4     # anticipation shading start (weeks)
ANTIC_XMAX    <-  0     # anticipation shading end (weeks)

anticipation_df <- data.frame(
  xmin = ANTIC_XMIN, xmax = ANTIC_XMAX,
  ymin = -Inf, ymax = Inf
)

# ------------------------------------------------------------------------------
# Build event-study dataset
# ------------------------------------------------------------------------------
event_study_df <- hp_installed %>%
  mutate(
    # Event time: integer weeks since installation
    weeks_since_hp = as.numeric(difftime(date, installed_at, units = "weeks")) %/% 1 + 1,

    # Bin event time to avoid sparse tails in the plot
    weeks_since_hp = case_when(
      weeks_since_hp < -WEEK_BIN ~ -WEEK_BIN,
      weeks_since_hp >  WEEK_BIN ~  WEEK_BIN,
      TRUE ~ weeks_since_hp
    ),

    # Weeklyise half-hourly kWh to kWh/week:
    # 48 half-hours/day * 7 days
    elec_consumption_weekly_kwh = 7 * 48 * consumption_hh
  ) %>%
  select(
    account_id, weeks_since_hp,
    consumption_hh, elec_consumption_weekly_kwh,
    hdd, date, rate_period
  )

gc()

# ------------------------------------------------------------------------------
# Helper: run model
# ------------------------------------------------------------------------------
run_event_study <- function(data, outcome, fe_rhs) {
  feols(
    as.formula(paste0(outcome, " ~ i(weeks_since_hp, ref = ", REF_WEEK, ") | ", fe_rhs)),
    data    = data,
    cluster = ~account_id
  )
}

# ------------------------------------------------------------------------------
# Helper: extract coefficients + plot with consistent styling
# ------------------------------------------------------------------------------
plot_event_study <- function(model, filename, ylab,
                            legend_pos = "bottom",
                            add_anticipation = TRUE,
                            comma_y = TRUE) {

  # Extract i() coefficients
  coefs <- coeftable(model) %>%
    data.frame() %>%
    tibble::rownames_to_column("term") %>%
    as_tibble() %>%
    separate(term, into = c("var", "weeks_since_hp"), sep = "::") %>%
    mutate(
      weeks_since_hp = as.numeric(weeks_since_hp),
      lower_ci = Estimate - 1.96 * `Std..Error`,
      upper_ci = Estimate + 1.96 * `Std..Error`,
      # Used only for colouring points
      post = (weeks_since_hp > -1)
    )

  p <- ggplot(coefs, aes(x = weeks_since_hp, y = Estimate, color = post)) +
    {if (add_anticipation)
      geom_rect(
        data = anticipation_df,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE,
        fill = "grey80",
        alpha = 0.25
      )
    } +
    geom_point(show.legend = TRUE) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    labs(
      x = "Weeks Since HP Installation",
      y = ylab,
      color = "Is HP Installed"
    ) +
    scale_color_manual(
      values = c("TRUE" = hp_color, "FALSE" = not_hp_color),
      labels = c("No", "Yes")
    ) +
    {if (comma_y) scale_y_continuous(labels = scales::label_comma()) } +
    {if (add_anticipation)
      annotate(
        "text",
        x = (ANTIC_XMIN + ANTIC_XMAX) / 2,
        y = Inf,
        label = "Anticipation\nwindow",
        vjust = 1.2,
        size = 3.5,
        colour = "grey30"
      )
    } +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = legend_pos
    )

  ggsave(filename, plot = p, width = 16, height = 8, units = "cm")
  invisible(p)
}

# ==============================================================================
# (1) Overall rate — baseline TWFE event study (weeklyised kWh)
# ==============================================================================
cat("\n>>> Event study (1): Overall rate <<<\n")
m_overall <- run_event_study(
  data    = event_study_df %>% filter(rate_period == "Overall"),
  outcome = "elec_consumption_weekly_kwh",
  fe_rhs  = "account_id + hdd + date"
)

etable(m_overall)

plot_event_study(
  model    = m_overall,
  filename = "graphs/hp_event_study_overall.png",
  ylab     = "Heat Pump Instal on Weekly\nElec Consumption (kWh)",
  legend_pos = "bottom",
  add_anticipation = TRUE,
  comma_y = TRUE
)

rm(m_overall); gc()

# ==============================================================================
# (2) Peak Rate — same spec, weeklyised kWh (for consistency across figures)
# ==============================================================================
cat("\n>>> Event study (2): Peak Rate <<<\n")
m_peak <- run_event_study(
  data    = event_study_df %>% filter(rate_period == "Peak Rate"),
  outcome = "elec_consumption_weekly_kwh",
  fe_rhs  = "account_id + hdd + date"
)

etable(m_peak)

plot_event_study(
  model    = m_peak,
  filename = "graphs/hp_event_study_peak_rate.png",
  ylab     = "Heat Pump Install on Weekly\n Peak Elec Consumption (kWh)",
  legend_pos = "bottom",
  add_anticipation = TRUE,
  comma_y = TRUE
)

cat("\n>>> Event study: done <<<\n")

rm(m_peak); gc()