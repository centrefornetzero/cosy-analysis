# ============================================================
# HP installation: TWFE + Callaway–Sant’Anna (CS) tables + plots
# Cleaned-up, single script.
#
# Key fixes:
#  - One definition per helper function (no duplicates).
#  - Robust CS pre-treatment averages (and no dplyr-on-duplicate-names issues).
#  - Robust LaTeX post-processing: correct column counts for 4-model tables
#    (label | TWFE Elec | TWFE Gas | CS Elec | CS Gas) = 5 cols total.
#  - Safer grep patterns (avoid matching the wrong line).
#  - Checkpoints printed throughout.
# ============================================================

# Define your colors (assumes hp_color/not_hp_color exist in your environment)
elec_color <- hp_color
gas_color  <- not_hp_color


# ============================================================
# 0) Small utilities
# ============================================================

checkpoint <- function(msg) cat(paste0(">>> ", msg, " <<<\n"))

format_decimal <- function(x, digits = 1) formatC(x, format = "f", digits = digits, big.mark = ",")
format_number  <- function(x) formatC(x, format = "d", big.mark = ",")

confidence_star <- function(coef, se, alpha) {
  ci_lower <- coef - qnorm(1 - alpha / 2) * se
  ci_upper <- coef + qnorm(1 - alpha / 2) * se
  if (ci_lower > 0 | ci_upper < 0) "***" else ""
}

# Make DIDparams$data dplyr-safe (removes duplicate column names like duplicate "id")
clean_didparams_data <- function(x) {
  # x is often a data.table; remove duplicated names using data.table semantics if available
  nms <- names(x)
  if (anyDuplicated(nms)) {
    # data.table subset syntax works if x is data.table; if not, fallback to base
    if ("data.table" %in% class(x)) {
      x <- x[, nms[!duplicated(nms)], with = FALSE]
    } else {
      x <- x[, !duplicated(nms), drop = FALSE]
    }
  }
  as.data.frame(x)
}

# Compute pre-treatment average safely from an aggte_simple object
# outcome_col must exist in DIDparams$data (e.g., "elec_consumption" / "gas_consumption" / "consumption_hh")
pre_avg_from_aggte <- function(aggte_simple, outcome_col) {
  dat <- clean_didparams_data(aggte_simple$DIDparams$data)
  stopifnot(outcome_col %in% names(dat))
  dat %>%
    filter(week < firstweek - 1) %>%
    summarise(pre_avg = mean(.data[[outcome_col]], na.rm = TRUE)) %>%
    pull(pre_avg)
}

# ============================================================
# 1) LaTeX table creator for CS-only (2-column: Electricity / Gas)
# ============================================================
create_latex_table_cs <- function(models, headers, title, file, label,
                                  pre_treatment_values,
                                  note = "",
                                  digits = 1,
                                  variable_label = "Is HP Installed $=$ 1",
                                  pretreat_row_label = "Yearly Consumption",
                                  estimation_method = "Doubly Robust",
                                  control_group = "Not Yet Treated",
                                  anticipation = 1) {

  stopifnot(length(headers) == length(models))
  stopifnot(length(pre_treatment_values) == length(models))

  # --- Coefs + stars ---
  coefficients <- sapply(models, function(m) {
    coef  <- m$overall.att
    se    <- m$overall.se
    alpha <- m$DIDparams$alp
    paste0(format_decimal(coef, digits), confidence_star(coef, se, alpha))
  })

  # --- SEs ---
  standard_errors <- sapply(models, function(m) {
    paste0("(", format_decimal(m$overall.se, digits), ")")
  })

  # --- Fit stats (use the fields that actually exist in your objects) ---
    get_did_stat <- function(m, stat) {
    dp <- m$DIDparams

    # new names
    if (stat == "n_households") {
    if (!is.null(dp$id_count)) return(dp$id_count)
    if (!is.null(dp$n))       return(dp$n)      # old
    }

    if (stat == "nG") {
    if (!is.null(dp$treated_groups_count)) return(dp$treated_groups_count)
    if (!is.null(dp$nG))                   return(dp$nG)  # old
    }

    if (stat == "nT") {
    if (!is.null(dp$time_periods_count)) return(dp$time_periods_count)
    if (!is.null(dp$nT))                 return(dp$nT)   # old
    }

    NA
    }
    n_households <- sapply(models, function(m) format_number(get_did_stat(m, "n_households")))
    nG           <- sapply(models, function(m) format_number(get_did_stat(m, "nG")))
    nT           <- sapply(models, function(m) format_number(get_did_stat(m, "nT")))

  # if anything came back empty, fail loudly instead of writing character(0)
  if (any(nchar(n_households) == 0) || any(nchar(nG) == 0) || any(nchar(nT) == 0)) {
    stop("Some fit statistics are empty. Check DIDparams fields in your aggte objects.")
  }

  alpha <- models[[1]]$DIDparams$alp
  conf_level <- (1 - alpha) * 100

  pretreat_fmt <- sapply(pre_treatment_values, function(x) format_decimal(x, digits))

  k <- length(headers)

  # --- Build LaTeX with SINGLE backslashes (do NOT double-escape here) ---
  latex <- "\\begin{table}[htbp]\n"
  latex <- paste0(latex, "   \\caption{\\label{", label, "} ", title, "}\n")

  if (note != "") {
    latex <- paste0(
      latex,
      "   \\floatfoot{\\justifying \\footnotesize \\upshape \\textbf{Note:} ",
      note,
      "}\n"
    )
  }

  latex <- paste0(latex, "   \\centering\n")
  latex <- paste0(latex, "   \\begin{tabular}{l", paste(rep("c", k), collapse = ""), "}\n")
  latex <- paste0(latex, "      \\tabularnewline \\midrule \\midrule\n")
  latex <- paste0(latex, "                                     & ", paste(headers, collapse = " & "), " \\\\\n")
  latex <- paste0(latex, "      Model:                         & ",
                  paste(paste0("(", seq_len(k), ")"), collapse = " & "), " \\\\\n")
  latex <- paste0(latex, "      \\midrule\n")
  latex <- paste0(latex, "      \\emph{Variable}\\\\\n")
  latex <- paste0(latex, "      ", variable_label, " & ", paste(coefficients, collapse = " & "), " \\\\\n")
  latex <- paste0(latex, "                                     & ", paste(standard_errors, collapse = " & "), " \\\\\n")
  latex <- paste0(latex, "      \\midrule\n")
  latex <- paste0(latex, "      \\emph{Pre-treatment Average}\\\\\n")
  latex <- paste0(latex, "      ", pretreat_row_label, " & ", paste(pretreat_fmt, collapse = " & "), " \\\\\n")
  latex <- paste0(latex, "      \\midrule\n")
  latex <- paste0(latex, "      \\emph{Fit statistics}\\\\\n")
  latex <- paste0(latex, "      Number of Households & ", paste(n_households, collapse = " & "), " \\\\\n")
  latex <- paste0(latex, "      Number of Cohorts & ", paste(nG, collapse = " & "), " \\\\\n")
  latex <- paste0(latex, "      Number of Time Periods & ", paste(nT, collapse = " & "), " \\\\\n")
  latex <- paste0(latex, "      \\midrule \\midrule\n")
  latex <- paste0(latex, "      \\multicolumn{", k + 1, "}{l}{Clustered (Household) standard-errors in parentheses}\\\\\n")
  latex <- paste0(latex, "      \\multicolumn{", k + 1, "}{l}{Estimation Method: ", estimation_method, "}\\\\\n")
  latex <- paste0(latex, "      \\multicolumn{", k + 1, "}{l}{Control Group: ", control_group,
                  ", Anticipation Periods: ", anticipation, "}\\\\\n")
  latex <- paste0(latex, "      \\multicolumn{", k + 1, "}{l}{Signif. Codes: *** ",
                  conf_level, "\\% confidence band does not cover 0}\\\\\n")
  latex <- paste0(latex, "   \\end{tabular}\n")
  latex <- paste0(latex, "\\end{table}\n")

  writeLines(latex, file)
}
# ============================================================
# 2) LaTeX patcher for TWFE+CS combined table produced by fixest::etable
#    Works for EXACT structure: m1,m2,m1,m2  => 4 models => 5 columns total
# ============================================================

patch_etable_twfe_cs <- function(file_path,
                                cs_estimates, cs_se, cs_n, cs_nG, cs_nT,
                                coef_pattern = "Is HP Installed \\$=\\$ 1") {

  checkpoint(paste0("Patching LaTeX table: ", file_path))
  file_content <- readLines(file_path)

  # overwrite CS columns ONLY: parts[4]=CS Elec, parts[5]=CS Gas
  replace_cs_cols <- function(line, new_values) {
    parts <- strsplit(line, "&")[[1]]
    if (length(parts) < 5) return(line)
    stopifnot(length(new_values) == 2)
    parts[4] <- str_trim(new_values[[1]])
    parts[5] <- str_trim(new_values[[2]])
    paste(parts, collapse = " & ")
  }
  clear_cs_cols <- function(line) {
    parts <- strsplit(line, "&")[[1]]
    if (length(parts) < 5) return(line)
    parts[4] <- ""
    parts[5] <- ""
    paste(parts, collapse = " & ")
  }

  # Construct coefficient/se strings (assumes stars always *** here; adapt if needed)
  new_estimates <- c(paste0(cs_estimates[["Electricity"]], "$^{***}$"),
                     paste0(cs_estimates[["Gas"]], "$^{***}$"))
  new_se <- c(paste0("(", cs_se[["Electricity"]], ")"),
              paste0("(", cs_se[["Gas"]], ")"))

  # Safer row targeting
  coeff_line   <- grep(coef_pattern, file_content)
  se_line      <- coeff_line + 1

  obs_line     <- grep("^\\s*Observations\\s*&", file_content)
  sample_line  <- grep("^\\s*Number of Households\\s*&|Size of the 'effective' sample", file_content)
  periods_line <- grep("^\\s*Number of Time Periods\\s*&", file_content)

  hdd_line     <- grep("^\\s*HDD\\s*&", file_content)
  hh_line      <- grep("^\\s*Household\\s*&", file_content)
  week_line    <- grep("^\\s*Week\\s*&", file_content)
  r2_line      <- grep("^\\s*R\\$\\^2\\$\\s*&", file_content)

  # Replace CS columns in key rows
  if (length(sample_line) > 0) {
    file_content[sample_line] <- gsub("Size of the 'effective' sample", "Number of Households", file_content[sample_line])
    file_content[sample_line] <- paste0(
      replace_cs_cols(file_content[sample_line], c(cs_n[["Electricity"]], cs_n[["Gas"]])),
      " \\\\"
    )
  }

  if (length(coeff_line) > 0) {
    file_content[coeff_line] <- paste0(replace_cs_cols(file_content[coeff_line], new_estimates), " \\\\")
    file_content[se_line]    <- paste0(replace_cs_cols(file_content[se_line],    new_se),       " \\\\")
  }

  if (length(periods_line) > 0) {
    file_content[periods_line] <- paste0(
      replace_cs_cols(file_content[periods_line], c(cs_nT[["Electricity"]], cs_nT[["Gas"]])),
      " \\\\"
    )
  }

  # Clear CS columns where TWFE-only info is shown
  if (length(obs_line)  > 0) file_content[obs_line]  <- paste0(clear_cs_cols(file_content[obs_line]),  " \\\\")
  if (length(hdd_line)  > 0) file_content[hdd_line]  <- paste0(clear_cs_cols(file_content[hdd_line]),  " \\\\")
  if (length(hh_line)   > 0) file_content[hh_line]   <- paste0(clear_cs_cols(file_content[hh_line]),   " \\\\")
  if (length(week_line) > 0) file_content[week_line] <- paste0(clear_cs_cols(file_content[week_line]), " \\\\")
  if (length(r2_line)   > 0) file_content[r2_line]   <- paste0(clear_cs_cols(file_content[r2_line]),   " \\\\")

  # Clustering line must match 5 columns
  clustering_line_index <- grep("Clustered \\(Household\\)", file_content)
  if (length(clustering_line_index) > 0) {
    file_content[clustering_line_index] <- "\\multicolumn{5}{l}{\\emph{Clustered (Household) standard-errors in parentheses}}\\\\"
  }

  # Add "Number of cohorts (CS)" row (5-column compliant)
  if (length(sample_line) > 0) {
    new_row <- paste0("Number of cohorts (CS) &  &  & ",
                      cs_nG[["Electricity"]], " & ", cs_nG[["Gas"]], " \\\\")
    file_content <- append(file_content, new_row, after = sample_line[1])
  }

  writeLines(file_content, file_path)
  checkpoint("LaTeX patch applied")
}

# ============================================================
# 3) Plot helpers
# ============================================================

create_dynamic_plot <- function(elec_data, gas_data, elec_color, gas_color) {
  plot_data <- data.frame(
    event_time = c(elec_data$egt, gas_data$egt),
    coefficient = c(elec_data$att.egt / 52.25, gas_data$att.egt / 52.25),
    se = c(elec_data$se.egt / 52.25, gas_data$se.egt / 52.25),
    type = rep(c("Electricity", "Gas"), each = length(elec_data$egt))
  ) %>%
    mutate(
      lower_ci = coefficient - 1.96 * se,
      upper_ci = coefficient + 1.96 * se
    )

  ggplot(plot_data, aes(x = event_time, y = coefficient, group = type)) +
    geom_line(aes(color = type)) +
    geom_point(aes(color = type, shape = type), size = 3) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = type), width = 0.2, alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_colour_manual(
      name = "Type",
      labels = c("Electricity", "Gas"),
      values = c(Electricity = elec_color, Gas = gas_color)
    ) +
    scale_shape_manual(
      name = "Type",
      labels = c("Electricity", "Gas"),
      values = c(16, 17)
    ) +
    labs(
      x = "Weeks since installation",
      y = "Dynamic ATT for Weekly Consumption (kWh)"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

create_calendar_plot_data <- function(start_date, elec_data, gas_data) {
  elec <- data.frame(
    week_date = start_date + weeks(elec_data$egt),
    estimate  = elec_data$att.egt / 52.25,
    se        = elec_data$se.egt / 52.25,
    type      = "Electricity"
  )
  gas <- data.frame(
    week_date = start_date + weeks(gas_data$egt),
    estimate  = gas_data$att.egt / 52.25,
    se        = gas_data$se.egt / 52.25,
    type      = "Gas"
  )
  bind_rows(elec, gas) %>%
    mutate(
      lower_ci = estimate - 1.96 * se,
      upper_ci = estimate + 1.96 * se
    )
}

# ============================================================
# 4) Load data + set colors
# ============================================================

checkpoint("Load data + setup")

overall_weekly <- read_rds(file.path(datapath, "output/overall_weekly.rds"))

# Build DID data index (used for filtering)
start_date <- min(overall_weekly$settlement_week)

did_data <- overall_weekly %>%
  ungroup() %>%
  mutate(
    week      = as.numeric(difftime(settlement_week, start_date, units = "weeks")) %/% 1 + 1,
    firstweek = as.numeric(difftime(installed_at, start_date, units = "weeks")) %/% 1 + 1
  ) %>%
  group_by(account_id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  filter(week <= 129, firstweek <= 129)

checkpoint("DID index built")

# ============================================================
# 5) CS simple tables (full vs gas-only) — build once, reuse helpers
# ============================================================

checkpoint("CS simple: load RDS + build CS-only table")

# ---- CS files (FULL sample) ----
cs_files_full <- list(
  Electricity = file.path(datapath, "scratch/est_cs_elec_weekly.RDS"),
  Gas         = file.path(datapath, "scratch/est_cs_gas_weekly.RDS")
)

aggte_simple_elec <- aggte(readRDS(cs_files_full$Electricity), type = "simple",
                           na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.01)
aggte_simple_gas  <- aggte(readRDS(cs_files_full$Gas), type = "simple",
                           na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.01)

# Pre-treatment means (use DIDparams$data safely)
pre_elec <- pre_avg_from_aggte(aggte_simple_elec, "elec_consumption")
pre_gas  <- pre_avg_from_aggte(aggte_simple_gas,  "gas_consumption")

models_cs_full  <- list(Electricity = aggte_simple_elec, Gas = aggte_simple_gas)
headers_cs      <- c("Electricity", "Gas")
title_cs_full   <- "Heat Pump Installation Effects on Yearly Energy Consumption (kWh)"
file_cs_full    <- "tables/hp_did_overall_cs.tex"
label_cs_full   <- "tab:hp-did-cs"

note_cs_full <- paste(
  "This table reports CS estimates",
  "of the impact of heat pump installation on households\u2019 yearly electricity consumption (column 1)",
  "and gas consumption (column 2). Cohorts refer to households with the same week of installation. ",
  sep = " "
)

create_latex_table_cs(
  models = models_cs_full,
  headers = headers_cs,
  title = title_cs_full,
  file = file_cs_full,
  label = label_cs_full,
  pre_treatment_values = c(pre_elec, pre_gas),
  note = note_cs_full,
  digits = 1,
  variable_label = "Is HP Installed $=$ 1",
  pretreat_row_label = "Yearly Consumption"
)

checkpoint("Saved tables/hp_did_overall_cs.tex")

# ---- CS files (GAS-ONLY electricity + gas) ----
checkpoint("CS simple: gas-only subsample table")

cs_files_gas_only <- list(
  Electricity = file.path(datapath, "scratch/est_cs_elec_weekly_gas_only.RDS"),
  Gas         = file.path(datapath, "scratch/est_cs_gas_weekly.RDS")
)

aggte_simple_elec_gasonly <- aggte(readRDS(cs_files_gas_only$Electricity), type = "simple",
                                   na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.01)
aggte_simple_gas_gasonly  <- aggte(readRDS(cs_files_gas_only$Gas), type = "simple",
                                   na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.01)

pre_elec_gasonly <- pre_avg_from_aggte(aggte_simple_elec_gasonly, "elec_consumption")
pre_gas_gasonly  <- pre_avg_from_aggte(aggte_simple_gas_gasonly,  "gas_consumption")

models_cs_gasonly <- list(Electricity = aggte_simple_elec_gasonly, Gas = aggte_simple_gas_gasonly)
title_cs_gasonly  <- "Heat Pump Installation Effects on Yearly Energy Consumption (Gas-Metered Households)"
file_cs_gasonly   <- "tables/hp_did_overall_cs_gas_only.tex"
label_cs_gasonly  <- "tab:hp-did-cs-gas-only"

note_cs_gasonly <- paste(
  "This table reports CS estimates of the impact of heat pump installation on yearly electricity (column 1)",
  "and gas consumption (column 2). Both models are estimated on the subsample of households with observed",
  "gas consumption prior to installation, and therefore use a smaller sample than the full electricity-only analysis.",
  sep = " "
)

create_latex_table_cs(
  models = models_cs_gasonly,
  headers = headers_cs,
  title = title_cs_gasonly,
  file = file_cs_gasonly,
  label = label_cs_gasonly,
  pre_treatment_values = c(pre_elec_gasonly, pre_gas_gasonly),
  note = note_cs_gasonly,
  digits = 1,
  variable_label = "Is HP Installed $=$ 1",
  pretreat_row_label = "Yearly Consumption"
)

checkpoint("Saved tables/hp_did_overall_cs_gas_only.tex")

# ============================================================
# 6) Dynamic plot (CS)
# ============================================================

checkpoint("Dynamic CS plot (electricity + gas)")

elec_dyn <- aggte(readRDS(cs_files_full$Electricity), type = "dynamic",
                  na.rm = TRUE, clustervars = "id", bstrap = TRUE, min_e = -90, max_e = 90)
gas_dyn  <- aggte(readRDS(cs_files_full$Gas), type = "dynamic",
                  na.rm = TRUE, clustervars = "id", bstrap = TRUE, min_e = -90, max_e = 90)

p_dyn <- create_dynamic_plot(elec_dyn, gas_dyn, elec_color, gas_color)
ggsave("graphs/dynamic_hp_plot_combined.png", plot = p_dyn, width = 10, height = 8, dpi = 300)
checkpoint("Saved graphs/dynamic_hp_plot_combined.png")

# ============================================================
# 7) Calendar plot (CS)
# ============================================================

checkpoint("Calendar CS plot (electricity + gas)")

elec_cal <- aggte(readRDS(cs_files_full$Electricity), type = "calendar",
                  na.rm = TRUE, clustervars = "id", bstrap = TRUE)
gas_cal  <- aggte(readRDS(cs_files_full$Gas), type = "calendar",
                  na.rm = TRUE, clustervars = "id", bstrap = TRUE)

plot_cal_data <- create_calendar_plot_data(start_date, elec_cal, gas_cal)

p_cal <- ggplot(plot_cal_data, aes(x = as.Date(week_date), y = estimate, color = type)) +
  geom_point(aes(shape = type), size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_date(labels = scales::date_format("%b %y"), date_breaks = "3 month") +
  scale_colour_manual(
    name = "Type",
    labels = c("Electricity", "Gas"),
    values = c(Electricity = elec_color, Gas = gas_color)
  ) +
  scale_shape_manual(
    name = "Type",
    labels = c("Electricity", "Gas"),
    values = c(16, 17)
  ) +
  labs(
    x = "Week",
    y = "Calendar ATT for Weekly Consumption (kWh)",
    color = "Type",
    shape = "Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

write.csv(plot_cal_data, file.path(datapath, "output/hp_calendarplot_combined.csv"), row.names = FALSE)
ggsave("graphs/hp_calendarplot_combined.png", plot = p_cal, width = 8, height = 6, dpi = 300)
checkpoint("Saved graphs/hp_calendarplot_combined.png and output/hp_calendarplot_combined.csv")

# ============================================================
# 8) TWFE models + combined TWFE/CS LaTeX table (patched)
# ============================================================

checkpoint("TWFE models + TWFE/CS combined LaTeX table")

# Fitstat helpers (register once)
fitstat_register("pre_avg", function(x) {
  model_data <- eval(x$call$data, envir = x$call_env)
  outcome_variable <- all.vars(x$fml_all$linear)[1]
  obs_used <- obs(x)

  if (is.logical(obs_used) && length(obs_used) == nrow(model_data)) {
    data_used <- model_data[obs_used, ]
  } else if (is.numeric(obs_used) && all(obs_used <= nrow(model_data))) {
    data_used <- model_data[obs_used, ]
  } else {
    stop("Unable to correctly subset data. Check the obs_used vector.")
  }

  pre_avg <- mean(data_used[[outcome_variable]][data_used$is_hp_installed == 0], na.rm = TRUE)
  format_decimal(pre_avg, digits = 1)
}, "Yearly Consumption")

fitstat_register("t_obs", function(x) {
  t_var <- x$fixef_vars[3]
  format_number(x$fixef_sizes[t_var])
}, "Number of Time Periods")

# TWFE models (filtered to DID ids)
m1 <- feols(elec_consumption ~ i(is_hp_installed) | account_id + hdd + settlement_week,
            data = overall_weekly %>% filter(account_id %in% did_data$account_id),
            cluster = ~account_id)

m2 <- feols(gas_consumption ~ i(is_hp_installed) | account_id + hdd + settlement_week,
            data = overall_weekly %>% filter(account_id %in% did_data$account_id),
            cluster = ~account_id)

# CS stats for patching (use *formatted strings* for LaTeX injection)
main_periods <- c("Electricity", "Gas")
cs_estimates <- list(
  Electricity = format_decimal(aggte_simple_elec$overall.att, 1),
  Gas         = format_decimal(aggte_simple_gas$overall.att, 1)
)
cs_se <- list(
  Electricity = format_decimal(aggte_simple_elec$overall.se, 1),
  Gas         = format_decimal(aggte_simple_gas$overall.se, 1)
)
cs_n <- list(
  Electricity = format_number(aggte_simple_elec$DIDparams$n),
  Gas         = format_number(aggte_simple_gas$DIDparams$n)
)
cs_nG <- list(
  Electricity = format_number(aggte_simple_elec$DIDparams$nG),
  Gas         = format_number(aggte_simple_gas$DIDparams$nG)
)
cs_nT <- list(
  Electricity = format_number(aggte_simple_elec$DIDparams$nT),
  Gas         = format_number(aggte_simple_gas$DIDparams$nT)
)

# Create the initial 4-model table: (TWFE Elec, TWFE Gas, placeholder, placeholder)
etable(
  m1, m2,
  m1, m2,
  headers = list(
    list("TWFE" = 2, "CS" = 2),
    list(rep(c("Electricity", "Gas"), times = 2))
  ),
  depvar = FALSE,
  tex = TRUE,
  title = "HP Installation on Yearly Energy Consumption in kWh",
  fitstat = ~ N + g + pre_avg + t_obs + r2,
  file = "tables/hp_did_overall_detailed.tex",
  replace = TRUE,
  label = "tab:hp-did-overall-conso-detailed",
  style.tex = style.tex(tpt = TRUE)
)

# (Optional) your re-positioning helper
CleanPreAverage("tables/hp_did_overall_detailed.tex")

# Patch CS values into the right-hand two columns (cols 4 and 5)
patch_etable_twfe_cs(
  file_path = "tables/hp_did_overall_detailed.tex",
  cs_estimates = cs_estimates,
  cs_se = cs_se,
  cs_n = cs_n,
  cs_nG = cs_nG,
  cs_nT = cs_nT,
  coef_pattern = "Is HP Installed \\$=\\$ 1"
)

checkpoint("Saved tables/hp_did_overall_detailed.tex (patched)")

# ============================================================
# 9) NEVER-TREATED robustness: TWFE + CS detailed table (patched)
# Output: tables/hp_did_never_treated_detailed.tex
# ============================================================

checkpoint("NEVER-TREATED: build DID index and models")

# Rebuild DID index with 'never treated' coding (firstweek=0 if after window)
start_date <- min(overall_weekly$settlement_week)

did_data_never <- overall_weekly %>%
  ungroup() %>%
  mutate(
    week      = as.numeric(difftime(settlement_week, start_date, units = "weeks")) %/% 1 + 1,
    firstweek = as.numeric(difftime(installed_at, start_date, units = "weeks")) %/% 1 + 1
  ) %>%
  group_by(account_id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  filter(week <= 129) %>%
  mutate(firstweek = ifelse(firstweek > 129, 0, firstweek))

# TWFE models (your original date cut)
m1_never <- feols(
  elec_consumption ~ i(is_hp_installed) | account_id + hdd + settlement_week,
  data = overall_weekly %>%
    filter(settlement_week < as.Date("2024-06-03"),
           account_id %in% did_data_never$account_id),
  cluster = ~account_id
)

m2_never <- feols(
  gas_consumption ~ i(is_hp_installed) | account_id + hdd + settlement_week,
  data = overall_weekly %>%
    filter(settlement_week < as.Date("2024-06-03"),
           account_id %in% did_data_never$account_id),
  cluster = ~account_id
)

checkpoint("NEVER-TREATED: load CS results")

# CS files for never-treated robustness
cs_files_never <- list(
  Electricity = file.path(datapath, "scratch/est_cs_never_treated_elec_weekly.RDS"),
  Gas         = file.path(datapath, "scratch/est_cs_never_treated_gas_weekly.RDS")
)

aggte_simple_elec_never <- aggte(readRDS(cs_files_never$Electricity), type = "simple",
                                 na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.01)
aggte_simple_gas_never  <- aggte(readRDS(cs_files_never$Gas), type = "simple",
                                 na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.01)

# Stats used to patch CS columns (store formatted strings for LaTeX)
cs_estimates_never <- list(
  Electricity = format_decimal(aggte_simple_elec_never$overall.att, 1),
  Gas         = format_decimal(aggte_simple_gas_never$overall.att, 1)
)
cs_se_never <- list(
  Electricity = format_decimal(aggte_simple_elec_never$overall.se, 1),
  Gas         = format_decimal(aggte_simple_gas_never$overall.se, 1)
)
cs_n_never <- list(
  Electricity = format_number(aggte_simple_elec_never$DIDparams$n),
  Gas         = format_number(aggte_simple_gas_never$DIDparams$n)
)
cs_nG_never <- list(
  Electricity = format_number(aggte_simple_elec_never$DIDparams$nG),
  Gas         = format_number(aggte_simple_gas_never$DIDparams$nG)
)
cs_nT_never <- list(
  Electricity = format_number(aggte_simple_elec_never$DIDparams$nT),
  Gas         = format_number(aggte_simple_gas_never$DIDparams$nT)
)

checkpoint("NEVER-TREATED: create etable + patch")

# Create the initial 4-model table (TWFE Elec, TWFE Gas, placeholder, placeholder)
etable(
  m1_never, m2_never,
  m1_never, m2_never,
  headers = list(
    list("TWFE" = 2, "CS" = 2),
    list(rep(c("Electricity", "Gas"), times = 2))
  ),
  depvar = FALSE,
  tex = TRUE,
  title = "HP Installation on Yearly Energy Consumption in kWh (Never-treated)",
  fitstat = ~ N + g + pre_avg + t_obs + r2,
  file = "tables/hp_did_never_treated_detailed.tex",
  replace = TRUE,
  label = "tab:hp-did-never-treated-conso-detailed",
  style.tex = style.tex(tpt = TRUE)
)

# Optional: reposition pre-treatment average (your existing helper)
CleanPreAverage("tables/hp_did_never_treated_detailed.tex")

# Patch CS columns (cols 4 & 5) + add cohorts row etc.
patch_etable_twfe_cs(
  file_path = "tables/hp_did_never_treated_detailed.tex",
  cs_estimates = cs_estimates_never,
  cs_se = cs_se_never,
  cs_n = cs_n_never,
  cs_nG = cs_nG_never,
  cs_nT = cs_nT_never,
  coef_pattern = "Is HP Installed \\$=\\$ 1"
)

checkpoint("Saved tables/hp_did_never_treated_detailed.tex (patched)")
                   
   
# ============================================================
# X) anticipation
# ============================================================

checkpoint("Plotting anticipation graph")                         
                         
                         
# Define paths and base filenames for each anticipation period
output_base_path <- file.path(datapath, "/scratch/")
output_filenames <- c("est_cs_elec_weekly", "est_cs_gas_weekly")
anticipation_periods <- 0:10  # The range of anticipation periods

# Define a function to process each anticipation week
process_week <- function(anticipation_week) {
  # File paths
  elec_file <- paste0(output_base_path, output_filenames[1], "_anticipation_", anticipation_week, ".RDS")
  gas_file  <- paste0(output_base_path, output_filenames[2], "_anticipation_", anticipation_week, ".RDS")
  
  # Read and calculate aggregate estimates
  elec_agg <- aggte(readRDS(elec_file), type = "simple", na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.01)
  gas_agg  <- aggte(readRDS(gas_file), type = "simple", na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.01)
  
  # Create data frames for each type
  df_elec <- data.frame(
    anticipation_week = anticipation_week,
    estimate = elec_agg$overall.att ,
    lower_ci = elec_agg$overall.att  - 1.96 * elec_agg$overall.se,
    upper_ci = elec_agg$overall.att  + 1.96 * elec_agg$overall.se,
    type = "Electricity"
  )
  
  df_gas <- data.frame(
    anticipation_week = anticipation_week,
    estimate = gas_agg$overall.att ,
    lower_ci = gas_agg$overall.att  - 1.96 * gas_agg$overall.se,
    upper_ci = gas_agg$overall.att  + 1.96 * gas_agg$overall.se,
    type = "Gas"
  )
  
  list(df_elec, df_gas)
}

# Apply the function over anticipation weeks and combine results
results_list <- lapply(anticipation_periods, process_week)

# Flatten the list and bind rows into one data frame
plot_data <- do.call(rbind, unlist(results_list, recursive = FALSE))

# Plotting the results with confidence intervals
ggplot(plot_data, aes(x = anticipation_week, y = estimate, color = type, fill = type)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  scale_color_manual(values = c("Electricity" = elec_color, "Gas" = gas_color)) +
  scale_fill_manual(values = c("Electricity" = elec_color, "Gas" = gas_color)) +
  labs(
    title = "Anticipation Period Estimates for Electricity and Gas",
    x = "Anticipation Week",
    y = "Estimate",
    color = "Type",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("graphs/HP_anticipation.png") 
                         
checkpoint("Anticipation graph saved: graphs/HP_anticipation.png")                         
                         
# ============================================================
# X) Dynamic plots with trends
# ============================================================

checkpoint("Dynamic plot for monthly trends")

# ---- CS files (FULL sample) ----
cs_files_full <- list(
  Electricity = file.path(datapath, "scratch/est_cs_elec_weekly_with_trends.RDS"),
  Gas         = file.path(datapath, "scratch/est_cs_gas_weekly_with_trends.RDS")
)
                         
elec_dyn <- aggte(readRDS(cs_files_full$Electricity), type = "dynamic",
                  na.rm = TRUE, clustervars = "id", bstrap = TRUE, min_e = -90, max_e = 90)
gas_dyn  <- aggte(readRDS(cs_files_full$Gas), type = "dynamic",
                  na.rm = TRUE, clustervars = "id", bstrap = TRUE, min_e = -90, max_e = 90)

p_dyn <- create_dynamic_plot(elec_dyn, gas_dyn, elec_color, gas_color)
ggsave("graphs/dynamic_hp_plot_combined_with_trends.png", plot = p_dyn, width = 10, height = 8, dpi = 300)
checkpoint("Saved graphs/dynamic_hp_plot_combined_with_trends.png")
                         
                         
                         
checkpoint("DONE")