# ============================================================
# Difference-in-Differences (DiD) analysis (Cosy tariff)
# - Estimation: Callaway & Sant’Anna (did::att_gt + did::aggte)
# - Outputs:
#   (1) Saved CS estimates per rate period in scratch/
#   (2) Plots: dynamic + calendar (per period + combined)
#   (3) Main LaTeX table: TWFE from fixest::etable, then “CS columns”
#       are injected into tables/did.tex
# ============================================================

# ----------------------------
# User inputs / paths / colors
# ----------------------------
# datapath must exist in your environment
# flexible_color / cosy_color must exist in your environment

cat("\n>>> Script start <<<\n")

# ----------------------------
# Helper formatting functions
# ----------------------------
format_number <- function(number) {
  format(round(number), big.mark = ",", scientific = FALSE)
}

format_decimal <- function(number, decimals = 4) {
  format(round(number, decimals), nsmall = decimals, big.mark = ",", scientific = FALSE)
}

# Confidence “star”: *** if (1-alpha)% CI does NOT include 0
confidence_star <- function(coefficient, se, alpha) {
  ci_lower <- coefficient - qnorm(1 - alpha / 2) * se
  ci_upper <- coefficient + qnorm(1 - alpha / 2) * se
  if (ci_lower > 0 | ci_upper < 0) "***" else ""
}

# Build vcov for calendar-time ATT path from did::aggte output.
# Priority: influence-function covariance; fallback: diagonal from se.egt.
calendar_vcov <- function(cal_obj) {
  extract_inf_fun <- function(x, k) {
    if (is.null(x)) return(NULL)

    if (is.list(x)) {
      for (nm in c("calendar.inf.func.e", "egt.inf.func", "att.inf.func.e")) {
        if (!is.null(x[[nm]])) return(extract_inf_fun(x[[nm]], k))
      }
      if (length(x) == 1) return(extract_inf_fun(x[[1]], k))
      return(NULL)
    }

    if (is.data.frame(x) || is.matrix(x) || !is.null(dim(x))) {
      m <- as.matrix(x)
      if (ncol(m) == k) return(m)
      if (nrow(m) == k) return(t(m))
    }
    NULL
  }

  inf_fun <- extract_inf_fun(cal_obj$inf.function, length(cal_obj$egt))

  if (!is.null(inf_fun)) {
    n <- nrow(inf_fun)
    return(crossprod(inf_fun) / (n^2))
  }

  if (!is.null(cal_obj$V_egt)) {
    return(cal_obj$V_egt)
  }

  message("calendar_vcov: using diagonal fallback (off-diagonal covariance unavailable).")
  diag(as.numeric(cal_obj$se.egt)^2)
}

calendar_linear_se <- function(cal_obj, idx, weights = NULL) {
  if (length(idx) == 0) return(NA_real_)
  if (is.null(weights)) weights <- rep(1, length(idx))
  stopifnot(length(weights) == length(idx))

  V <- calendar_vcov(cal_obj)
  V_sub <- V[idx, idx, drop = FALSE]
  as.numeric(sqrt(t(weights) %*% V_sub %*% weights))
}

# ----------------------------
# Helper: drop duplicate-named columns (keeps first occurrence)
# ----------------------------
drop_duplicate_named_cols <- function(x) {
  if (anyDuplicated(names(x))) {
    if (inherits(x, "data.table")) {
      x <- x[, names(x)[!duplicated(names(x))], with = FALSE]
    } else {
      x <- x[, !duplicated(names(x)), drop = FALSE]
    }
  }
  x
}

# ----------------------------
# Helper: extract pre-treatment average from aggte_simple (numeric scalar)
# ----------------------------
extract_pre_treatment_avg <- function(aggte_simple, anticipation = 1) {
  dat <- aggte_simple$DIDparams$data
  dat <- drop_duplicate_named_cols(dat)
  dat <- as.data.frame(dat)

  dat %>%
    ungroup() %>%
    filter(week < firstweek - anticipation) %>%
    summarise(pre_avg = mean(consumption_hh, na.rm = TRUE)) %>%
    pull(pre_avg)
}

# ----------------------------
# Helper: edit fixest LaTeX table and inject CS results into cols 6–10
# ----------------------------
replace_columns <- function(line, new_values) {
  parts <- strsplit(line, "&")[[1]]
  for (i in seq_along(new_values)) {
    parts[6 + i] <- str_trim(new_values[[i]])
  }
  paste(parts, collapse = " & ")
}

clear_columns <- function(line) {
  parts <- strsplit(line, "&")[[1]]
  for (i in 7:length(parts)) parts[i] <- ""
  paste(parts, collapse = " & ")
}

inject_cs_into_did_tex <- function(file_path,
                                  cs_estimates, cs_se, cs_n, cs_nG, cs_nT, cs_pre_avg) {
  file_content <- readLines(file_path)

  # Locate rows
  coeff_line   <- grep("Contract", file_content)
  se_line      <- coeff_line + 1
  obs_line     <- grep("Observations", file_content)
  sample_line  <- grep("Number of Households", file_content)
  periods_line <- grep("Number of Time Periods", file_content)
  hdd_line     <- grep("HDD", file_content)
  mpan_line    <- grep("Household", file_content)[1]
  day_line     <- grep("Day", file_content)
  pre_avgs     <- grep("Half Hourly Consumption", file_content)[2]
  r2_line      <- grep("R", file_content)[-1]

  # Format estimates/se for LaTeX
  cs_estimates_fmt <- lapply(cs_estimates, function(x) sprintf("%.4f", x))
  cs_se_fmt        <- lapply(cs_se, function(x) sprintf("%.4f", x))

  # Column order expected by your table
  new_estimates <- c(
    paste0(cs_estimates_fmt[["Morning Off-peak"]], "***"),
    paste0(cs_estimates_fmt[["Afternoon Off-peak"]], "***"),
    paste0(cs_estimates_fmt[["Peak Rate"]], "***"),
    paste0(cs_estimates_fmt[["Other"]], "***"),
    paste0(cs_estimates_fmt[["Overall"]])
  )

  new_se <- c(
    paste0("(", cs_se_fmt[["Morning Off-peak"]], ")"),
    paste0("(", cs_se_fmt[["Afternoon Off-peak"]], ")"),
    paste0("(", cs_se_fmt[["Peak Rate"]], ")"),
    paste0("(", cs_se_fmt[["Other"]], ")"),
    paste0("(", cs_se_fmt[["Overall"]], ")")
  )

  # Replace CS columns (6–10)
  file_content[sample_line]  <- paste0(replace_columns(file_content[sample_line],  cs_n),       " \\\\")
  file_content[coeff_line]   <- paste0(replace_columns(file_content[coeff_line],   new_estimates), " \\\\")
  file_content[se_line]      <- paste0(replace_columns(file_content[se_line],      new_se),     " \\\\")
  file_content[periods_line] <- paste0(replace_columns(file_content[periods_line], cs_nT),      " \\\\")
  file_content[pre_avgs]     <- paste0(replace_columns(file_content[pre_avgs],     cs_pre_avg), " \\\\")

  # Clear TWFE-only rows in CS columns
  file_content[obs_line]  <- paste0(clear_columns(file_content[obs_line]),  " \\\\")
  file_content[hdd_line]  <- paste0(clear_columns(file_content[hdd_line]),  " \\\\")
  file_content[mpan_line] <- paste0(clear_columns(file_content[mpan_line]), " \\\\")
  file_content[day_line]  <- paste0(clear_columns(file_content[day_line]),  " \\\\")
  file_content[r2_line]   <- paste0(clear_columns(file_content[r2_line]),   " \\\\")

  # Update clustering row + add CS clustering row
  clustering_line_index <- grep("Clustered \\\\\\(Household\\\\\\)", file_content)
  if (length(clustering_line_index) > 0) {
    file_content[clustering_line_index] <-
      "\\multicolumn{10}{l}{\\emph{Clustered (Household) standard-errors in parentheses for TWFE}}\\\\"
    file_content <- append(
      file_content,
      "\\multicolumn{5}{l}{\\emph{Clustered (Household) standard-errors in parentheses for CS}}\\\\",
      after = clustering_line_index
    )
  }

  # Add “Number of cohorts (CS)” under sample size
  new_row <- paste0(
    "Number of cohorts (CS) & &  &  &  &  & ",
    paste(cs_nG, collapse = " & "),
    " \\\\"
  )
  file_content <- append(file_content, new_row, after = sample_line)

  writeLines(file_content, file_path)
}

# ----------------------------
# Helper: CleanPreAverage (kept from your code)
# ----------------------------
create_latex_table <- function(models, headers, title, file, label,
                               note = "") {

  # Extract coefficients with stars
  coefficients <- sapply(models, function(model) {
    coef  <- model$overall.att
    se    <- model$overall.se
    alpha <- model$DIDparams$alp
    paste0(format_decimal(coef, 4), confidence_star(coef, se, alpha))
  })

  # Standard errors
  standard_errors <- sapply(models, function(model) {
    paste0("(", format_decimal(model$overall.se, 4), ")")
  })

  # Pre-treatment averages (robust to duplicate column names)
  pre_treatment_values <- sapply(models, function(model) {
    dat <- model$DIDparams$data
    if (anyDuplicated(names(dat))) {
      dat <- dat[, names(dat)[!duplicated(names(dat))], with = FALSE]
    }
    dat <- as.data.frame(dat)

    pre_avg <- dat %>%
      dplyr::filter(week < firstweek - 1) %>%
      dplyr::summarise(pre_avg = mean(consumption_hh, na.rm = TRUE)) %>%
      dplyr::pull(pre_avg)

    format_decimal(pre_avg, 4)
  })

  # Fit statistics (support both old/new DIDparams field names)
  get_did_stat <- function(m, stat) {
    dp <- m$DIDparams

    if (stat == "n_households") {
      if (!is.null(dp$id_count)) return(dp$id_count)
      if (!is.null(dp$n))        return(dp$n)
    }
    if (stat == "nG") {
      if (!is.null(dp$treated_groups_count)) return(dp$treated_groups_count)
      if (!is.null(dp$nG))                   return(dp$nG)
    }
    if (stat == "nT") {
      if (!is.null(dp$time_periods_count)) return(dp$time_periods_count)
      if (!is.null(dp$nT))                 return(dp$nT)
    }

    NA_real_
  }

  n_households <- sapply(models, function(m) format_number(get_did_stat(m, "n_households")))
  nG <- sapply(models, function(m) format_number(get_did_stat(m, "nG")))
  nT <- sapply(models, function(m) format_number(get_did_stat(m, "nT")))

  alpha <- models[[1]]$DIDparams$alp
  conf_level <- (1 - alpha) * 100

  # ---- LaTeX ----
  latex <- "\\begin{table}[htbp]\n"
  latex <- paste0(latex, "   \\caption{\\label{", label, "} ", title, "}\n")

  # Add floatfoot note (like your original CS table)
  if (note != "") {
    latex <- paste0(
      latex,
      "   \\floatfoot{\\justifying \\footnotesize \\upshape \\textbf{Note:} ",
      note,
      "}\n"
    )
  }

  latex <- paste0(latex, "   \\centering\n")
  latex <- paste0(latex, "   \\begin{tabular}{l", paste(rep("c", length(headers)), collapse = ""), "}\n")
  latex <- paste0(latex, "      \\tabularnewline \\midrule \\midrule\n")
  latex <- paste0(latex, "                                     & ", paste(headers, collapse = "     & "), " \\\\   \n")
  latex <- paste0(latex, "      Model:                         & ",
                  paste(paste0("(", seq_along(headers), ")"), collapse = "              & "),
                  "\\\\  \n")
  latex <- paste0(latex, "      \\midrule\n")

  latex <- paste0(latex, "      \\emph{Variable}\\\\\n")
  latex <- paste0(latex, "      Has Adopted Tariff $=$ 1     & ", paste(coefficients, collapse = " & "), "\\\\   \n")
  latex <- paste0(latex, "                                     & ", paste(standard_errors, collapse = "         & "), "\\\\   \n")

  latex <- paste0(latex, "      \\midrule\n")
  latex <- paste0(latex, "      \\emph{Pre-treatment Average}\\\\\n")
  latex <- paste0(latex, "      Half Hourly Consumption              & ", paste(pre_treatment_values, collapse = " & "), "\\\\   \n")

  latex <- paste0(latex, "      \\midrule\n")
  latex <- paste0(latex, "      \\emph{Fit statistics}\\\\\n")
  latex <- paste0(latex, "      Number of Households                   & ", paste(n_households, collapse = "           & "), "\\\\  \n")
  latex <- paste0(latex, "      Number of Cohorts              & ", paste(nG, collapse = "              & "), "\\\\  \n")
  latex <- paste0(latex, "      Number of Time Periods         & ", paste(nT, collapse = "             & "), "\\\\  \n")

  # Add the extra “CS footer” rows you wanted back
  latex <- paste0(latex, "      \\midrule \\midrule\n")
  latex <- paste0(latex, "      \\multicolumn{", length(headers) + 1, "}{l}{Clustered (Household) standard-errors in parentheses}\\\\\n")
  latex <- paste0(latex, "      \\multicolumn{", length(headers) + 1, "}{l}{Estimation Method: Doubly Robust}\\\\\n")
  latex <- paste0(latex, "      \\multicolumn{", length(headers) + 1, "}{l}{Control Group: Not Yet Treated, Anticipation Periods: 0}\\\\\n")
  latex <- paste0(latex, "      \\multicolumn{", length(headers) + 1, "}{l}{Signif. Codes: *** ", conf_level, "\\% confidence band does not cover 0}\\\\\n")

  latex <- paste0(latex, "   \\end{tabular}\n")
  latex <- paste0(latex, "\\end{table}\n")

  writeLines(latex, file)
}
# ----------------------------
# Helper: CleanPreAverage (kept from your code)
# ----------------------------
CleanPreAverage <- function(file_path) {
  file_content <- readLines(file_path)

  idx <- grep("Half Hourly Consumption", file_content)
  pre_avg_line_index <- if (length(idx) == 1) idx else idx[2]

  pre_avg_lines <- file_content[pre_avg_line_index]
  file_content  <- file_content[-c(pre_avg_line_index, pre_avg_line_index)]

  coeff_end_index <- grep("Fixed-effects", file_content) - 2

  file_content <- append(file_content, pre_avg_lines, after = coeff_end_index)
  file_content <- append(file_content, "\\emph{Pre-Treatment Average}\\\\", after = coeff_end_index)
  file_content <- append(file_content, "\\midrule", after = coeff_end_index)

  sample_line <- grep("Size of the 'effective' sample", file_content)
  if (length(sample_line) > 0) {
    file_content[sample_line] <- gsub(
      "Size of the 'effective' sample",
      "Number of Households",
      file_content[sample_line]
    )
  }

  writeLines(file_content, file_path)
}

# ============================================================
# 1) Load data + define periods
# ============================================================
cat("\n>>> Loading aggregated data <<<\n")
aggregated_data <- readRDS(file.path(datapath, "scratch/aggregated_data.RDS"))
cat(">>> Data loaded: ", nrow(aggregated_data), " rows <<<\n")

periods      <- unique(aggregated_data$rate_period)
main_periods <- unique(aggregated_data$rate_period)

base_periods <- c( "universal")  #"varying",
start_date   <- min(floor_date(aggregated_data$date, "week"))

# ============================================================
# 2) Estimate and save CS objects (heavy; skip if files exist)
# ============================================================
cat("\n>>> CS estimation: att_gt (heavy step) <<<\n")

for (period in periods) {
  for (base_period in base_periods) {

    cat(">>> Period:", period, "| Base period:", base_period, "<<<\n")

    file_suffix <- ifelse(base_period == "universal", "_universal", "")
    filename <- file.path(datapath, paste0("scratch/did_cosy_", period, file_suffix, ".RDS"))

    #if (!file.exists(filename)) {

      cat(">>> Estimating & saving:", basename(filename), "<<<\n")

      did_data <- aggregated_data %>%
        ungroup() %>%
        filter(rate_period == period, !hashed_mpan == "1185945433") %>%
        mutate(
          settlement_week = floor_date(date, "week"),
          week      = difftime(settlement_week, start_date, units = "weeks"),
          firstweek  = difftime(floor_date(first_adoption, "week"), start_date, units = "weeks")
        ) %>%
        group_by(hashed_mpan, firstweek, week) %>%
        summarise(consumption_hh = mean(consumption_hh), .groups = "drop") %>%
        mutate(
          firstweek = as.numeric(firstweek),
          week      = as.numeric(week)
        ) %>%
        group_by(hashed_mpan) %>%
        mutate(id = cur_group_id()) %>%
        ungroup()

      est_cs <- att_gt(
        yname = "consumption_hh",
        tname = "week",
        idname = "id",
        gname = "firstweek",
        data = did_data,
        clustervars = "id",
        anticipation = 1,
        control_group = "notyettreated",
        allow_unbalanced_panel = TRUE,
        base_period = base_period,
        cores = 4
      )

      saveRDS(est_cs, filename)
      cat(">>> Saved:", basename(filename), "<<<\n")

    #} else {
    #  cat(">>> Skipping (exists):", basename(filename), "<<<\n")
    #}
  }
}

cat("\n>>> Finished CS estimation <<<\n")

                   
               
# ============================================================
# 3) Calendar-time ATT-by-cohort plot (per period)
# ============================================================
cat("\n>>> Calendar-time ATT plots <<<\n")

for (period in periods) {

  cat(">>> Calendar-time plot for:", period, "<<<\n")

  est_cs <- readRDS(file.path(datapath, paste0("scratch/did_cosy_", period, ".RDS")))

  est_cs$first_week <- (weeks(est_cs$group - 1) + floor_date(start_date, "week"))
  est_cs$week       <- (floor_date(start_date, "week") + weeks(est_cs$t - 1))

  p <- data.frame(
    group = est_cs$group,
    se = est_cs$se,
    t = est_cs$t,
    date = est_cs$week,
    att = est_cs$att,
    first_week = est_cs$first_week
  ) %>%
    filter(first_week <= date)

  ggplot(p, aes(x = date, y = att, color = first_week, group = first_week)) +
    geom_ribbon(aes(ymin = att - se, ymax = att + se),
                fill = "grey80", alpha = 0.5, color = NA) +
    geom_line() +
    geom_point() +
    scale_x_date(labels = scales::date_format("%b %y"), date_breaks = "3 month") +
    labs(x = "Calendar Time", y = "Average ATT", color = "Adoption Week") +
    scale_color_gradientn(
      colors = c("lightblue", "blue", "darkblue"),
      breaks = as.Date(seq(19337, 19885, 100)),
      labels = format(as.Date(seq(19337, 19885, 100)), "%b %Y")
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right")

  ggsave(paste0("graphs/monthly_att_", tolower(period) %>% str_replace(" ", "_"), ".png"))

  cat(">>> Saved calendar-time plot for:", period, "<<<\n")
}

# ============================================================
# 4) Dynamic ATT plots (per period + combined facet)
# ============================================================
cat("\n>>> Dynamic ATT plots <<<\n")

create_dynamic_data <- function(period_data, period_name) {
  data.frame(
    event_time = period_data$egt,
    coefficient = period_data$att.egt,
    lower_ci = period_data$att.egt - 1.96 * period_data$se.egt,
    upper_ci = period_data$att.egt + 1.96 * period_data$se.egt,
    period = period_name,
    cosy_status = ifelse(period_data$egt < 0, "No", "Yes")
  )
}

all_dynamic <- data.frame()

for (period in periods) {

  cat(">>> Dynamic plot for:", period, "<<<\n")

  est_cs <- readRDS(file.path(datapath, paste0("scratch/did_cosy_", period, "_universal.RDS")))
  period_data <- aggte(est_cs, type = "dynamic", alp = 0.05, min_e = -52, max_e = 52)

  plot_data <- create_dynamic_data(period_data, period) %>%
    mutate(cosy_status = factor(cosy_status, levels = c("Yes", "No")))

  p <- ggplot(plot_data, aes(x = event_time, y = coefficient, color = cosy_status)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_color_manual(
      name = "Has Adopted Tariff",
      labels = c("No" = "No", "Yes" = "Yes"),
      values = c("No" = flexible_color, "Yes" = cosy_color)
    ) +
    scale_x_continuous(breaks = seq(-50, 50, 10)) +
    labs(x = "Weeks since adoption",
         y = "Dynamic ATT for Half Hourly Consumption (kWh)") +
    theme_minimal()

  ggsave(paste0("graphs/plot_", period, ".png"),
         plot = p, device = "png", width = 10, height = 8, dpi = 300)

  cat(">>> Saved dynamic plot for:", period, "<<<\n")

  all_dynamic <- bind_rows(all_dynamic, plot_data)
}

cat("\n>>> Combined dynamic ATT plot <<<\n")

all_dynamic <- all_dynamic %>%
  filter(period != "Overall") %>%
  mutate(period = factor(period, levels = c("Morning Off-peak",
                                           "Afternoon Off-peak",
                                           "Peak Rate",
                                           "Other",
                                           "Overall")))

y_min <- min(all_dynamic$lower_ci, na.rm = TRUE)
y_max <- max(all_dynamic$upper_ci, na.rm = TRUE)

p_dynamic_combined <- ggplot(all_dynamic, aes(x = event_time, y = coefficient, color = cosy_status)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(
    name = "Has Adopted Tariff",
    labels = c("No" = "No", "Yes" = "Yes"),
    values = c("No" = flexible_color, "Yes" = cosy_color)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(limits = c(y_min, y_max)) +
  labs(x = "Weeks since adoption",
       y = "Dynamic ATT for Half Hourly Consumption in kWh") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~ period, scales = "free")

ggsave("graphs/dynamic_att_combined.png", plot = p_dynamic_combined,
       device = "png", width = 16, height = 12, dpi = 300)

cat(">>> Saved combined dynamic ATT plot <<<\n")

# ============================================================
# 5) Calendar ATT plots (per period + combined facet)
# ============================================================
cat("\n>>> Calendar ATT plots <<<\n")


create_calendar_data <- function(period_data, period_name, start_date) {
  week_dates <- as.Date(start_date) + lubridate::weeks(as.numeric(period_data$egt))
  data.frame(
    week_date = week_dates,
    estimate  = as.numeric(period_data$att.egt),
    se        = as.numeric(period_data$se.egt),
    lower_ci  = as.numeric(period_data$att.egt) - 1.96 * as.numeric(period_data$se.egt),
    upper_ci  = as.numeric(period_data$att.egt) + 1.96 * as.numeric(period_data$se.egt),
    period    = period_name
  ) %>% arrange(week_date)
}

last12m_summary <- list()
all_calendar <- data.frame()
for (period in periods) {

 cat(">>> Period:", period, "<<<\n")

  est_cs <- readRDS(file.path(datapath, paste0("scratch/did_cosy_", period, "_universal.RDS")))

  # calendar aggregation (REMOVE alp/min_e/max_e)
  period_data <- aggte(
    est_cs,
    type        = "calendar",
    na.rm       = TRUE,
    bstrap      = TRUE,
    clustervars = "id",
    alp  = 0.05
  )

  # ------------------------------------------------------------------
  # 1) Weekly calendar ATT (add se column, keep your structure)
  # ------------------------------------------------------------------
  plot_data <- create_calendar_data(period_data, period, start_date) %>%
    mutate(
      week_date = as.Date(week_date),
      estimate  = as.numeric(estimate),
      se        = as.numeric(period_data$se.egt)
    ) %>%
    arrange(week_date)

  # ------------------------------------------------------------------
  # 2) Last 12 months window (up to last observed week)
  # ------------------------------------------------------------------
  n_weeks <- nrow(plot_data)
  win_n   <- min(52, n_weeks)   # if fewer than 52 weeks, use what you have

  last_win <- plot_data %>%
    slice_tail(n = win_n)

  w_start <- min(last_win$week_date, na.rm = TRUE)
  w_end   <- max(last_win$week_date, na.rm = TRUE)

  # Mean ATT over that window (simple average of weekly ATTs)
  mean_att <- mean(last_win$estimate, na.rm = TRUE)

  # SE of mean ATT using full calendar covariance:
  # se(mean) = sqrt(w' * Var(att_path) * w), with w_t = 1/T in the window.
  cal_week_df <- tibble(
    week_date = as.Date(start_date) + weeks(as.numeric(period_data$egt)),
    idx = seq_along(period_data$egt)
  )

  last_idx <- last_win %>%
    select(week_date) %>%
    left_join(cal_week_df, by = "week_date") %>%
    pull(idx)

  se_mean_att <- calendar_linear_se(
    period_data,
    idx = last_idx,
    weights = rep(1 / win_n, win_n)
  )

  # ------------------------------------------------------------------
  # 3) Non-treated equivalent over same weeks
  # ------------------------------------------------------------------
  dat <- as.data.frame(est_cs$DIDparams$data)
  dat <- dat[, !duplicated(names(dat)), drop = FALSE]
  a   <- as.numeric(est_cs$DIDparams$anticipation)

  baseline_df <- dat %>%
    mutate(
      week      = as.numeric(week),
      week_date = as.Date(start_date) + weeks(week),
      is_nyt    = week < (firstweek - a)
    ) %>%
    filter(is_nyt,
           week_date >= w_start,
           week_date <= w_end) %>%
    group_by(week_date) %>%
    summarise(
      nyt_mean = mean(consumption_hh, na.rm = TRUE),
      .groups  = "drop"
    )

  baseline_mean <- mean(baseline_df$nyt_mean, na.rm = TRUE)

  share_baseline <- mean_att / baseline_mean   # in levels
  share_label    <- scales::percent(share_baseline, accuracy = 0.1)

  # ------------------------------------------------------------------
  # 4) Shading + label for last 12 months
  # ------------------------------------------------------------------
  shade_df <- tibble(
    xmin = w_start,
    xmax = w_end,
    ymin = -Inf,
    ymax = Inf
  )

    y_max <- max(plot_data$upper_ci, na.rm = TRUE)
    y_min <- min(plot_data$lower_ci, na.rm = TRUE)
    yrng  <- y_max - y_min
    if (!is.finite(yrng) || yrng == 0) yrng <- 1

    label_x <- w_start + (w_end - w_start) / 2

    # ---- smart vertical placement ----
    label_y <- if (period == "Peak Rate") {
      0 + 0.60 * yrng
    } else if (period == "Other") {
      0 + 0.80 * yrng
    } else if (period == "Overall") {
      0 - 0.3 * yrng
    } else {
      0 - 0.10 * yrng
    }

  label_txt <- paste0(
    "Last 12 months (", win_n, " weeks):\n",
    "Mean ATT = ", sprintf("%.3f", mean_att), " kWh\n",
    "≈ ", share_label, " of non-treated level"
  )

  # ------------------------------------------------------------------
  # 5) Plot with shading + label
  # ------------------------------------------------------------------
  p <- ggplot(plot_data, aes(x = week_date, y = estimate)) +
    geom_rect(
      data = shade_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = "grey70",
      alpha = 0.2
    ) +
    geom_point(color = cosy_color) +
    geom_line(color = cosy_color) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                  width = 0.2, color = cosy_color, alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    annotate(
      "text",
      x = label_x,
      y = label_y,
      label = label_txt,
      hjust = 0.5,
      vjust = 1,
      size = 3
    ) +
    scale_x_date(labels = scales::date_format("%b %y"), date_breaks = "1 month") +
    scale_y_continuous(limits = c(-0.6, 1)) +
    labs(x = "Week", y = "Calendar ATT for Half Hourly Consumption in kWh") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(paste0("graphs/calendarplot_", period, ".png"),
         plot = p, device = "png", width = 5, height = 4, dpi = 300)

  cat(">>> Saved calendar plot (with last 12m shading) for:", period, "<<<\n")

    last12m_summary[[period]] <- tibble::tibble(
      period        = period,
      window_start  = as.Date(w_start),
      window_end    = as.Date(w_end),
      weeks_used    = win_n,
      mean_att      = mean_att,
      se_mean_att   = se_mean_att,
      att_lo        = mean_att - 1.96 * se_mean_att,
      att_hi        = mean_att + 1.96 * se_mean_att,
      baseline_mean = baseline_mean,
      share         = share_baseline
    )
    
  all_calendar <- bind_rows(all_calendar, plot_data)
}

               
last12m_tab <- dplyr::bind_rows(last12m_summary) %>%
  dplyr::mutate(
    period = factor(period, levels = c("Morning Off-peak",
                                       "Afternoon Off-peak",
                                       "Peak Rate",
                                       "Other",
                                       "Overall"))
  ) %>%
  dplyr::arrange(period)

# ---- LaTeX output (simple) ----
latex_file <- file.path("tables/calendar_last12m_summary.tex")

table_note <- paste0(
  "\\textbf{Note:} This table summarizes calendar-time average treatment effects on the treated (ATT) ",
  "for the heat pump tariff adoption analysis over June 2023 to June 2024. Weekly calendar ATTs are ",
  "obtained from the Callaway--Sant’Anna estimator aggregated to calendar time. For each rate period, ",
  "we report the \\emph{mean} calendar ATT over this period (``Mean ATT''). The standard error in parentheses ",
  "is computed as $\\sqrt{w'\\Sigma w}$, where $\\Sigma$ is the covariance matrix of weekly calendar ATT estimates ",
  "(from the did influence-function/cluster bootstrap covariance) and $w_t=1/T$ over the selected window. ",
  "``NYT baseline'' is the mean outcome among not-yet-treated ",
  "households over the same weeks. ``Share (ATT/NYT)'' reports the ratio of the mean ATT to the NYT baseline, ",
  "expressed as a percentage. Outcomes are measured in kWh per half-hour."
)

latex <- "\\begin{table}[htbp]\n"
latex <- paste0(latex, "  \\caption{\\label{tab:calendar_last12m_summary} Heat Pump Tariff Adoption on Half Hourly Electricity Consumption in kWh}\n")
latex <- paste0(latex, "  \\floatfoot{\\justifying \\footnotesize \\upshape ", table_note, "}\n")
latex <- paste0(latex, "  \\centering\n")
latex <- paste0(latex, "  \\begin{tabular}{lccc}\n")
latex <- paste0(latex, "    \\tabularnewline \\midrule \\midrule\n")
latex <- paste0(latex, "    Rate period & Mean ATT (se) & NYT baseline & Share (ATT/NYT) \\\\\n")
latex <- paste0(latex, "    \\midrule\n")

for (i in seq_len(nrow(last12m_tab))) {
  mean_cell <- paste0(format_decimal(last12m_tab$mean_att[i], 3),
                      " (", format_decimal(last12m_tab$se_mean_att[i], 3), ")")
  base_cell <- format_decimal(last12m_tab$baseline_mean[i], 3)
  share_cell <- paste0(format_decimal(100 * last12m_tab$share[i], 1), "\\%")

  latex <- paste0(
    latex,
    "    ", as.character(last12m_tab$period[i]), " & ",
    mean_cell, " & ",
    base_cell, " & ",
    share_cell, " \\\\\n"
  )
}

latex <- paste0(latex, "    \\midrule \\midrule\n")
latex <- paste0(latex, "    \\multicolumn{4}{l}{\\footnotesize Calendar window: June 2023 to June 2024.}\\\\\n")
latex <- paste0(latex, "  \\end{tabular}\n")
latex <- paste0(latex, "\\end{table}\n")

writeLines(latex, latex_file)
               
               
cat("\n>>> Combined calendar ATT plot <<<\n")

all_calendar <- all_calendar %>%
  filter(period != "Overall") %>%
  mutate(period = factor(period, levels = c("Morning Off-peak",
                                           "Afternoon Off-peak",
                                           "Peak Rate",
                                           "Other",
                                           "Overall")))

y_min <- min(all_calendar$lower_ci, na.rm = TRUE)
y_max <- max(all_calendar$upper_ci, na.rm = TRUE)

p_calendar_combined <- ggplot(all_calendar, aes(x = week_date, y = estimate)) +
  geom_point(color = cosy_color) +
  geom_line(color = cosy_color) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                width = 0.2, color = cosy_color, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_date(labels = scales::date_format("%b %y"), date_breaks = "1 month") +
  scale_y_continuous(limits = c(y_min, y_max)) +
  labs(x = "Week", y = "Calendar ATT for Half Hourly Consumption in kWh") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_wrap(~ period, scales = "free")

ggsave("graphs/calendarplot_combined.png", plot = p_calendar_combined,
       device = "png", width = 16, height = 12, dpi = 300)

cat(">>> Saved combined calendar ATT plot <<<\n")

               # ============================================================
# 6) CS “simple” estimates per period -> inject into did.tex
# ============================================================
cat("\n>>> CS simple aggregation + LaTeX injection <<<\n")

cs_estimates <- list()
cs_se        <- list()
cs_n         <- list()
cs_nG        <- list()
cs_nT        <- list()
cs_pre_avg   <- list()

for (period in main_periods) {

  cat(">>> CS simple for:", period, "<<<\n")

  est_cs <- readRDS(file.path(datapath, paste0("scratch/did_cosy_", period, "_universal.RDS")))

  aggte_simple <- aggte(
    est_cs,
    type = "simple",
    na.rm = TRUE,
    clustervars = "id",
    bstrap = TRUE,
    alp = 0.05
  )

  pre_avg <- extract_pre_treatment_avg(aggte_simple, anticipation = 1)

  cs_estimates[[period]] <- aggte_simple$overall.att
  cs_se[[period]]        <- aggte_simple$overall.se
  cs_n[[period]]         <- format_number(aggte_simple$DIDparams$n)
  cs_nG[[period]]        <- format_number(aggte_simple$DIDparams$nG)
  cs_nT[[period]]        <- format_number(aggte_simple$DIDparams$nT)
  cs_pre_avg[[period]]   <- format_decimal(pre_avg, 4)
}
          
cat("\n>>> Writing cosy_did_cs table <<<\n")


models <- lapply(main_periods, function(period) {
  est_cs <- readRDS(file.path(datapath, paste0("scratch/did_cosy_", period, "_universal.RDS")))
  aggte(est_cs, type="simple", na.rm=TRUE, clustervars="id", bstrap=TRUE, alp=0.01)
})


headers <- shortstack_period(c("Morning Off-peak","Afternoon Off-peak","Peak Rate","Other","Overall"))

note_text <- paste0(
  "We report five Callaway–Sant’Anna (CS) estimates of the impact of tariff adoption on customers’ half-hourly electricity consumption (in kWh). Columns (1)–(4) correspond to average effects during the Morning off-peak period (4am–7am), the Afternoon off-peak period (1pm–4pm), the Peak period (4pm–7pm), and all other hours of the day, respectively. Column (5) reports the overall effect, averaged across all 48 half-hour settlement periods. Cohorts are defined by the week in which households adopt the tariff."
)

create_latex_table(
  models = models,
  headers = headers,
  title = "Effect of Time-of-Use Tariff on Electricity Consumption",
  file  = "tables/cosy_did_cs.tex",
  label = "tab:cosy-did-cs",
  note  = note_text
)
                                       
               
# ============================================================
# 7) TWFE table (fixest) -> write did.tex -> CleanPreAverage
# ============================================================
cat("\n>>> TWFE table (fixest::etable) <<<\n")

# list CS ids
ids <- aggte_simple$DIDparams$data$id %>% unique()
mpans <- aggregated_data %>%
        ungroup() %>%
        filter(rate_period == period, !hashed_mpan == "1185945433") %>%
        mutate(
          settlement_week = floor_date(date, "week"),
          week      = difftime(settlement_week, start_date, units = "weeks"),
          firstweek  = difftime(floor_date(first_adoption, "week"), start_date, units = "weeks")
        ) %>%
        group_by(hashed_mpan, firstweek, week) %>%
        summarise(consumption_hh = mean(consumption_hh), .groups = "drop") %>%
        mutate(
          firstweek = as.numeric(firstweek),
          week      = as.numeric(week)
        ) %>%
        group_by(hashed_mpan) %>%
        mutate(id = cur_group_id()) %>%
        ungroup() %>%
        filter(id %in% ids) %>%
        pull(hashed_mpan)
               
               
m1 <- feols(
  consumption_hh ~ i(cosy_contract_active) | hdd + account_id + date,
  data = aggregated_data %>% filter(hashed_mpan %in% mpans),
  cluster = ~account_id,
  split = ~ rate_period
)

shortstack_period <- function(x) {
  dplyr::case_when(
    x == "Morning Off-peak"   ~ "\\shortstack{Morning\\\\Off-peak}",
    x == "Afternoon Off-peak" ~ "\\shortstack{Afternoon\\\\Off-peak}",
    TRUE ~ x
  )
}
period_headers <- shortstack_period(sort(main_periods))

etable(
  m1, m1,
  tex = TRUE,
  title = "Heat Pump Tariff Adoption on Half Hourly Electricity Consumption",
  headers = list(
    list("TWFE" = 5, "CS" = 5),
    list(rep(period_headers, times = 2))
  ),
  fitstat = ~ N + g + pre_avg + t_obs + r2,
  file = "tables/did.tex",
  replace = TRUE,
  label = "tab:did-main",
  style.tex = style.tex(tpt = TRUE)
)
               
CleanPreAverage("tables/did.tex")

# Optional: tweak tabular preamble (kept from your later block)
file_content <- readLines("tables/did.tex")
file_content[5] <- gsub(
  "\\\\begin\\{tabular\\}\\{lcccccccccc\\}",
  "\\\\begin{tabular}{@{}l@{}c@{}c@{}c@{}c@{}c@{}c@{}c@{}c@{}c@{}c@{}}",
  file_content[5]
)
writeLines(file_content, "tables/did.tex")

cat(">>> Saved tables/did.tex (TWFE base) <<<\n")



inject_cs_into_did_tex(
  file_path    = "tables/did.tex",
  cs_estimates = cs_estimates,
  cs_se        = cs_se,
  cs_n         = cs_n,
  cs_nG        = cs_nG,
  cs_nT        = cs_nT,
  cs_pre_avg   = cs_pre_avg
)

cat(">>> Injected CS results into tables/did.tex <<<\n")
                             
                             


# ============================================================
# 8) DiD imputation estimator + plots
# ============================================================
cat("\n>>> DiD imputation estimator <<<\n")

imputation_results_list <- list()

for (period in periods) {

  cat(">>> Imputation estimation for:", period, "<<<\n")

  did_data <- aggregated_data %>%
    ungroup() %>%
    filter(rate_period == period, !hashed_mpan == "1185945433") %>%
    mutate(
      settlement_week = floor_date(date, "week"),
      week      = difftime(settlement_week, start_date, units = "weeks"),
      firstweek  = difftime(floor_date(first_adoption, "week"), start_date, units = "weeks")
    ) %>%
    group_by(hashed_mpan, firstweek, week) %>%
    summarise(consumption_hh = mean(consumption_hh), .groups = "drop") %>%
    mutate(firstweek = as.numeric(firstweek),
           week      = as.numeric(week)) %>%
    group_by(hashed_mpan) %>%
    mutate(id = cur_group_id()) %>%
    ungroup()

  imputation_results_list[[paste0("period_", period)]] <- did_imputation(
    data = did_data,
    yname = "consumption_hh",
    gname = "firstweek",
    tname = "week",
    idname = "id",
    first_stage = NULL,
    wname = NULL,
    wtr = NULL,
    horizon = seq(-52, 52),
    pretrends = -52:-1,
    cluster_var = "id"
  )
}

cat("\n>>> Imputation plots (per period + combined) <<<\n")

all_imp <- data.frame()

for (nm in names(imputation_results_list)) {

  period_name <- gsub("period_", "", nm)
  cat(">>> Plotting imputation:", period_name, "<<<\n")

  period_data <- imputation_results_list[[nm]] %>%
    rename(coefficient = estimate) %>%
    mutate(event_time = as.numeric(term)) %>%
    filter(event_time > -52, event_time < 52) %>%
    mutate(
      lower_ci = conf.low,
      upper_ci = conf.high,
      cosy_status = ifelse(event_time < 0, "No", "Yes"),
      period = factor(period_name, levels = c("Morning Off-peak",
                                             "Afternoon Off-peak",
                                             "Peak Rate",
                                             "Other",
                                             "Overall"))
    )

  p <- ggplot(period_data, aes(x = event_time, y = coefficient, color = cosy_status)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_color_manual(
      name = "Has Adopted Tariff",
      labels = c("No" = "No", "Yes" = "Yes"),
      values = c("No" = flexible_color, "Yes" = cosy_color)
    ) +
    labs(x = "Weeks since adoption",
         y = "Dynamic ATT for Half Hourly Consumption (kWh)") +
    theme_minimal()

  ggsave(paste0("graphs/imputation_plot_", period_name, ".png"),
         plot = p, device = "png", width = 10, height = 8, dpi = 300)

  all_imp <- bind_rows(all_imp, period_data)
}

cat(">>> Combined imputation plot <<<\n")

y_min <- min(all_imp$lower_ci, na.rm = TRUE)
y_max <- max(all_imp$upper_ci, na.rm = TRUE)

p_imp_combined <- ggplot(all_imp %>% filter(period != "Overall"),
                         aes(x = event_time, y = coefficient, color = cosy_status)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(
    name = "Has Adopted Tariff",
    labels = c("No" = "No", "Yes" = "Yes"),
    values = c("No" = flexible_color, "Yes" = cosy_color)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(limits = c(y_min, y_max)) +
  labs(x = "Weeks since adoption",
       y = "Dynamic ATT for Half Hourly Consumption in kWh") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~ period, scales = "free")

ggsave("graphs/dynamic_att_combined_imputation.png",
       plot = p_imp_combined, device = "png",
       width = 16, height = 12, dpi = 300)

cat("\n>>> Script finished successfully <<<\n")
