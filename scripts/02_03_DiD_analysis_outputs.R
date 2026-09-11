# ============================================================
# HP installation: TWFE + Callaway–Sant’Anna (CS) tables + plots
#
# Produces the LaTeX tables and figures summarizing heat pump
# installation effects on electricity and gas consumption, combining
# two-way fixed effects (TWFE) and Callaway-Sant'Anna (CS) estimates:
#   - CS-only summary tables (full sample, gas-only subsample, and
#     anticipation-period robustness checks)
#   - combined TWFE/CS tables built with fixest::etable and then
#     patched with CS estimates in the right-hand columns
#     (label | TWFE Elec | TWFE Gas | CS Elec | CS Gas), including a
#     never-treated robustness version
#   - dynamic (event-study) and calendar-time plots, rolling
#     12-month sums with quarterly callouts, and an implied
#     "empirical efficiency" (COP) panel
# ============================================================

# Colors for electricity/gas series (hp_color/not_hp_color are set in main.R)
elec_color <- hp_color
gas_color  <- not_hp_color

# ----------------------------
# Small utilities
# ----------------------------

checkpoint <- function(msg) cat(paste0(">>> ", msg, " <<<\n"))

# Suffixed "_local" so these don't shadow the shared format_decimal/
# format_number defined in 02_00_heatpump.R's top-level setup (digits = 2
# default there, vs digits = 1 here) -- a plain `format_decimal <- ...`
# reassignment here would silently persist in the global environment for
# every sub-script sourced after this one, since it isn't a fitstat_register()
# call and the per-script environment cleanup only removes *new* names, not
# reassignments of names already whitelisted in list_env.
format_decimal_local <- function(x, digits = 1) formatC(x, format = "f", digits = digits, big.mark = ",")
format_number_local  <- function(x) formatC(x, format = "d", big.mark = ",")

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
  a <- as.numeric(aggte_simple$DIDparams$anticipation)
  stopifnot(outcome_col %in% names(dat))
  dat %>%
    filter(week < firstweek - a) %>%
    summarise(pre_avg = mean(.data[[outcome_col]], na.rm = TRUE)) %>%
    pull(pre_avg)
}

# ----------------------------
# LaTeX table creator for CS-only (2-column: Electricity / Gas)
# ----------------------------
create_latex_table_cs <- function(models, headers, title, file, label,
                                  pre_treatment_values,
                                  note = "",
                                  digits = 1,
                                  variable_label = "Is HP Installed $=$ 1",
                                  pretreat_row_label = "Pre-Treatment Consumption",
                                  estimation_method = "Doubly Robust",
                                  control_group = "Not Yet Treated") {

  stopifnot(length(headers) == length(models))
  stopifnot(length(pre_treatment_values) == length(models))
  
  # --- Coefs + stars ---
  coefficients <- sapply(models, function(m) {
    coef  <- m$overall.att
    se    <- m$overall.se
    alpha <- m$DIDparams$alp
    paste0(format_decimal_local(coef, digits), confidence_star(coef, se, alpha))
  })

  # --- SEs ---
  standard_errors <- sapply(models, function(m) {
    paste0("(", format_decimal_local(m$overall.se, digits), ")")
  })
 
  # Anticipation
  anticipation <-  sapply(models, function(m) {
      m$DIDparams$anticipation
  }) %>% unique() 
    
  # --- Fit statistics (field names differ across did package versions) ---
    get_did_stat <- function(m, stat) {
      dp <- m$DIDparams

      if (stat == "n_households") {
        if (!is.null(dp$id_count)) return(dp$id_count)
        if (!is.null(dp$n))        return(dp$n)      # old
      }

      if (stat == "nG") {
        if (!is.null(dp$treated_groups_count)) return(dp$treated_groups_count)
        if (!is.null(dp$nG))                   return(dp$nG)  # old
      }

      if (stat == "nT") {
        if (!is.null(dp$time_periods_count)) return(dp$time_periods_count)
        if (!is.null(dp$nT))                 return(dp$nT)    # old
      }

      NA
    }

    # Extract and format fit stats exactly the same way
    n_households <- sapply(models, function(m)
      format_number_local(get_did_stat(m, "n_households"))
    )

    nG <- sapply(models, function(m)
      format_number_local(get_did_stat(m, "nG"))
    )

    nT <- sapply(models, function(m)
      format_number_local(get_did_stat(m, "nT"))
    )


  # if anything came back empty, fail loudly instead of writing character(0)
  if (any(nchar(n_households) == 0) || any(nchar(nG) == 0) || any(nchar(nT) == 0)) {
    stop("Some fit statistics are empty. Check DIDparams fields in your aggte objects.")
  }

  alpha <- models[[1]]$DIDparams$alp
  conf_level <- (1 - alpha) * 100

  pretreat_fmt <- sapply(pre_treatment_values, function(x) format_decimal_local(x, digits))

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
latex <- paste0(latex, "      \\multicolumn{", k + 1,"}{l}{Control Group: ", control_group, "}\\\\\n")
latex <- paste0(latex, "      \\multicolumn{", k + 1,"}{l}{Anticipation Period: ", anticipation, "}\\\\\n")
latex <- paste0(latex, "      \\multicolumn{", k + 1, "}{l}{Signif. Codes: *** ",
                conf_level, "\\% confidence band does not cover 0}\\\\\n")
latex <- paste0(latex, "   \\end{tabular}\n")
latex <- paste0(latex, "\\end{table}\n")

  writeLines(latex, file)
}
# ----------------------------
# LaTeX patcher for TWFE+CS combined table produced by fixest::etable
# ----------------------------
#    Works for EXACT structure: m1,m2,m1,m2  => 4 models => 5 columns total
patch_etable_twfe_cs <- function(file_path,
                                cs_estimates, cs_pre, cs_se, cs_n, cs_nG, cs_nT,
                                coef_pattern = "Is HP Installed \\$=\\$ 1") {

  library(stringr)

  checkpoint(paste0("Patching LaTeX table: ", file_path))
  file_content <- readLines(file_path)

  # helper: numeric formatting for table (1 decimal, thousands sep)
  format_num <- function(x, digits = 1) {
    formatC(x, format = "f", big.mark = ",", digits = digits)
  }

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

  # Construct coefficient/SE strings; CS estimates are always rendered with *** (significance is not re-derived here)
  new_estimates <- c(paste0(cs_estimates[["Electricity"]], "$^{***}$"),
                     paste0(cs_estimates[["Gas"]],        "$^{***}$"))
  new_se <- c(paste0("(", cs_se[["Electricity"]], ")"),
              paste0("(", cs_se[["Gas"]],        ")"))

  # --- locate lines -----------------------------------------------------------
  coeff_line   <- grep(coef_pattern, file_content)
  se_line      <- coeff_line + 1

  obs_line     <- grep("^\\s*Observations\\s*&",              file_content)
  sample_line  <- grep("^\\s*Size of the 'effective' sample\\s*&|^\\s*Number of Households\\s*&",
                       file_content)
  pre_line     <- grep("^\\s*Pre-Treatment Consumption\\s*&", file_content)
  periods_line <- grep("^\\s*Number of Time Periods\\s*&",    file_content)

  hdd_line     <- grep("^\\s*HDD\\s*&",        file_content)
  hh_line      <- grep("^\\s*Household\\s*&",  file_content)
  week_line    <- grep("^\\s*Week\\s*&",       file_content)
  r2_line      <- grep("^\\s*R\\$\\^2\\$\\s*&",file_content)

  # --- replace CS columns in key rows ----------------------------------------

  # 1) Number of Households row (formerly "Size of the 'effective' sample")
  if (length(sample_line) > 0) {
    file_content[sample_line] <- gsub(
      "Size of the 'effective' sample",
      "Number of Households",
      file_content[sample_line]
    )
    file_content[sample_line] <- paste0(
      replace_cs_cols(
        file_content[sample_line],
        c(cs_n[["Electricity"]], cs_n[["Gas"]])
      ),
      " \\\\"
    )
  }

  # 2) Main coefficient + SE row
  if (length(coeff_line) > 0) {
    file_content[coeff_line] <- paste0(
      replace_cs_cols(file_content[coeff_line], new_estimates),
      " \\\\"
    )
    file_content[se_line] <- paste0(
      replace_cs_cols(file_content[se_line], new_se),
      " \\\\"
    )
  }

  # 3) Pre-treatment consumption row (NEW)
  if (length(pre_line) > 0) {
    new_pre_vals <- c(
      format_num(cs_pre[["Electricity"]]),
      format_num(cs_pre[["Gas"]])
    )

    file_content[pre_line] <- paste0(
      replace_cs_cols(file_content[pre_line], new_pre_vals),
      " \\\\"
    )
  }

  # 4) Number of time periods row
  if (length(periods_line) > 0) {
    file_content[periods_line] <- paste0(
      replace_cs_cols(
        file_content[periods_line],
        c(cs_nT[["Electricity"]], cs_nT[["Gas"]])
      ),
      " \\\\"
    )
  }

  # --- clear CS columns where only TWFE info makes sense ---------------------
  if (length(obs_line)  > 0) file_content[obs_line]  <- paste0(clear_cs_cols(file_content[obs_line]),  " \\\\")
  if (length(hdd_line)  > 0) file_content[hdd_line]  <- paste0(clear_cs_cols(file_content[hdd_line]),  " \\\\")
  if (length(hh_line)   > 0) file_content[hh_line]   <- paste0(clear_cs_cols(file_content[hh_line]),   " \\\\")
  if (length(week_line) > 0) file_content[week_line] <- paste0(clear_cs_cols(file_content[week_line]), " \\\\")
  if (length(r2_line)   > 0) file_content[r2_line]   <- paste0(clear_cs_cols(file_content[r2_line]),   " \\\\")

  # --- clustering line: ensure 5 columns -------------------------------------
  clustering_line_index <- grep("Clustered \\(Household\\)", file_content)
  if (length(clustering_line_index) > 0) {
    file_content[clustering_line_index] <-
      "\\multicolumn{5}{l}{\\emph{Clustered (Household) standard-errors in parentheses}}\\\\"
  }

  # --- Number of cohorts (CS) row (5-column compliant) -----------------------
  if (length(sample_line) > 0) {
    new_row <- paste0("Number of cohorts (CS) &  &  & ",
                      cs_nG[["Electricity"]], " & ", cs_nG[["Gas"]], " \\\\")
    file_content <- append(file_content, new_row, after = sample_line[1])
  }

  writeLines(file_content, file_path)
  checkpoint("LaTeX patch applied")
}

# ----------------------------
# Plot helpers
# ----------------------------

create_dynamic_plot <- function(elec_data, gas_data, elec_color, gas_color) {
  # Anticipation shading
  anticipation_df <- data.frame(
      xmin = -elec_dyn$DIDparams$anticipation,
      xmax = 0,
      ymin = -Inf,
      ymax = Inf
    )

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
    geom_rect(
      data = anticipation_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = "grey80",
      alpha = 0.25
    ) +
    geom_errorbar(
      aes(ymin = lower_ci, ymax = upper_ci, colour = type),
      width = 0,
      linewidth = 0.6,
      alpha = 0.6
    ) +
    geom_line(aes(color = type)) +
    geom_point(aes(color = type, shape = type), size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_fill_manual(
      values = c(Electricity = elec_color, Gas = gas_color),
      guide = "none"   # hide duplicate legend
    ) +
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
    annotate(
      "text",
      x = -2,
      y = Inf,
      label = "Anticipation\nwindow",
      vjust = 1.2,
      size = 3.5,
      colour = "grey30"
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

# Build vcov for calendar-time ATT path from did::aggte output.
# Priority: influence-function covariance; fallback: diagonal from se.egt.
calendar_vcov <- function(cal_obj, scale = 1) {
  extract_inf_fun <- function(x, k) {
    if (is.null(x)) return(NULL)

    if (is.list(x)) {
      for (nm in c("calendar.inf.func.t", "calendar.inf.func.e", "egt.inf.func", "att.inf.func.e")) {
        if (!is.null(x[[nm]])) return(extract_inf_fun(x[[nm]], k))
      }
      # Fallback: choose the matrix/data.frame element closest to k columns/rows.
      candidates <- x[vapply(x, function(obj) is.matrix(obj) || is.data.frame(obj), logical(1))]
      if (length(candidates) > 0) {
        dims <- lapply(candidates, dim)
        score <- vapply(dims, function(d) min(abs(d[1] - k), abs(d[2] - k)), numeric(1))
        return(extract_inf_fun(candidates[[which.min(score)]], k))
      }
      if (length(x) == 1) return(extract_inf_fun(x[[1]], k))
      return(NULL)
    }

    if (is.data.frame(x) || is.matrix(x) || !is.null(dim(x))) {
      m <- data.matrix(x)
      if (ncol(m) != k && nrow(m) == k) m <- t(m)
      if (ncol(m) == k) return(m)

      # Some did objects carry one extra IF column; drop the column that
      # best matches reported se.egt when reconstructing diag(vcov).
      if (ncol(m) == k + 1 && !is.null(cal_obj$se.egt)) {
        se_target <- as.numeric(cal_obj$se.egt)
        errs <- sapply(seq_len(ncol(m)), function(j) {
          mm <- m[, -j, drop = FALSE]
          se_try <- sqrt(diag(crossprod(mm) / (nrow(mm)^2)))
          sum((se_try - se_target)^2, na.rm = TRUE)
        })
        return(m[, -which.min(errs), drop = FALSE])
      }
    }
    NULL
  }

  inf_fun <- extract_inf_fun(cal_obj$inf.function, length(cal_obj$egt))

  if (!is.null(inf_fun)) {
    n <- nrow(inf_fun)
    return((scale^2) * crossprod(inf_fun) / (n^2))
  }

  if (!is.null(cal_obj$V_egt)) {
    return((scale^2) * cal_obj$V_egt)
  }

  message("calendar_vcov: using diagonal fallback (off-diagonal covariance unavailable).")
  diag((scale * as.numeric(cal_obj$se.egt))^2)
}

calendar_linear_se <- function(cal_obj, idx, weights = NULL, scale = 1) {
  if (length(idx) == 0) return(NA_real_)
  if (is.null(weights)) weights <- rep(1, length(idx))
  stopifnot(length(weights) == length(idx))

  V <- calendar_vcov(cal_obj, scale = scale)
  V_sub <- V[idx, idx, drop = FALSE]
  as.numeric(sqrt(t(weights) %*% V_sub %*% weights))
}

window_annual_summary <- function(cal_obj, start_date, w_start, w_end, type_label, scale = 1/52.25) {
  week_dates <- as.Date(start_date) + weeks(as.numeric(cal_obj$egt))
  idx <- which(week_dates >= w_start & week_dates <= w_end)

  annual_kwh <- sum(as.numeric(cal_obj$att.egt[idx]) * scale, na.rm = TRUE)
  annual_se  <- calendar_linear_se(
    cal_obj = cal_obj,
    idx = idx,
    weights = rep(1, length(idx)),
    scale = scale
  )

  tibble(
    type = type_label,
    annual_kwh = annual_kwh,
    annual_se = annual_se,
    annual_lo = annual_kwh - 1.96 * annual_se,
    annual_hi = annual_kwh + 1.96 * annual_se,
    count_obs = length(idx)
  )
}
                         
# ---- Rolling 12-month sum (yearly effect in kWh/year) + CI ----
add_rolling_12m_sum <- function(df, cal_objs = NULL, window_weeks = 52, scale = 1 / 52.25) {
  df <- df %>%
    arrange(type, week_date) %>%
    group_by(type) %>%
    mutate(
      estimate_12m = zoo::rollapply(
        estimate, width = window_weeks, FUN = sum,
        align = "right", fill = NA, na.rm = TRUE
      )
    ) %>%
    ungroup()

  # Backward-compatible fallback: diagonal-only rolling SE.
  if (is.null(cal_objs)) {
    return(
      df %>%
        group_by(type) %>%
        mutate(
          se_12m = sqrt(zoo::rollapply(
            se^2, width = window_weeks, FUN = sum,
            align = "right", fill = NA, na.rm = TRUE
          )),
          lower_ci_12m = estimate_12m - 1.96 * se_12m,
          upper_ci_12m = estimate_12m + 1.96 * se_12m
        ) %>%
        ungroup()
    )
  }

  # Covariance-aware rolling SE: se(sum_window) = sqrt(1' * V_window * 1)
  out <- df %>%
    group_split(type) %>%
    lapply(function(dsub) {
      type_name <- as.character(dsub$type[1])
      cal_obj <- cal_objs[[type_name]]
      if (is.null(cal_obj)) {
        stop("Missing calendar object for type: ", type_name)
      }

      V <- calendar_vcov(cal_obj, scale = scale)
      n <- nrow(dsub)
      se_roll <- rep(NA_real_, n)

      for (j in seq_len(n)) {
        if (j < window_weeks) next
        idx <- (j - window_weeks + 1):j
        se_roll[j] <- sqrt(sum(V[idx, idx, drop = FALSE]))
      }

      dsub %>%
        mutate(
          se_12m = se_roll,
          lower_ci_12m = estimate_12m - 1.96 * se_12m,
          upper_ci_12m = estimate_12m + 1.96 * se_12m
        )
    }) %>%
    bind_rows()

  out
}

# ---- Rolling pre-treatment mean baseline (calendar time) + 52w rolling mean ----
rolling_pre_avg_calendar_52w <- function(aggte_obj, start_date, outcome_col, window_weeks = 52) {

  dat <- clean_didparams_data(aggte_obj$DIDparams$data)
  a   <- as.numeric(aggte_obj$DIDparams$anticipation)

  stopifnot(all(c("week", "firstweek") %in% names(dat)))
  stopifnot(outcome_col %in% names(dat))

  dat %>%
    mutate(is_pre = week < (firstweek - a)) %>%
    filter(is_pre) %>%
    group_by(week) %>%
    summarise(
      pre_avg_weekly = mean(.data[[outcome_col]], na.rm = TRUE),
      n_pre = sum(!is.na(.data[[outcome_col]])),
      .groups = "drop"
    ) %>%
    arrange(week) %>%
    mutate(
      # align week=1 to start_date
      week_date = as.Date(start_date + weeks(week)),
      pre_avg_52w = zoo::rollapply(
        pre_avg_weekly, width = window_weeks, FUN = mean,
        align = "right", fill = NA, na.rm = TRUE
      ),
      pre_52w_kwhyr = pre_avg_52w 
    )
}
   

# ----------------------------
# Load data + set colors
# ----------------------------

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

# ----------------------------
# CS simple tables (full vs gas-only) — build once, reuse helpers
# ----------------------------

checkpoint("CS simple: load RDS + build CS-only table")

# ---- CS files (FULL sample) ----
cs_files_full <- list(
  Electricity = file.path(datapath, "scratch/est_cs_elec_weekly.RDS"),
  Gas         = file.path(datapath, "scratch/est_cs_gas_weekly.RDS")
)

aggte_simple_elec <- aggte(readRDS(cs_files_full$Electricity), type = "simple",
                           na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.05, min_e=-80, max_e=80)
aggte_simple_gas  <- aggte(readRDS(cs_files_full$Gas), type = "simple", 
                           na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.05, min_e=-80, max_e=80)
                                                  

# Save IDs to keep consistent sample through the analysis
ids_cs_elec <- did_data %>% 
 filter(id %in% unique(aggte_simple_elec$DIDparams$data$id)) %>%
  pull(account_id)
saveRDS(ids_cs_elec, file.path(datapath, "scratch/ids_cs_elec.RS"))
ids_cs_gas <- did_data %>% 
 filter(id %in% unique(aggte_simple_gas$DIDparams$data$id)) %>%
  pull(account_id)
saveRDS(ids_cs_gas, file.path(datapath, "scratch/ids_cs_gas.RS"))
       
# Pre-treatment means (use DIDparams$data safely)
pre_elec <- pre_avg_from_aggte(aggte_simple_elec, "elec_consumption")
pre_gas  <- pre_avg_from_aggte(aggte_simple_gas,  "gas_consumption")

models_cs_full  <- list(Electricity = aggte_simple_elec, Gas = aggte_simple_gas)
headers_cs      <- c("Electricity", "Gas")
title_cs_full   <- "Heat Pump Installation Effects on Overall Energy Consumption (kWh)"
file_cs_full    <- "tables/hp_did_overall_cs.tex"
label_cs_full   <- "tab:hp-did-cs"

note_cs_full <- paste(
  "This table reports the simple CS estimates",
  "of the impact of heat pump installation on households\u2019 electricity consumption (column 1)",
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
  pretreat_row_label = "Pre-Treatment Consumption"
)

checkpoint("Saved tables/hp_did_overall_cs.tex")
                         
                         
# ----------------------------
# CS simple: load RDS + build CS-only tables for anticipation = 0..10
# ----------------------------

checkpoint("CS simple: loop over anticipation periods")

anticipation_periods <- 0:10  # range of anticipation periods considered

for (a in anticipation_periods) {

  checkpoint(paste0("CS simple (anticipation = ", a, "): load RDS + build CS-only table"))

  # ---- CS files (FULL sample) ----
  cs_files_a <- list(
    Electricity = file.path(datapath, paste0("scratch/est_cs_elec_weekly_anticipation_", a, ".RDS")),
    Gas         = file.path(datapath, paste0("scratch/est_cs_gas_weekly_anticipation_", a, ".RDS"))
  )

  # Fail fast if a required CS estimation file is missing
  if (!file.exists(cs_files_a$Electricity)) {
    stop("Electricity CS file not found for anticipation = ", a, ": ", cs_files_a$Electricity)
  }
  if (!file.exists(cs_files_a$Gas)) {
    stop("Gas CS file not found for anticipation = ", a, ": ", cs_files_a$Gas)
  }

  # ---- Simple CS effects ----
  aggte_simple_elec_ant <- aggte(
    readRDS(cs_files_a$Electricity),
    type = "simple",
    na.rm = TRUE,
    clustervars = "id",
    bstrap = TRUE,
    alp = 0.05,
    min_e=-80, max_e=80
  )

  aggte_simple_gas_ant <- aggte(
    readRDS(cs_files_a$Gas),
    type = "simple",
    na.rm = TRUE,
    clustervars = "id",
    bstrap = TRUE,
    alp = 0.05,
    min_e=-80, max_e=80
  )

  # ---- Pre-treatment means (via pre_avg_from_aggte) ----
  pre_elec <- pre_avg_from_aggte(aggte_simple_elec_ant, "elec_consumption")
  pre_gas  <- pre_avg_from_aggte(aggte_simple_gas_ant,  "gas_consumption")

  models_cs_full <- list(
    Electricity = aggte_simple_elec_ant,
    Gas         = aggte_simple_gas_ant
  )

  headers_cs <- c("Electricity", "Gas")

  # Anticipation period is reflected in both the table title and the note below
  title_cs_full <- paste0(
    "Heat Pump Installation Effects on Energy Consumption (kWh), Anticipation = ",
    a
  )

  file_cs_full  <- file.path("tables", paste0("hp_did_overall_cs_anticipation_", a, ".tex"))
  label_cs_full <- paste0("tab:hp-did-cs-anticipation-", a)

  note_cs_full <- paste(
    "This table reports simple Callaway–Sant'Anna estimates of the impact of heat pump installation",
    "on households’ electricity consumption (column 1) and gas consumption (column 2).",
    "Cohorts refer to households with the same week of installation.",
    "Estimates are computed using an anticipation window of", a, "period(s).",
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
    variable_label = "Is HP Installed $= 1$",
    pretreat_row_label = "Pre-Treatment Consumption"
  )

  checkpoint(paste0("Saved ", file_cs_full))
}

checkpoint("Finished CS simple tables for all anticipation periods")

# ---- CS files (GAS-ONLY electricity + gas) ----
checkpoint("CS simple: gas-only subsample table")

cs_files_gas_only <- list(
  Electricity = file.path(datapath, "scratch/est_cs_elec_weekly_gas_only.RDS"),
  Gas         = file.path(datapath, "scratch/est_cs_gas_weekly.RDS")
)

# est_cs_elec_weekly_gas_only.RDS (built in 02_02) is estimated on "any
# account with a non-missing gas reading", which is a slightly larger
# population than ids_cs_gas (the gas model's own att_gt estimation sample)
# -- 1,111 vs 1,110 households. For this table specifically, refit the
# electricity model restricted to exactly ids_cs_gas, so both columns
# describe the identical population. Scoped to just this table/script: does
# not touch 02_02 or overwrite the shared est_cs_elec_weekly_gas_only.RDS
# file, which other (non-pipeline) scripts may still rely on.
did_data_elec_gas_only <- did_data %>% dplyr::filter(account_id %in% ids_cs_gas)

est_cs_elec_gas_only_matched <- did::att_gt(
  yname = "elec_consumption",
  tname = "week",
  idname = "id",
  gname = "firstweek",
  data = did_data_elec_gas_only,
  anticipation = 4,
  clustervars = "id",
  control_group = "notyettreated",
  est_method = "ipw",
  allow_unbalanced_panel = TRUE,
  base_period = "universal"
)

aggte_simple_elec_gasonly <- aggte(est_cs_elec_gas_only_matched, type = "simple",
                                   na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.05,min_e=-80, max_e=80)
aggte_simple_gas_gasonly  <- aggte(readRDS(cs_files_gas_only$Gas), type = "simple",
                                   na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.05, min_e=-80, max_e=80)

pre_elec_gasonly <- pre_avg_from_aggte(aggte_simple_elec_gasonly, "elec_consumption")
pre_gas_gasonly  <- pre_avg_from_aggte(aggte_simple_gas_gasonly,  "gas_consumption")

models_cs_gasonly <- list(Electricity = aggte_simple_elec_gasonly, Gas = aggte_simple_gas_gasonly)
title_cs_gasonly  <- "Heat Pump Installation Effects on Energy Consumption (Gas-Metered Households)"
file_cs_gasonly   <- "tables/hp_did_overall_cs_gas_only.tex"
label_cs_gasonly  <- "tab:hp-did-cs-gas-only"

note_cs_gasonly <- paste(
  "This table reports the simple CS estimates of the impact of heat pump installation on electricity (column 1)",
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
  pretreat_row_label = "Pre-Treatment Consumption"
)

checkpoint("Saved tables/hp_did_overall_cs_gas_only.tex")

# ----------------------------
# Dynamic plot (CS)
# ----------------------------

checkpoint("Dynamic CS plot (electricity + gas)")

elec_dyn <- aggte(readRDS(cs_files_full$Electricity), type = "dynamic",
                  na.rm = TRUE, clustervars = "id", bstrap = TRUE,alp = 0.05, min_e=-80, max_e=80)
gas_dyn  <- aggte(readRDS(cs_files_full$Gas), type = "dynamic",
                  na.rm = TRUE, clustervars = "id", bstrap = TRUE,alp = 0.05, min_e=-80, max_e=80)

p_dyn <- create_dynamic_plot(elec_dyn, gas_dyn, elec_color, gas_color)
ggsave("graphs/dynamic_hp_plot_combined.png", plot = p_dyn, width = 10, height = 8, dpi = 300)
checkpoint("Saved graphs/dynamic_hp_plot_combined.png")

# ----------------------------
# Calendar plot (CS) + shaded last-2-years windows + annual sums + 'Empirical efficiency' labels
# ----------------------------
# 'Empirical efficiency' = elec_increase / (0.9 * |gas_decrease|)

checkpoint("Calendar CS plot (electricity + gas) + annual labels + 'Empirical efficiency'")

# ---- 1) Get calendar-time ATTs (weekly) ----
elec_cal <- aggte(readRDS(cs_files_full$Electricity), type = "calendar",
                  na.rm = TRUE, clustervars = "id", bstrap = TRUE,alp = 0.05, min_e=-80, max_e=80)
gas_cal  <- aggte(readRDS(cs_files_full$Gas), type = "calendar",
                  na.rm = TRUE, clustervars = "id", bstrap = TRUE,alp = 0.05, min_e=-80, max_e=80)
                         
                         
# ---- 2) Rolling pre baselines (annualised) ----
pre_elec_df <- rolling_pre_avg_calendar_52w(elec_cal, start_date, "elec_consumption", window_weeks = 52) %>%
  mutate(type = "Electricity") %>%
  select(type, week_date, pre_52w_kwhyr)

pre_gas_df  <- rolling_pre_avg_calendar_52w(gas_cal,  start_date, "gas_consumption",  window_weeks = 52) %>%
  mutate(type = "Gas") %>%
  select(type, week_date, pre_52w_kwhyr)

pre_df <- bind_rows(pre_elec_df, pre_gas_df) %>%
  mutate(week_date = as.Date(week_date)) %>%
  filter(!is.na(pre_52w_kwhyr))
                         
# ---- 3) CS plot data ----

plot_cal_data <- create_calendar_plot_data(start_date, elec_cal, gas_cal) %>%
  mutate(
    week_date = as.Date(week_date)
  ) %>%
  arrange(type, week_date)

# ---- 3) Define two 52-week windows ending at the latest available week ----
max_date <- max(pre_df$week_date, na.rm = TRUE)

w2_start <- max_date - weeks(52) + days(1)   # most recent 52 weeks (window 2)
w2_end   <- max_date

w1_start <- max_date - weeks(104) + days(1)  # previous 52 weeks (window 1)
w1_end   <- max_date - weeks(52)


# Tag weeks into windows
cal_win <- plot_cal_data %>%
  mutate(
    window = case_when(
      week_date >= w1_start & week_date <= w1_end ~ "Prev 12 months",
      week_date >= w2_start & week_date <= w2_end ~ "Last 12 months",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(window))

# Annual sums of weekly ATTs within each window using full covariance:
# se(sum) = sqrt(1' * Var(att_path_window) * 1)
annual_sums <- bind_rows(
  window_annual_summary(elec_cal, start_date, w1_start, w1_end, "Electricity") %>%
    mutate(window = "Prev 12 months"),
  window_annual_summary(gas_cal,  start_date, w1_start, w1_end, "Gas") %>%
    mutate(window = "Prev 12 months"),
  window_annual_summary(elec_cal, start_date, w2_start, w2_end, "Electricity") %>%
    mutate(window = "Last 12 months"),
  window_annual_summary(gas_cal,  start_date, w2_start, w2_end, "Gas") %>%
    mutate(window = "Last 12 months")
)

# Window-specific "pre" baseline (annualised) to express as % of pre
# We take the median pre within each window for stability.
pre_win <- pre_df %>%
  filter(week_date == w2_end | week_date == w1_end) %>%
  mutate(window = case_when(
      week_date >= w1_start & week_date <= w1_end ~ "Prev 12 months",
      week_date >= w2_start & week_date <= w2_end ~ "Last 12 months",
      TRUE ~ NA_character_
    ))
annual_sums <- annual_sums %>%
  left_join(pre_win, by = c("type", "window")) %>%
  mutate(pct_of_pre = 100 * annual_kwh / pre_52w_kwhyr)

# Empirical efficiency per window, computed from the annual sums above
eff_df <- annual_sums %>%
  select(window, type, annual_kwh) %>%
  pivot_wider(names_from = type, values_from = annual_kwh) %>%
  mutate(emp_eff = (0.9 * (-Gas)) / Electricity)
fwrite(eff_df, file.path(datapath, "output/eff_df.csv")) # save main results for later plots

# Also save a copy to the git-tracked public_data/ folder: these are the
# consumption-change results reported in the paper, used as input to
# scripts/05_MVPF.R.
dir.create("public_data", showWarnings = FALSE)
fwrite(eff_df, "public_data/eff_df.csv")
       
# Shading rectangles for the calendar plot (harmonised)
shade_df <- tibble(
  xmin = as.Date(c(w1_start, w2_start)),
  xmax = as.Date(c(w1_end,   w2_end)),
  ymin = -Inf,
  ymax = Inf,
  window = c("Prev 12 months", "Last 12 months")
)

# Label x positions, shifted left of the window midpoint for readability
mid_df <- tibble(
  window = c("Prev 12 months", "Last 12 months"),
  x = as.Date(c(
    w1_start + (w1_end - w1_start)/2 - weeks(12),
    w2_start + (w2_end - w2_start)/2 - weeks(12)
  ))
)

# y bounds for safe label placement
y_max <- max(plot_cal_data$upper_ci, na.rm = TRUE)
y_min <- min(plot_cal_data$lower_ci, na.rm = TRUE)
yrng  <- y_max - y_min
if (!is.finite(yrng) || yrng == 0) yrng <- 1

label_pos <- annual_sums %>%
  left_join(eff_df, by = "window") %>%
  left_join(mid_df, by = "window") %>%
  mutate(
    y = if_else(type == "Electricity", y_max - 0.15*yrng, y_min + 0.15*yrng),
    label = if_else(
      type == "Electricity",
      paste0(
        window, ": ", comma(round(annual_kwh)), " kWh/yr\n",
        "[", comma(round(annual_lo)), ", ", comma(round(annual_hi)), "]\n",
        "Share of pre: ", sprintf("%.1f", pct_of_pre), "%\n",
        "Empirical efficiency ≈ ", sprintf("%.2f", emp_eff)
      ),
      paste0(
        window, ": ", comma(round(annual_kwh)), " kWh/yr\n",
        "[", comma(round(annual_lo)), ", ", comma(round(annual_hi)), "]\n",
        "Share of pre: ", sprintf("%.1f", pct_of_pre), "%"
      )
    )
  )

# Build calendar plot
p_cal <- ggplot(plot_cal_data, aes(x = week_date, y = estimate, colour = type, group = type)) +
  geom_vline(xintercept = w1_end, linetype = "dotted", colour = "grey40") +
  geom_linerange(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.6, linewidth = 0.6) +
  geom_line(linewidth = 0.8) +
  geom_point(aes(shape = type), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_label(
    data = label_pos,
    aes(x = x, y = y, label = label, colour = type),
    inherit.aes = FALSE,
    fill = "white",
    alpha = 0.90,
    label.size = 0.2,
    size = 3.2,
    show.legend = FALSE
  ) +
  scale_x_date(labels = date_format("%b %y"), date_breaks = "3 month") +
  scale_colour_manual(values = c(Electricity = elec_color, Gas = gas_color)) +
  scale_shape_manual(values  = c(Electricity = 16, Gas = 17)) +
  labs(x = "Week", y = "Calendar ATT for Weekly Consumption (kWh)", colour = "Type", shape = "Type") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("graphs/hp_calendarplot_combined_with_annual_labels.png",
       plot = p_cal, width = 10, height = 6, dpi = 300)
                         
                         
                  
                         
                         
# Build calendar plot
p_cal <- ggplot(plot_cal_data, aes(x = week_date, y = estimate, colour = type, group = type)) +
  geom_vline(xintercept = w1_end, linetype = "dotted", colour = "grey40") +
  geom_linerange(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.6, linewidth = 0.6) +
  geom_line(linewidth = 0.8) +
  geom_point(aes(shape = type), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  scale_x_date(labels = date_format("%b %y"), date_breaks = "3 month") +
  scale_colour_manual(values = c(Electricity = elec_color, Gas = gas_color)) +
  scale_shape_manual(values  = c(Electricity = 16, Gas = 17)) +
  labs(x = "Week", y = "Calendar ATT for Weekly Consumption (kWh)", colour = "Type", shape = "Type") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("graphs/hp_calendarplot_combined.png",
       plot = p_cal, width = 10, height = 6, dpi = 300)
                         
checkpoint("Saved graphs/hp_calendarplot_combined_with_annual_labels.png and output/hp_calendarplot_combined.csv")
       
# ----------------------------
# 12m rolling ATT plot + quarterly callouts + COP panel + median label
# ----------------------------

checkpoint("Build 12m rolling plot + quarterly points + COP panel")

# ---- 1) Weekly calendar ATT series -> 12m rolling yearly series ----
plot_data_12m <- add_rolling_12m_sum(
  plot_cal_data,
  cal_objs = list(Electricity = elec_cal, Gas = gas_cal),
  window_weeks = 52
) %>%
  filter(!is.na(estimate_12m)) %>%
  mutate(week_date = as.Date(week_date)) %>%
  arrange(type, week_date)

# Merge pre baseline onto ATT series (for share-of-pre at each date)
plot_data_12m <- plot_data_12m %>%
  left_join(pre_df, by = c("type", "week_date")) %>%
  filter(!is.na(pre_52w_kwhyr)) %>%
  mutate(share_pre = 100 * estimate_12m / pre_52w_kwhyr)

# ---- 3) Quarterly points (every ~3 months) with label: total + share of pre ----
# Create target dates every 3 months within the available period
date_min <- min(plot_data_12m$week_date, na.rm = TRUE)
date_max <- max(plot_data_12m$week_date, na.rm = TRUE)

target_dates <- seq(from = floor_date(date_min, "month"),
                    to   = ceiling_date(date_max, "month"),
                    by   = "3 months")

# For each type and target date, pick the closest observed week_date
quarter_pts <- plot_data_12m %>%
  group_by(type) %>%
  tidyr::crossing(target_date = as.Date(target_dates)) %>%
  mutate(dist_days = abs(as.numeric(week_date - target_date))) %>%
  group_by(type, target_date) %>%
  slice_min(dist_days, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    pt_label = paste0(
      comma(round(estimate_12m)), " kWh/yr\n",
      "Share: ", sprintf("%.1f", share_pre), "%"
    )
  )

# ---- 4) COP time series (from 12m sums) + simple CI (delta approx, no cov) ----
cop_df <- plot_data_12m %>%
  select(type, week_date, estimate_12m, se_12m) %>%
  pivot_wider(names_from = type,
              values_from = c(estimate_12m, se_12m),
              names_sep = "__")

# Implied COP: 0.9 * (-Gas) / Elec
# (Gas is negative, Elec positive; if Elec is near 0, COP will blow up -> drop those points)
cop_ts <- cop_df %>%
  transmute(
    week_date,
    elec = estimate_12m__Electricity,
    gas  = estimate_12m__Gas,
    se_elec = se_12m__Electricity,
    se_gas  = se_12m__Gas,
    cop = (0.9 * (-gas)) / elec,
    # delta approx var(cop) ~ (dC/dE)^2 Var(E) + (dC/dG)^2 Var(G)
    # dC/dE = -0.9*(-gas)/E^2 = -cop/E ; dC/dG = -0.9/E
    se_cop = sqrt( ( (cop/elec)^2 ) * (se_elec^2) + ( (0.9/elec)^2 ) * (se_gas^2) ),
    cop_lo = cop - 1.96 * se_cop,
    cop_hi = cop + 1.96 * se_cop
  ) %>%
  filter(is.finite(cop), is.finite(se_cop), abs(elec) > 1e-6)

# ---- 5) Median label (top panel): median elec/gas + shares + implied COP ----
med_elec <- plot_data_12m %>%
  filter(type == "Electricity") %>%
  summarise(
    med_att = median(estimate_12m, na.rm = TRUE),
    med_pre = median(pre_52w_kwhyr, na.rm = TRUE),
    med_share = 100 * med_att / med_pre
  )

med_gas <- plot_data_12m %>%
  filter(type == "Gas") %>%
  summarise(
    med_att = median(estimate_12m, na.rm = TRUE),
    med_pre = median(pre_52w_kwhyr, na.rm = TRUE),
    med_share = 100 * med_att / med_pre
  )

med_cop <- (0.9 * (-as.numeric(med_gas$med_att))) / as.numeric(med_elec$med_att)

# Label position (right side of the plot)
x_max <- max(plot_data_12m$week_date, na.rm = TRUE)
x_lab <- x_max - weeks(16)

y_max <- max(plot_data_12m$upper_ci_12m, na.rm = TRUE)
y_min <- min(plot_data_12m$lower_ci_12m, na.rm = TRUE)
yrng  <- y_max - y_min
pad_y <- 0.4* ifelse(yrng == 0, 1, yrng)

summary_label <- paste0(
  "Median (Elec): ", comma(round(med_elec$med_att)), " kWh/yr  |  Share: ", sprintf("%.1f", med_elec$med_share), "%\n",
  "Median (Gas): ",  comma(round(med_gas$med_att)),  " kWh/yr  |  Share: ", sprintf("%.1f", med_gas$med_share), "%\n",
  "Empirical efficiency (median): ", sprintf("%.2f", med_cop)
)

label_box <- data.frame(
  x = x_lab,
  y = y_max - pad_y,
  label = summary_label
)

# ---- 6) TOP PANEL: 12m rolling sum with quarterly points ----
minY= min(plot_data_12m$lower_ci_12m)*1.2
maxY =  max(plot_data_12m$upper_ci_12m)*1.2                    
p_top <- ggplot(plot_data_12m, aes(x = week_date, y = estimate_12m, color = type, fill = type, group = type)) +
  geom_ribbon(aes(ymin = lower_ci_12m, ymax = upper_ci_12m), alpha = 0.20, color = NA) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +

  # quarterly callout points
  geom_point(data = quarter_pts, aes(x = week_date, y = estimate_12m, color = type),
             inherit.aes = FALSE, size = 2.8) +
  geom_label(data = quarter_pts,
             aes(x = week_date, y = estimate_12m, label = pt_label, color = type),
             inherit.aes = FALSE,
             fill = "white", alpha = 0.85, label.size = 0, size = 2.8,
             vjust = -0.6, show.legend = FALSE) +

  # overall median label box
  geom_label(data = label_box,
             aes(x = x, y = y, label = label),
             inherit.aes = FALSE,
             fill = "white", alpha = 0.90, label.size = 0,
             hjust = 0, vjust = 1, size = 3.0, color = "black") +

  scale_color_manual(values = c("Electricity" = elec_color, "Gas" = gas_color)) +
  scale_fill_manual(values  = c("Electricity" = elec_color, "Gas" = gas_color)) +
  scale_x_date(labels = date_format("%b %y"), date_breaks = "3 month") +
  scale_y_continuous(limits = c(minY,maxY))+
  labs(x = NULL, y = "ATT (rolling 12-month sum, kWh/year)", color = "Type", fill = "Type") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(5, 60, 5, 5))

# ---- 7) BOTTOM PANEL: implied COP ----
p_cop <- ggplot(cop_ts, aes(x = week_date, y = cop)) +
  geom_ribbon(aes(ymin = cop_lo, ymax = cop_hi), alpha = 0.20) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_date(labels = date_format("%b %y"), date_breaks = "3 month") +
  labs(x = "Week", y = "Empirical Efficiency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(5, 60, 5, 5))

# ---- 8) Stack panels and save ----
p_combined <- p_top / p_cop + plot_layout(heights = c(2.2, 1))

out_file <- file.path("graphs", "calendar_att_12m_with_quarter_points_and_cop.png")
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
ggsave(out_file, plot = p_combined, width = 12, height = 9, dpi = 300)

message("Saved ", out_file)
checkpoint("DONE: 12m plot + quarterly points + COP panel")  
                         
# ----------------------------
# TWFE models + combined TWFE/CS LaTeX table (patched)
# ----------------------------

checkpoint("TWFE models + TWFE/CS combined LaTeX table")

# Fitstat helpers (register once). Named distinctly from the generic
# pre_avg/t_obs registered in 02_00_heatpump.R -- these read fixef_vars[2]
# because the TWFE models below have only two fixed effects (account_id +
# settlement_week), unlike the generic 3-FE (hdd + account_id + date) models
# elsewhere. fitstat_register() is a global, session-wide side effect not
# reset by the per-script environment cleanup, so reusing the shared
# "pre_avg"/"t_obs" names here would silently corrupt those fitstats for
# every sub-script sourced after this one (e.g. 02_06_ev_ownership.R).
fitstat_register("pre_avg_2fe", function(x) {
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
  format_decimal_local(pre_avg, digits = 1)
}, "Pre-Treatment Consumption")

fitstat_register("t_obs_2fe", function(x) {
  t_var <- x$fixef_vars[2]
  format_number_local(x$fixef_sizes[t_var])
}, "Number of Time Periods")

# Build week / firstweek and the anticipation=4 treatment indicator
overall_weekly_fe <-  overall_weekly %>%
  ungroup() %>%
  mutate(
    week = as.numeric(difftime(settlement_week, start_date, units = "weeks")) %/% 1 + 1,
    firstweek = as.numeric(difftime(installed_at, start_date, units = "weeks")) %/% 1 + 1
  ) %>%
  group_by(account_id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  filter(week < firstweek - 4 | week >= firstweek)
                         

# TWFE models (filtered to DID ids)
m1 <- feols(elec_consumption ~ i(is_hp_installed) | account_id  + settlement_week ,
            data = overall_weekly_fe %>% filter(account_id %in% ids_cs_elec),
            fixef.rm = "none", 
            cluster = ~account_id)

m2 <- feols(gas_consumption ~ i(is_hp_installed) |  account_id  + settlement_week ,
            data = overall_weekly_fe %>% filter(account_id %in% ids_cs_gas),
            fixef.rm = "none", 
            cluster = ~account_id)

# CS stats for patching (use *formatted strings* for LaTeX injection)
main_periods <- c("Electricity", "Gas")
cs_estimates <- list(
  Electricity = format_decimal_local(aggte_simple_elec$overall.att, 1),
  Gas         = format_decimal_local(aggte_simple_gas$overall.att, 1)
)
cs_se <- list(
  Electricity = format_decimal_local(aggte_simple_elec$overall.se, 1),
  Gas         = format_decimal_local(aggte_simple_gas$overall.se, 1)
)
cs_n <- list(
  Electricity = format_number_local(aggte_simple_elec$DIDparams$id_count),
  Gas         = format_number_local(aggte_simple_gas$DIDparams$id_count)
)
cs_nG <- list(
  Electricity = format_number_local(aggte_simple_elec$DIDparams$treated_groups_count),
  Gas         = format_number_local(aggte_simple_gas$DIDparams$treated_groups_count)
)
cs_nT <- list(
  Electricity = format_number_local(aggte_simple_elec$DIDparams$time_periods_count),
  Gas         = format_number_local(aggte_simple_gas$DIDparams$time_periods_count)
)

# Create the initial 4-model table: (TWFE Elec, TWFE Gas, placeholder, placeholder)
etable(
  m1, m2,
  m1, m2,
  fitstat = ~ N + g + pre_avg_2fe + t_obs_2fe + r2,
  headers = list(
    list("TWFE" = 2, "CS" = 2),
    list(rep(c("Electricity", "Gas"), times = 2))
  ),
  dict = c("id" = "Household", "week" = "Week"),
  depvar = FALSE,
  tex = TRUE,
  title = "HP Installation on Energy Consumption in kWh",
  file = "tables/hp_did_overall_detailed.tex",
  replace = TRUE,
  label = "tab:hp-did-overall-conso-detailed",
  style.tex = style.tex(tpt = TRUE)
)

# Reposition the pre-treatment average row in the LaTeX table
CleanPreAverage("tables/hp_did_overall_detailed.tex")

# Patch CS values into the right-hand two columns (cols 4 and 5)
patch_etable_twfe_cs(
  file_path = "tables/hp_did_overall_detailed.tex",
  cs_estimates = cs_estimates,
  cs_pre =  c(Electricity = pre_elec, Gas = pre_gas),
  cs_se = cs_se,
  cs_n = cs_n,
  cs_nG = cs_nG,
  cs_nT = cs_nT,
  coef_pattern = "Is HP Installed \\$=\\$ 1"
)

checkpoint("Saved tables/hp_did_overall_detailed.tex (patched)")

# ----------------------------
# NEVER-TREATED robustness: TWFE + CS detailed table (patched)
# ----------------------------
# Output: tables/hp_did_never_treated_detailed.tex

checkpoint("NEVER-TREATED: build DID index and models")

# CS files for never-treated robustness
cs_files_never <- list(
  Electricity = file.path(datapath, "scratch/est_cs_never_treated_elec_weekly.RDS"),
  Gas         = file.path(datapath, "scratch/est_cs_never_treated_gas_weekly.RDS")
)

aggte_simple_elec_never <- aggte(readRDS(cs_files_never$Electricity), type = "simple",
                                 na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.05, min_e=-80, max_e=80)
aggte_simple_gas_never  <- aggte(readRDS(cs_files_never$Gas), type = "simple",
                                 na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.05, min_e=-80, max_e=80)

# Pre-treatment means (use DIDparams$data safely)
pre_elec_never <- pre_avg_from_aggte(aggte_simple_elec_never, "elec_consumption")
pre_gas_never  <- pre_avg_from_aggte(aggte_simple_gas_never,  "gas_consumption")

                         
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
  mutate(firstweek = ifelse(firstweek > 129, 0, firstweek)) %>%
  filter(week < firstweek - 4 | week >= firstweek)


# TWFE models, using the same date-window cutoff as the main specification
m1_never <- feols(
  elec_consumption ~ i(is_hp_installed) | account_id  + settlement_week,
  data = did_data_never %>% filter(id %in% unique(aggte_simple_elec_never$DIDparams$data$id)),
  cluster = ~account_id
)

m2_never <- feols(
  gas_consumption ~ i(is_hp_installed) | account_id + settlement_week,
  data = did_data_never %>% filter(id %in% unique(aggte_simple_gas_never$DIDparams$data$id)),
  cluster = ~account_id
)

checkpoint("NEVER-TREATED: load CS results")

# Statistics used for patching the CS columns (stored as formatted strings for LaTeX)
cs_estimates_never <- list(
  Electricity = format_decimal_local(aggte_simple_elec_never$overall.att, 1),
  Gas         = format_decimal_local(aggte_simple_gas_never$overall.att, 1)
)
cs_se_never <- list(
  Electricity = format_decimal_local(aggte_simple_elec_never$overall.se, 1),
  Gas         = format_decimal_local(aggte_simple_gas_never$overall.se, 1)
)
cs_n_never <- list(
  Electricity = format_number_local(aggte_simple_elec_never$DIDparams$id_count),
  Gas         = format_number_local(aggte_simple_gas_never$DIDparams$id_count)
)
cs_nG_never <- list(
  Electricity = format_number_local(aggte_simple_elec_never$DIDparams$treated_groups_count),
  Gas         = format_number_local(aggte_simple_gas_never$DIDparams$treated_groups_count)
)
cs_nT_never <- list(
  Electricity = format_number_local(aggte_simple_elec_never$DIDparams$time_periods_count),
  Gas         = format_number_local(aggte_simple_gas_never$DIDparams$time_periods_count)
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
  title = "HP Installation on Energy Consumption in kWh (Never-treated)",
  fitstat = ~ N + g + pre_avg_2fe + t_obs_2fe + r2,
  file = "tables/hp_did_never_treated_detailed.tex",
  replace = TRUE,
  label = "tab:hp-did-never-treated-conso-detailed",
  style.tex = style.tex(tpt = TRUE)
)

# Reposition the pre-treatment average row in the LaTeX table
CleanPreAverage("tables/hp_did_never_treated_detailed.tex")

# Patch CS columns (cols 4 & 5) + add cohorts row etc.
patch_etable_twfe_cs(
  file_path = "tables/hp_did_never_treated_detailed.tex",
  cs_estimates = cs_estimates_never,
  cs_pre =  c(Electricity = pre_elec_never, Gas = pre_gas_never),
  cs_se = cs_se_never,
  cs_n = cs_n_never,
  cs_nG = cs_nG_never,
  cs_nT = cs_nT_never,
  coef_pattern = "Is HP Installed \\$=\\$ 1"
)

checkpoint("Saved tables/hp_did_never_treated_detailed.tex (patched)")
                   
   
# ----------------------------
# Compare simple CS estimates using various anticipation periods
# ----------------------------

checkpoint("Plotting anticipation graph")                         
                         
                         
# Define paths and base filenames for each anticipation period
output_base_path <- file.path(datapath, "scratch")
output_filenames <- c("est_cs_elec_weekly", "est_cs_gas_weekly")
anticipation_periods <- 0:10  # The range of anticipation periods

# Define a function to process each anticipation week
process_week <- function(anticipation_week) {
  # File paths
  elec_file <- file.path(output_base_path, paste0(output_filenames[1], "_anticipation_", anticipation_week, ".RDS"))
  gas_file  <- file.path(output_base_path, paste0(output_filenames[2], "_anticipation_", anticipation_week, ".RDS"))
  
  # Read and calculate aggregate estimates
  elec_agg <- aggte(readRDS(elec_file), type = "simple", na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.05, min_e=-80, max_e=80)
  gas_agg  <- aggte(readRDS(gas_file), type = "simple", na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.05,  min_e=-80, max_e=80)
  
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
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("Electricity" = elec_color, "Gas" = gas_color)) +
  scale_fill_manual(values = c("Electricity" = elec_color, "Gas" = gas_color)) +
  labs(
    x = "Anticipation Week",
    y = "Estimate",
    color = "Type",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("graphs/HP_anticipation.png") 
                         
checkpoint("Anticipation graph saved: graphs/HP_anticipation.png")                         

# ----------------------------
# Dynamic CS plot (electricity + gas) for anticipation = 0..10
# ----------------------------
# Saves one combined plot per anticipation period

checkpoint("Dynamic CS plots by anticipation (electricity + gas)")

# ---- Uses colors and datapath already set earlier in the pipeline ----
anticipation_periods <- 0:10

# Location of the CS estimation objects (same naming convention as earlier)
cs_base_dir <- file.path(datapath, "scratch")
graphs_dir  <- file.path("graphs")
if (!dir.exists(graphs_dir)) dir.create(graphs_dir, recursive = TRUE)

# Helper: build CS file paths for a given anticipation value
get_cs_files <- function(a) {
  list(
    Electricity = file.path(cs_base_dir, paste0("est_cs_elec_weekly_anticipation_", a, ".RDS")),
    Gas         = file.path(cs_base_dir, paste0("est_cs_gas_weekly_anticipation_", a, ".RDS"))
  )
}

# Helper: safe file existence check with informative error
assert_exists <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  invisible(TRUE)
}

# Loop over anticipation periods and save plots
for (a in anticipation_periods) {

  cs_files <- get_cs_files(a)

  # Check that the underlying CS estimation objects exist
  assert_exists(cs_files$Electricity)
  assert_exists(cs_files$Gas)

  checkpoint(paste0("Dynamic CS plot for anticipation = ", a))

  # Dynamic aggregation (event-study)
  elec_dyn <- aggte(
    readRDS(cs_files$Electricity),
    type = "dynamic",
    na.rm = TRUE,
    clustervars = "id",
    bstrap = TRUE,
    min_e = -80,
    max_e = 80
  )

  gas_dyn <- aggte(
    readRDS(cs_files$Gas),
    type = "dynamic",
    na.rm = TRUE,
    clustervars = "id",
    bstrap = TRUE,
    min_e = -80,
    max_e = 80
  )

  # Create combined plot using create_dynamic_plot()
  p_dyn <- create_dynamic_plot(elec_dyn, gas_dyn, elec_color, gas_color) +
    labs(
      title = paste0("Dynamic ATT (CS) by anticipation = ", a),
      subtitle = "Electricity and gas"
    )

  out_file <- file.path(graphs_dir, paste0("dynamic_hp_plot_combined_anticipation_", a, ".png"))

  ggsave(out_file, plot = p_dyn, width = 10, height = 8, dpi = 300)

  checkpoint(paste0("Saved ", out_file))
}

checkpoint("Finished saving dynamic CS plots for anticipation = 0..10")
    
# ----------------------------
# Calendar CS plots for anticipation = 0..10 (weekly only)
# ----------------------------
# Saves: graphs/calendar_weekly_anticipation_<a>.png
#        output/hp_calendarplot_weekly_anticipation_<a>.csv

anticipation_periods <- 0:10
cs_base_dir <- file.path(datapath, "scratch")
graphs_dir  <- file.path("graphs")
dir.create(graphs_dir, recursive = TRUE, showWarnings = FALSE)

for (a in anticipation_periods) {

  checkpoint(paste0("Calendar plot (anticipation = ", a, ")"))

  elec_path <- file.path(cs_base_dir, paste0("est_cs_elec_weekly_anticipation_", a, ".RDS"))
  gas_path  <- file.path(cs_base_dir, paste0("est_cs_gas_weekly_anticipation_", a, ".RDS"))

  if (!file.exists(elec_path)) stop("Missing file: ", elec_path)
  if (!file.exists(gas_path))  stop("Missing file: ", gas_path)

  # Calendar aggregation
  elec_cal <- aggte(readRDS(elec_path), type = "calendar",
                    na.rm = TRUE, clustervars = "id", bstrap = TRUE,  min_e=-80, max_e=80)
  gas_cal  <- aggte(readRDS(gas_path),  type = "calendar",
                    na.rm = TRUE, clustervars = "id", bstrap = TRUE,  min_e=-80, max_e=80)

  # Build plotting DF (weekly)
  plot_cal_data <- create_calendar_plot_data(start_date, elec_cal, gas_cal) %>%
    mutate(week_date = as.Date(week_date)) %>%
    arrange(type, week_date)

  # Plot (error bars match type colour)
  p_cal <- ggplot(plot_cal_data, aes(x = week_date, y = estimate, group = type, colour = type)) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 7, alpha = 0.6, linewidth = 0.6) +
    geom_line(linewidth = 0.8) +
    geom_point(aes(shape = type), size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
    scale_x_date(labels = scales::date_format("%b %y"), date_breaks = "3 month") +
    scale_colour_manual(values = c("Electricity" = elec_color, "Gas" = gas_color)) +
    scale_shape_manual(values = c(Electricity = 16, Gas = 17)) +
    labs(
      title = paste0("Calendar ATT (weekly) — anticipation = ", a),
      x = "Week",
      y = "Calendar ATT for Weekly Consumption (kWh)",
      colour = "Type",
      shape  = "Type"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1))

  # Save outputs
  out_png <- file.path(graphs_dir, paste0("hp_calendarplot_combined_anticipation_", a, ".png"))
  out_csv <- file.path(datapath, "output", paste0("hp_calendarplot_combined_anticipation_", a, ".csv"))

  ggsave(out_png, plot = p_cal, width = 8, height = 6, dpi = 300)

  checkpoint(paste0("Saved ", out_png, " and ", out_csv))
}
                         
# ----------------------------
# Dynamic plots with trends
# ----------------------------

checkpoint("Dynamic plot for monthly trends")

# ---- CS files (trend sample) ----
cs_files_trends <- list(
  Electricity = file.path(datapath, "scratch/est_cs_elec_weekly_with_trends.RDS"),
  Gas         = file.path(datapath, "scratch/est_cs_gas_weekly_with_trends.RDS")
)
                         
elec_dyn <- aggte(readRDS(cs_files_trends$Electricity), type = "dynamic",
                  na.rm = TRUE, clustervars = "id", bstrap = TRUE, min_e = -80, max_e = 80)
gas_dyn  <- aggte(readRDS(cs_files_trends$Gas), type = "dynamic",
                  na.rm = TRUE, clustervars = "id", bstrap = TRUE, min_e = -80, max_e = 80)

p_dyn <- create_dynamic_plot(elec_dyn, gas_dyn, elec_color, gas_color)
ggsave("graphs/dynamic_hp_plot_combined_with_trends.png", plot = p_dyn, width = 10, height = 8, dpi = 300)
checkpoint("Saved graphs/dynamic_hp_plot_combined_with_trends.png")
                         
                         
                         
checkpoint("DONE")
