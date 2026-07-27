# ============================================================
# Gas-only sample robustness check
#
# Outputs (gas-only specific):
# - data/output/eff_df_gas_only.csv
# - graphs/hp_calendarplot_combined_with_annual_labels_gas_only.png
# - graphs/calendar_att_12m_with_quarter_points_and_cop_gas_only.png
# - graphs/quasi_cop_gas_only.png
#
# Optional: set WRITE_MAIN_FILENAMES=1 to also overwrite main filenames
# (eff_df.csv, hp_calendarplot_combined_with_annual_labels.png,
#  calendar_att_12m_with_quarter_points_and_cop.png, quasi_cop.png).
# ============================================================


write_main <- identical(Sys.getenv("WRITE_MAIN_FILENAMES", unset = "0"), "1")
B_bootstrap <- as.integer(Sys.getenv("COP_BOOT_B", unset = "500"))
if (is.na(B_bootstrap) || B_bootstrap < 10) B_bootstrap <- 500

# ----------------------------
# Helpers
# ----------------------------
clean_didparams_data <- function(x) {
  nms <- names(x)
  if (anyDuplicated(nms)) {
    if ("data.table" %in% class(x)) {
      x <- x[, nms[!duplicated(nms)], with = FALSE]
    } else {
      x <- x[, !duplicated(nms), drop = FALSE]
    }
  }
  as.data.frame(x)
}

calendar_vcov <- function(cal_obj, scale = 1) {
  extract_inf_fun <- function(x, k) {
    if (is.null(x)) return(NULL)

    if (is.list(x)) {
      for (nm in c("calendar.inf.func.t", "calendar.inf.func.e", "egt.inf.func", "att.inf.func.e")) {
        if (!is.null(x[[nm]])) return(extract_inf_fun(x[[nm]], k))
      }
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

  message("calendar_vcov: using diagonal fallback (off-diagonal covariance unavailable).")
  diag((scale * as.numeric(cal_obj$se.egt))^2)
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
      .groups = "drop"
    ) %>%
    arrange(week) %>%
    mutate(
      week_date = as.Date(start_date + weeks(week)),
      pre_avg_52w = zoo::rollapply(
        pre_avg_weekly, width = window_weeks, FUN = mean,
        align = "right", fill = NA, na.rm = TRUE
      ),
      pre_52w_kwhyr = pre_avg_52w
    )
}

window_annual_summary <- function(cal_obj, start_date, w_start, w_end, type_label, scale = 1 / 52.25) {
  week_dates <- as.Date(start_date) + weeks(as.numeric(cal_obj$egt))
  idx <- which(week_dates >= w_start & week_dates <= w_end)

  annual_kwh <- sum(as.numeric(cal_obj$att.egt[idx]) * scale, na.rm = TRUE)
  V <- calendar_vcov(cal_obj, scale = scale)
  annual_se <- sqrt(sum(V[idx, idx, drop = FALSE]))

  tibble(
    type = type_label,
    annual_kwh = annual_kwh,
    annual_se = annual_se,
    annual_lo = annual_kwh - 1.96 * annual_se,
    annual_hi = annual_kwh + 1.96 * annual_se,
    count_obs = length(idx)
  )
}

add_rolling_12m_sum <- function(df, cal_objs, window_weeks = 52, scale = 1 / 52.25) {
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

  out <- df %>%
    group_split(type) %>%
    lapply(function(dsub) {
      type_name <- as.character(dsub$type[1])
      cal_obj <- cal_objs[[type_name]]
      if (is.null(cal_obj)) stop("Missing calendar object for type: ", type_name)

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

# ----------------------------
# Inputs
# ----------------------------
checkpoint("Load gas-only CS files")

cs_files_gas_only <- list(
  Electricity = file.path(datapath, "scratch/est_cs_elec_weekly_gas_only.RDS"),
  Gas         = file.path(datapath, "scratch/est_cs_gas_weekly.RDS")
)

missing_files <- cs_files_gas_only[!file.exists(unlist(cs_files_gas_only))]
if (length(missing_files) > 0) {
  stop("Missing required gas-only CS files: ", paste(unlist(missing_files), collapse = ", "))
}

overall_weekly_path <- file.path(datapath, "output/overall_weekly.rds")
if (!file.exists(overall_weekly_path)) stop("Missing: ", overall_weekly_path)
overall_weekly <- read_rds(overall_weekly_path)
start_date <- min(overall_weekly$settlement_week)

# ----------------------------
# Calendar + annual labels (gas-only)
# ----------------------------
checkpoint("Build gas-only calendar ATT objects")

elec_cal <- did::aggte(readRDS(cs_files_gas_only$Electricity), type = "calendar",
                       na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.05,
                       min_e = -80, max_e = 80)
gas_cal  <- did::aggte(readRDS(cs_files_gas_only$Gas), type = "calendar",
                       na.rm = TRUE, clustervars = "id", bstrap = TRUE, alp = 0.05,
                       min_e = -80, max_e = 80)

pre_elec_df <- rolling_pre_avg_calendar_52w(elec_cal, start_date, "elec_consumption", window_weeks = 52) %>%
  mutate(type = "Electricity") %>%
  select(type, week_date, pre_52w_kwhyr)

pre_gas_df  <- rolling_pre_avg_calendar_52w(gas_cal, start_date, "gas_consumption", window_weeks = 52) %>%
  mutate(type = "Gas") %>%
  select(type, week_date, pre_52w_kwhyr)

pre_df <- bind_rows(pre_elec_df, pre_gas_df) %>%
  mutate(week_date = as.Date(week_date)) %>%
  filter(!is.na(pre_52w_kwhyr))

plot_cal_data <- create_calendar_plot_data(start_date, elec_cal, gas_cal) %>%
  mutate(week_date = as.Date(week_date)) %>%
  arrange(type, week_date)

max_date <- max(pre_df$week_date, na.rm = TRUE)
w2_start <- max_date - weeks(52) + days(1)
w2_end   <- max_date
w1_start <- max_date - weeks(104) + days(1)
w1_end   <- max_date - weeks(52)

annual_sums <- bind_rows(
  window_annual_summary(elec_cal, start_date, w1_start, w1_end, "Electricity") %>% mutate(window = "Prev 12 months"),
  window_annual_summary(gas_cal,  start_date, w1_start, w1_end, "Gas") %>% mutate(window = "Prev 12 months"),
  window_annual_summary(elec_cal, start_date, w2_start, w2_end, "Electricity") %>% mutate(window = "Last 12 months"),
  window_annual_summary(gas_cal,  start_date, w2_start, w2_end, "Gas") %>% mutate(window = "Last 12 months")
)

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

eff_df_gas_only <- annual_sums %>%
  select(window, type, annual_kwh) %>%
  pivot_wider(names_from = type, values_from = annual_kwh) %>%
  mutate(emp_eff = (0.9 * (-Gas)) / Electricity)

fwrite(eff_df_gas_only, file.path(datapath, "output/eff_df_gas_only.csv"))
if (write_main) fwrite(eff_df_gas_only, file.path(datapath, "output/eff_df.csv"))

mid_df <- tibble(
  window = c("Prev 12 months", "Last 12 months"),
  x = as.Date(c(
    w1_start + (w1_end - w1_start) / 2 - weeks(12),
    w2_start + (w2_end - w2_start) / 2 - weeks(12)
  ))
)

y_max <- max(plot_cal_data$upper_ci, na.rm = TRUE)
y_min <- min(plot_cal_data$lower_ci, na.rm = TRUE)
yrng  <- y_max - y_min
if (!is.finite(yrng) || yrng == 0) yrng <- 1

label_pos <- annual_sums %>%
  left_join(eff_df_gas_only, by = "window") %>%
  left_join(mid_df, by = "window") %>%
  mutate(
    y = if_else(type == "Electricity", y_max - 0.15 * yrng, y_min + 0.15 * yrng),
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
  scale_x_date(labels = scales::date_format("%b %y"), date_breaks = "3 month") +
  scale_colour_manual(values = c(Electricity = elec_color, Gas = gas_color)) +
  scale_shape_manual(values = c(Electricity = 16, Gas = 17)) +
  labs(x = "Week", y = "Calendar ATT for Weekly Consumption (kWh)", colour = "Type", shape = "Type") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("graphs/hp_calendarplot_combined_with_annual_labels_gas_only.png", plot = p_cal, width = 10, height = 6, dpi = 300)
if (write_main) ggsave("graphs/hp_calendarplot_combined_with_annual_labels.png", plot = p_cal, width = 10, height = 6, dpi = 300)

# ----------------------------
# 12m rolling + COP panel (gas-only)
# ----------------------------
checkpoint("Build gas-only 12m rolling ATT + COP panel")

plot_data_12m <- add_rolling_12m_sum(
  plot_cal_data,
  cal_objs = list(Electricity = elec_cal, Gas = gas_cal),
  window_weeks = 52
) %>%
  filter(!is.na(estimate_12m)) %>%
  mutate(week_date = as.Date(week_date)) %>%
  arrange(type, week_date)

plot_data_12m <- plot_data_12m %>%
  left_join(pre_df, by = c("type", "week_date")) %>%
  filter(!is.na(pre_52w_kwhyr)) %>%
  mutate(share_pre = 100 * estimate_12m / pre_52w_kwhyr)

# Quarterly labels
date_min <- min(plot_data_12m$week_date, na.rm = TRUE)
date_max <- max(plot_data_12m$week_date, na.rm = TRUE)
target_dates <- seq(from = floor_date(date_min, "month"), to = ceiling_date(date_max, "month"), by = "3 months")

quarter_pts <- plot_data_12m %>%
  group_by(type) %>%
  tidyr::crossing(target_date = as.Date(target_dates)) %>%
  mutate(dist_days = abs(as.numeric(week_date - target_date))) %>%
  group_by(type, target_date) %>%
  slice_min(dist_days, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(pt_label = paste0(comma(round(estimate_12m)), " kWh/yr\n", "Share: ", sprintf("%.1f", share_pre), "%"))

cop_df <- plot_data_12m %>%
  select(type, week_date, estimate_12m, se_12m) %>%
  pivot_wider(names_from = type, values_from = c(estimate_12m, se_12m), names_sep = "__")

cop_ts <- cop_df %>%
  transmute(
    week_date,
    elec = estimate_12m__Electricity,
    gas  = estimate_12m__Gas,
    se_elec = se_12m__Electricity,
    se_gas  = se_12m__Gas,
    cop = (0.9 * (-gas)) / elec,
    se_cop = sqrt(((cop / elec)^2) * (se_elec^2) + ((0.9 / elec)^2) * (se_gas^2)),
    cop_lo = cop - 1.96 * se_cop,
    cop_hi = cop + 1.96 * se_cop
  ) %>%
  filter(is.finite(cop), is.finite(se_cop), abs(elec) > 1e-6)

med_elec <- plot_data_12m %>%
  filter(type == "Electricity") %>%
  summarise(med_att = median(estimate_12m, na.rm = TRUE), med_pre = median(pre_52w_kwhyr, na.rm = TRUE), med_share = 100 * med_att / med_pre)

med_gas <- plot_data_12m %>%
  filter(type == "Gas") %>%
  summarise(med_att = median(estimate_12m, na.rm = TRUE), med_pre = median(pre_52w_kwhyr, na.rm = TRUE), med_share = 100 * med_att / med_pre)

med_cop <- (0.9 * (-as.numeric(med_gas$med_att))) / as.numeric(med_elec$med_att)

x_max <- max(plot_data_12m$week_date, na.rm = TRUE)
x_lab <- x_max - weeks(16)

y_max <- max(plot_data_12m$upper_ci_12m, na.rm = TRUE)
y_min <- min(plot_data_12m$lower_ci_12m, na.rm = TRUE)
yrng <- y_max - y_min
pad_y <- 0.4 * ifelse(yrng == 0, 1, yrng)

summary_label <- paste0(
  "Median (Elec): ", comma(round(med_elec$med_att)), " kWh/yr  |  Share: ", sprintf("%.1f", med_elec$med_share), "%\n",
  "Median (Gas): ",  comma(round(med_gas$med_att)),  " kWh/yr  |  Share: ", sprintf("%.1f", med_gas$med_share), "%\n",
  "Empirical efficiency (median): ", sprintf("%.2f", med_cop)
)

label_box <- data.frame(x = x_lab, y = y_max - pad_y, label = summary_label)

minY <- min(plot_data_12m$lower_ci_12m) * 1.2
maxY <- max(plot_data_12m$upper_ci_12m) * 1.2

p_top <- ggplot(plot_data_12m, aes(x = week_date, y = estimate_12m, color = type, fill = type, group = type)) +
  geom_ribbon(aes(ymin = lower_ci_12m, ymax = upper_ci_12m), alpha = 0.20, color = NA) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_point(data = quarter_pts, aes(x = week_date, y = estimate_12m, color = type), inherit.aes = FALSE, size = 2.8) +
  geom_label(
    data = quarter_pts,
    aes(x = week_date, y = estimate_12m, label = pt_label, color = type),
    inherit.aes = FALSE,
    fill = "white", alpha = 0.85, label.size = 0, size = 2.8, vjust = -0.6,
    show.legend = FALSE
  ) +
  geom_label(
    data = label_box,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    fill = "white", alpha = 0.90, label.size = 0, hjust = 0, vjust = 1, size = 3.0, color = "black"
  ) +
  scale_color_manual(values = c("Electricity" = elec_color, "Gas" = gas_color)) +
  scale_fill_manual(values = c("Electricity" = elec_color, "Gas" = gas_color)) +
  scale_x_date(labels = scales::date_format("%b %y"), date_breaks = "3 month") +
  scale_y_continuous(limits = c(minY, maxY)) +
  labs(x = NULL, y = "ATT (rolling 12-month sum, kWh/year)", color = "Type", fill = "Type") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = margin(5, 60, 5, 5))

p_cop <- ggplot(cop_ts, aes(x = week_date, y = cop)) +
  geom_ribbon(aes(ymin = cop_lo, ymax = cop_hi), alpha = 0.20) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_date(labels = scales::date_format("%b %y"), date_breaks = "3 month") +
  labs(x = "Week", y = "Empirical Efficiency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = margin(5, 60, 5, 5))

p_combined <- p_top / p_cop + plot_layout(heights = c(2.2, 1))

ggsave("graphs/calendar_att_12m_with_quarter_points_and_cop_gas_only.png", plot = p_combined, width = 12, height = 9, dpi = 300)
if (write_main) ggsave("graphs/calendar_att_12m_with_quarter_points_and_cop.png", plot = p_combined, width = 12, height = 9, dpi = 300)

# ----------------------------
# quasi_cop plot (gas-only)
# ----------------------------
checkpoint("Build quasi_cop plot for gas-only sample")

# IDs from the gas model's own CS estimation sample -- not the electricity
# model's, so the temperature-interaction sample matches the same households
# behind the main gas ATT figure rather than whichever households survived a
# different outcome's att_gt fit.
ids_cs_gas <- readRDS(file.path(datapath, "scratch/ids_cs_gas.RS"))

overall_weekly_cop <- read_rds(overall_weekly_path) %>%
  mutate_at(vars(elec_consumption, gas_consumption, total_consumption), ~ .x / 52.25) %>%
  filter(account_id %in% ids_cs_gas)

start_date_cop <- min(overall_weekly_cop$settlement_week)
overall_weekly_cop <- overall_weekly_cop %>%
  ungroup() %>%
  mutate(
    week = as.numeric(difftime(settlement_week, start_date_cop, units = "weeks")) %/% 1 + 1,
    firstweek = as.numeric(difftime(installed_at, start_date_cop, units = "weeks")) %/% 1 + 1
  ) %>%
  group_by(account_id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  filter(week <= 129, firstweek <= 129) %>%
  filter(week < firstweek - 4 | week >= firstweek)

required_vars <- c("temp_degree", "is_hp_installed", "account_id", "settlement_week", "elec_consumption", "gas_consumption")
missing_vars <- setdiff(required_vars, names(overall_weekly_cop))
if (length(missing_vars) > 0) stop("Missing variables for quasi COP: ", paste(missing_vars, collapse = ", "))

tempreg <- feols(
  c(elec_consumption, gas_consumption) ~ i(is_hp_installed, temp_degree, ref = 0) |
    account_id + temp_degree + settlement_week,
  data = overall_weekly_cop,
  cluster = ~account_id
)

# Readable labels
temp_levels <- 0:25
temp_labels <- as.character(temp_levels)
temp_labels[temp_levels == 0]  <- "≤ 0°C"
temp_labels[temp_levels == 25] <- "≥ 25°C"
temp_labels[!(temp_levels %in% c(0, 25))] <- paste0(temp_levels[!(temp_levels %in% c(0, 25))], "°C")

cop_boot_path <- file.path(datapath, "scratch/cop_boot_gas_only.csv")

if (!file.exists(cop_boot_path)) {
  set.seed(123456789)
  B <- B_bootstrap
  results <- vector("list", B)
  pb <- progress_bar$new(total = B, format = "Bootstrapping gas-only [:bar] :percent ETA: :eta")

  for (b in seq_len(B)) {
    pb$tick()

    sampled_ids <- sample(unique(overall_weekly_cop$account_id), replace = TRUE)
    boot_data <- overall_weekly_cop %>% inner_join(data.frame(account_id = sampled_ids), by = "account_id")

    boot_model <- feols(
      c(elec_consumption, gas_consumption) ~ i(is_hp_installed, temp_degree, ref = 0) |
        account_id + temp_degree + settlement_week,
      data = boot_data,
      cluster = ~account_id,
      lean = TRUE
    )

    boot_coefs <- coeftable(boot_model) %>%
      data.frame() %>%
      separate(coefficient, into = c("is_hp_installed", "remove1", "temp", "remove2"), sep = "::") %>%
      select(lhs, Estimate, temp) %>%
      pivot_wider(names_from = lhs, values_from = Estimate) %>%
      mutate(quasi_cop = abs(0.9 * gas_consumption / elec_consumption)) %>%
      select(temp, quasi_cop)

    results[[b]] <- boot_coefs
  }

  cop_boot <- bind_rows(results, .id = "bootstrap") %>%
    mutate(
      degree = factor(temp, levels = temp_levels, labels = temp_labels, ordered = TRUE)
    ) %>%
    group_by(temp, degree) %>%
    summarise(
      lower = quantile(quasi_cop, 0.025, na.rm = TRUE),
      upper = quantile(quasi_cop, 0.975, na.rm = TRUE),
      median = median(quasi_cop, na.rm = TRUE),
      .groups = "drop"
    )

  fwrite(cop_boot, cop_boot_path)
} else {
  cop_boot <- fread(cop_boot_path)
}

cop_boot <- cop_boot %>%
  mutate(
    degree = factor(temp, levels = temp_levels, labels = temp_labels, ordered = TRUE)
  )

main_results <- eff_df_gas_only %>% filter(window == "Last 12 months")
avg_cop <- round(main_results$emp_eff, digits = 2)

# Reference lines
ashp_cop <- data.frame(
  temp_f = c(-20, -10, 0, 10, 20, 30, 40, 50, 60),
  cop = c(1.8, 1.8, 1.9, 2.1, 2.4, 2.7, 3.1, 3.5, 3.9)
) %>% mutate(temp_c = (temp_f - 32) * 5 / 9)

target_temps <- tibble(temp_c = seq(0, 15, by = 1))
ashp_interp_df <- target_temps %>% mutate(cop = approx(x = ashp_cop$temp_c, y = ashp_cop$cop, xout = temp_c)$y)

brattle_cop <- tibble(temp_c = seq(0, 15, by = 1)) %>%
  mutate(temp_f = temp_c * 9 / 5 + 32, cop = 1.2 + 0.05 * temp_f)

cop_reference_lines <- bind_rows(
  ashp_interp_df %>% mutate(source = "EPRI"),
  brattle_cop %>% select(temp_c, cop) %>% mutate(source = "Brattle")
)
                        
cop_boot_plot <- cop_boot %>%
  mutate(
    temp_num = as.integer(as.character(temp)),
    degree = factor(temp_num, levels = temp_levels, labels = temp_labels, ordered = TRUE),
    upper_plot = if_else(degree == "15°C", pmin(upper, 6), upper)
) %>%
  filter(temp_num <= 15)                       

p_quasi <- ggplot(cop_boot_plot, aes(x = degree, y = median)) +
  geom_col(alpha = 0.6, fill = hp_color) +
  geom_hline(yintercept = 3.49, linetype = "dashed", color = hp_color) +
  geom_errorbar(aes(ymin = lower, ymax = upper_plot), width = 0.2, color = hp_color) +
  annotate("text", x = "15°C", y = 6.1, label = "truncated", size = 2, color = hp_color) +
  annotate("text", x = "5°C", y = avg_cop + 1.5, label = paste0("Sample average ~ ", round(avg_cop, 2)), color = hp_color, size = 4) +
  geom_line(
    data = cop_reference_lines %>%
      mutate(degree = factor(round(temp_c), levels = temp_levels, labels = temp_labels, ordered = TRUE)),
    aes(x = degree, y = cop, linetype = source, group = source),
    color = flexible_color
  ) +
  scale_linetype_manual(values = c("EPRI" = "solid", "Brattle" = "dashed")) +
  scale_x_discrete(
    limits = temp_labels[temp_levels <= 15],
    breaks = c("≤ 0°C", "0°C", "5°C", "10°C", "15°C"),
    drop = FALSE
  ) +
  labs(
    x = "Average Weekly Temperature in Degrees (°C)",
    y = "Estimated ratio of heat output to energy input",
    linetype = "Engineering Models of COP"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("graphs/quasi_cop_gas_only.png", plot = p_quasi, width = 16, height = 8, units = "cm")
if (write_main) ggsave("graphs/quasi_cop.png", plot = p_quasi, width = 16, height = 8, units = "cm")

checkpoint("DONE: gas-only robustness outputs generated")
