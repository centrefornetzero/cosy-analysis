# mc_subsample_checkpointed_fixed.R
suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(dplyr)
  library(stringr)
  library(fixest)
  library(future.apply)
  library(pbapply)
  library(ggplot2)
  library(tidyr)
  library(tibble)
})

# -----------------------------
# USER SETTINGS
# -----------------------------
n_sample_accounts <- 500
R_runs <- 100                # set to 1000 if you want
use_parallel <- TRUE
n_workers <- 4
seed_base <- 12345

# ---- paths (adjust datapath variable in your environment) ----
parquet_base_path <- "../gcs/cosy/hp_adopters_elec/"
parquet_base_path_cosy <- "../gcs/cosy/cosy_elec/"

hh_flag_path        <- file.path(datapath, "input/cosy_-_is_charged_half_hourly_hp_accounts_2025_06_11.csv")
hp_details_path     <- file.path(datapath, "input/cosy_-_hp_details_2024_06_25.csv")
ev_path             <- file.path(datapath, "input/cosy_-_ev_detection_2024_07_04.csv")
weather_path        <- file.path(datapath, "input/Cosy Analysis Weather Mar 26 daily.csv")
first_adoption_path <- file.path(datapath, "input/Cosy_-_agreement_data_2024_07_24.csv")

# ---- outputs ----
out_dir <- file.path(datapath, "scratch/results")
run_dir <- file.path(out_dir, "mc_runs")
diag_dir <- file.path(out_dir, "diagnostics")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(run_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# HELPERS
# -----------------------------
run_file <- function(draw_id) file.path(run_dir, sprintf("draw_%05d.rds", draw_id))

settlement_period_to_time <- function(period) {
  if (is.na(period)) return(NA_character_)
  hours <- (period - 1) %/% 2
  minutes <- ifelse((period %% 2) == 1, "00", "30")
  sprintf("%02d:%s", hours, minutes)
}

safe_collect_dataset <- function(path, cols) {
  tryCatch({
    ds <- arrow::open_dataset(path, format = "parquet")
    df <- ds %>% dplyr::select(dplyr::all_of(cols)) %>% collect()
    if (nrow(df) == 0) return(NULL)
    df
  }, error = function(e) {
    message("Failed to read path: ", path, " | ", e$message)
    NULL
  })
}

atomic_saveRDS <- function(obj, filepath) {
  tmp <- paste0(filepath, ".tmp")
  saveRDS(obj, tmp)
  ok <- file.rename(tmp, filepath)
  if (!ok) {
    file.copy(tmp, filepath, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(TRUE)
}

# -----------------------------
# PRELOAD STATIC COVARIATES / ELIGIBLE ACCOUNT POOLS
# -----------------------------
message("Loading covariates + eligible account pools...")

ev_charging <- fread(ev_path) %>%
  mutate(ev_charging = 1,
         account_id = as.character(account_id))

weather <- fread(weather_path) %>%
  rename_with(.cols = starts_with("weekly"),
              .fn = ~ sub("^weekly", "daily", .)) %>%
  rename(tariff_gsp_group_id = gsp_group_id) %>%
  mutate(date = as.Date(date_day, format = "%Y-%m-%d"))

hp_details <- fread(hp_details_path) %>%
  distinct(hashed_mpan, .keep_all = TRUE) %>%
  mutate(installed_at = as.Date(installed_at),
         account_id = as.character(account_id),
         hashed_mpan = as.character(hashed_mpan))

hh_flag <- fread(hh_flag_path)
non_hh_accounts <- hh_flag[is_charged_half_hourly == FALSE, unique(as.character(account_id))]

hp_account_dirs <- list.files(parquet_base_path, pattern = "^account_id=", full.names = TRUE)
hp_account_ids  <- str_replace(basename(hp_account_dirs), "^account_id=", "")
eligible_hp_accounts <- intersect(hp_account_ids, non_hh_accounts)
message("Eligible non-HH HP accounts in parquet: ", length(eligible_hp_accounts))

cosy_account_dirs <- list.files(parquet_base_path_cosy, pattern = "^account_id=", full.names = TRUE)
cosy_account_ids  <- str_replace(basename(cosy_account_dirs), "^account_id=", "")
message("Eligible Cosy accounts in parquet: ", length(cosy_account_ids))

first_adoption <- fread(first_adoption_path) %>%
  filter(product_display_name == "Cosy Octopus") %>%
  mutate(
    agreement_valid_from = as.Date(agreement_valid_from),
    agreement_valid_to   = as.Date(agreement_valid_to),
    hashed_mpan = as.character(hashed_mpan)
  ) %>%
  group_by(hashed_mpan, tariff_gsp_group_id) %>%
  summarise(first_adoption = min(agreement_valid_from, na.rm = TRUE), .groups = "drop") %>%
  mutate(first_adoption = as.Date(first_adoption))

# -----------------------------
# ONE DRAW = sample accounts + run regressions + extract coefs
# -----------------------------
run_one_draw <- function(draw_id) {
  set.seed(seed_base + draw_id)

  sample_hp   <- sample(eligible_hp_accounts, size = min(n_sample_accounts, length(eligible_hp_accounts)), replace = FALSE)
  sample_cosy <- sample(cosy_account_ids,      size = min(n_sample_accounts, length(cosy_account_ids)),      replace = FALSE)

  out <- list(draw = draw_id, ok_hp = FALSE, ok_cosy = FALSE, hp = NULL, cosy = NULL)

  # ---------- HP ----------
  hp <- NULL
  tryCatch({
    hp_list <- lapply(sample_hp, function(aid) {
      path <- file.path(parquet_base_path, paste0("account_id=", aid))
      df <- safe_collect_dataset(path)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      if (!("account_id" %in% names(df))) df$account_id <- aid
      df$account_id <- as.character(df$account_id)
      df$hashed_mpan <- as.character(df$hashed_mpan)
      df
    })
    hp <- bind_rows(hp_list)
    rm(hp_list); gc()

    if (!is.null(hp) && nrow(hp) > 0) {
      hp <- hp %>%
        inner_join(hp_details) %>%
        mutate(
          settlement_date = as.Date(interval_start),
          installed_at    = as.Date(installed_at),
          settlement_time = format(as.POSIXct(interval_start, tz = "UTC"),
                                   tz = "Europe/London", "%H:%M"),
          is_hp_installed = as.numeric(installed_at <= settlement_date)
        ) %>%
        left_join(weather, by = c("settlement_date" = "date", "tariff_gsp_group_id")) %>%
        arrange(interval_start) %>%
        group_by(settlement_time) %>%
        mutate(settlement_period = cur_group_id()) %>%
        ungroup() %>%
        left_join(
          ev_charging %>% select(account_id, interval_start, ev_charging) %>% distinct()) %>%
        mutate(ev_charging = ifelse(is.na(ev_charging), 0, ev_charging))

      m_hp <- tryCatch({
        feols(
          value ~ i(settlement_period, ref = 1) +
            i(settlement_period, is_hp_installed, ref2 = 0) |
            account_id + settlement_date + daily_avg_heating_degree + ev_charging,
          cluster = ~account_id,
          data = hp,
          lean = TRUE,
          mem.clean = TRUE
        )
      }, error = function(e) {
        message("feols HP error draw ", draw_id, ": ", conditionMessage(e))
        NULL
      })

      if (!is.null(m_hp)) {
        # coeftable -> df
        ct <- fixest::coeftable(m_hp) %>% as.data.frame()
        terms <- rownames(ct)

        # console sample (may be buffered in parallel)
        message("hp: draw=", draw_id, " n_terms=", length(terms))
        message("hp: sample terms:\n", paste0(head(terms, 20), collapse = "\n"))

        # debug file (always written even in parallel)
        try({
          dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)
          writeLines(
            c(
              paste0("draw_id=", draw_id),
              paste0("n_terms=", length(terms)),
              "---- first 200 terms ----",
              head(terms, 200)
            ),
            file.path(diag_dir, paste0("debug_terms_hp_draw_", draw_id, ".txt"))
          )
        }, silent = TRUE)

        # Robust extraction using regex (no brittle separate())
        coefs_hp <- ct %>%
          tibble::rownames_to_column("term") %>%
          tibble::as_tibble() %>%
          separate(term, into = c("remove", "remove2", "settlement_period", "has_hp"), sep = ":") %>%
          mutate(settlement_period = as.numeric(settlement_period),
                 treatment = "hp",
                 draw = draw_id) %>% 
          filter(!is.na(has_hp))%>%
          select(settlement_period, Estimate, `Std. Error`, treatment, draw)

        out$hp <- coefs_hp
        out$ok_hp <- TRUE
      }
    }
  }, error = function(e) {
    message("ERROR (hp) draw ", draw_id, ": ", conditionMessage(e))
    # write quick debug if possible
    try({
      writeLines(conditionMessage(e), file.path(diag_dir, paste0("err_hp_draw_", draw_id, ".txt")))
    }, silent = TRUE)
  })

  rm(hp); gc()

  # ---------- COSY ----------
  cosy <- NULL
  tryCatch({
    cosy_list <- lapply(sample_cosy, function(aid) {
      path <- file.path(parquet_base_path_cosy, paste0("account_id=", aid))
      df <- safe_collect_dataset(path)
      if (is.null(df) || nrow(df) == 0) return(NULL)

      df <- df %>%
        filter(import_or_export_product == "IMPORT") %>%
        transmute(
          account_id = as.character(aid),
          hashed_mpan = as.character(hashed_mpan),
          interval_start = as.POSIXct(interval_start, tz = "UTC"),
          read_value = as.numeric(value)
        )
      df
    })

    cosy <- bind_rows(cosy_list)
    rm(cosy_list); gc()

    if (!is.null(cosy) && nrow(cosy) > 0) {
      cosy <- cosy %>%
        inner_join(first_adoption) %>%
        mutate(
          settlement_date = as.Date(interval_start),
          settlement_time = format(as.POSIXct(interval_start, tz = "UTC"),
                                   tz = "Europe/London", "%H:%M"),
          is_cosy = as.numeric(first_adoption <= settlement_date)
        ) %>%
        left_join(weather, by = c("settlement_date" = "date", "tariff_gsp_group_id" = "tariff_gsp_group_id")) %>%
        arrange(interval_start) %>%
        group_by(settlement_time) %>%
        mutate(settlement_period = cur_group_id()) %>%
        ungroup() %>%
        left_join(
          ev_charging %>% select(account_id, interval_start, ev_charging) %>% distinct()) %>%
        mutate(ev_charging = ifelse(is.na(ev_charging), 0, ev_charging))

      m_cosy <- tryCatch({
        feols(
          read_value ~ i(settlement_period, ref = 1) +
            i(settlement_period, is_cosy, ref2 = 0) |
            account_id + settlement_date + daily_avg_heating_degree + ev_charging,
          cluster = ~account_id,
          data = cosy,
          lean = TRUE,
          mem.clean = TRUE
        )
      }, error = function(e) {
        message("feols Cosy error draw ", draw_id, ": ", conditionMessage(e))
        NULL
      })

      if (!is.null(m_cosy)) {
        ct <- fixest::coeftable(m_cosy) %>% as.data.frame()
        terms <- rownames(ct)
        message("cosy: draw=", draw_id, " n_terms=", length(terms))
        message("cosy: sample terms:\n", paste0(head(terms, 20), collapse = "\n"))

        try({
          dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)
          writeLines(
            c(
              paste0("draw_id=", draw_id),
              paste0("n_terms=", length(terms)),
              "---- first 200 terms ----",
              head(terms, 200)
            ),
            file.path(diag_dir, paste0("debug_terms_cosy_draw_", draw_id, ".txt"))
          )
        }, silent = TRUE)

        coefs_cosy <- ct %>%
          tibble::rownames_to_column("term") %>%
          tibble::as_tibble() %>%
          separate(term, into = c("remove", "remove2", "settlement_period", "has_cosy"), sep = ":") %>%
          mutate(settlement_period = as.numeric(settlement_period),
                 treatment = "cosy",
                 draw = draw_id) %>% 
          filter(!is.na(has_cosy))%>%
          select(settlement_period, Estimate, `Std. Error`, treatment, draw)

        out$cosy <- coefs_cosy
        out$ok_cosy <- TRUE
      }
    }
  }, error = function(e) {
    message("ERROR (cosy) draw ", draw_id, ": ", conditionMessage(e))
    try({
      writeLines(conditionMessage(e), file.path(diag_dir, paste0("err_cosy_draw_", draw_id, ".txt")))
    }, silent = TRUE)
  })

  rm(cosy); gc()
  out
}

# -----------------------------
# RUN (checkpointed)
# -----------------------------
draw_ids <- seq_len(R_runs)

already_done <- file.exists(vapply(draw_ids, run_file, FUN.VALUE = character(1)))
message("Already completed draws: ", sum(already_done), " / ", length(draw_ids))
pending_ids <- draw_ids[!already_done]
message("Pending draws: ", length(pending_ids))

run_and_save_one <- function(i) {
  f <- run_file(i)
  if (file.exists(f)) return(f)
  res <- run_one_draw(i)
  atomic_saveRDS(res, f)
  f
}

if (length(pending_ids) > 0) {
  if (use_parallel) {
    # correct parallel plan
    future::plan(future::multisession, workers = n_workers)
    message("Running pending draws in parallel with ", n_workers, " workers")
    invisible(future.apply::future_lapply(pending_ids, run_and_save_one, future.seed = TRUE))
    future::plan(future::sequential)  # reset
  } else {
    message("Running pending draws sequentially")
    invisible(pblapply(pending_ids, run_and_save_one))
  }
} else {
  message("Nothing to do: all draws completed.")
}

message("All available draws saved in: ", run_dir)

# -----------------------------
# AGGREGATE (from saved draw files)
# -----------------------------
run_files <- sort(list.files(run_dir, pattern = "^draw_\\d{5}\\.rds$", full.names = TRUE))
message("Found draw files: ", length(run_files))
results_list <- lapply(run_files, readRDS)

all_hp <- rbindlist(lapply(results_list, function(x) x$hp), use.names = TRUE, fill = TRUE)
all_cosy <- rbindlist(lapply(results_list, function(x) x$cosy), use.names = TRUE, fill = TRUE)

all_coefs_long <- bind_rows(all_hp, all_cosy) %>%
  mutate(
    settlement_period = as.integer(settlement_period),
    time_label = vapply(settlement_period, settlement_period_to_time, FUN.VALUE = character(1))
  ) %>%
  arrange(treatment, settlement_period, draw)

fwrite(all_coefs_long, file.path(out_dir, "mc_runs_coefs_long.csv"))
saveRDS(all_coefs_long, file.path(out_dir, "mc_runs_coefs_long.rds"))

# -----------------------------
# MONTE CARLO DIAGNOSTICS
# -----------------------------
diag <- all_coefs_long %>%
  group_by(treatment, settlement_period, time_label) %>%
  summarise(
    R_obs = n(),
    mean_est = mean(Estimate, na.rm = TRUE),
    sd_est = sd(Estimate, na.rm = TRUE),
    mean_model_se = mean(`Std. Error`, na.rm = TRUE),
    mc_se = sd_est / sqrt(pmax(1, R_obs)),
    rel_mcse_vs_model_se = ifelse(mean_model_se == 0, NA_real_, (sd_est / sqrt(pmax(1, R_obs))) / mean_model_se),
    .groups = "drop"
  )

fwrite(diag, file.path(out_dir, "mc_aggregated.csv"))
saveRDS(diag, file.path(diag_dir, "mc_diagnostics_summary.rds"))

# -----------------------------
# DIAGNOSTICS PLOTS
# -----------------------------
message("Creating diagnostics plots...")

# 1) running mean plots for a subset of settlement periods
periods_all <- sort(unique(na.omit(all_coefs_long$settlement_period)))
periods_to_plot <- periods_all[seq(1, min(48, length(periods_all)), by = 4)]

for (treat in unique(all_coefs_long$treatment)) {
  for (p in periods_to_plot) {
    dfsub <- all_coefs_long %>%
      filter(treatment == treat, settlement_period == p) %>%
      arrange(draw)

    if (nrow(dfsub) < 20) next

    dfsub <- dfsub %>%
      mutate(draw_index = seq_len(n()),
             running_mean = cummean(Estimate))

    g <- ggplot(dfsub, aes(x = draw_index, y = running_mean)) +
      geom_line() +
      labs(
        title = paste0("Running mean: ", treat, " | period ", p, " (", settlement_period_to_time(p), ")"),
        x = "Completed draws",
        y = "Running mean of Estimate"
      )

    ggsave(
      filename = file.path(diag_dir, paste0("running_mean_", treat, "_p", p, ".png")),
      plot = g, width = 7, height = 4
    )
  }
}

# 2) histograms
for (treat in unique(all_coefs_long$treatment)) {
  df_t <- all_coefs_long %>% filter(treatment == treat)

  for (p in unique(df_t$settlement_period)) {
    if (is.na(p)) next
    dfsub <- df_t %>% filter(settlement_period == p)
    if (nrow(dfsub) < 30) next

    g <- ggplot(dfsub, aes(x = Estimate)) +
      geom_histogram(bins = 30) +
      labs(
        title = paste("Estimate distribution:", treat, "| period", p, "(", settlement_period_to_time(p), ")"),
        x = "Estimate", y = "Count"
      )

    ggsave(
      filename = file.path(diag_dir, paste0("hist_", treat, "_p", p, ".png")),
      plot = g, width = 6, height = 4
    )
  }
}

message("Done.")
message("Per-draw checkpoints: ", run_dir)
message("Stacked coefficients:  ", file.path(out_dir, "mc_runs_coefs_long.csv"))
message("Diagnostics summary:   ", file.path(out_dir, "mc_aggregated.csv"))
                             
                             
                             
                             
# -----------------------------
# HALF HOURLY TREATMENT PLOT
# -----------------------------

coefs_summarised <- all_coefs_long %>%
  group_by(settlement_period, treatment) %>%
  summarise(
    Estimate = mean(Estimate, na.rm = TRUE),
    lower_ci = quantile(Estimate, 0.025, na.rm = TRUE),
    upper_ci = quantile(Estimate, 0.975, na.rm = TRUE),
    time_label = dplyr::first(time_label),
    .groups = "drop"
  )

# -------------------------------------------------------------------
# 2) X-axis labels (use summarised data now)
# -------------------------------------------------------------------

selected_periods <- seq(min(coefs_summarised$settlement_period),
                        max(coefs_summarised$settlement_period),
                        by = 2)

display_labels <- coefs_summarised %>%
  filter(settlement_period %in% selected_periods) %>%
  distinct(settlement_period, time_label) %>%
  arrange(settlement_period) %>%
  pull(time_label)

# -------------------------------------------------------------------
# 3) Shading (unchanged)
# -------------------------------------------------------------------

shaded_periods <- data.frame(
  xmin = c(9, 27, 33),
  xmax = c(15, 33, 39),
  period_type = c("Morning and Afternoon Off-peak", "Morning and Afternoon Off-peak", "Peak Rate")
)

# -------------------------------------------------------------------
# 4) Plot: mean line + bootstrap CI (ribbon)
# -------------------------------------------------------------------

p <- ggplot(coefs_summarised, aes(x = settlement_period, y = Estimate, group = treatment, color = treatment)) +
  geom_rect(
    data = shaded_periods,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period_type),
    inherit.aes = FALSE, alpha = 0.2
  ) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
              width = 0.2, alpha = 0.6)+
  geom_line() +
  scale_color_manual(
    name = "Treatment",
    labels = c("hp" = "Heat Pump", "cosy" = "Heat Pump Tariff"),
    values = c("hp" = hp_color, "cosy" = cosy_color)
  ) +
    scale_fill_manual(
      name = "Rate Period",
      values = c(
        "Morning and Afternoon Off-peak" = "red",
        "Peak Rate" = "lightblue"
      )
    ) +
  guides(
    fill = guide_legend(
      override.aes = list(alpha = c(0.2, 0.2, 0.15, 0.15)),
      order = 2
    ),
    color = guide_legend(order = 1)
  ) +
  labs(
    x = "Time of Day (Settlement Period)",
    y = "Impact on Electricity Consumption (kWh)",
    color = "Treatment",
    fill = "Price per kWh Period"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.text = element_markdown()
  ) +
  scale_x_continuous(
    breaks = selected_periods,
    labels = display_labels,
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")

p

ggsave("graphs/combined_impact_hourly_consumption_bootstrap.png", p, width = 10, height = 6, dpi = 300)