# Standalone diagnostic for the "subscript out of bounds" crash in section 4
# of 01_06_DiD_analysis.R (the dynamic aggte() call). Not part of the
# pipeline. Reuses the cached scratch/did_cosy_<period>_universal.RDS files
# from the att_gt() loop -- does NOT rebuild aggregated_data.RDS or re-run
# att_gt(), so this should take seconds, not hours.
#
# What it checks, per period, WITHOUT calling aggte() at all (so it can't
# crash the same way): replicates exactly what did::compute.aggte() does
# internally to decide which event-time cells are "post-treatment" --
# event_time = t - group, na.rm-filtered, epos = event_time >= 0 -- and
# reports how many such cells exist (a) at all, (b) within the [-52, 52]
# window the pipeline uses. This tells us definitively whether the crash is
# because:
#   (A) that period has SOME valid post-treatment estimates, just outside
#       [-52, 52] -- fixable by widening max_e for that period, no package
#       bug workaround needed, and you get a real graph; or
#   (B) that period has ZERO valid (non-NA) post-treatment estimates at ANY
#       event time -- not fixable by widening the window; the underlying
#       att_gt() estimation itself has nothing post-treatment to show for
#       that period, which is a modeling/data question, not a bug to patch.

if (!exists("datapath")) {
  if (getwd() != "/Users/louise/Documents/GitHub/cosy-analysis") {
    setwd("/home/jupyter/cosy-analysis")
    datapath <- "../gcs/cosy2"
  } else {
    datapath <- "data"
  }
}

periods <- c("Morning Off-peak", "Afternoon Off-peak", "Peak Rate", "Other", "Overall")

cat(sprintf("%-20s %10s %10s %10s %10s %10s\n",
            "period", "n_cells", "n_na", "n_post", "n_post_ok", "n_post_win"))
cat(strrep("-", 75), "\n")

for (period in periods) {
  f <- file.path(datapath, paste0("scratch/did_cosy_", period, "_universal.RDS"))
  if (!file.exists(f)) {
    cat(sprintf("%-20s  FILE NOT FOUND: %s\n", period, f))
    next
  }
  est_cs <- readRDS(f)

  event_time <- est_cs$t - est_cs$group   # same as did's internal eseq basis
  att        <- est_cs$att
  is_na      <- is.na(att)

  n_cells    <- length(att)
  n_na       <- sum(is_na)
  n_post     <- sum(event_time >= 0)                       # post-treatment cells, before na.rm
  n_post_ok  <- sum(event_time >= 0 & !is_na)               # post-treatment cells surviving na.rm -- if this is 0, that's the "sum(epos)==0" bug trigger
  n_post_win <- sum(event_time >= 0 & event_time <= 52 & !is_na)  # post-treatment cells surviving na.rm AND inside the pipeline's max_e=52 window

  cat(sprintf("%-20s %10d %10d %10d %10d %10d\n",
              period, n_cells, n_na, n_post, n_post_ok, n_post_win))

  if (n_post_ok == 0) {
    cat(sprintf("  -> %s: ZERO valid post-treatment estimates at ANY event time (not just outside the window).\n", period))
    cat("     This is scenario (B) -- widening min_e/max_e will NOT produce a post-treatment graph for this\n")
    cat("     period; aggte(type=\"dynamic\") has nothing to aggregate on the post-treatment side, which is\n")
    cat("     exactly what triggers the did package's 1:0 indexing bug internally.\n")
  } else if (n_post_win < n_post_ok) {
    cat(sprintf("  -> %s: %d valid post-treatment estimates exist, but only %d fall inside [0, 52].\n",
                period, n_post_ok, n_post_win))
    cat("     This is scenario (A) -- widening max_e for this period would surface real data and avoid\n")
    cat("     the crash without needing to touch the did package at all.\n")
  } else {
    cat(sprintf("  -> %s: has valid post-treatment estimates well inside the current window -- this period\n", period))
    cat("     is very likely NOT the one that crashed.\n")
  }
  cat("\n")
}

cat("Whichever period shows n_post_ok == 0 above is almost certainly the one that crashed 01_06_DiD_analysis.R.\n")
