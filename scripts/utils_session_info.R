# Run this on the server (Vertex AI Workbench) to gather the facts needed for
# the README's "Computational Requirements" section (per the Social Science
# Data Editors template / DCAS #13), including a genuine cold-start timed run
# of the full pipeline. Not part of the analysis pipeline itself.
#
# Usage: source("scripts/utils_session_info.R") on its own -- self-contained,
# does not need scripts/main.R sourced first (it only reuses main.R's cheap
# working-directory/datapath logic below, not the six analysis stages).
#
# NOTE: this now runs the *entire* pipeline (source("scripts/main.R")) as
# part of gathering the Runtime figure, so it is NOT quick -- expect this to
# take as long as a full pipeline run does. If you only want the OS/CPU/R
# version/package info without the long wait, comment out the "RUNTIME" block
# at the bottom before sourcing.
#
# Outputs:
#   data/output/session_info.txt -- OS/CPU/memory/disk/R/package info, plus
#     console echo as it runs. Fully overwritten each run (opened in "wt"
#     mode, which truncates first), so re-runs never mix stale content with
#     fresh content.
#   data/output/runtime_log.txt -- one line per timed run, APPENDED (not
#     overwritten) so repeated attempts build a history. Written the instant
#     the timed run finishes -- success or error -- so the measurement
#     survives even if the interactive session disconnects right after.
#
# Belt-and-suspenders note: on most Jupyter/Workbench setups the kernel keeps
# running server-side even if your browser tab disconnects, so the above
# should be enough on its own. If you want to be independent of the
# interactive session from the start too (e.g. protect against a kernel
# restart), run this via Rscript in the background instead, from a terminal:
#   nohup Rscript -e 'source("scripts/utils_session_info.R")' &
# and check the two output files whenever you next check in.

if (!exists("datapath")) {
  # Same working-directory/datapath logic as scripts/main.R, duplicated here
  # so this script doesn't depend on main.R having been sourced first.
  if (getwd() != "/Users/louise/Documents/GitHub/cosy-analysis") {
    setwd("/home/jupyter/cosy-analysis")
    datapath <- "../gcs/cosy2"
  } else {
    datapath <- "~/gcs/cnz-oe-extract-57d7be9d0a/cosy2"
  }
}

local({
  output_path <- file.path(datapath, "output", "session_info.txt")
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

  con <- file(output_path, open = "wt")
  sink(con, split = TRUE)  # split = TRUE: still echo to console as it runs
  on.exit({
    sink(type = "output")
    close(con)
  }, add = TRUE)

  cat("Session info captured:", format(Sys.time()), "\n\n")

  cat("==== OPERATING SYSTEM ====\n")
  print(Sys.info())
  cat(readLines("/etc/os-release", warn = FALSE), sep = "\n")

  cat("\n==== CPU ====\n")
  cat(system("lscpu | grep -E 'Model name|Socket|Core|Thread|CPU\\(s\\)'", intern = TRUE), sep = "\n")

  cat("\n==== MEMORY ====\n")
  cat(system("free -h", intern = TRUE), sep = "\n")

  cat("\n==== DISK SPACE (home + data mount) ====\n")
  cat(system("df -h ~ 2>/dev/null", intern = TRUE), sep = "\n")
  cat(system("df -h /home/jupyter/gcs 2>/dev/null", intern = TRUE), sep = "\n")

  cat("\n==== R VERSION AND PACKAGES ====\n")
  print(sessionInfo())

  cat("\n==== EXACT VERSIONS OF PACKAGES USED BY main.R ====\n")
  packages <- c(
    "knitr", "kableExtra", "did", "fixest", "data.table", "lubridate",
    "dplyr", "ggplot2", "RColorBrewer", "tidyr", "scales", "readr",
    "forcats", "viridis", "stringr", "stargazer", "panelView", "readxl", "purrr",
    "progress", "lfe", "tibble", "didimputation", "ggtext", "MatchIt", "zoo",
    "patchwork"
  )
  for (p in packages) {
    v <- tryCatch(as.character(packageVersion(p)), error = function(e) "NOT INSTALLED")
    cat(sprintf("%-15s %s\n", p, v))
  }
})

cat("\nSaved to:", file.path(datapath, "output", "session_info.txt"), "\n")

# ==== RUNTIME (comment out this whole block if you only want the info above) ====
#
# Four cache locations exist in the pipeline (skip-if-exists): scratch/aggregated_data.RDS
# (01_01_load_data.R), scratch/did_cosy_<period>_universal.RDS, one per rate_period
# (01_06_DiD_analysis.R), scratch/cop_boot_no_boxing.csv (02_08_cop_analysis.R), and
# scratch/cop_boot_gas_only.csv (02_13_gas_only_sample_robustness_check.R). If any exist,
# source("scripts/main.R") would skip rebuilding them and understate a true cold-start run,
# so all are deleted below to force a full rebuild.
#
# Timed via system.time(), not a bare `start <- Sys.time()` variable -- 01_01_load_data.R and
# 02_08_cop_analysis.R both reassign a variable literally named `start` for their own internal
# checkpoint logging (everything is source()'d into the same global environment), which would
# silently clobber a bare variable and make the measured duration meaningless. system.time() is
# immune to this since it measures internally.

did_cosy_periods <- c("Morning Off-peak", "Afternoon Off-peak", "Peak Rate", "Other", "Overall")
cache_files <- file.path(datapath, c(
  "scratch/aggregated_data.RDS",
  paste0("scratch/did_cosy_", did_cosy_periods, "_universal.RDS"),
  "scratch/cop_boot_no_boxing.csv",
  "scratch/cop_boot_gas_only.csv"
))
for (f in cache_files) {
  if (file.exists(f)) {
    file.remove(f)
    cat("Deleted:", f, "\n")
  } else {
    cat("Not present (nothing to delete):", f, "\n")
  }
}

runtime_log_path <- file.path(datapath, "output", "runtime_log.txt")
start_ts <- Sys.time()
cat(sprintf("\n[%s] Starting full pipeline run (source(\"scripts/main.R\"))...\n", format(start_ts)))

result <- tryCatch(
  list(status = "SUCCESS", timing = system.time(source("scripts/main.R")), error = NA_character_),
  error = function(e) list(status = "ERROR", timing = NULL, error = conditionMessage(e))
)

end_ts <- Sys.time()

# Written immediately after the pipeline call returns (success or via the error handler above),
# before anything else can happen, so the result is durable even if the session disconnects a
# moment later.
log_line <- if (result$status == "SUCCESS") {
  sprintf(
    "[%s] SUCCESS started=%s ended=%s elapsed=%.1fs (user=%.1fs sys=%.1fs)",
    format(Sys.time()), format(start_ts), format(end_ts),
    result$timing[["elapsed"]], result$timing[["user.self"]], result$timing[["sys.self"]]
  )
} else {
  sprintf(
    "[%s] ERROR started=%s ended=%s elapsed=%.1fs error=%s",
    format(Sys.time()), format(start_ts), format(end_ts),
    as.numeric(difftime(end_ts, start_ts, units = "secs")), result$error
  )
}

cat(log_line, file = runtime_log_path, sep = "\n", append = TRUE)
cat("\n", log_line, "\n", sep = "")
cat("Logged to:", runtime_log_path, "\n")

if (identical(result$status, "ERROR")) stop(result$error)
