# Run this on the server (Vertex AI Workbench) to gather the facts needed for
# the README's "Computational Requirements" section (per the Social Science
# Data Editors template / DCAS #13). Not part of the analysis pipeline itself.
#
# Usage: source("scripts/main.R") first to install the analysis packages,
# then source("scripts/utils_session_info.R"). Output is written to
# data/output/session_info.txt (and also echoed to the console) -- paste the
# relevant parts back into the README's Computational Requirements section.
#
# Each run fully overwrites session_info.txt from scratch: the file
# connection below is opened in "wt" mode, which truncates any existing file
# before writing, so a re-run can never leave stale content from a previous
# run mixed in with the new output.

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

  cat("\n==== RUNTIME ====\n")
  cat("Only two files in the pipeline are cached (skip-if-exists): data/scratch/aggregated_data.RDS\n")
  cat("(01_01_load_data.R) and data/scratch/cop_boot_no_boxing.csv (02_09_cop_analysis.R). If either\n")
  cat("exists, source(\"scripts/main.R\") will skip rebuilding it and any timing would UNDERSTATE a\n")
  cat("true cold-start run, so both are deleted below to force a full rebuild on the next run.\n")

  cache_files <- file.path(datapath, c("scratch/aggregated_data.RDS", "scratch/cop_boot_no_boxing.csv"))
  for (f in cache_files) {
    if (file.exists(f)) {
      file.remove(f)
      cat("Deleted:", f, "\n")
    } else {
      cat("Not present (nothing to delete):", f, "\n")
    }
  }

  cat("\nNow time the full pipeline separately (this will take a while):\n")
  cat('  start <- Sys.time(); source("scripts/main.R"); Sys.time() - start\n')
})

cat("\nSaved to:", file.path(datapath, "output", "session_info.txt"), "\n")
