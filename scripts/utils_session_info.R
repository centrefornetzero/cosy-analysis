# Run this on the server (Vertex AI Workbench) to gather the facts needed for
# the README's "Computational Requirements" section (per the Social Science
# Data Editors template / DCAS #13). Not part of the analysis pipeline itself.
#
# Usage: source("scripts/main.R") first to install the analysis packages,
# then source("scripts/utils_session_info.R"). Paste the printed output back
# into the README.

cat("==== OPERATING SYSTEM ====\n")
print(Sys.info())
cat(readLines("/etc/os-release", warn = FALSE), sep = "\n")

cat("\n==== CPU ====\n")
system("lscpu | grep -E 'Model name|Socket|Core|Thread|CPU\\(s\\)'")

cat("\n==== MEMORY ====\n")
system("free -h")

cat("\n==== DISK SPACE (home + data mount) ====\n")
system("df -h ~ 2>/dev/null")
system("df -h /home/jupyter/gcs 2>/dev/null")

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
cat("To time the full pipeline, run separately (this will take a while):\n")
cat('  start <- Sys.time(); source("scripts/main.R"); Sys.time() - start\n')
