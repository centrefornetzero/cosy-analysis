# ===============================================
# DEV-ONLY quick setup -- NOT part of the main.R pipeline, not sourced by it.
# ===============================================
# Purpose: get `aggregated_data` and `hp_installed` into your R session from
# their existing caches, so you can test changes to:
#   scripts/01_05_balance_table.R
#   scripts/02_05_balance_table.R
#   scripts/02_08_heterogeneity_analysis.R
# without rerunning 01_01_load_data.R / 02_01_load_data.R (the expensive raw
# CSV merges) or anything else upstream.
#
# Requires that you've already run the full pipeline at least once, so that
# these caches exist:
#   data/scratch/aggregated_data.RDS   (built by 01_01_load_data.R)
#   data/output/hp_installed.rds       (built by 02_01_load_data.R)
#
# Usage:
#   source("scripts/dev_quick_setup.R")
#   source("scripts/01_05_balance_table.R")      # needs aggregated_data
#   source("scripts/02_05_balance_table.R")      # needs hp_installed
#   source("scripts/02_08_heterogeneity_analysis.R")  # reloads hp_installed itself
#
# NB: this intentionally skips each orchestrator's own setFixest_dict()/
# CleanPreAverage()/fitstat_register() setup (01_00_cosy.R, 02_00_heatpump.R).
# Those aren't used by the three scripts above, but if you later point this
# helper at a script that calls etable() with fitstat = c("pre_avg","t_obs"),
# or that expects orchestrator-specific variable labels, source the relevant
# bits of 01_00_cosy.R/02_00_heatpump.R directly instead.

packages <- c(
  "knitr", "kableExtra", "did", "fixest", "data.table", "lubridate",
  "dplyr", "ggplot2", "RColorBrewer", "tidyr", "scales", "readr",
  "forcats", "viridis", "stringr", "stargazer", "panelView", "readxl", "purrr",
  "progress", "lfe", "tibble", "stringr", "didimputation", "ggtext", "MatchIt", "zoo",
  "patchwork"
)
install_if_needed <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}
lapply(packages, install_if_needed)

# Same datapath logic as main.R
if (getwd() != "/Users/louise/Documents/GitHub/cosy-analysis") {
  setwd("/home/jupyter/cosy-analysis")
  datapath <- "../gcs/cosy2"
} else {
  datapath <- "data"
}

fixest::setFixest_estimation(fixef.rm = "none")

# Colors used across plots, including in 02_08_heterogeneity_analysis.R
flexible_color <- "#4C515C"
cosy_color <- "#5F8ED9"
hp_color <- "#AD87CA"
not_hp_color <- "#2D354A"
rating_colors <- c(
  "A" = "#00CC00", "B" = "#66FF33", "C" = "#FFFF00", "D" = "#FF9900",
  "E" = "#FF6600", "F" = "#FF0000", "G" = "#990000"
)
red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")

# Date parsing override used throughout the pipeline (see CLAUDE.md)
base_as_date <- base::as.Date
as.Date <- function(x, ...) {
  if (is.numeric(x)) {
    base_as_date(x, origin = "1970-01-01", ...)
  } else {
    base_as_date(x, ...)
  }
}

random_subsample <- FALSE

# --- load the already-built panels instead of rebuilding them ---
agg_path <- file.path(datapath, "scratch/aggregated_data.RDS")
hp_path <- file.path(datapath, "output/hp_installed.rds")

stopifnot(
  "data/scratch/aggregated_data.RDS not found -- run the full pipeline once first" = file.exists(agg_path),
  "data/output/hp_installed.rds not found -- run the full pipeline once first" = file.exists(hp_path)
)

aggregated_data <- readRDS(agg_path)
hp_installed <- readRDS(hp_path)

cat(
  "Loaded aggregated_data (", nrow(aggregated_data), " rows) and hp_installed (",
  nrow(hp_installed), " rows) from cache.\n",
  "You can now source():\n",
  "  scripts/01_05_balance_table.R\n",
  "  scripts/02_05_balance_table.R\n",
  "  scripts/02_08_heterogeneity_analysis.R\n",
  sep = ""
)
