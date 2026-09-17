# ============================================================
# Pipeline Entry Point
#
# Replicates the full analysis for "Decarbonizing Heat: The Impact
# of Heat Pumps and a Time-of-Use Heat Pump Tariff on Energy Demand"
# (Louise Bernard).
#
# This script:
#   1) installs/loads the required packages
#   2) reads `datapath` from the COSY_DATAPATH environment variable (see
#      .Renviron.example and the README's Environment section)
#   3) sets global plot colors (hp_color, cosy_color, flexible_color,
#      not_hp_color, rating_colors, red_palette) and the fixest
#      estimation config used throughout the pipeline
#   4) sources the six top-level orchestrators in order:
#      01_00_cosy.R, 02_00_heatpump.R,
#      03_00_balance_tables_and_reweighting.R,
#      04_00_half_hourly_analysis.R, 05_MVPF.R,
#      06_sample_descriptive_statistics.R
#      (05_MVPF.R itself sources scripts/05_01_lbd_model.R as an input before
#      its final welfare calculation -- LBD is not a separate top-level stage)
# ============================================================


# List packages to load
packages <- c(
  "knitr", "kableExtra", "did", "fixest", "data.table", "lubridate", 
  "dplyr", "ggplot2", "RColorBrewer", "tidyr", "scales", "readr",
  "forcats", "viridis",  "stringr", "stargazer", "panelView", "readxl","purrr",
  "progress", "lfe", "tibble", "stringr", "didimputation", "ggtext", "MatchIt", "zoo", "ggplot2",
  "patchwork", "arrow"
)


# Function to check if a package is installed, and if not, install it
install_if_needed <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load (and install if needed) each package
lapply(packages, install_if_needed)

# Run this from the repository root (e.g. an R session or Rscript invocation
# with the working directory already set to cosy-analysis/).
if (!file.exists("scripts/main.R")) {
  stop("Run this from the repository root: scripts/main.R was not found relative to getwd().")
}

# COSY_DATAPATH must point at the mounted data directory (the root containing
# input/, scratch/, and output/) -- copy .Renviron.example to .Renviron and
# set it there; see the README's Environment section.
datapath <- Sys.getenv("COSY_DATAPATH", unset = NA)
if (is.na(datapath) || datapath == "") {
  stop("COSY_DATAPATH is not set. Copy .Renviron.example to .Renviron, set COSY_DATAPATH to your mounted data directory, and restart R.")
}
datapath <- path.expand(datapath)

checkpoint <- function(msg) cat(paste0(">>> ", msg, " <<<\n"))

# Set estimation to mirror CS
fixest::setFixest_estimation(fixef.rm = "none")

# Load parameters
flexible_color <- "#4C515C"  
cosy_color <- "#5F8ED9"      
hp_color <- "#AD87CA"
not_hp_color <- "#2D354A"

rating_colors <- c(
  "A" = "#00CC00",  # Green
  "B" = "#66FF33",  # Light Green
  "C" = "#FFFF00",  # Yellow
  "D" = "#FF9900",  # Orange
  "E" = "#FF6600",  # Dark Orange
  "F" = "#FF0000",  # Red
  "G" = "#990000"   # Dark Red
)
red_palette <- c("#FF9999", "#FF8080", "#FF6666", "#FF4D4D", "#FF3333", "#FF1A1A", "#FF0000", "#E60000", "#CC0000", "#B20000")

# Cosy reproduction
source("scripts/01_00_cosy.R")

source("scripts/02_00_heatpump.R")

source("scripts/03_00_balance_tables_and_reweighting.R")

source("scripts/04_00_half_hourly_analysis.R")

source("scripts/05_MVPF.R")

source("scripts/06_sample_descriptive_statistics.R")
