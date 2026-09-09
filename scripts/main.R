# ============================================================
# Pipeline Entry Point
#
# Replicates the full analysis for "Decarbonizing Heat: The Impact
# of Heat Pumps and a Time-of-Use Heat Pump Tariff on Energy Demand"
# (Louise Bernard).
#
# This script:
#   1) installs/loads the required packages
#   2) detects the working directory and sets `datapath` accordingly
#      (Louise's Mac vs the GCP Vertex AI workbench)
#   3) sets global plot colors (hp_color, cosy_color, flexible_color,
#      not_hp_color, rating_colors, red_palette) and the fixest
#      estimation config used throughout the pipeline
#   4) sources the six top-level orchestrators in order:
#      01_00_cosy.R, 02_00_heatpump.R,
#      03_00_balance_tables_and_reweighting.R,
#      04_00_half_hourly_analysis.R, 05_MVPF.R,
#      06_sample_descriptive_statistics.R
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

# Make sure working directory is "cosy-analysis"
if (getwd() != "/Users/louise/Documents/GitHub/cosy-analysis") {
  setwd("/home/jupyter/cosy-analysis")
  # establish the home directory
  datapath <- "../gcs/cosy2"
} else {
  datapath <-  "~/gcs/cnz-oe-extract-57d7be9d0a/cosy2"
}

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
