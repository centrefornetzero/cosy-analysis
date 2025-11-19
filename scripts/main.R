# Decarbonizing Heat: The Impact of Heat Pumps and a Time-of-Use Heat Pump Tariff on Energy Demand
# Author of the files: Louise Bernard
# Run "main.R" to replicate the full analysis


# List packages to load
packages <- c(
  "knitr", "kableExtra", "did", "fixest", "data.table", "lubridate", 
  "dplyr", "ggplot2", "RColorBrewer", "tidyr", "scales", 
  "forcats", "viridis",  "stringr", "stargazer", "panelView", "readxl","purrr",
  "progress", "lfe", "tibble", "stringr", "didimputation", "ggtext", "MatchIt"
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

# Create folders
dir.create("graphs", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)
dir.create("data/scratch", showWarnings = FALSE)
dir.create("data/output", showWarnings = FALSE)
dir.create("data/input", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)

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

# Subsample analysis (faster processing)
random_subsample <- FALSE

# Cosy reproduction
source("scripts/02_00_cosy.R")


source("scripts/01_00_heatpump.R")
rm(list = setdiff(ls(), c("random_subsample", "flexible_color", "cosy_color", "hp_color", 
                          "not_hp_color", "rating_colors", "red_palette")))
gc()


source("scripts/03_00_balance_tables_and_reweighting.R")

source("scripts/04_00_half_hourly_analysis.R") 
