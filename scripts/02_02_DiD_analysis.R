# ============================================================
# Callaway & Sant’Anna (did::att_gt) — weekly panel
# Goal: estimate dynamic treatment effects of HP installation on
#       (i) electricity consumption and (ii) gas consumption.
#
# This script:
#   1) loads weekly panel data (overall_weekly)
#   2) builds a "did-ready" dataset (id, week, firstweek, outcomes)
#   3) estimates CS models under several choices:
#        - main spec (not-yet-treated controls)
#        - base period robustness (universal vs varying)
#        - add “never treated” control group
#        - gas-only subset
#        - add simple seasonality controls (id-by-month interactions)
#        - anticipation robustness (anticipation = 0..10)
#
# NOTE: Paths are centralized below so file locations are consistent.
# ============================================================

# -------------------------- Setup --------------------------

# Optional: define colors for plots later (not used in estimation below)
elec_color <- "#AD87CA"
gas_color  <- "#2D354A"

# Centralize all I/O paths so they follow the same structure
path_in  <- list(
  weekly = file.path(datapath, "output", "overall_weekly.rds")
)

path_out <- list(
  scratch = file.path(datapath, "scratch")
)

# Helper to build output file paths consistently
out_file <- function(...) file.path(path_out$scratch, ...)

# -------------------------- Load data --------------------------

# Weekly panel, one row per account_id x settlement_week
overall_weekly <- readr::read_rds(path_in$weekly)

# -------------------------- Helpers --------------------------

# Build the "did-ready" dataset expected by did::att_gt:
# - week: integer time index starting at 1
# - firstweek: integer "group" index = first treated week (install week)
# - id: integer unit id (required by did package)
# - keep only variables needed for estimation
make_did_data <- function(df,
                          start_date = NULL,
                          max_week = 129,
                          cap_firstweek = TRUE,
                          set_never_treated_to_0 = FALSE,
                          add_month = FALSE) {
  
  # Use first observed settlement_week as the time origin unless supplied
  if (is.null(start_date)) {
    start_date <- min(df$settlement_week, na.rm = TRUE)
  }
  
  out <- df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # Time index in weeks since start_date (1, 2, 3, ...)
      week = as.integer(as.numeric(difftime(settlement_week, start_date, units = "weeks")) %/% 1 + 1),
      
      # Group index: first treated week since start_date (1..), or NA if no install date
      firstweek = as.integer(as.numeric(difftime(installed_at, start_date, units = "weeks")) %/% 1 + 1)
    )
  
  if (add_month) {
    out <- out %>%
      dplyr::mutate(month = lubridate::month(settlement_week))
  }
  
  out <- out %>%
    dplyr::group_by(account_id) %>%
    # did::att_gt expects a numeric id (can be arbitrary as long as unique per unit)
    dplyr::mutate(id = dplyr::cur_group_id()) %>%
    dplyr::ungroup()
  
  # Optionally treat installs after the sample window as "never treated" (firstweek = 0)
  # This is a common trick to create a never-treated group in a truncated panel.
  if (set_never_treated_to_0) {
    out <- out %>%
      dplyr::mutate(firstweek = dplyr::if_else(firstweek > max_week, 0L, firstweek))
  }
  
  # Keep only variables needed downstream
  keep_vars <- c("id", "firstweek", "week", "account_id",
                 "total_consumption", "elec_consumption", "gas_consumption")
  if (add_month) keep_vars <- c("id", "firstweek", "week", "month",
                                "total_consumption", "elec_consumption", "gas_consumption")
  
  out <- out %>%
    dplyr::select(dplyr::all_of(keep_vars)) %>%
    dplyr::filter(week <= max_week)
  
  # The main spec also requires firstweek <= 129
  # (i.e., units treated after the window are dropped rather than reclassified).
  if (cap_firstweek && !set_never_treated_to_0) {
    out <- out %>% dplyr::filter(firstweek <= max_week)
  }
  
  return(list(data = out, start_date = start_date))
}

# Run and save CS models for multiple outcomes with consistent logging + filenames
run_cs_models <- function(did_data,
                          y_vars,
                          file_names,
                          # anticipation = 4 weeks here vs. 1 in
                          # 01_06_DiD_analysis.R (Cosy): this is a different
                          # treatment/programme — a heat pump installation,
                          # not a tariff switch.
                          anticipation = 4,
                          control_group = "notyettreated",
                          base_period = "universal",
                          xformla = NULL,
                          est_method = "ipw") {
  
  for (i in seq_along(y_vars)) {
    
    yname    <- y_vars[i]
    filename <- file_names[i]
    
    message("Estimating: y = ", yname,
            " | control_group = ", paste(control_group, collapse = ", "),
            " | base_period = ", base_period,
            " | anticipation = ", anticipation)
    message("Saving to: ", filename)
    
    est_cs <- did::att_gt(
      yname = yname,
      tname = "week",
      idname = "id",
      gname = "firstweek",
      data = did_data,
      xformla = xformla,
      anticipation = anticipation,
      clustervars = "id",
      control_group = control_group,
      est_method = est_method,             # ipw avoids fastglm issues in some setups
      allow_unbalanced_panel = TRUE,
      base_period = base_period
    )
    
    saveRDS(est_cs, filename)
    message("Saved: ", filename)
  }
}

# ---------------------- Build main did_data ----------------------

# Main analysis window: 129 weeks from the first observed week in the data
did_main <- make_did_data(
  df = overall_weekly,
  max_week = 129,
  cap_firstweek = TRUE,
  set_never_treated_to_0 = FALSE,
  add_month = FALSE
)
did_data   <- did_main$data
start_date <- did_main$start_date

# Outcomes to estimate (electricity + gas)
yname_vars <- c("elec_consumption", "gas_consumption")

# ---------------------- CS main results ----------------------

# (A) Main spec: controls are "not yet treated"
main_files <- c(
  out_file("est_cs_elec_weekly.RDS"),
  out_file("est_cs_gas_weekly.RDS")
)

run_cs_models(
  did_data   = did_data,
  y_vars     = yname_vars,
  file_names = main_files,
  anticipation = 4,
  control_group = "notyettreated",
  base_period = "universal",
  xformla = NULL
)

# (B) Base period robustness: varying base period instead of universal
varying_base_files <- sub("\\.RDS$", "_varying_base_period.RDS", main_files)

run_cs_models(
  did_data   = did_data,
  y_vars     = yname_vars,
  file_names = varying_base_files,
  anticipation = 4,
  control_group = "notyettreated",
  base_period = "varying",
  xformla = NULL
)

# ---------------------- Never-treated control group spec ----------------------

# Here we reclassify installs after week 129 as "never treated" by setting firstweek = 0.
# That creates an explicit never-treated group within the truncated panel.
did_never <- make_did_data(
  df = overall_weekly,
  start_date = start_date,        # keep same origin for comparability
  max_week = 129,
  cap_firstweek = FALSE,          # don't drop treated-after-window units
  set_never_treated_to_0 = TRUE,  # instead, recode them to 0
  add_month = FALSE
)
did_data_never <- did_never$data

never_files <- sub("est_cs_", "est_cs_never_treated_", main_files)

run_cs_models(
  did_data   = did_data_never,
  y_vars     = yname_vars,
  file_names = never_files,
  anticipation = 4,
  control_group = "nevertreated",
  base_period = "universal",
  xformla = NULL
)

# ---------------------- Gas-only subset ----------------------

# Restrict to accounts with at least one non-missing gas consumption observation.
gas_accounts <- overall_weekly %>%
  dplyr::group_by(account_id) %>%
  dplyr::summarise(has_gas = any(!is.na(gas_consumption)), .groups = "drop") %>%
  dplyr::filter(has_gas) %>%
  dplyr::pull(account_id)

did_gas_only <- make_did_data(
  df = dplyr::filter(overall_weekly, account_id %in% gas_accounts),
  start_date = start_date,
  max_week = 129,
  cap_firstweek = TRUE,
  set_never_treated_to_0 = FALSE,
  add_month = FALSE
)
did_data_gas_only <- did_gas_only$data

gas_only_file <- out_file("est_cs_elec_weekly_gas_only.RDS")

message("Estimating electricity CS model in gas-only sample")
est_cs_gas_only <- did::att_gt(
  yname = "elec_consumption",
  tname = "week",
  idname = "id",
  gname = "firstweek",
  data = did_data_gas_only,
  anticipation = 4,
  clustervars = "id",
  control_group = "notyettreated",
  est_method = "ipw",
  allow_unbalanced_panel = TRUE,
  base_period = "universal"
)


saveRDS(est_cs_gas_only, gas_only_file)
message("Saved: ", gas_only_file)

# extra: save account ids for gas only analysis
ids_cs_elec_gas_only <- did_gas_only$data %>% 
 filter(id %in% unique(est_cs_gas_only$DIDparams$data$id)) %>%
  pull(account_id)
saveRDS(ids_cs_elec_gas_only, file.path(datapath, "scratch/ids_cs_elec_gas_only.RS"))



# ---------------------- CS with seasonality controls (trends) ----------------------

# Add month-of-year and allow each id to have its own month pattern via id:month.
# (This is a flexible way to control for seasonal demand differences by household.)
did_trends <- make_did_data(
  df = overall_weekly,
  start_date = start_date,
  max_week = 129,
  cap_firstweek = TRUE,
  set_never_treated_to_0 = FALSE,
  add_month = TRUE
)
did_data_trends <- did_trends$data

trend_files <- c(
  out_file("est_cs_elec_weekly_with_trends.RDS"),
  out_file("est_cs_gas_weekly_with_trends.RDS")
)

run_cs_models(
  did_data   = did_data_trends,
  y_vars     = yname_vars,
  file_names = trend_files,
  anticipation = 4,
  control_group = "notyettreated",
  base_period = "universal",
  xformla = ~ id:month
)

# ============================================================
# Anticipation robustness: re-estimate CS for anticipation = 0..10
# ============================================================

# Reuse the main did_data construction (same origin, same truncation)
did_ant <- make_did_data(
  df = overall_weekly,
  start_date = start_date,
  max_week = 129,
  cap_firstweek = TRUE,
  set_never_treated_to_0 = FALSE,
  add_month = FALSE
)
did_data_ant <- did_ant$data

anticipation_periods <- 0:10
output_stub <- c("est_cs_elec_weekly", "est_cs_gas_weekly")

for (a in anticipation_periods) {
  
  message("Estimating CS models with anticipation = ", a)
  
  for (i in seq_along(yname_vars)) {
    
    yname <- yname_vars[i]
    filename <- out_file(paste0(output_stub[i], "_anticipation_", a, ".RDS"))
    
    est_cs <- did::att_gt(
      yname = yname,
      tname = "week",
      idname = "id",
      gname = "firstweek",
      data = did_data_ant,
      anticipation = a,
      clustervars = "id",
      control_group = "notyettreated",
      est_method = "ipw",
      allow_unbalanced_panel = TRUE,
      base_period = "universal"
    )
    
    saveRDS(est_cs, filename)
    message("Saved: ", filename)
  }
}