# ============================================================
# MVPF (HP only): HMG baseline + marginal share + Rennert sensitivity
# Output: LaTeX table + Rennert SCC sensitivity plot
# ============================================================

# ----------------------------
# 0) PATHS
# ----------------------------
PATH_XLSX <- file.path(datapath, "input/HP and Cosy paper welfare analysis.xlsx")
stopifnot(file.exists(PATH_XLSX))

dir.create("tables", showWarnings = FALSE)
dir.create("graphs", showWarnings = FALSE)

# ----------------------------
# 1) FIXED SETTINGS (HP only)
# ----------------------------
YEARS <- 2024:2043
T <- length(YEARS)
base_year <- min(YEARS)
tt <- YEARS - base_year  # 0..19

# Behavioural / policy inputs (from your clean script)
m_default <- 0.50

SUBSIDY_HP  <- 7500
BOILER_COST <- 2250

# Calculate the average value for the dashed line
main_results <- fread(file.path(datapath, "output/eff_df.csv"))  %>% filter(window == "Last 12 months")

ELEC_KWH_CHANGE <- main_results$Electricity
GAS_KWH_CHANGE  <- main_results$Gas

# Fiscal share for climate FE (your same parameters)
uk_gdp_as_proportion_of_global <- 0.032
uk_tax_as_proportion_of_gdp <- 0.335
CLIMATE_FE_SHARE <- uk_gdp_as_proportion_of_global * uk_tax_as_proportion_of_gdp

# AQ discounting (actually using 3.5%)
r_aq <- 0.035

# LBD numbers (from your old script)
lbd_environmental_heatpump <- 3192.62
lbd_price_heatpump <- 1697.59
LBD_TOTAL <- lbd_environmental_heatpump + lbd_price_heatpump

# First-£ parameters (keep explicit & editable)
elasticity <- 1.2

# Rennert preferred SCC (Rennert et al 2022): $185/tCO2 (2020 USD)
RENNERT_USD2020 <- 185
# IMPORTANT: set this to your assumed GBP per USD in 2020 (old script used 0.8)
GBP_PER_USD_2020 <- 0.80

# ----------------------------
# 2) HELPERS
# ----------------------------
disc <- function(r) 1 / (1 + r)^tt

npv <- function(x, r) sum(as.numeric(x) * disc(r), na.rm = TRUE)

money_gbp <- function(x) paste0("£", format(round(x), big.mark = ","))
pct <- function(x) paste0(round(100*x), "\\%")

# ----------------------------
# 3) READ SERIES FROM EXCEL (same ranges as your clean script)
# ----------------------------

# Electricity CI (DESNZ long-run marginal: Domestic)
elec_ci <- read_excel(PATH_XLSX, sheet = "DESNZ elec carbon intensity", range = "B13:F54") |>
  rename(year = 1) |>
  mutate(year = as.integer(year)) |>
  transmute(year, elec_ci = Domestic) |>
  right_join(tibble(year = YEARS), by = "year") |>
  arrange(year) |>
  pull(elec_ci)
stopifnot(length(elec_ci) == T, !any(is.na(elec_ci)))

# Gas CI (DEFRA single cell)
gas_ci <- read_excel(PATH_XLSX, sheet = "Defra gas and elec carbon inten",
                     range = "D26:D26", col_names = FALSE)[[1]][1]

# AQ costs (already inflated in Formulas)
aq_vals <- read_excel(PATH_XLSX, sheet = "Formulas", range = "F119:Y120", col_names = FALSE)
aq_elec <- as.numeric(unlist(aq_vals[1, ]))
aq_gas  <- as.numeric(unlist(aq_vals[2, ]))
stopifnot(length(aq_elec) == T, length(aq_gas) == T)

# Retail prices (HMG) from Formulas
prices <- read_excel(PATH_XLSX, sheet = "Formulas", range = "F134:Y135", col_names = FALSE)
elec_price <- as.numeric(unlist(prices[1, ]))
gas_price  <- as.numeric(unlist(prices[2, ]))
stopifnot(length(elec_price) == T, length(gas_price) == T)

# Gas standing charge (Formulas)
gas_sc <- as.numeric(unlist(read_excel(PATH_XLSX, sheet = "Formulas", range = "F133:Y133", col_names = FALSE)))
stopifnot(length(gas_sc) == T)

# Boiler price for VAT loss (Formulas)
boiler_price <- as.numeric(read_excel(PATH_XLSX, sheet = "Formulas", range = "F128:F128", col_names = FALSE)[1, ])
vat_boiler_oneoff <- -0.2 * boiler_price

# MCS installation costs: use mean as total installation cost
mcs_cost <- read_excel(PATH_XLSX, sheet = "MCS cost data", range = "C50:C57", col_names = FALSE) |>
  unlist() |>
  as.numeric()
total_installation_cost_hp <- mean(mcs_cost, na.rm = TRUE)
private_cost_heatpump <- total_installation_cost_hp - SUBSIDY_HP

# ----------------------------
# 4) PHYSICAL IMPACTS (independent of SCC)
# ----------------------------

# CO2 tonnes saved each year (tonnes)
tonnes_saved <- - (GAS_KWH_CHANGE * gas_ci + ELEC_KWH_CHANGE * elec_ci) / 1000
stopifnot(length(tonnes_saved) == T)

# AQ benefits each year (£): negative of damages change (so benefits positive if damages fall)
aq_benefits <- - (ELEC_KWH_CHANGE * aq_elec + GAS_KWH_CHANGE * aq_gas)
stopifnot(length(aq_benefits) == T)

# VAT on energy each year (£)
vat_energy <- 0.05 * (ELEC_KWH_CHANGE * elec_price + GAS_KWH_CHANGE * gas_price)

# Running cost change each year (£) using HMG unit rates + removing gas standing charge if no connection
# This matches what you were doing in your latest script:
running_cost <- (rep(ELEC_KWH_CHANGE, T) * elec_price +
                   rep(GAS_KWH_CHANGE,  T) * gas_price -
                   gas_sc * 365)

# ----------------------------
# 5) SCC: HMG central in £2023 + Rennert constant in £2023
# ----------------------------
gdp_defl <- read_excel(PATH_XLSX, sheet = "GDP deflator", col_names = FALSE)
get_cell <- function(df, r, c) as.numeric(as.matrix(df)[r, c])

DEF_I76 <- get_cell(gdp_defl, 76, 9) # 2020
DEF_I79 <- get_cell(gdp_defl, 79, 9) # 2023
GBP2020_to_GBP2023 <- 1 / (DEF_I76 / DEF_I79)

scc_hmg_tbl <- read_excel(PATH_XLSX, sheet = "SCC HMG", skip = 2) |>
  rename(year = 1) |>
  transmute(year = as.integer(year),
            scc_gbp2023 = as.numeric(`Central Series`) * GBP2020_to_GBP2023)

scc_hmg <- tibble(year = YEARS) |>
  left_join(scc_hmg_tbl, by = "year") |>
  arrange(year) |>
  mutate(scc_gbp2023 = na.locf(scc_gbp2023, na.rm = FALSE),
         scc_gbp2023 = na.locf(scc_gbp2023, fromLast = TRUE)) |>
  pull(scc_gbp2023) |>
  as.numeric()

# Rennert SCC (constant across years), converted to £2023
rennert_scc_gbp2023 <- (RENNERT_USD2020 * GBP_PER_USD_2020) * GBP2020_to_GBP2023
scc_rennert <- rep(rennert_scc_gbp2023, T)

# ----------------------------
# 6) ONE SCENARIO CALC (SHORT, READABLE)
# ----------------------------
calc_hp <- function(label, r_disc, m, scc_vec) {
  
  # --- climate damages monetised ---
  # Total climate benefit (global SCC) each year
  clim_global <- tonnes_saved * scc_vec
  
  # Consumer WTP for climate in your old script excludes the UK fiscal share:
  # consumer climate WTP = SCC * (1 - UK share taxed)
  clim_consumer <- clim_global * (1 - CLIMATE_FE_SHARE)
  
  # Gov “climate FE” revenue effect = SCC * CLIMATE_FE_SHARE
  clim_gov <- clim_global * CLIMATE_FE_SHARE
  
  # --- discounted NPVs ---
  NPV_clim_consumer <- npv(clim_consumer, r_disc)
  NPV_clim_gov      <- npv(clim_gov, r_disc)
  
  # AQ: in your old script you discounted AQ at discount_rate; in your newer approach you separated r_aq.
  # Keep the newer (better) choice: AQ discounted at r_aq.
  NPV_aq <- npv(aq_benefits, r_aq)
  
  # Energy VAT (discount at r_disc)
  NPV_vat_energy <- npv(vat_energy, r_disc)
  
  # Running costs + capex (resource cost)
  NPV_running <- npv(running_cost, r_disc)
  resource_cost_total <- private_cost_heatpump - BOILER_COST + NPV_running
  
  # Abatement (tonnes) over horizon (not discounted)
  tonnes_total <- sum(tonnes_saved, na.rm = TRUE)
  
  # -----------------
  # A) "Average" MVPF (your paper definition)
  # -----------------
  # consumer transfer WTP = subsidy*(1 - 0.5*m) exactly as in your old script logic
  consumer_transfer <- SUBSIDY_HP * (1 - 0.5*m)
  
  # environmental WTP in numerator: marginal share * (NPV climate consumer + NPV AQ)
  env_wtp_marginal <- m * (NPV_clim_consumer + NPV_aq)
  
  numerator_avg <- consumer_transfer + env_wtp_marginal
  
  # fiscal components (scaled by m, consistent with your old script)
  vat_component <- m * (NPV_vat_energy + vat_boiler_oneoff)
  climate_gov_component <- m * (NPV_clim_gov)
  
  denominator_avg <- SUBSIDY_HP - vat_component - climate_gov_component
  
  mvpf_avg <- numerator_avg / denominator_avg
  
  # -----------------
  # B) "First £" MVPF (your old-script formula)
  # -----------------
  # This is the marginal impact per £ of gov spending, scaled by elasticity / total install cost.
  # Use your exact structure:
  # Numerator_first_pound = 1 + (Discounted_environmental_wtp * elasticity / total_installation_cost)
  # Denominator_first_pound = 1 + ((Discounted_env_gov_rev - Discounted_VAT)* elasticity / total_installation_cost)
  #
  # Map terms carefully:
  # - Discounted_environmental_wtp_heatpump in your old code is (AQ + climate consumer) * -1 then summed.
  #   Here our series are already “benefits” (positive), so no *-1 needed.
  #
  disc_env_wtp_total <- (NPV_clim_consumer + NPV_aq)           # not multiplied by m in First-£ formula in your old code
  disc_env_gov_total <- (NPV_clim_gov)                         # same
  disc_vat_total     <- (NPV_vat_energy + vat_boiler_oneoff)   # same
  
  numerator_fp <- 1 + (disc_env_wtp_total * elasticity / total_installation_cost_hp)
  denominator_fp <- 1 + ((disc_env_gov_total - disc_vat_total) * elasticity / total_installation_cost_hp)
  
  mvpf_fp <- numerator_fp / denominator_fp
  
  # With LBD: add LBD (environmental + price) to the environmental WTP term in numerator
  numerator_fp_lbd <- 1 + ((disc_env_wtp_total + LBD_TOTAL) * elasticity / total_installation_cost_hp)
  mvpf_fp_lbd <- numerator_fp_lbd / denominator_fp
  
  # -----------------
  # C) cost per tonne metrics (match your table)
  # -----------------
  resource_cpt <- resource_cost_total / tonnes_total
  gov_cost_total <- denominator_avg
  gov_cpt <- gov_cost_total / (tonnes_total * m)
  social_cpt <- (resource_cost_total + gov_cost_total) / tonnes_total
  
  tibble(
    type = label,
    discount_rate = r_disc,
    marginal = m,
    Average = mvpf_avg,
    `First £` = mvpf_fp,
    `First £, w/ LBD` = mvpf_fp_lbd,
    Resource = resource_cpt,
    Government = gov_cpt,
    Social = social_cpt
  )
}

# ----------------------------
# 7) THREE ROWS 
# ----------------------------
out_tbl <- bind_rows(
  calc_hp("MAC-based", r_disc = 0.035, m = 0.50, scc_vec = scc_hmg),
  calc_hp("MAC-based", r_disc = 0.035, m = 0.25, scc_vec = scc_hmg),
  calc_hp("SCC (IAM)", r_disc = 0.020, m = 0.50, scc_vec = scc_rennert),
  calc_hp("SCC (IAM)", r_disc = 0.035, m = 0.50, scc_vec = scc_rennert)

)

print(out_tbl)

# 1) Format a display table (NO % characters left unescaped)
latex_tbl <- out_tbl %>%
  mutate(
    disc_str = paste0(round(100 * discount_rate, 1), "\\%"),  # <- key fix
    marg_str = paste0(round(100 * marginal, 0), "\\%"),       # <- key fix
    
    avg_str  = format(signif(Average, 3), trim = TRUE),
    fp_str   = format(signif(`First £`, 3), trim = TRUE),
    fplbd_str = format(signif(`First £, w/ LBD`, 3), trim = TRUE),
    
    res_str  = money_gbp(Resource),
    gov_str  = money_gbp(Government),
    soc_str  = money_gbp(Social)
  ) %>%
  select(type, disc_str, marg_str, avg_str, fp_str, fplbd_str, res_str, gov_str, soc_str)

# 2) Turn rows into LaTeX lines
row_lines <- apply(latex_tbl, 1, function(r) {
  paste0(
    r[[1]], " & ", r[[2]], " & ", r[[3]], " & ",
    r[[4]], " & ", r[[5]], " & ", r[[6]], " & ",
    r[[7]], " & ", r[[8]], " & ", r[[9]], " \\\\"
  )
})

# 3) Optional: insert a midrule after row 2 (matches your old layout)
if (length(row_lines) >= 3) {
  row_lines <- append(row_lines, "\\midrule", after = 2)
}

# 4) Assemble a pure tabular (NO table env)
latex_lines <- c(
  "\\begin{tabular}{lcccccccc}",   # <- also: first column left-aligned usually looks nicer
  "\\toprule",
  "\\multicolumn{3}{c}{ } & \\multicolumn{3}{c}{MVPF} & \\multicolumn{3}{c}{Cost per tonne} \\\\",
  "\\cmidrule(l{3pt}r{3pt}){4-6} \\cmidrule(l{3pt}r{3pt}){7-9}",
  " & Discount Rate & Marginal & Average & First \\pounds & First \\pounds, w/ LBD & Resource & Government & Social \\\\",
  "\\midrule",
  row_lines,
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(latex_lines, "tables/MVPF.tex")
cat("Saved LaTeX tabular to tables/MVPF.tex\n")

# ============================================================
# 10) Waterfall chart — preferred MVPF
#     (UK SCC = HMG central, r = 3.5%, m = m_default)
# ============================================================

# --- Preferred scenario discount rate (baseline) ---
r_pref <- 0.035

# --- Build the underlying streams using already-defined objects ---
clim_global_stream   <- tonnes_saved * scc_hmg
clim_consumer_stream <- clim_global_stream * (1 - CLIMATE_FE_SHARE)
clim_gov_stream      <- clim_global_stream * CLIMATE_FE_SHARE

# --- NPVs (using your npv() helper + existing r_aq) ---
NPV_clim_consumer <- npv(clim_consumer_stream, r_pref)
NPV_clim_gov      <- npv(clim_gov_stream, r_pref)
NPV_aq            <- npv(aq_benefits, r_aq)
NPV_vat_energy    <- npv(vat_energy, r_pref)

# ============================================================
# Subsidy level S such that MVPF = 1 (First-£ algebra), using current script objects
# ============================================================

# These are already computed above in your script:
# NPV_clim_consumer, NPV_aq, NPV_clim_gov, NPV_vat_energy, vat_boiler_oneoff, LBD_TOTAL

Discounted_environmental_wtp_heatpump <- NPV_clim_consumer + NPV_aq

# Gov-side “offsets” term consistent with your denominator structure:
# (environmental gov rev) - (VAT change on energy) - (VAT boiler change)
Discounted_environmental_gov_rev_heatpump <- NPV_clim_gov
Discounted_vat_elec_gas_change_heatpump   <- NPV_vat_energy
Vat_boiler_change_heatpump                <- vat_boiler_oneoff

# Solve S where Numerator = Denominator (elasticity/total_cost cancels out)
S_at_MVPF_1 <- Discounted_environmental_wtp_heatpump -
  (Discounted_environmental_gov_rev_heatpump -
     Discounted_vat_elec_gas_change_heatpump -
     Vat_boiler_change_heatpump)

S_at_MVPF_1_LBD <- (Discounted_environmental_wtp_heatpump + LBD_TOTAL) -
  (Discounted_environmental_gov_rev_heatpump -
     Discounted_vat_elec_gas_change_heatpump -
     Vat_boiler_change_heatpump)

cat("S at MVPF = 1 (base): ", money_gbp(S_at_MVPF_1), "\n")
cat("S at MVPF = 1 (+LBD): ", money_gbp(S_at_MVPF_1_LBD), "\n")


# ============================================================
# Waterfall components per £ of subsidy
# (match your original plotting conventions)
# ============================================================

# ---- BENEFITS (scaled per £ subsidy) ----
transfer_benefit <- (1 - 0.5 * m_default) * SUBSIDY_HP
co2_benefit      <- m_default * NPV_clim_consumer
aq_benefit       <- m_default * NPV_aq
total_benefit    <- transfer_benefit + co2_benefit + aq_benefit

# ---- GOVERNMENT COST COMPONENTS (scaled per £ subsidy) ----
# Use the same sign logic as your calc_hp() denominator:
# denominator_avg = SUBSIDY_HP - m*(NPV_vat_energy + vat_boiler_oneoff) - m*(NPV_clim_gov)
# For the waterfall "cost" bars, we plot the *positive cost contributions*:
subsidy_cost     <- SUBSIDY_HP
lost_vat_boiler  <- m_default * (-vat_boiler_oneoff)     # vat_boiler_oneoff is negative => loss is positive
extra_vat_energy <- m_default * (-NPV_vat_energy)
climate_fe_cost  <- m_default * (-NPV_clim_gov)

total_gov_cost <- subsidy_cost + lost_vat_boiler + extra_vat_energy + climate_fe_cost

# ---- scale everything per £1 of subsidy ----
scale_denom <- SUBSIDY_HP

benefits_scaled <- c(
  transfer_benefit,
  co2_benefit,
  aq_benefit,
  total_benefit
) / scale_denom

costs_scaled <- c(
  subsidy_cost,
  lost_vat_boiler,
  extra_vat_energy,
  climate_fe_cost,
  total_gov_cost
) / scale_denom

# ---- labels and ordering ----
categories <- factor(
  c("Transfers", "CO2 benefits", "Air pollution", "Total benefits",
    "Subsidy cost", "Lost VAT (Boiler Purchase)",
    "Extra VAT (Energy)", "Climate change FE", "Total Govt Cost"),
  levels = c("Transfers", "CO2 benefits", "Air pollution", "Total benefits",
             "Subsidy cost", "Lost VAT (Boiler Purchase)",
             "Extra VAT (Energy)", "Climate change FE", "Total Govt Cost")
)

values <- c(benefits_scaled, costs_scaled)

colors <- c(
  "#87B6F8", "#87B6F8", "#87B6F8", "#2E354A",
  "#D5AFF2", "#D5AFF2", "#D5AFF2", "#D5AFF2", "#4B2C6F"
)

# ---- waterfall cumulative bounds ----
ymin <- c(
  0,
  benefits_scaled[1],
  benefits_scaled[1] + benefits_scaled[2],
  0,
  0,
  costs_scaled[1],
  costs_scaled[1] + costs_scaled[2],
  costs_scaled[1] + costs_scaled[2] + costs_scaled[3],
  0
)

ymax <- c(
  benefits_scaled[1],
  benefits_scaled[1] + benefits_scaled[2],
  benefits_scaled[1] + benefits_scaled[2] + benefits_scaled[3],
  benefits_scaled[4],
  costs_scaled[1],
  costs_scaled[1] + costs_scaled[2],
  costs_scaled[1] + costs_scaled[2] + costs_scaled[3],
  costs_scaled[1] + costs_scaled[2] + costs_scaled[3] + costs_scaled[4],
  costs_scaled[5]
)

wf_df <- data.frame(categories, values, colors, ymin, ymax)

custom_labels <- c(
  "Transfers",
  expression(CO[2]~benefits),
  "Air pollution",
  "Total benefits",
  "Subsidy cost",
  "Lost VAT\n(Boiler Purchase)",
  "Extra VAT\n(Energy)",
  "Climate change FE",
  "Total Govt Cost"
)

# ---- plot ----
p_wf <- ggplot(wf_df) +
  geom_rect(aes(
    xmin = as.numeric(categories) - 0.4,
    xmax = as.numeric(categories) + 0.4,
    ymin = ymin,
    ymax = ymax,
    fill = colors
  )) +
  scale_fill_identity() +
  geom_text(aes(x = categories, y = ymax + 0.02, label = round(values, 3)),
            vjust = -0.3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),
                                angle = 0, vjust = 0.5),
    plot.title = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(y = "£ per £ of subsidy") +
  scale_x_discrete(labels = custom_labels) +
  coord_cartesian(ylim = c(0, max(ymax) + 0.08))

p_wf

ggsave("graphs/waterfall_hp_preferred.png", plot = p_wf,
       width = 10, height = 6, bg = "transparent", dpi = 300)

cat("Saved waterfall to graphs/waterfall_hp_preferred.png\n")


