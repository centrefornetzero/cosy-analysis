# ---------------------------------------------------------------------------
# cosy_survey_figures.R
#
# Reproduces every number cited in the "Survey Responses" appendix
# (\label{subsec:survey}) and the automation figure in ssec:cosy_main,
# from the raw questionnaire export (responses.csv, 393 rows x 41 cols).
#
# Every statistic carries an EXPLICIT denominator: item non-response varies
# (393 submitted; 384 answered the unit-rates question in some form; 382 gave
# an install date; 391 answered the low-carbon-technology block).
#
# Three customer keys (kid) submitted twice. Both raw and deduplicated figures
# are reported; see the duplicate diagnostic below.
#
# Writes survey_numbers.tex (\newcommand definitions).
# ---------------------------------------------------------------------------

library(dplyr)
library(readr)
library(tibble)

IN_CSV    <- "responses.csv"
OUT_TEX   <- "survey_numbers.tex"
N_SENT    <- 1000
SEND_DATE <- as.Date("2024-07-08")

# --- load ------------------------------------------------------------------
# Five columns are literally named "Other" and two headers carry trailing
# spaces, so we validate the trimmed names then address columns by POSITION.
raw <- read_csv(IN_CSV, col_types = cols(.default = col_character()),
                name_repair = "minimal")

stopifnot(ncol(raw) == 41)
nm <- trimws(names(raw))
stopifnot(
  nm[1]  == "#",
  nm[2]  == "Do you have a heat pump?",
  nm[5]  == "What's the brand of your current heat pump?",
  nm[7]  == "When was your heat pump installed?",
  nm[8]  == "Do you know your heat pump's size in kW?",
  nm[10] == "Other",                                    # free text for Q9
  nm[11] == "Smart thermostat or other remote control",
  nm[12] == "Lower flow temperatures",
  nm[13] == "Activate a special mode on heat pump",
  nm[14] == "Manual adjustments",
  nm[15] == "Use alternative heating sources",
  nm[18] == "EV charger",
  nm[19] == "Home battery",
  nm[29] == "Electric vehicle(s)",
  nm[30] == "Solar PV panels",
  nm[31] == "Batteries",
  nm[32] == "None of the above",
  nm[35] == "kid",
  nm[36] == "Response Type"
)

names(raw) <- paste0("v", seq_len(ncol(raw)))   # kill duplicate names

dat <- raw %>%
  transmute(
    id             = v1,
    has_hp         = v2,
    brand          = v5,
    install_raw    = v7,
    size_band      = v8,
    responds       = v9,
    responds_other = v10,   # prose written instead of picking an option
    m_thermostat   = v11,
    m_flowtemp     = v12,
    m_specialmode  = v13,
    m_manual       = v14,
    m_altheat      = v15,
    m_noadjust     = v16,
    heat_other     = v17,
    a_evcharger    = v18,
    a_battery      = v19,
    a_appliances   = v20,
    a_noadjust     = v21,
    appl_other     = v22,
    lct_ev         = v29,
    lct_solar      = v30,
    lct_battery    = v31,
    lct_none       = v32,
    lct_other      = v33,
    kid            = v35,
    response_type  = v36,
    start_raw      = v37,
    submit_raw     = v39,
    network_id     = v40
  ) %>%
  filter(response_type == "completed") %>%
  mutate(
    install_date = as.Date(substr(install_raw, 1, 10)),
    submit_ts    = as.POSIXct(submit_raw, tz = "UTC"),
    submit_date  = as.Date(submit_ts),
    install_year = as.integer(format(install_date, "%Y")),
    responds_yes = !is.na(responds) & responds == "Yes",
    # Answered in SOME form: picked an option, or wrote prose.
    responds_ans = !is.na(responds) | !is.na(responds_other),
    knows_size   = !is.na(size_band) & size_band != "Not sure",
    # Automation = smart thermostat OR special mode OR lower flow temperature.
    # Flow temperature counts because it is set on a schedule (lower at peak,
    # higher off-peak) rather than adjusted manually day to day.
    automation        = !is.na(m_thermostat) | !is.na(m_specialmode) | !is.na(m_flowtemp),
    automation_narrow = !is.na(m_thermostat) | !is.na(m_specialmode),
    lct_answered = !is.na(lct_ev) | !is.na(lct_solar) | !is.na(lct_battery) |
      !is.na(lct_none) | !is.na(lct_other)
  )

stopifnot(nrow(dat) == 393, n_distinct(dat$id) == nrow(dat))
# The adjustment-method block is only shown to those who said "Yes".
stopifnot(all(dat$automation[!dat$responds_yes] == FALSE))
# "None of the above" must be exclusive of owning a technology.
stopifnot(!any(!is.na(dat$lct_none) &
                 (!is.na(dat$lct_ev) | !is.na(dat$lct_solar) | !is.na(dat$lct_battery))))

pct <- function(k, n) round(100 * k / n, 1)

# ---------------------------------------------------------------------------
# DUPLICATE DIAGNOSTIC
#
# `kid` is the customer/account key; `Network ID` is a network/session
# identifier and is NOT a household -- three distinct kids share one Network ID
# with entirely different answers. So deduplicate on kid, never on Network ID.
# ---------------------------------------------------------------------------
cat("\n=== DUPLICATE DIAGNOSTIC ===\n")

dup_kids <- dat %>% count(kid) %>% filter(n > 1) %>% pull(kid)
cat(sprintf("customer keys submitting more than once: %d (%d rows)\n",
            length(dup_kids), sum(dat$kid %in% dup_kids)))
cat(sprintf("Network IDs shared across >1 kid: %d\n",
            dat %>% distinct(network_id, kid) %>% count(network_id) %>%
              filter(n > 1) %>% nrow()))

cmp_cols <- c("id", "brand", "size_band", "responds", "install_date",
              "automation", "submit_ts", "network_id")
for (k in dup_kids) {
  cat(sprintf("\n-- kid %s --\n", k))
  g <- dat %>% filter(kid == k) %>% arrange(submit_ts) %>% select(all_of(cmp_cols))
  print(as.data.frame(t(g)))
  gap <- as.numeric(difftime(max(g$submit_ts), min(g$submit_ts), units = "hours"))
  cat(sprintf("   %.1f hours apart | same network: %s | brand agrees: %s | size agrees: %s | automation agrees: %s\n",
              gap, n_distinct(g$network_id) == 1, n_distinct(g$brand) == 1,
              n_distinct(g$size_band) == 1, n_distinct(g$automation) == 1))
}
cat("\nIf every pair agrees on brand, size, response and automation and differs\n",
    "only in recalled dates and fuller free text on the later attempt, these are\n",
    "the same household submitting twice. Keeping the LATEST (most complete).\n", sep = "")

ded <- dat %>%
  group_by(kid) %>%
  slice_max(submit_ts, n = 1, with_ties = FALSE) %>%
  ungroup()
stopifnot(nrow(ded) == nrow(dat) - length(dup_kids), !any(duplicated(ded$kid)))

# --- install dates that cannot be right ------------------------------------
bad <- dat %>% filter(!is.na(install_date), install_date > submit_date)
if (nrow(bad) > 0) {
  cat(sprintf("\nWARNING: %d install date(s) fall after the response was submitted:\n",
              nrow(bad)))
  print(bad %>% select(id, install_date, submit_date) %>% arrange(install_date))
}

# ---------------------------------------------------------------------------
# FIGURES
# ---------------------------------------------------------------------------
size_levels <- c("2-3 kW", "3-4 kW", "4-5 kW", "5-7 kW",
                 "7-9 kW", "9-12 kW", "12-16 kW", "16 kW or more")

survey_figures <- function(d, label) {
  n <- nrow(d)
  cat(sprintf("\n\n##### %s (n = %d) #####\n", label, n))
  
  # -- brands
  cat("\n-- BRAND --\n")
  print(d %>% count(brand, sort = TRUE) %>% mutate(share = pct(n, !!n)), n = Inf)
  top3 <- c("Mitsubishi", "Daikin", "Vaillant")
  k <- sum(d$brand %in% top3); named <- sum(!is.na(d$brand))
  cat(sprintf("top three: %d = %.1f%% of %d respondents | %.1f%% of %d who named a brand\n",
              k, pct(k, n), n, pct(k, named), named))
  
  # -- size
  n_knows <- sum(d$knows_size); n_ans <- sum(!is.na(d$size_band))
  cat("\n-- SIZE --\n")
  cat(sprintf("gave a band: %d = %.1f%% of %d | %.1f%% of the %d who answered\n",
              n_knows, pct(n_knows, n), n, pct(n_knows, n_ans), n_ans))
  cat(sprintf("\"Not sure\": %d | missing: %d\n",
              sum(d$size_band == "Not sure", na.rm = TRUE), sum(is.na(d$size_band))))
  st <- d %>% filter(knows_size) %>%
    mutate(size_band = factor(size_band, levels = size_levels)) %>%
    count(size_band, .drop = FALSE) %>%
    mutate(share = pct(n, n_knows), cum = cumsum(share))
  stopifnot(!any(is.na(st$size_band)))          # catches an unexpected new band
  print(st, n = Inf)
  median_band <- as.character(st$size_band[which(st$cum >= 50)[1]])
  k716 <- sum(d$size_band[d$knows_size] %in% c("7-9 kW", "9-12 kW", "12-16 kW"))
  cat(sprintf("median band: %s | 7-16 kW: %d = %.1f%% of those who know\n",
              median_band, k716, pct(k716, n_knows)))
  
  # -- responds to unit rates
  n_yes <- sum(d$responds_yes); n_ansq <- sum(d$responds_ans)
  cat("\n-- RESPONDS TO UNIT RATES --\n")
  print(d %>% count(responds) %>% mutate(share = pct(n, !!n)), n = Inf)
  cat(sprintf("wrote prose instead of picking an option: %d\n",
              sum(!is.na(d$responds_other))))
  cat(sprintf("Yes: %d = %.1f%% of %d respondents  <-- recommended headline\n",
              n_yes, pct(n_yes, n), n))
  cat(sprintf("          %.1f%% of the %d who answered in some form\n",
              pct(n_yes, n_ansq), n_ansq))
  
  # -- methods, base = those who said Yes
  y <- d %>% filter(responds_yes)
  mt <- tibble(
    method = c("Smart thermostat / remote", "Manual adjustments",
               "Lower flow temperatures", "Alternative heating sources",
               "Special mode on heat pump", "Do not adjust heating at all",
               "Home battery", "EV charger", "Other appliances",
               "Do not adjust other electricity use"),
    n = c(sum(!is.na(y$m_thermostat)),  sum(!is.na(y$m_manual)),
          sum(!is.na(y$m_flowtemp)),    sum(!is.na(y$m_altheat)),
          sum(!is.na(y$m_specialmode)), sum(!is.na(y$m_noadjust)),
          sum(!is.na(y$a_battery)),     sum(!is.na(y$a_evcharger)),
          sum(!is.na(y$a_appliances)),  sum(!is.na(y$a_noadjust)))
  ) %>% mutate(share = pct(n, n_yes)) %>% arrange(desc(n))
  cat(sprintf("\n-- METHODS (base: %d who said Yes; multi-select) --\n", n_yes))
  print(mt, n = Inf)
  
  n_auto <- sum(y$automation)
  cat(sprintf("\nAUTOMATION (thermostat | special mode | flow temp): %d = %.1f%% of %d\n",
              n_auto, pct(n_auto, n_yes), n_yes))
  cat(sprintf("  narrow (thermostat | special mode only): %d = %.1f%%\n",
              sum(y$automation_narrow), pct(sum(y$automation_narrow), n_yes)))
  cat(sprintf("  as a share of all %d respondents: %.1f%%\n", n, pct(n_auto, n)))
  
  # -- low-carbon technology
  n_lct <- sum(d$lct_answered)
  cat(sprintf("\n-- LOW-CARBON TECHNOLOGY (base: %d who answered the block) --\n", n_lct))
  print(tibble(
    tech = c("Solar PV", "Batteries", "Electric vehicle(s)", "None of the above", "Other"),
    n = c(sum(!is.na(d$lct_solar)), sum(!is.na(d$lct_battery)), sum(!is.na(d$lct_ev)),
          sum(!is.na(d$lct_none)),  sum(!is.na(d$lct_other)))
  ) %>% mutate(share = pct(n, n_lct)), n = Inf)
  
  # -- installation year
  inst <- d %>% filter(!is.na(install_date))
  n_inst <- nrow(inst)
  cat(sprintf("\n-- INSTALL YEAR (base: %d who gave a date) --\n", n_inst))
  yt <- inst %>%
    mutate(period = if_else(install_year < 2020, "pre-2020", as.character(install_year))) %>%
    count(period) %>% mutate(share = pct(n, n_inst)) %>% arrange(period)
  print(yt, n = Inf)
  cat(sprintf("shares sum to %.1f%% over %d responses\n", sum(yt$share), sum(yt$n)))
  stopifnot(sum(yt$n) == n_inst)
  cat(sprintf("pre-2020: %d (%.1f%%) | of which 2010-2019: %d, before 2010: %d\n",
              sum(inst$install_year < 2020), pct(sum(inst$install_year < 2020), n_inst),
              sum(inst$install_year >= 2010 & inst$install_year <= 2019),
              sum(inst$install_year < 2010)))
  n24   <- sum(inst$install_year == 2024)
  n24h1 <- sum(inst$install_year == 2024 &
                 as.integer(format(inst$install_date, "%m")) <= 6)
  cat(sprintf("2024 in full: %d = %.1f%% | Jan-Jun 2024 only: %d = %.1f%%\n",
              n24, pct(n24, n_inst), n24h1, pct(n24h1, n_inst)))
  
  invisible(list(
    n = n, n_yes = n_yes, n_ansq = n_ansq, n_auto = n_auto,
    n_auto_narrow = sum(y$automation_narrow),
    n_inst = n_inst, n_lct = n_lct, n_knows = n_knows,
    median_band = median_band,
    pct_yes        = pct(n_yes, n),
    pct_auto       = pct(n_auto, n_yes),
    pct_thermostat = pct(sum(!is.na(y$m_thermostat)), n_yes),
    pct_manual     = pct(sum(!is.na(y$m_manual)), n_yes),
    pct_flowtemp   = pct(sum(!is.na(y$m_flowtemp)), n_yes),
    pct_altheat    = pct(sum(!is.na(y$m_altheat)), n_yes),
    pct_special    = pct(sum(!is.na(y$m_specialmode)), n_yes),
    pct_battery    = pct(sum(!is.na(y$a_battery)), n_yes),
    pct_evcharger  = pct(sum(!is.na(y$a_evcharger)), n_yes),
    pct_solar_own  = pct(sum(!is.na(d$lct_solar)), n_lct),
    pct_batt_own   = pct(sum(!is.na(d$lct_battery)), n_lct),
    pct_ev_own     = pct(sum(!is.na(d$lct_ev)), n_lct),
    pct_no_lct     = pct(sum(!is.na(d$lct_none)), n_lct),
    pct_knows_size = pct(n_knows, n),
    pct_pre2020    = pct(sum(inst$install_year < 2020), n_inst),
    pct_2020       = pct(sum(inst$install_year == 2020), n_inst),
    pct_2021       = pct(sum(inst$install_year == 2021), n_inst),
    pct_2022       = pct(sum(inst$install_year == 2022), n_inst),
    pct_2023       = pct(sum(inst$install_year == 2023), n_inst),
    pct_2024       = pct(n24, n_inst),
    pct_2024h1     = pct(n24h1, n_inst)
  ))
}

cat(sprintf("\n=== RESPONSE ===\nsent %d | submitted %d (%.1f%%) | unique households %d\n",
            N_SENT, nrow(dat), pct(nrow(dat), N_SENT), nrow(ded)))

r_raw <- survey_figures(dat, "ALL SUBMISSIONS")
r_ded <- survey_figures(ded, "DEDUPLICATED ON kid (latest submission)")

# --- sensitivity -----------------------------------------------------------
cat("\n\n##### RAW vs DEDUPLICATED #####\n")
print(tibble(
  statistic = c("n", "responds Yes", "% Yes", "% automation", "% thermostat",
                "% home battery", "% solar owned", "% pre-2020 install"),
  raw = c(r_raw$n, r_raw$n_yes, r_raw$pct_yes, r_raw$pct_auto, r_raw$pct_thermostat,
          r_raw$pct_battery, r_raw$pct_solar_own, r_raw$pct_pre2020),
  dedup = c(r_ded$n, r_ded$n_yes, r_ded$pct_yes, r_ded$pct_auto, r_ded$pct_thermostat,
            r_ded$pct_battery, r_ded$pct_solar_own, r_ded$pct_pre2020)
) %>% mutate(diff = dedup - raw), n = Inf)

# --- emit LaTeX (deduplicated = preferred) ---------------------------------
r  <- r_ded
p0 <- function(x) sprintf("%.0f\\%%", x)
p1 <- function(x) sprintf("%.1f\\%%", x)
# Size bands arrive as raw data labels ("9-12 kW"). Typeset them properly:
# en dash for the range, non-breaking space before the unit.
fmt_band <- function(x) {
  x <- gsub("-", "--", x, fixed = TRUE)
  gsub(" kW", "~kW", x, fixed = TRUE)
}
tex <- c(
  sprintf("%% Auto-generated %s by cosy_survey_figures.R -- do not edit by hand.",
          format(Sys.time(), "%Y-%m-%d %H:%M")),
  "% Base: deduplicated on customer key (kid), latest submission retained.",
  sprintf("\\newcommand{\\SurveyGenerated}{%s}", format(Sys.Date())),
  sprintf("\\newcommand{\\SurveySent}{%s}",              format(N_SENT, big.mark = ",")),
  sprintf("\\newcommand{\\SurveyN}{%d}",                 r$n),
  sprintf("\\newcommand{\\SurveyNSubmitted}{%d}",        r_raw$n),
  sprintf("\\newcommand{\\SurveyNInstall}{%d}",          r$n_inst),
  sprintf("\\newcommand{\\SurveyNLCT}{%d}",              r$n_lct),
  sprintf("\\newcommand{\\SurveyNResponds}{%d}",         r$n_yes),
  sprintf("\\newcommand{\\SurveyRespondsPct}{%s}",       p0(r$pct_yes)),
  sprintf("\\newcommand{\\SurveyAutoPct}{%s}",           p0(r$pct_auto)),
  sprintf("\\newcommand{\\SurveyThermostatPct}{%s}",     p0(r$pct_thermostat)),
  sprintf("\\newcommand{\\SurveyManualPct}{%s}",         p0(r$pct_manual)),
  sprintf("\\newcommand{\\SurveyFlowTempPct}{%s}",       p0(r$pct_flowtemp)),
  sprintf("\\newcommand{\\SurveyAltHeatPct}{%s}",        p0(r$pct_altheat)),
  sprintf("\\newcommand{\\SurveySpecialModePct}{%s}",    p0(r$pct_special)),
  sprintf("\\newcommand{\\SurveyBatteryPct}{%s}",        p0(r$pct_battery)),
  sprintf("\\newcommand{\\SurveyEVChargerPct}{%s}",      p0(r$pct_evcharger)),
  sprintf("\\newcommand{\\SurveySolarPct}{%s}",          p0(r$pct_solar_own)),
  sprintf("\\newcommand{\\SurveyBatteryOwnPct}{%s}",     p0(r$pct_batt_own)),
  sprintf("\\newcommand{\\SurveyEVOwnPct}{%s}",          p0(r$pct_ev_own)),
  sprintf("\\newcommand{\\SurveyNoLCTPct}{%s}",          p0(r$pct_no_lct)),
  sprintf("\\newcommand{\\SurveyKnowsSizePct}{%s}",      p0(r$pct_knows_size)),
  sprintf("\\newcommand{\\SurveyMedianSize}{%s}",        fmt_band(r$median_band)),
  "% Install-year shares at ONE decimal place: at 0 dp they do not sum to 100.",
  sprintf("\\newcommand{\\SurveyInstPreTwenty}{%s}",        p1(r$pct_pre2020)),
  sprintf("\\newcommand{\\SurveyInstYrTwenty}{%s}",         p1(r$pct_2020)),
  sprintf("\\newcommand{\\SurveyInstYrTwentyOne}{%s}",      p1(r$pct_2021)),
  sprintf("\\newcommand{\\SurveyInstYrTwentyTwo}{%s}",      p1(r$pct_2022)),
  sprintf("\\newcommand{\\SurveyInstYrTwentyThree}{%s}",    p1(r$pct_2023)),
  sprintf("\\newcommand{\\SurveyInstYrTwentyFour}{%s}",     p1(r$pct_2024)),
  sprintf("\\newcommand{\\SurveyInstYrTwentyFourHOne}{%s}", p1(r$pct_2024h1))
)

# Guard: the reported shares must actually sum to 100.0, or the text needs a
# "does not sum due to rounding" caveat rather than silently misleading.
shares <- c(r$pct_pre2020, r$pct_2020, r$pct_2021, r$pct_2022, r$pct_2023, r$pct_2024)
stopifnot(abs(sum(shares) - 100) < 0.15)
cat(sprintf("install-year shares sum to %.1f%%\n", sum(shares)))
writeLines(tex, OUT_TEX)
cat(sprintf("\nWrote %s (%d definitions)\n",
            OUT_TEX, sum(grepl("^\\\\newcommand", tex))))
