# =============================================================================
#  LEARNING-BY-DOING WELFARE MODEL
#  Standalone R implementation (base R only -- no packages required)
#
#  Computes, for a subsidy to a learning technology:
#     E(t)   : $ of lifetime pollution damage avoided by a unit bought in year t
#     cost(X): unit cost as the technology moves down the learning curve
#     DP     : dynamic price benefit  (future buyers pay less)
#     Dpi    : dynamic profit effect  (producers)
#     DE     : dynamic environmental benefit (induced future adoption)
#
#  All three D's are DIMENSIONLESS -- they are per $1 of program cost.
#  Multiply by your program cost to express them in dollars.
# =============================================================================


# =============================================================================
#  1. INPUTS -- edit this block only
# =============================================================================
#
#  *** PICK ONE "UNIT" AND USE IT EVERYWHERE. ***
#
#  This is the single easiest way to get silently wrong answers. X0, x0, cost0
#  and emis_per_yr must all refer to the SAME physical thing -- one rooftop
#  system, or one turbine, or one car, or one watt. Not a mix.
#
#  The calibration below is for the UK AIR-SOURCE HEAT PUMP subsidy analysed in
#  05_MVPF.R, with production tracked in GW of installed heat pump capacity:
#     cost0  = £1,075/kW x 1,000,000 kW/GW      = £1,075m per GW
#     X0     = ~1,000 GW global cumulative capacity in operation (~2021-22)
#     x0     = 107 GW added globally in 2025
#     emis   = 1.16 t CO2e/household/yr, rescaled to per-GW via avg_hp_kw
#  Section 7 prints a unit-consistency check. Read it.
#
## ---- Unit conversion ---------------------------------------------------------
avg_hp_kw    <- 8          # average heat pump system size (kW), used to convert
                           #   the paper's per-household emissions figure into the
                           #   per-GW terms used for X0/x0/cost0 below.
                           #   The size the paper's own LBD calculation assumed
                           #   (Sethu Odayappan/Robert Metcalfe email thread,
                           #   "LBD", Sep 2024: "the average HP size that we are
                           #   assuming (based on a message from Rob) is 8 kW").

## ---- Learning curve ---------------------------------------------------------
theta        <- -0.04995697  # learning elasticity: d ln(cost) / d ln(cum. production)
                           #   NOT an independent estimate -- solved jointly with
                           #   epsilon below so this script's DP_dollars/DE_dollars
                           #   reproduce the pre-existing lbd_price_heatpump (£1,697.59)
                           #   and lbd_environmental_heatpump (£3,192.62) this script
                           #   replaced, holding X0/x0/avg_hp_kw/cost0 at their
                           #   sourced values (see below). This implies a learning
                           #   rate of only ~3.4% cost decline per doubling of
                           #   cumulative installations -- markedly below the 14%
                           #   rate (Weiss et al. 2009) the paper currently cites
                           #   for heat pumps (main.tex line 611). Flagged to
                           #   Rob/Andrew -- see email draft -- pending their view
                           #   on whether reproducing the old figures or keeping
                           #   the cited 14% rate should take priority.
                           #   MUST be negative

## ---- Demand -----------------------------------------------------------------
epsilon      <- -2.265146 # own-price elasticity of demand. MUST be negative.
                           #   NOT an independent estimate -- see theta above; the
                           #   two were solved jointly to reproduce the old
                           #   lbd_price_heatpump/lbd_environmental_heatpump
                           #   figures exactly, given cost0's updated MCS-dashboard
                           #   value below. Close to the Muehlegger & Rapson (2022)
                           #   EV estimate (-2.1) used earlier in this process, and
                           #   further from the additionality-implied estimate
                           #   (~-1.695, derived from this program's own m=0.5
                           #   additionality assumption and
                           #   total_installation_cost_hp/SUBSIDY_HP) than the
                           #   previous solve was.
pass_through <- 1.0       # share of a $1 subsidy that actually reaches the price (0-1)

## ---- Where we are on the curve today ----------------------------------------
X0           <- 1220      # CUMULATIVE production to date, in GW, EXCLUDING this year
                           #   The paper's own figure (Odayappan/Metcalfe/Schein
                           #   email thread, "LBD", Sep 2024, quoting draft text
                           #   citing \citep{iea2023net}): "By the end of 2023,
                           #   there was 1220 GW of heat pump capacity operating
                           #   worldwide (up from 500 GW in 2010)". Not currently
                           #   in main.tex -- likely cut from an earlier draft.
x0           <- 110       # production DURING the current year, GW/year
                           #   Same source: "with sales in 2023 being 110 GW".
                           #   X0 = where you are on the curve; x0 = how fast you're moving

## ---- Costs ------------------------------------------------------------------
cost0        <- 1423.25e6 # total production cost of ONE unit today ($) -- here, £ per GW
                           #   £1,423.25/kW x 1e6 kW/GW: average "Average
                           #   installation cost per kW" from the MCS Installation
                           #   Insights dashboard
                           #   (datadashboard.mcscertified.com/InstallationInsights),
                           #   Nov 2023 - Jun 2024 -- the exact 8-month window
                           #   05_MVPF.R's own "MCS cost data" sheet averages to get
                           #   total_installation_cost_hp: the same dashboard's
                           #   "Average installation cost" column over these same
                           #   8 months averages £12,712.62, matching
                           #   total_installation_cost_hp to the penny. Dividing
                           #   the two implies an average system size of ~8.93kW,
                           #   corroborating avg_hp_kw = 8 above.
markup       <- 0.0       # mu. 0 = perfect competition (price = marginal cost)
                           #   Not separately estimated for HPs in the paper; kept
                           #   at the model's original perfect-competition default.
fixed_frac   <- 0.0       # share of cost0 that does NOT fall with experience
                           #   0 because Weiss et al.'s 14% rate is applied to the
                           #   whole installed cost, with no battery-style split.

## ---- Discounting ------------------------------------------------------------
rho          <- 0.035     # annual discount rate: UK Green Book / HMG preferred
                           #   rate, matching 05_MVPF.R's preferred r_disc = 3.5%
                           #   (use 0.02 for the IAM/Rennert sensitivity row).

## ---- Pollution / E(t) --------------------------------------------------------
lifetime     <- 20        # years the product lasts: the paper's assumed 20-year
                           #   heat pump lifetime (main.tex line 603).
#  emis_per_yr, grid_decarb, scc0 and scc_growth below are used ONLY as a
#  standalone fallback E(t) when this script is run on its own. When sourced
#  from 05_MVPF.R, Section 5 overrides E_vec with that script's own
#  tonnes_saved x scc_hmg stream (rescaled to per-GW via avg_hp_kw), which is
#  the actual empirical/SCC path used elsewhere in the paper.
emis_per_yr  <- 1.16 / avg_hp_kw * 1e6  # tons CO2e avoided per GW PER YEAR, as of today
                           #   1.16 t CO2e/household/yr at 2024 GB grid intensity
                           #   (main.tex line 603), rescaled to per-GW.
grid_decarb  <- -0.0216    # annual proportional decline in emissions avoided,
                           #   because the counterfactual grid keeps getting cleaner.
                           #   NEGATIVE here: a heat pump's avoided emissions rise
                           #   over time as the GB grid decarbonizes (main.tex line
                           #   603: 1.16 -> 1.74 t/yr from 2024 to 2043), the
                           #   opposite of a technology displacing a cleaning grid.
scc0         <- 287       # social cost of carbon today, $/ton -- here, £/tonne
                           #   UK MAC-based carbon value, 2023 prices, the paper's
                           #   preferred spec (main.tex line 605/619).
scc_growth   <- 0.0       # annual real growth in the SCC
                           #   No explicit flat growth rate is stated in the paper;
                           #   left at 0 for the standalone fallback (see note above
                           #   -- the real scc_hmg path is used when sourced).

## ---- Numerics ---------------------------------------------------------------
T_max        <- 300       # integration horizon in years (discounting kills the tail)
dt           <- 0.05      # time step
program_cost <- 1.0       # $ size of the subsidy program, to convert D's into dollars
                           #   NOTE: this is independent of the GW units above --
                           #   it scales the dimensionless per-$1 welfare terms
                           #   into the $ benefit attributable to ONE HOUSEHOLD's
                           #   adoption, for use in 05_MVPF.R.
if (exists("total_installation_cost_hp", inherits = TRUE)) {
  program_cost <- total_installation_cost_hp  # sourced from 05_MVPF.R: use its
                                               # own per-household install cost
}


# =============================================================================
#  2. VALIDATION -- catch the errors that silently produce nonsense
# =============================================================================

stopifnot(theta < 0, epsilon < 0, X0 > 0, x0 > 0, cost0 > 0)
stopifnot(fixed_frac >= 0, fixed_frac < 1, markup >= 0, lifetime >= 1)
if (x0 > X0) warning("x0 > X0: current output exceeds cumulative output. Inputs flipped?")

# The runaway condition. If epsilon*theta >= 1 the exponent 1/(1-epsilon*theta)
# flips sign and the model has no sensible real solution.
if (epsilon * theta >= 1) {
  stop(sprintf("epsilon*theta = %.3f >= 1. Learning feeds demand faster than it decays; no real solution.",
               epsilon * theta))
}
if (epsilon * theta > 0.9) {
  warning(sprintf("epsilon*theta = %.3f is close to 1. Results will be extremely sensitive.",
                  epsilon * theta))
}


# =============================================================================
#  3. CALIBRATE THE COST CURVE:   cost(X) = F + kappa * X^theta
# =============================================================================
# theta gives the SLOPE of the curve. kappa is solved for -- not estimated --
# so that the curve passes through today's observed cost at today's cumulative
# production. F is the part of cost that never falls.

F     <- fixed_frac * cost0                 # non-learning component ($/unit)
kappa <- (1 - fixed_frac) * cost0 / X0^theta # scale constant of the learning part
price0 <- cost0 * (1 + markup)               # price today

# Check the calibration reproduces today's cost exactly:
stopifnot(abs((F + kappa * X0^theta) - cost0) < 1e-9)

unit_cost <- function(X) F + kappa * X^theta


# =============================================================================
#  4. SOLVE FOR THE PRODUCTION PATH X(t)
# =============================================================================
# The technology's own dynamics, with no ongoing subsidy:
#
#     X'' = epsilon * theta * kappa * X^(theta-1) * (X')^2 / (F + kappa*X^theta)
#
# When F = 0 the kappa cancels and there is a closed form. When F > 0 it does
# not, so we integrate numerically (RK4).

tgrid <- seq(0, T_max, by = dt)
n     <- length(tgrid)

# --- derivative of the state y = (X, x). Returns c(dX, dx), UNNAMED. ---------
# (Unnamed matters: if these carried names, R would propagate them through the
#  RK4 arithmetic below and silently turn the lookups into NA.)
deriv <- function(X, x) {
  c(x,
    epsilon * theta * kappa * X^(theta - 1) * x^2 / (F + kappa * X^theta))
}

# --- RK4 ---------------------------------------------------------------------
X      <- numeric(n); x <- numeric(n); xprime <- numeric(n)
X[1]   <- X0
x[1]   <- x0
xprime[1] <- deriv(X0, x0)[2]

for (i in 1:(n - 1)) {
  k1 <- deriv(X[i],                 x[i])
  k2 <- deriv(X[i] + dt/2 * k1[1],  x[i] + dt/2 * k1[2])
  k3 <- deriv(X[i] + dt/2 * k2[1],  x[i] + dt/2 * k2[2])
  k4 <- deriv(X[i] + dt   * k3[1],  x[i] + dt   * k3[2])
  X[i+1] <- X[i] + dt/6 * (k1[1] + 2*k2[1] + 2*k3[1] + k4[1])
  x[i+1] <- x[i] + dt/6 * (k1[2] + 2*k2[2] + 2*k3[2] + k4[2])
  # Take x' straight from the ODE rather than differencing x -- differencing a
  # numerical solution twice amplifies noise badly.
  xprime[i+1] <- deriv(X[i+1], x[i+1])[2]
}

# --- self-check against the closed form (only valid when F = 0) --------------
if (fixed_frac == 0) {
  k  <- 1 / (1 - epsilon * theta)
  C2 <- X0 / (x0 * (1 - epsilon * theta))
  C1 <- X0 / C2^k
  X_exact <- C1 * (tgrid + C2)^k
  rel_err <- max(abs(X - X_exact) / X_exact)
  cat(sprintf("RK4 vs closed form: max relative error = %.2e\n\n", rel_err))
  if (rel_err > 1e-6) warning("Numerical solution drifting from closed form. Reduce dt.")
}


# =============================================================================
#  5. BUILD E(t)
# =============================================================================
# E(t) = $ of pollution damage avoided by ONE extra unit bought in year t,
#        summed over that unit's entire lifetime, discounted back to year t.
#
# This is an INPUT to the model, not an output -- it encodes your forecast of
# the counterfactual. A solar panel doesn't avoid pollution in the abstract; it
# avoids whatever the grid would otherwise have burned, and that changes as the
# grid decarbonizes.

emis_at <- function(s) emis_per_yr * (1 - grid_decarb)^s   # tons/yr avoided, year s
scc_at  <- function(s) scc0 * (1 + scc_growth)^s           # $/ton, year s

E_of_t <- function(t) {
  a <- 0:(lifetime - 1)                  # age of the unit
  sum(emis_at(t + a) * scc_at(t + a) / (1 + rho)^a)
}
E_vec <- vapply(tgrid, E_of_t, numeric(1))

# ---- OVERRIDE: use 05_MVPF.R's real streams instead of the approximation above ----
# When sourced from 05_MVPF.R, tonnes_saved and scc_hmg already exist (its actual
# per-household CO2e-avoided and MAC-based SCC paths for YEARS <- 2024:2043).
# s = 0..19 indexes calendar time since 2024; held flat beyond 2043 via rule = 2,
# since neither the grid-intensity nor SCC projections extend further. Rescaled
# from per-household to per-GW via avg_hp_kw to match X0/x0/cost0's units.
if (exists("tonnes_saved", inherits = TRUE) && exists("scc_hmg", inherits = TRUE) &&
    exists("YEARS", inherits = TRUE)) {
  hp_s    <- seq_along(YEARS) - 1
  emis_at <- function(s) approx(hp_s, tonnes_saved, xout = s, rule = 2)$y / avg_hp_kw * 1e6
  scc_at  <- function(s) approx(hp_s, scc_hmg,      xout = s, rule = 2)$y
  E_of_t <- function(t) {
    a <- 0:(lifetime - 1)
    sum(emis_at(t + a) * scc_at(t + a) / (1 + rho)^a)
  }
  E_vec <- vapply(tgrid, E_of_t, numeric(1))
}


# =============================================================================
#  6. THE THREE WELFARE INTEGRALS
# =============================================================================
# Sign convention follows the source model: gamma is NEGATIVE pass-through.
# With gamma < 0 and epsilon < 0, DP and DE come out positive and Dpi negative.

gamma <- -pass_through

trapz <- function(y, dx) dx * (sum(y) - 0.5 * (y[1] + y[length(y)]))

disc <- exp(-rho * tgrid)

# --- DP: future buyers pay less ---------------------------------------------
#   (dc/dX) * (experience created) * (buyers who benefit) -- hence x^2
integrand_DP <- theta * kappa * (1 + markup) * x^2 * X^(theta - 1) * disc
DP <- (-gamma * epsilon) / (x0 * price0) * trapz(integrand_DP, dt)

# --- Dpi: producer surplus ---------------------------------------------------
#   Identically zero when markup = 0 (perfect competition): the entire learning
#   benefit flows to consumers via DP instead of being split with producers.
integrand_Dpi <- disc * xprime * (F + kappa * X^theta)
Dpi <- (-markup / (markup + 1)) * DP +
       (-gamma * markup * epsilon) / (x0 * price0) * trapz(integrand_Dpi, dt)

# --- DE: extra units induced in the future, each avoiding E(t) ---------------
integrand_DE <- disc * xprime * E_vec
DE <- (gamma * epsilon) / (x0 * price0) * trapz(integrand_DE, dt)

# --- $ versions, at program_cost, for 05_MVPF.R to read directly -------------
DP_dollars  <- DP  * program_cost
Dpi_dollars <- Dpi * program_cost
DE_dollars  <- DE  * program_cost


# =============================================================================
#  7. OUTPUT
# =============================================================================

cat("================ CALIBRATION ================\n")
cat(sprintf("  theta                  %10.4f\n", theta))
cat(sprintf("  epsilon                %10.4f\n", epsilon))
cat(sprintf("  epsilon*theta          %10.4f   (must be < 1)\n", epsilon * theta))
cat(sprintf("  kappa (solved)         %10.4g\n", kappa))
cat(sprintf("  F  (never learns)      %10.4f  $/unit\n", F))
cat(sprintf("  cost today             %10.4f  $/unit\n", unit_cost(X0)))
cat(sprintf("  mode                   %10s\n",
            if (fixed_frac == 0) "closed form" else "numerical ODE"))

cat("\n================ COST CURVE =================\n")
show_yrs <- c(0, 5, 10, 25, 50, 100)
show_yrs <- show_yrs[show_yrs <= T_max]
idx <- vapply(show_yrs, function(y) which.min(abs(tgrid - y)), integer(1))
print(data.frame(
  year      = show_yrs,
  cum_prod  = signif(X[idx], 4),
  annual    = signif(x[idx], 4),
  cost_unit = round(unit_cost(X[idx]), 4),
  pct_of_today = paste0(round(100 * unit_cost(X[idx]) / cost0, 1), "%"),
  row.names = NULL))

cat("\n================ E(t) =======================\n")
cat("  $ lifetime damages avoided per unit bought in year t\n\n")
print(data.frame(
  year = show_yrs,
  E_t  = round(E_vec[idx], 2),
  row.names = NULL))

cat("\n================ WELFARE TERMS ==============\n")
cat("  Per $1 of program cost (dimensionless):\n")
cat(sprintf("    DP  (dynamic price)      %12.5f\n", DP))
cat(sprintf("    Dpi (dynamic profit)     %12.5f%s\n", Dpi,
            if (markup == 0) "   <- structurally 0 when markup = 0" else ""))
cat(sprintf("    DE  (dynamic enviro)     %12.5f\n", DE))
cat(sprintf("    ---------------------------------\n"))
cat(sprintf("    TOTAL                    %12.5f\n", DP + Dpi + DE))
cat(sprintf("\n  In dollars, at program_cost = %s:\n", format(program_cost, big.mark = ",")))
cat(sprintf("    DP  %14.2f\n    Dpi %14.2f\n    DE  %14.2f\n    TOT %14.2f\n",
            DP * program_cost, Dpi * program_cost,
            DE * program_cost, (DP + Dpi + DE) * program_cost))

# --- sanity checks the user should actually look at --------------------------
cat("\n================ SANITY CHECKS ==============\n")
chk <- function(ok, msg) cat(sprintf("  [%s] %s\n", if (ok) "OK  " else "FAIL", msg))
chk(DP > 0,  "DP > 0  (learning makes future units cheaper)")
chk(DE > 0 || emis_per_yr <= 0, "DE > 0  (induced units avoid pollution)")
if (markup == 0) {
  chk(abs(Dpi) < 1e-12, "Dpi = 0  (markup = 0, so all learning gains go to consumers)")
} else {
  chk(Dpi < 0, "Dpi < 0  (falling prices erode producer surplus)")
}
chk(all(diff(X) > 0), "cumulative production is increasing")
chk(all(diff(unit_cost(X)) <= 1e-12), "unit cost is falling")

# Unit-consistency smell test. E(0) is the lifetime pollution value of one unit;
# price0 is what one unit costs. Their ratio should be economically plausible --
# somewhere around 0.01x to 10x. A ratio of 1000 almost always means X0/cost0/
# emis_per_yr are denominated in different things (per-watt vs per-system, etc).
ratio <- E_vec[1] / price0
chk(ratio > 0.001 && ratio < 100,
    sprintf("E(0)/price = %.3f  (expect roughly 0.001-100 if units agree)", ratio))
if (ratio <= 0.001 || ratio >= 100) {
  cat(sprintf("\n  !! E(0) = $%.0f but one unit costs $%.0f.\n", E_vec[1], price0))
  cat("     Check that X0, x0, cost0 and emis_per_yr all describe the SAME unit.\n")
}
chk(x0 <= X0, "x0 <= X0 (annual output below cumulative)")

# --- plots -------------------------------------------------------------------
# Skipped when sourced from 05_MVPF.R (detected via total_installation_cost_hp)
# so these base-R diagnostic plots don't interleave with that script's own
# ggplot figures.
if (!exists("total_installation_cost_hp", inherits = TRUE)) {
  op <- par(mfrow = c(2, 2), mar = c(4.2, 4.4, 2.6, 1))

  plot(tgrid, unit_cost(X), type = "l", lwd = 2, col = "#1f4e79",
       xlab = "Year from today", ylab = "Cost per unit ($)",
       main = "Cost per unit over time")
  abline(h = F, lty = 3, col = "grey40")
  if (F > 0) text(T_max * 0.6, F, "fixed floor F", pos = 3, cex = 0.75, col = "grey40")

  plot(X, unit_cost(X), type = "l", lwd = 2, col = "#1f4e79", log = "xy",
       xlab = "Cumulative production (log)", ylab = "Cost per unit (log)",
       main = "The learning curve")

  plot(tgrid, E_vec, type = "l", lwd = 2, col = "#c00000",
       xlab = "Year of purchase, t", ylab = "E(t), $ per unit",
       main = "E(t): lifetime damages avoided")

  barplot(c(DP = DP, Dpi = Dpi, DE = DE), col = c("#1f4e79", "#7f7f7f", "#c00000"),
          main = "Welfare terms, per $1 of program cost", ylab = "$")
  abline(h = 0)

  par(op)
}
