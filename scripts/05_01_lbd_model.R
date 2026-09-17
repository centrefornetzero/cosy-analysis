# =============================================================================
#  LEARNING-BY-DOING WELFARE MODEL
#  Input sub-script to 05_MVPF.R -- sourced at its Section 5b, after that
#  script has built tonnes_saved, scc_hmg, YEARS and total_installation_cost_hp.
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
#  The calibration below is for the UK AIR-SOURCE HEAT PUMP subsidy analysed in
#  05_MVPF.R, with production tracked in NUMBER OF INSTALLS (one unit = one
#  household's heat pump), matching the actual Policy-Impacts/mvpf-climate
#  cost_curve_simple.wls convention this was validated against:
#     price0 = total_installation_cost_hp - SUBSIDY_HP  (net, per install)
#     X0     = global cumulative installs to date, EXCLUDING this year's
#              additions, converted from GW via avg_hp_kw
#     x0     = installs added globally this year, converted from GW
#     emis   = tonnes_saved is already per household -- no rescaling needed
#  Section 7 prints a unit-consistency check. Read it.
#
## ---- Unit conversion ---------------------------------------------------------
avg_hp_kw    <- 8          # average heat pump system size (kW), used to convert
                           #   the paper's GW-denominated global production
                           #   figures (X0/x0 below) into number-of-installs
                           #   (Odayappan/Metcalfe, "LBD" email thread, Sep 2024).

## ---- Learning curve ---------------------------------------------------------
learning_rate <- 0.14     # 14% cost reduction per doubling of cumulative
                           #   production: air-source heat pump rate from
                           #   Weiss et al. (2009); main.tex line 611.
theta         <- log(1 - learning_rate) / log(2)
                           # learning elasticity: d ln(cost) / d ln(cum. production).
                           #   progress ratio = 2^theta = 1 - learning_rate.
                           #   MUST be negative

## ---- Demand -----------------------------------------------------------------
epsilon      <- -1.2      # own-price elasticity of demand. MUST be negative.
                           #   Reflects 50% infra-marginal buyers; main.tex line 611.
pass_through <- 1.0       # share of a $1 subsidy that actually reaches the price (0-1)

## ---- Where we are on the curve today ----------------------------------------
X0_gw        <- 1220      # CUMULATIVE production to date, in GW
                           #   \citep{iea2023net}): "By the end of 2023,
                           #   there was 1220 GW of heat pump capacity operating
                           #   worldwide (up from 500 GW in 2010)".
x0_gw        <- 110       # production DURING the current year, GW/year
                           #   Same source: "with sales in 2023 being 110 GW".
                           #   X0 = where you are on the curve; x0 = how fast you're moving

X0           <- (X0_gw - x0_gw) / avg_hp_kw * 1e6
                           # cumulative INSTALLS to date, EXCLUDING this year's own
                           #   additions -- the 1220 GW figure already includes this
                           #   year's 110 GW, so it's netted out here before
                           #   converting GW -> number of avg_hp_kw-sized installs.
x0           <- x0_gw / avg_hp_kw * 1e6
                           # installs added globally this year

## ---- Costs ------------------------------------------------------------------
cost0        <- total_installation_cost_hp - pass_through * SUBSIDY_HP
                           # net (post-subsidy) price of ONE install, £ -- the
                           #   quantity cost_curve_simple.wls's own "price" argument
                           #   represents. total_installation_cost_hp and
                           #   SUBSIDY_HP are both sourced from 05_MVPF.R's
                           #   environment (05_MVPF.R:26,110).
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
#  The emissions-avoided and SCC paths themselves are not set here: Section 5
#  below builds E(t) directly from 05_MVPF.R's own tonnes_saved/scc_hmg/YEARS.

## ---- Numerics ---------------------------------------------------------------
T_max        <- 300       # integration horizon in years (discounting kills the tail)
dt           <- 0.05      # time step
program_cost <- SUBSIDY_HP
                           # $ size of the subsidy program, to convert D's into
                           #   dollars -- matches the actual replication
                           #   package's convention (e.g. federal_ev.do:753:
                           #   cost_wtp = DP * avg_subsidy), confirmed by
                           #   reproducing Sethu's original £1,697.59/£3,192.62
                           #   figures with program_cost = SUBSIDY_HP.


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
#
# Built directly from 05_MVPF.R's own tonnes_saved (per-household CO2e avoided,
# already reflecting the DESNZ grid-decarbonization path) and scc_hmg (MAC-based
# SCC), both indexed over YEARS <- 2024:2043 -- the same grid-decarbonization
# and carbon-value series 05_MVPF.R itself uses, so LBD and MVPF are always
# consistent. s = 0..19 indexes calendar time since 2024; held flat beyond 2043
# via rule = 2, since neither projection extends further. tonnes_saved is
# already per household -- no rescaling, since X0/x0/cost0 are now also
# denominated per install (Section 1).
hp_s    <- seq_along(YEARS) - 1
emis_at <- function(s) approx(hp_s, tonnes_saved, xout = s, rule = 2)$y
scc_at  <- function(s) approx(hp_s, scc_hmg,      xout = s, rule = 2)$y

E_of_t <- function(t) {
  a <- 0:(lifetime - 1)                  # age of the unit
  sum(emis_at(t + a) * scc_at(t + a) / (1 + rho)^a)
}
E_vec <- vapply(tgrid, E_of_t, numeric(1))


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
chk(DE > 0 || all(tonnes_saved <= 0), "DE > 0  (induced units avoid pollution)")
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
# tonnes_saved are denominated in different things (per-watt vs per-system, etc).
ratio <- E_vec[1] / price0
chk(ratio > 0.001 && ratio < 100,
    sprintf("E(0)/price = %.3f  (expect roughly 0.001-100 if units agree)", ratio))
if (ratio <= 0.001 || ratio >= 100) {
  cat(sprintf("\n  !! E(0) = $%.0f but one unit costs $%.0f.\n", E_vec[1], price0))
  cat("     Check that X0, x0, cost0 and tonnes_saved all describe the SAME unit.\n")
}
chk(x0 <= X0, "x0 <= X0 (annual output below cumulative)")
