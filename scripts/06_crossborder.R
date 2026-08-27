# =============================================================================
# 06_crossborder.R
# Cross-Border Settlement: Herstatt Risk and Trapped Liquidity
#
# Theory:
#   Herstatt risk arises from the temporal mismatch between payment legs
#   in foreign exchange transactions. The expected loss is:
#     E[Loss] = P * (1 - (1-p)^delta_t)
#   where P is the transaction principal, p is the hourly default
#   probability, and delta_t is the settlement gap in hours.
#
#   Three settlement arrangements are compared:
#   - Correspondent banking: delta_t = 24 hours
#   - CLS: delta_t = 5 hours (PvP within settlement window)
#   - XRPL: delta_t = 0 hours (atomic PvP settlement)
#
#   CLS reduces Herstatt risk primarily through frequency reduction
#   (shorter exposure window) rather than severity mitigation: when
#   a default occurs within the CLS window, the full principal remains
#   at risk. XRPL eliminates Herstatt risk entirely through atomic
#   payment-versus-payment settlement.
#
#   Two distinct XRPL liquidity concepts are tracked:
#   - Gross pool value = MD * P_xrp = EUR 2,287,349
#     (total corridor liquidity; comparable to nostro prefunding)
#   - Volatility buffer L_XRPL = MD * sigma_xrp * P_xrp = EUR 114,367
#     (capital at risk from XRP price movements during settlement window;
#      enters the TCS function as the opportunity cost base)
#
#   Cross-border results are validated through 100,000 Monte Carlo runs
#   rather than the 1,000 used elsewhere, because Herstatt default events
#   are rare (0.24% and 0.06% under correspondent and CLS respectively),
#   requiring a larger sample to obtain stable default rate estimates.
#
# References:
#   Bank for International Settlements (2012), Principles for FMIs
#   Bank for International Settlements (2020), CPMI Red Book
#   Kahn & Roberds (2009), Journal of Financial Intermediation
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Reproducibility and directory setup
# -----------------------------------------------------------------------------

set.seed(42)

if (!dir.exists("output")) stop("Run 02_rtgs.R first — output/ not found.")

# Helper: format large numbers with commas, no scientific notation
fmt <- function(x) formatC(round(x), format="f", digits=0, big.mark=",")

# -----------------------------------------------------------------------------
# 1. Parameters
# -----------------------------------------------------------------------------

# Herstatt risk
P         <- 1000000   # transaction principal EUR 1,000,000
p_default <- 1e-4      # counterparty default probability per hour
gap_corr  <- 24        # correspondent banking settlement gap (hours)
gap_cls   <- 5         # CLS settlement window (hours)
gap_xrpl  <- 0         # XRPL atomic settlement gap (hours)

# Trapped liquidity
daily_fx       <- 1000       # daily FX transactions in corridor
nostro_total   <- 200000000  # correspondent nostro prefunding EUR 200M
cls_netting    <- 0.70       # CLS netting efficiency
cls_prefunding <- nostro_total * (1 - cls_netting)   # EUR 60M

# XRPL parameters — consistent with 04_xrpl.R
MD        <- 1971853   # pool depth in XRP
P_xrp     <- 1.16      # XRP/EUR exchange rate (CoinGecko, March 2026)
sigma_xrp <- 0.05      # XRP daily volatility (conservative; two-year
# realised average April 2024-April 2026 = 4.46%)
r_rate    <- 0.03      # opportunity cost of capital

# Two distinct liquidity concepts:
gross_pool <- MD * P_xrp              # EUR 2,287,349 — gross corridor liquidity
# comparable to nostro prefunding
xrpl_pool  <- MD * sigma_xrp * P_xrp  # EUR 114,367  — volatility buffer L_XRPL
# capital at risk; enters TCS function

# MD* threshold: gross pool value at which XRPL cost = nostro cost
# MD* in XRP = nostro_total / P_xrp
# MD* in EUR = nostro_total (by construction)
md_star     <- nostro_total / P_xrp   # in XRP: 172,413,793
md_star_eur <- nostro_total           # in EUR: EUR 200,000,000
headroom    <- md_star / MD           # 87.44x (reported as 87.4x in thesis)

# Opportunity costs
oc_nostro <- nostro_total   * r_rate  # EUR 6,000,000
oc_cls    <- cls_prefunding * r_rate  # EUR 1,800,000
oc_xrpl   <- xrpl_pool      * r_rate  # EUR 3,431 (based on volatility buffer)

# Monte Carlo
n_mc <- 100000

cat("=======================================================\n")
cat("  CROSS-BORDER SIMULATION SETUP\n")
cat("=======================================================\n")
cat(sprintf("  Transaction principal:    EUR %s\n",    fmt(P)))
cat(sprintf("  Default probability:      %.4f/hr\n",   p_default))
cat(sprintf("  Settlement gaps:          Corr=%dh, CLS=%dh, XRPL=%dh\n",
            gap_corr, gap_cls, gap_xrpl))
cat(sprintf("  Nostro prefunding:        EUR %s\n",    fmt(nostro_total)))
cat(sprintf("  CLS prefunding:           EUR %s\n",    fmt(cls_prefunding)))
cat(sprintf("  XRPL gross pool:          EUR %s\n",    fmt(gross_pool)))
cat(sprintf("  XRPL volatility buffer:   EUR %s\n",    fmt(xrpl_pool)))
cat(sprintf("  Monte Carlo runs:         %d\n",        n_mc))
cat("=======================================================\n\n")

# -----------------------------------------------------------------------------
# 2. Analytical Herstatt risk
# -----------------------------------------------------------------------------

herstatt <- function(P, p, dt) P * (1 - (1 - p)^dt)

loss_corr <- herstatt(P, p_default, gap_corr)
loss_cls  <- herstatt(P, p_default, gap_cls)
loss_xrpl <- herstatt(P, p_default, gap_xrpl)

cat("=======================================================\n")
cat("  ANALYTICAL HERSTATT RISK\n")
cat("=======================================================\n")
cat(sprintf("  Correspondent E[Loss]:  EUR %.2f\n",  loss_corr))
cat(sprintf("  CLS E[Loss]:            EUR %.2f\n",  loss_cls))
cat(sprintf("  XRPL E[Loss]:           EUR %.2f\n",  loss_xrpl))
cat(sprintf("  CLS reduction:          %.1f%%\n",
            (1 - loss_cls / loss_corr) * 100))
cat(sprintf("  XRPL reduction:         100%%\n"))
cat("=======================================================\n\n")

# -----------------------------------------------------------------------------
# 3. Monte Carlo Herstatt risk (100,000 runs)
# -----------------------------------------------------------------------------
# 100,000 runs used (vs 1,000 elsewhere) because Herstatt default events
# are rare: expected ~240 defaults for correspondent, ~60 for CLS.
# 1,000 runs would yield only 2-6 defaults — statistically unreliable.

loss_mc_corr <- rep(0, n_mc)
loss_mc_cls  <- rep(0, n_mc)

cat("Running Herstatt Monte Carlo (100,000 runs)...\n")

for (sim in 1:n_mc) {
  set.seed(sim)
  
  # Correspondent: check each of 24 hours for default
  if (any(runif(gap_corr) < p_default)) {
    loss_mc_corr[sim] <- P
  }
  
  # CLS: check each of 5 hours for default
  # Full principal at risk — CLS reduces window not severity
  if (any(runif(gap_cls) < p_default)) {
    loss_mc_cls[sim] <- P
  }
}

# Default rates
dr_corr <- mean(loss_mc_corr > 0) * 100
dr_cls  <- mean(loss_mc_cls  > 0) * 100

# Unconditional expected loss
uel_corr <- mean(loss_mc_corr)
uel_cls  <- mean(loss_mc_cls)

# Conditional expected loss (given default occurs)
cel_corr <- if (sum(loss_mc_corr > 0) > 0)
  mean(loss_mc_corr[loss_mc_corr > 0]) else NA
cel_cls  <- if (sum(loss_mc_cls > 0) > 0)
  mean(loss_mc_cls[loss_mc_cls > 0])   else NA

# Binomial standard errors on default rates
se_dr_corr <- sqrt(dr_corr/100 * (1 - dr_corr/100) / n_mc) * 100
se_dr_cls  <- sqrt(max(dr_cls/100, 1e-10) *
                     (1 - max(dr_cls/100, 1e-10)) / n_mc) * 100

# Risk reduction based on analytical values (MC too noisy for rare events)
cls_reduction <- (1 - loss_cls / loss_corr) * 100

cat("\n=======================================================\n")
cat("  HERSTATT RISK: MONTE CARLO RESULTS (100,000 runs)\n")
cat("=======================================================\n")
cat(sprintf("  %-32s  %10s  %10s  %10s\n",
            "Metric", "Corr", "CLS", "XRPL"))
cat(sprintf("  %-32s  %10d  %10d  %10d\n",
            "Settlement gap (hrs)", gap_corr, gap_cls, gap_xrpl))
cat(sprintf("  %-32s  %10.2f  %10.2f  %10.2f\n",
            "Analytical E[Loss] (EUR)", loss_corr, loss_cls, loss_xrpl))
cat(sprintf("  %-32s  %10.2f%%  %9.2f%%  %9.2f%%\n",
            "MC default rate", dr_corr, dr_cls, 0.0))
cat(sprintf("  %-32s  %10.2f  %10.2f  %10.2f\n",
            "MC unconditional E[Loss]", uel_corr, uel_cls, 0.0))
cat(sprintf("  %-32s  %10.2f  %10.2f  %10s\n",
            "MC conditional E[Loss]",
            ifelse(is.na(cel_corr), 0, cel_corr),
            ifelse(is.na(cel_cls),  0, cel_cls),
            "N/A"))
cat(sprintf("  %-32s  %10s  %9.1f%%  %9s\n",
            "Risk reduction vs corr", "—", cls_reduction, "100%"))
cat("-------------------------------------------------------\n")
cat(sprintf("  %-32s  %10.4f%%  %9.4f%%  %9s\n",
            "Default rate SE", se_dr_corr, se_dr_cls, "0%"))
cat(sprintf("  %-32s  %10s  %10s  %10s\n",
            "SE < 5%",
            ifelse(se_dr_corr < 5, "PASS", "FAIL"),
            ifelse(se_dr_cls  < 5, "PASS", "FAIL"),
            "PASS"))
cat("=======================================================\n\n")

# -----------------------------------------------------------------------------
# 4. Trapped liquidity comparison
# -----------------------------------------------------------------------------
# Note: gross pool savings uses gross_pool (comparable to nostro concept)
#       opportunity cost savings uses xrpl_pool (volatility buffer, TCS concept)
#       Table 7 in thesis reports opportunity cost savings = oc_nostro - oc_xrpl

cat("=======================================================\n")
cat("  TRAPPED LIQUIDITY COMPARISON\n")
cat("  (1,000 daily FX transactions, r = 3%)\n")
cat("=======================================================\n")
cat(sprintf("  %-32s  %12s  %12s  %12s\n",
            "Metric", "Correspondent", "CLS", "XRPL"))
cat(sprintf("  %-32s  %12s  %12s  %12s\n",
            "Gross pool / nostro (EUR)",
            fmt(nostro_total), fmt(cls_prefunding), fmt(gross_pool)))
cat(sprintf("  %-32s  %12s  %12s  %12s\n",
            "Volatility buffer L_XRPL",
            "—", "—", fmt(xrpl_pool)))
cat(sprintf("  %-32s  %12s  %12s  %12s\n",
            "Opportunity cost (EUR)",
            fmt(oc_nostro), fmt(oc_cls), fmt(oc_xrpl)))
cat(sprintf("  %-32s  %12s  %12s  %12s\n",
            "Opp. cost saving vs corr (EUR)",
            "—", fmt(oc_nostro - oc_cls), fmt(oc_nostro - oc_xrpl)))
cat(sprintf("  %-32s  %12s  %11.1f%%  %11.1f%%\n",
            "Gross pool reduction vs corr", "—",
            (1 - cls_prefunding / nostro_total) * 100,
            (1 - gross_pool     / nostro_total) * 100))
cat("=======================================================\n\n")

# -----------------------------------------------------------------------------
# 5. MD* threshold
# -----------------------------------------------------------------------------

cat("=======================================================\n")
cat("  POOL DEPTH THRESHOLD MD*\n")
cat("=======================================================\n")
cat(sprintf("  Nostro prefunding:        EUR %s\n", fmt(nostro_total)))
cat(sprintf("  MD* (XRP):                %s\n",
            formatC(round(md_star), format="d", big.mark=",")))
cat(sprintf("  MD* (EUR equivalent):     EUR %s\n", fmt(md_star_eur)))
cat(sprintf("  Current gross pool (EUR): EUR %s\n", fmt(gross_pool)))
cat(sprintf("  Headroom (MD*/MD):        %.2fx  (reported as 87.4x in thesis)\n",
            headroom))
cat(sprintf("  Current pool < MD*:       %s\n",
            ifelse(gross_pool < md_star_eur,
                   "YES — XRPL currently cheaper", "NO")))
cat("=======================================================\n\n")

# -----------------------------------------------------------------------------
# 6. Save results
# -----------------------------------------------------------------------------

crossborder_results <- list(
  loss_corr      = loss_corr,
  loss_cls       = loss_cls,
  loss_xrpl      = loss_xrpl,
  dr_corr        = dr_corr,
  dr_cls         = dr_cls,
  uel_corr       = uel_corr,
  uel_cls        = uel_cls,
  cel_corr       = cel_corr,
  cel_cls        = cel_cls,
  nostro_total   = nostro_total,
  cls_prefunding = cls_prefunding,
  gross_pool     = gross_pool,
  xrpl_pool      = xrpl_pool,
  oc_nostro      = oc_nostro,
  oc_cls         = oc_cls,
  oc_xrpl        = oc_xrpl,
  md_star        = md_star,
  md_star_eur    = md_star_eur,
  headroom       = headroom
)

saveRDS(crossborder_results, "output/crossborder_results.rds")

write.csv(
  data.frame(sim       = 1:n_mc,
             loss_corr = loss_mc_corr,
             loss_cls  = loss_mc_cls,
             loss_xrpl = rep(0, n_mc)),
  "output/mc_crossborder_results.csv",
  row.names = FALSE
)

cat("Saved:\n")
cat("  output/crossborder_results.rds\n")
cat("  output/mc_crossborder_results.csv\n\n")