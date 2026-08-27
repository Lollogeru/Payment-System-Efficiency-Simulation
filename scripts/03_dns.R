# =============================================================================
# 03_dns.R
# DNS Simulation: Deferred Net Settlement baseline
#
# Theory:
#   DNS systems accumulate payment instructions across discrete clearing
#   cycles of fixed duration. At the end of each cycle, bilateral positions
#   are multilaterally netted and only net obligations are settled. This
#   recovers liquidity efficiency relative to RTGS through the Liquidity
#   Saving Ratio (LSR), but reintroduces settlement delays and counterparty
#   exposure during the SF1-SF3 settlement gap (Berndsen, 2018).
#
#   During the SF1-SF3 settlement gap, net receivables held by participants
#   against net debtors constitute unsecured short-term interbank credit
#   exposures in the sense of the Basel II standardised approach (BCBS,
#   2006). The credit risk premium is derived as:
#     credit_premium = 20% risk weight x 8% minimum capital ratio = 1.6%
#   This represents the regulatory minimum capital buffer against short-term
#   interbank credit risk under normal operating conditions (BCBS, 2006;
#   Manning et al., 2009). Using the regulatory minimum is deliberately
#   conservative: any higher empirical estimate would increase L_DNS,
#   further widening the cost gap between DNS and XRPL.
#
#   Safety stock is computed as:
#     L_DNS = sum(max(0, -net_i)) / (1 - credit_premium)
#
#   The LSR is computed per cycle following the BoF-PSS2 methodology
#   (Leinonen & Soramaki, 2003), then averaged across cycles:
#     LSR = 1 - (net_obligations / gross_value)
#
#   Time discretisation: 32 steps of 0.25 hours each = 8 operating hours.
#   Each DNS clearing cycle spans 8 steps (8 x 0.25 = 2 hours), consistent
#   with the EURO1 2-hour clearing cycle (Manning et al., 2009).
#   Settlement latency in hours is computed as:
#     delay_hrs = (settlement_step - arrival_step) * step_length
#   Under continuous arrivals the theoretical expected wait is tau/2 = 1 hr
#   (Manning et al., 2009). The 32-step discretisation produces a value
#   close to this theoretical benchmark.
#
#   The Total Cost of Settlement is:
#     TCS = (L * r) + phi(D),  phi(D) = alpha * D^beta
#
# Inputs:
#   data/network.rds
#   output/rtgs_payments.rds  — same payment population as RTGS
#   output/rtgs_results.rds   — RTGS results for comparison
#
# Outputs:
#   output/dns_payments.rds
#   output/dns_results.rds
#
# References:
#   BCBS (2006), Basel II: International Convergence of Capital Measurement
#     and Capital Standards, Bank for International Settlements
#   Berndsen (2018), Economics of Financial Market Infrastructures
#   Leinonen & Soramaki (2003), Bank of Finland Discussion Papers 23/2003
#   Manning et al. (2009), Economics of Large-Value Payments and Settlement
# =============================================================================

library(igraph)

# -----------------------------------------------------------------------------
# 0. Reproducibility and directory setup
# -----------------------------------------------------------------------------

set.seed(42)

if (!dir.exists("data"))   stop("Run 01_network.R first — data/ not found.")
if (!dir.exists("output")) stop("Run 02_rtgs.R first — output/ not found.")

# -----------------------------------------------------------------------------
# 1. Load network and payment population
# -----------------------------------------------------------------------------
# IMPORTANT: DNS uses the SAME payment population as RTGS to ensure
# all differences in results are attributable to settlement architecture
# alone, not to different random payment draws

g           <- readRDS("data/network.rds")
hub_nodes   <- readRDS("data/hub_nodes.rds")
spoke_nodes <- readRDS("data/spoke_nodes.rds")
n_nodes     <- vcount(g)
deg         <- degree(g, mode = "all")
node_weight <- deg / sum(deg)

# Load exact same payments as RTGS
payments <- readRDS("output/rtgs_payments.rds")
n_pay    <- nrow(payments)

# Load RTGS parameters for consistency
rtgs_results <- readRDS("output/rtgs_results.rds")
n_steps      <- rtgs_results$n_steps      # 32
step_length  <- rtgs_results$step_length  # 0.25 hrs

cat("=======================================================\n")
cat("  PAYMENT POPULATION (loaded from RTGS)\n")
cat("=======================================================\n")
cat(sprintf("  Payments loaded:        %d\n", n_pay))
cat(sprintf("  n_steps:                %d (%.2f hrs each = %.0f hr day)\n",
            n_steps, step_length, n_steps * step_length))
cat(sprintf("  Mean amount (EUR):      %d\n", round(mean(payments$amount))))
cat(sprintf("  Median amount (EUR):    %d\n", round(median(payments$amount))))
cat(sprintf("  Max amount (EUR):       %d\n", round(max(payments$amount))))
cat("=======================================================\n\n")

# -----------------------------------------------------------------------------
# 2. Parameters
# -----------------------------------------------------------------------------

lambda       <- 17500   # baseline daily payments
r_rate       <- 0.03    # opportunity cost of capital
alpha        <- 0.1     # delay cost scaling parameter
beta         <- 1.8     # delay cost convexity

# DNS cycle parameters
# cycle_length = 8 steps x 0.25 hrs = 2 hours (EURO1 standard)
cycle_length <- 8                        # steps per cycle
n_cycles     <- n_steps / cycle_length   # 4 cycles per day
cycle_hrs    <- cycle_length * step_length  # 2.0 hours per cycle

# Credit risk premium on net exposures during SF1-SF3 settlement gap
# Derived from Basel II standardised approach:
#   20% risk weight on short-term interbank exposures (BCBS, 2006)
#   x 8% minimum capital adequacy ratio (BCBS, 2006)
#   = 1.6% credit risk premium
credit_premium <- 0.016

cat(sprintf("  cycle_length:           %d steps (%.1f hrs, EURO1 standard)\n",
            cycle_length, cycle_hrs))
cat(sprintf("  n_cycles:               %d per day\n", n_cycles))
cat(sprintf("  Credit risk premium:    %.1f%% (Basel II: 20%% x 8%%)\n\n",
            credit_premium * 100))

# -----------------------------------------------------------------------------
# 3. DNS settlement loop
# -----------------------------------------------------------------------------
# At each cycle end:
#   1. Accumulate all payments arriving during the cycle
#   2. Compute multilateral net position for each node
#   3. Net payers (negative position) must fund their net obligation
#   4. All payments in cycle are marked settled at cycle end
#   5. Settlement latency = (settlement_step - arrival_step) * step_length

settled         <- logical(n_pay)
settlement_step <- rep(NA_integer_, n_pay)
net_positions   <- matrix(0, nrow = n_cycles, ncol = n_nodes)
cycle_lsr       <- rep(0, n_cycles)
cycle_safety    <- rep(0, n_cycles)

for (cycle in 1:n_cycles) {
  
  t_start <- (cycle - 1) * cycle_length + 1
  t_end   <-  cycle * cycle_length
  
  # Payments arriving during this cycle
  in_cycle <- which(payments$arrival_step >= t_start &
                      payments$arrival_step <= t_end)
  
  if (length(in_cycle) == 0) next
  
  # Compute multilateral net position for each node
  net <- rep(0, n_nodes)
  for (i in in_cycle) {
    snd      <- payments$sender[i]
    rec      <- payments$receiver[i]
    amt      <- payments$amount[i]
    net[snd] <- net[snd] - amt   # outflow
    net[rec] <- net[rec] + amt   # inflow
  }
  
  net_positions[cycle, ] <- net
  
  # Safety stock for this cycle:
  # Net payer obligations / (1 - credit_premium)
  net_obligations     <- sum(pmax(0, -net))
  cycle_safety[cycle] <- net_obligations / (1 - credit_premium)
  
  # LSR for this cycle (BoF-PSS2 methodology)
  gross_cycle <- sum(payments$amount[in_cycle])
  if (gross_cycle > 0) {
    cycle_lsr[cycle] <- (1 - net_obligations / gross_cycle) * 100
  }
  
  # All payments settle at end of cycle (step t_end)
  settled[in_cycle]         <- TRUE
  settlement_step[in_cycle] <- t_end
}

payments$settled         <- settled
payments$settlement_step <- settlement_step

# -----------------------------------------------------------------------------
# 4. Compute settlement metrics
# -----------------------------------------------------------------------------

n_settled       <- sum(settled)
n_unsettled     <- sum(!settled)
settlement_rate <- n_settled / n_pay * 100

# Latency in hours: step difference * step_length (0.25 hrs per step)
delay_hrs       <- ifelse(settled,
                          (settlement_step - payments$arrival_step) * step_length,
                          NA_real_)
avg_latency_hrs <- mean(delay_hrs, na.rm = TRUE)

# Total liquidity requirement = sum of per-cycle safety stocks
L_total <- sum(cycle_safety)

# Average LSR across cycles (only non-zero cycles)
LSR <- mean(cycle_lsr[cycle_lsr > 0])

# -----------------------------------------------------------------------------
# 5. Total Cost of Settlement
# -----------------------------------------------------------------------------

phi_D <- alpha * (avg_latency_hrs ^ beta)
TCS   <- (L_total * r_rate) + phi_D

cat("=======================================================\n")
cat("  DNS SETTLEMENT RESULTS\n")
cat("=======================================================\n")
cat(sprintf("  Payments settled:         %d / %d\n",  n_settled, n_pay))
cat(sprintf("  Settlement rate:          %.2f%%\n",   settlement_rate))
cat(sprintf("  Unsettled payments:       %d\n",       n_unsettled))
cat(sprintf("  Avg settlement delay (D): %.4f hrs\n", avg_latency_hrs))
cat(sprintf("  Liquidity Saving Ratio:   %.2f%%  (benchmark: ~82%%)\n", LSR))
cat("-------------------------------------------------------\n")
cat(sprintf("  Credit risk premium:        %.1f%% (Basel II: 20%% x 8%%)\n",
            credit_premium * 100))
cat(sprintf("  Liquidity requirement (L):  EUR %s\n",
            format(round(L_total), big.mark=",")))
cat(sprintf("  Opportunity cost (L * r):   EUR %s\n",
            format(round(L_total * r_rate), big.mark=",")))
cat(sprintf("  Delay penalty phi(D):       EUR %.4f\n", phi_D))
cat(sprintf("  Total Cost of Settlement:   EUR %s\n",
            format(round(TCS), big.mark=",")))
cat("=======================================================\n\n")

# -----------------------------------------------------------------------------
# 6. Comparison with RTGS
# -----------------------------------------------------------------------------

cat("=======================================================\n")
cat("  RTGS vs DNS COMPARISON\n")
cat("=======================================================\n")
cat(sprintf("  RTGS TCS:         EUR %s\n",
            format(round(rtgs_results$TCS), big.mark=",")))
cat(sprintf("  DNS  TCS:         EUR %s\n",
            format(round(TCS), big.mark=",")))
cat(sprintf("  DNS saving:       EUR %s\n",
            format(round(rtgs_results$TCS - TCS), big.mark=",")))
cat(sprintf("  DNS saving %%:     %.2f%%\n",
            (1 - TCS / rtgs_results$TCS) * 100))
cat(sprintf("  RTGS L:           EUR %s\n",
            format(round(rtgs_results$L), big.mark=",")))
cat(sprintf("  DNS  L:           EUR %s\n",
            format(round(L_total), big.mark=",")))
cat(sprintf("  Liquidity saving: %.2f%%\n",
            (1 - L_total / rtgs_results$L) * 100))
cat("=======================================================\n\n")

# -----------------------------------------------------------------------------
# 7. Validation against BoF-PSS2 benchmark
# -----------------------------------------------------------------------------

cat("=======================================================\n")
cat("  VALIDATION AGAINST BoF-PSS2 BENCHMARK\n")
cat("=======================================================\n")
cat(sprintf("  LSR:               %.2f%%  (BoF-PSS2 benchmark: ~82%%)\n", LSR))
cat(sprintf("  Settlement rate:   %.2f%%  (expected: 100%%)\n", settlement_rate))
cat(sprintf("  Cycle duration:    %.1f hrs  (EURO1 standard: 2 hrs)\n", cycle_hrs))
cat(sprintf("  Max SF1-SF3 gap:   %.1f hrs  (by construction)\n", cycle_hrs))
cat(sprintf("  Avg SF1-SF3 gap:   %.4f hrs  (theoretical benchmark: ~1.0 hr)\n",
            avg_latency_hrs))
cat(sprintf("  Manning benchmark: tau/2 = %.1f hrs (continuous arrival)\n",
            cycle_hrs / 2))
cat("=======================================================\n\n")

# -----------------------------------------------------------------------------
# 8. Save results
# -----------------------------------------------------------------------------

dns_results <- list(
  lambda          = lambda,
  n_steps         = n_steps,
  step_length     = step_length,
  cycle_length    = cycle_length,
  cycle_hrs       = cycle_hrs,
  n_cycles        = n_cycles,
  r_rate          = r_rate,
  credit_premium  = credit_premium,
  L               = L_total,
  D               = avg_latency_hrs,
  phi_D           = phi_D,
  TCS             = TCS,
  LSR             = LSR,
  settlement_rate = settlement_rate,
  n_unsettled     = n_unsettled
)

saveRDS(payments,    "output/dns_payments.rds")
saveRDS(dns_results, "output/dns_results.rds")

cat("Saved:\n")
cat("  output/dns_payments.rds\n")
cat("  output/dns_results.rds\n\n")