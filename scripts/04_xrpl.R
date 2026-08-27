# =============================================================================
# 04_xrpl.R — XRPL atomic settlement (reports BOTH liquidity concepts)
#   Gross capital  : L_gross = MD * P_xrp           (PRIMARY, symmetric w/ RTGS)
#   Volatility buf : L_vbuf  = MD * sigma * P_xrp    (risk-adjusted ALTERNATIVE)
# =============================================================================
library(igraph)
set.seed(42)
if (!dir.exists("data"))   stop("Run 01_network.R first.")
if (!dir.exists("output")) stop("Run 02_rtgs.R first.")

g           <- readRDS("data/network.rds")
n_nodes     <- vcount(g)
deg         <- degree(g, mode = "all")
node_weight <- deg / sum(deg)

payments     <- readRDS("output/rtgs_payments.rds")
rtgs_results <- readRDS("output/rtgs_results.rds")
dns_results  <- readRDS("output/dns_results.rds")
n_pay        <- nrow(payments)
n_steps      <- rtgs_results$n_steps
step_length  <- rtgs_results$step_length

# ---- parameters -------------------------------------------------------------
lambda <- 17500; r_rate <- 0.03; alpha <- 0.1; beta <- 1.8
p_rollback <- 1e-4; finality_min <- 3; finality_max <- 5
kappa <- 0.05; delta_slip <- 1.5
sigma_xrp <- 0.05; P_xrp <- 1.16
MD <- 1971853

# ---- liquidity: both concepts ----------------------------------------------
L_gross <- MD * P_xrp                 # primary
L_vbuf  <- MD * sigma_xrp * P_xrp     # alternative

# ---- atomic settlement loop -------------------------------------------------
settled <- logical(n_pay); slippage_cost <- rep(0, n_pay); finality_hrs <- rep(NA_real_, n_pay)
for (i in 1:n_pay) {
  amt <- payments$amount[i]
  if (runif(1) < p_rollback) next
  slippage_cost[i] <- kappa * (amt / MD)^delta_slip * amt
  finality_hrs[i]  <- runif(1, finality_min, finality_max) / 3600
  settled[i] <- TRUE
}
n_settled <- sum(settled); settlement_rate <- n_settled / n_pay * 100
avg_latency_hrs <- mean(finality_hrs, na.rm = TRUE)
total_slippage  <- sum(slippage_cost)
phi_D <- alpha * (avg_latency_hrs ^ beta)

# ---- TCS under both concepts ------------------------------------------------
TCS_gross <- (L_gross * r_rate) + phi_D + total_slippage
TCS_vbuf  <- (L_vbuf  * r_rate) + phi_D + total_slippage

cat("================ XRPL RESULTS ================\n")
cat(sprintf("  Settlement rate:        %.4f%%\n", settlement_rate))
cat(sprintf("  Total slippage:         EUR %.2f\n", total_slippage))
cat(sprintf("  L_gross  = MD*P:        EUR %s\n", format(round(L_gross), big.mark=",")))
cat(sprintf("  L_vbuf   = MD*sig*P:    EUR %s\n", format(round(L_vbuf),  big.mark=",")))
cat(sprintf("  TCS (gross, PRIMARY):   EUR %s\n", format(round(TCS_gross), big.mark=",")))
cat(sprintf("  TCS (vol-buffer, ALT):  EUR %s\n", format(round(TCS_vbuf),  big.mark=",")))
cat("---------------------------------------------\n")
cat(sprintf("  RTGS TCS:               EUR %s\n", format(round(rtgs_results$TCS), big.mark=",")))
cat(sprintf("  DNS  TCS:               EUR %s\n", format(round(dns_results$TCS),  big.mark=",")))
cat(sprintf("  Under GROSS: XRPL ~ RTGS, DNS lowest  -> %s\n",
            ifelse(TCS_gross > dns_results$TCS, "CONFIRMED (DNS cheapest)", "check")))
cat(sprintf("  Under VBUF : XRPL < DNS < RTGS        -> %s\n",
            ifelse(TCS_vbuf < dns_results$TCS & dns_results$TCS < rtgs_results$TCS,
                   "CONFIRMED", "VIOLATED")))
cat("=============================================\n\n")

xrpl_results <- list(
  lambda = lambda, n_steps = n_steps, step_length = step_length, r_rate = r_rate,
  L = L_gross, TCS = TCS_gross,            # PRIMARY = gross
  L_gross = L_gross, L_vbuf = L_vbuf,
  TCS_gross = TCS_gross, TCS_vbuf = TCS_vbuf,
  D = avg_latency_hrs, phi_D = phi_D, slippage = total_slippage,
  settlement_rate = settlement_rate, MD = MD, P_xrp = P_xrp, sigma_xrp = sigma_xrp
)
payments$settled <- settled; payments$slippage_cost <- slippage_cost
saveRDS(payments,     "output/xrpl_payments.rds")
saveRDS(xrpl_results, "output/xrpl_results.rds")
cat("Saved output/xrpl_results.rds (L/TCS now default to GROSS)\n\n")