# =============================================================================
# 07b_hybrid.R — Hybrid RTGS+LSM as high-frequency multilateral offsetting
#   Netting cycle = 4 steps (1 hour) vs DNS's 8 steps (2 hours).
#   Lands between RTGS and DNS on both liquidity and latency.
# =============================================================================
library(igraph); set.seed(42)
g <- readRDS("data/network.rds"); n_nodes <- vcount(g)
pays <- readRDS("output/rtgs_payments.rds")
rtgs <- readRDS("output/rtgs_results.rds")
n_steps <- rtgs$n_steps; step_length <- rtgs$step_length
r_rate <- 0.03; alpha <- 0.1; beta <- 1.8; credit_premium <- 0.016
cycle_length <- 4                                  # 1-hour offsetting interval

n_cycles <- n_steps / cycle_length; L_total <- 0; delays <- c()
for (cyc in 1:n_cycles) {
  t1 <- (cyc-1)*cycle_length + 1; t2 <- cyc*cycle_length
  idx <- which(pays$arrival_step >= t1 & pays$arrival_step <= t2)
  if (length(idx) == 0) next
  net <- rep(0, n_nodes)
  for (i in idx) { net[pays$sender[i]]   <- net[pays$sender[i]]   - pays$amount[i]
  net[pays$receiver[i]] <- net[pays$receiver[i]] + pays$amount[i] }
  L_total <- L_total + sum(pmax(0, -net)) / (1 - credit_premium)
  delays  <- c(delays, (t2 - pays$arrival_step[idx]) * step_length)
}
D_hybrid <- mean(delays); TCS_hybrid <- L_total * r_rate + alpha * D_hybrid^beta
cat(sprintf("HYBRID: L = EUR %s | D = %.4f hrs | TCS = EUR %s\n",
            format(round(L_total),big.mark=","), D_hybrid, format(round(TCS_hybrid),big.mark=",")))
cat("Expected: L between DNS (1,315,335) and RTGS (2,287,265); D between ~0.44 and DNS 0.8881\n")
saveRDS(list(L=L_total, D=D_hybrid, TCS=TCS_hybrid, cycle_length=cycle_length),
        "output/hybrid_results.rds")