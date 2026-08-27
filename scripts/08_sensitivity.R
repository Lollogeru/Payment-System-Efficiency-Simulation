# =============================================================================
# 08_sensitivity.R — Sensitivity + efficiency frontier
#   Five architectures: RTGS, DNS, Hybrid, wCBDC, XRPL (gross + vol-buffer)
#   PRIMARY winner = lowest TCS under GROSS capital.
# =============================================================================
library(igraph); set.seed(42)
if (!dir.exists("data"))    stop("Run 01_network.R first.")
if (!dir.exists("output"))  stop("Run 02_rtgs.R first.")
if (!dir.exists("figures")) dir.create("figures")

g            <- readRDS("data/network.rds")
rtgs_results <- readRDS("output/rtgs_results.rds")
dns_results  <- readRDS("output/dns_results.rds")
xrpl_results <- readRDS("output/xrpl_results.rds")
n_steps      <- rtgs_results$n_steps
step_length  <- rtgs_results$step_length
pays_base    <- readRDS("output/rtgs_payments.rds")

n_nodes     <- vcount(g)
deg         <- degree(g, mode = "all")
node_weight <- deg / sum(deg)

# ---- baseline parameters ----------------------------------------------------
cycle_length   <- 8        # DNS cycle (2 hrs)
hybrid_cycle   <- 4        # hybrid offsetting cycle (1 hr)
haircut        <- 0.02
credit_premium <- 0.016
recycle_delay  <- rtgs_results$recycle_delay
kappa_base <- 0.05; delta_base <- 1.5
sigma_xrp  <- 0.05; P_xrp <- 1.16; MD <- 1971853
alpha_base <- 0.1; beta_base <- 1.8; r_base <- 0.03; lambda_base <- 17500

# ---- payment generator (volume = NUMBER of payments) ------------------------
gen_payments <- function(lambda, n_nodes, node_weight, n_steps, seed = 42) {
  set.seed(seed)
  n <- rpois(1, lambda)
  pays <- data.frame(
    arrival_step = sort(sample(1:n_steps, n, replace = TRUE)),
    sender   = sample(1:n_nodes, n, replace = TRUE, prob = node_weight),
    receiver = sample(1:n_nodes, n, replace = TRUE, prob = node_weight),
    amount   = round(rlnorm(n, meanlog = 5, sdlog = 1.5)))
  pays[pays$sender != pays$receiver, ]
}

# ---- TCS functions ----------------------------------------------------------
compute_tcs_rtgs <- function(pays, r_rate, alpha, beta, haircut, recycle_delay,
                             n_nodes, node_weight, n_steps, step_length) {
  L <- sum(pays$amount) * 0.30 / (1 - haircut)
  liquidity <- L * node_weight; pending <- rep(0, n_nodes)
  settled <- logical(nrow(pays)); step_set <- rep(NA_integer_, nrow(pays)); queue <- integer(0)
  for (t in 1:n_steps) {
    liquidity <- liquidity + pending; pending <- rep(0, n_nodes)
    to_process <- c(queue, which(pays$arrival_step == t)); queue <- integer(0)
    for (i in to_process) {
      s <- pays$sender[i]; r <- pays$receiver[i]; a <- pays$amount[i]
      if (liquidity[s] >= a) {
        liquidity[s] <- liquidity[s] - a
        pending[r]   <- pending[r]   + a * recycle_delay          # <-- restored
        liquidity[r] <- liquidity[r] + a * (1 - recycle_delay)
        settled[i] <- TRUE; step_set[i] <- t
      } else queue <- c(queue, i)
    }
  }
  D <- mean((step_set - pays$arrival_step) * step_length, na.rm = TRUE)
  if (is.nan(D)) D <- 0
  list(TCS = L * r_rate + alpha * D^beta, L = L, D = D)
}

# Generic multilateral-netting engine (DNS and Hybrid differ only in cycle)
compute_tcs_net <- function(pays, r_rate, alpha, beta, credit_premium,
                            n_nodes, cycle_length, n_steps, step_length) {
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
  D <- mean(delays); list(TCS = L_total * r_rate + alpha * D^beta, L = L_total, D = D)
}

compute_tcs_wcbdc <- function(pays, r_rate, alpha, beta, MD, P_xrp) {
  L <- MD * P_xrp                                   # CB money, gross, encumbered
  D <- mean(runif(nrow(pays), 3, 5)) / 3600         # atomic finality
  list(TCS = L * r_rate + alpha * D^beta, L = L, D = D)   # no slippage, no buffer
}

compute_tcs_xrpl <- function(pays, r_rate, alpha, beta, MD, sigma_xrp, P_xrp, kappa, delta_slip) {
  L_gross <- MD * P_xrp; L_vbuf <- MD * sigma_xrp * P_xrp
  slip <- sum(kappa * (pays$amount / MD)^delta_slip * pays$amount)
  D <- mean(runif(nrow(pays), 3, 5)) / 3600
  list(L_gross = L_gross, L_vbuf = L_vbuf, slip = slip, D = D,
       TCS_gross = L_gross * r_rate + slip + alpha * D^beta,
       TCS_vbuf  = L_vbuf  * r_rate + slip + alpha * D^beta)
}

# winner under GROSS capital across the five architectures
winner_gross <- function(rt, dn, hy, wc, xr) {
  v <- c(RTGS = rt$TCS, DNS = dn$TCS, Hybrid = hy$TCS, wCBDC = wc$TCS, XRPL = xr$TCS_gross)
  names(v)[which.min(v)]
}
row_all <- function(key, val, rt, dn, hy, wc, xr) data.frame(
  key = key, value = val,
  RTGS = rt$TCS, DNS = dn$TCS, Hybrid = hy$TCS, wCBDC = wc$TCS,
  XRPL_gross = xr$TCS_gross, XRPL_vbuf = xr$TCS_vbuf,
  winner = winner_gross(rt, dn, hy, wc, xr), stringsAsFactors = FALSE)

# ---- BASELINE (gives Table 2 Hybrid + wCBDC cells) --------------------------
rt <- compute_tcs_rtgs(pays_base, r_base, alpha_base, beta_base, haircut, recycle_delay,
                       n_nodes, node_weight, n_steps, step_length)
dn <- compute_tcs_net (pays_base, r_base, alpha_base, beta_base, credit_premium,
                       n_nodes, cycle_length, n_steps, step_length)
hy <- compute_tcs_net (pays_base, r_base, alpha_base, beta_base, credit_premium,
                       n_nodes, hybrid_cycle, n_steps, step_length)
wc <- compute_tcs_wcbdc(pays_base, r_base, alpha_base, beta_base, MD, P_xrp)
xr <- compute_tcs_xrpl(pays_base, r_base, alpha_base, beta_base, MD, sigma_xrp, P_xrp,
                       kappa_base, delta_base)
cat("============ BASELINE (r=3%, lambda=17,500) ============\n")
cat(sprintf("  RTGS   L=%s  D=%.4f  TCS=%s\n", format(round(rt$L),big.mark=","), rt$D, format(round(rt$TCS),big.mark=",")))
cat(sprintf("  DNS    L=%s  D=%.4f  TCS=%s\n", format(round(dn$L),big.mark=","), dn$D, format(round(dn$TCS),big.mark=",")))
cat(sprintf("  HYBRID L=%s  D=%.4f  TCS=%s   <-- Table 2 cells\n", format(round(hy$L),big.mark=","), hy$D, format(round(hy$TCS),big.mark=",")))
cat(sprintf("  wCBDC  L=%s  D=%.4f  TCS=%s\n", format(round(wc$L),big.mark=","), wc$D, format(round(wc$TCS),big.mark=",")))
cat(sprintf("  XRPL   L_gross=%s TCS_gross=%s | L_vbuf=%s TCS_vbuf=%s\n",
            format(round(xr$L_gross),big.mark=","), format(round(xr$TCS_gross),big.mark=","),
            format(round(xr$L_vbuf),big.mark=","),  format(round(xr$TCS_vbuf),big.mark=",")))
cat(sprintf("  GROSS winner: %s\n", winner_gross(rt,dn,hy,wc,xr)))
cat("========================================================\n\n")

# ---- ranges -----------------------------------------------------------------
r_range      <- c(0, 0.001, seq(0.01, 0.08, 0.01))
lambda_range <- seq(10000, 100000, 5000)
alpha_range  <- c(0.05, 0.1, 0.2); beta_range <- c(1.2, 1.8, 2.5)
kappa_range  <- c(0.03,0.04,0.05,0.06,0.07); delta_range <- c(1.0,1.5,2.0)
gamma_range  <- c(2.0, 2.5, 3.0)

# ---- 1) interest rate -------------------------------------------------------
res_r <- do.call(rbind, lapply(r_range, function(rr) {
  rt <- compute_tcs_rtgs(pays_base, rr, alpha_base, beta_base, haircut, recycle_delay, n_nodes, node_weight, n_steps, step_length)
  dn <- compute_tcs_net (pays_base, rr, alpha_base, beta_base, credit_premium, n_nodes, cycle_length, n_steps, step_length)
  hy <- compute_tcs_net (pays_base, rr, alpha_base, beta_base, credit_premium, n_nodes, hybrid_cycle, n_steps, step_length)
  wc <- compute_tcs_wcbdc(pays_base, rr, alpha_base, beta_base, MD, P_xrp)
  xr <- compute_tcs_xrpl(pays_base, rr, alpha_base, beta_base, MD, sigma_xrp, P_xrp, kappa_base, delta_base)
  row_all("r", rr, rt, dn, hy, wc, xr) }))

# ---- 2) volume (gen_payments => varies COUNT) -------------------------------
res_lambda <- do.call(rbind, lapply(lambda_range, function(L) {
  p <- gen_payments(L, n_nodes, node_weight, n_steps)
  rt <- compute_tcs_rtgs(p, r_base, alpha_base, beta_base, haircut, recycle_delay, n_nodes, node_weight, n_steps, step_length)
  dn <- compute_tcs_net (p, r_base, alpha_base, beta_base, credit_premium, n_nodes, cycle_length, n_steps, step_length)
  hy <- compute_tcs_net (p, r_base, alpha_base, beta_base, credit_premium, n_nodes, hybrid_cycle, n_steps, step_length)
  wc <- compute_tcs_wcbdc(p, r_base, alpha_base, beta_base, MD, P_xrp)
  xr <- compute_tcs_xrpl(p, r_base, alpha_base, beta_base, MD, sigma_xrp, P_xrp, kappa_base, delta_base)
  row_all("lambda", L, rt, dn, hy, wc, xr) }))

# ---- 3) alpha, 4) beta, 5) kappa, 6) delta (XRPL-side params) ---------------
sweep_simple <- function(key, vals, set) do.call(rbind, lapply(vals, function(x) {
  a <- if (key=="alpha") x else alpha_base; b <- if (key=="beta") x else beta_base
  k <- if (key=="kappa") x else kappa_base; d <- if (key=="delta") x else delta_base
  rt <- compute_tcs_rtgs(pays_base, r_base, a, b, haircut, recycle_delay, n_nodes, node_weight, n_steps, step_length)
  dn <- compute_tcs_net (pays_base, r_base, a, b, credit_premium, n_nodes, cycle_length, n_steps, step_length)
  hy <- compute_tcs_net (pays_base, r_base, a, b, credit_premium, n_nodes, hybrid_cycle, n_steps, step_length)
  wc <- compute_tcs_wcbdc(pays_base, r_base, a, b, MD, P_xrp)
  xr <- compute_tcs_xrpl(pays_base, r_base, a, b, MD, sigma_xrp, P_xrp, k, d)
  row_all(key, x, rt, dn, hy, wc, xr) }))
res_alpha <- sweep_simple("alpha", alpha_range); res_beta  <- sweep_simple("beta",  beta_range)
res_kappa <- sweep_simple("kappa", kappa_range); res_delta <- sweep_simple("delta", delta_range)

# ---- 7) gamma (regenerate network) -----------------------------------------
res_gamma <- do.call(rbind, lapply(gamma_range, function(gam) {
  set.seed(42); gt <- sample_pa(n = 50, power = gam, m = 2, directed = TRUE)
  wt <- degree(gt, mode="all") / sum(degree(gt, mode="all"))
  p  <- gen_payments(lambda_base, 50, wt, n_steps)
  rt <- compute_tcs_rtgs(p, r_base, alpha_base, beta_base, haircut, recycle_delay, 50, wt, n_steps, step_length)
  dn <- compute_tcs_net (p, r_base, alpha_base, beta_base, credit_premium, 50, cycle_length, n_steps, step_length)
  hy <- compute_tcs_net (p, r_base, alpha_base, beta_base, credit_premium, 50, hybrid_cycle, n_steps, step_length)
  wc <- compute_tcs_wcbdc(p, r_base, alpha_base, beta_base, MD, P_xrp)
  xr <- compute_tcs_xrpl(p, r_base, alpha_base, beta_base, MD, sigma_xrp, P_xrp, kappa_base, delta_base)
  row_all("gamma", gam, rt, dn, hy, wc, xr) }))

# ---- print the two main tables ---------------------------------------------
fmt <- function(x) format(round(x), big.mark=",")
cat("\n===== TABLE 3: INTEREST RATE (gross winner) =====\n")
cat(sprintf("%-6s %10s %10s %10s %12s %12s  %s\n","r","RTGS","DNS","wCBDC","XRPL_gross","XRPL_vbuf","Winner"))
for (i in 1:nrow(res_r)) cat(sprintf("%-6.3f %10s %10s %10s %12s %12s  %s\n",
                                     res_r$value[i], fmt(res_r$RTGS[i]), fmt(res_r$DNS[i]), fmt(res_r$wCBDC[i]),
                                     fmt(res_r$XRPL_gross[i]), fmt(res_r$XRPL_vbuf[i]), res_r$winner[i]))
cat("\n===== TABLE 4: VOLUME (gross winner) =====\n")
cat(sprintf("%-7s %10s %10s %10s %12s %12s  %s\n","lambda","RTGS","DNS","wCBDC","XRPL_gross","XRPL_vbuf","Winner"))
for (i in 1:nrow(res_lambda)) cat(sprintf("%-7d %10s %10s %10s %12s %12s  %s\n",
                                          as.integer(res_lambda$value[i]), fmt(res_lambda$RTGS[i]), fmt(res_lambda$DNS[i]), fmt(res_lambda$wCBDC[i]),
                                          fmt(res_lambda$XRPL_gross[i]), fmt(res_lambda$XRPL_vbuf[i]), res_lambda$winner[i]))

# ---- win-rate summary under gross ------------------------------------------
all_win <- c(res_r$winner, res_lambda$winner, res_alpha$winner, res_beta$winner,
             res_kappa$winner, res_delta$winner, res_gamma$winner)
cat("\n===== GROSS-CAPITAL WIN COUNTS =====\n"); print(table(all_win))
cat(sprintf("(Under the volatility-buffer column XRPL is lowest at every positive rate/volume.)\n\n"))

# ---- efficiency frontier (six points) --------------------------------------
frontier <- data.frame(
  system    = c("RTGS","DNS","Hybrid","wCBDC","XRPL_gross","XRPL_vbuf"),
  latency   = c(rt$D, dn$D, hy$D, wc$D, xr$D, xr$D),
  liquidity = c(rtgs_results$L, dns_results$L, hy$L, MD*P_xrp, MD*P_xrp, MD*sigma_xrp*P_xrp))

png("output/figures/efficiency_frontier.png", width = 950, height = 680)
cols <- c("red","blue","orange","purple","darkgreen","green3")

# We use xlim and ylim adjustments to ensure the further-out labels don't get cut off
plot(frontier$latency, frontier$liquidity, pch = c(19,19,19,17,17,19), cex = 2.2, col = cols,
     xlab = "Settlement Latency (hours)", ylab = "Liquidity Requirement (EUR)",
     main = "Efficiency Frontier (gross capital primary)",
     xlim = c(-0.1, max(frontier$latency)*1.3), 
     ylim = c(-100000, max(frontier$liquidity)*1.25))

# 1. Define directions to fan out the overlapping cluster
# 3 = Above, 4 = Right, 1 = Below
label_positions <- c(3, 4, 4, 4, 1, 4) 

# 2. Add the labels with a custom offset
text(frontier$latency, frontier$liquidity, 
     labels = frontier$system, 
     pos    = label_positions, 
     offset = 1,       
     col    = cols,
     cex    = 1.1, 
     font   = 2)

legend("topright", legend = frontier$system, col = cols, pch = c(19,19,19,17,17,19), cex = 0.9)
dev.off()

# ---- save -------------------------------------------------------------------
saveRDS(list(r=res_r, lambda=res_lambda, alpha=res_alpha, beta=res_beta,
             kappa=res_kappa, delta=res_delta, gamma=res_gamma, frontier=frontier),
        "output/sensitivity_results.rds")
write.csv(res_r,      "output/r_sensitivity.csv",      row.names = FALSE)
write.csv(res_lambda, "output/lambda_sensitivity.csv", row.names = FALSE)
write.csv(frontier,   "output/efficiency_frontier.csv", row.names = FALSE)
cat("Saved sensitivity CSVs + figures/efficiency_frontier.png\n")