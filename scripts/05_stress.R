# =============================================================================
# 05_stress.R
# Stress Testing: Hub Default and Contagion Dynamics
#
# Theory:
#   The Exposure Amplification Factor (EAF) measures contagion intensity:
#     EAF = Secondary systemic exposure / Direct hub exposure
#
#   A hub default at peak volume propagates differently across the five
#   settlement configurations:
#
#   RTGS:   full-breadth, full-magnitude contagion (no containment ex ante)
#   DNS:    breadth-compressed contagion (multilateral netting in 2-hr cycle)
#   Hybrid: same netting mechanism at higher frequency (1-hr cycle),
#           recovering less offset than DNS but containing more than RTGS
#   XRPL:   magnitude-compressed contagion (3-5 sec atomic finality)
#   wCBDC:  same atomic-finality containment as XRPL, by construction
#
#   Time discretisation: 32 steps of 0.25 hours each = 8 operating hrs.
#   Hub default at step t=16 (4-hour midpoint).
#   DNS cycle: 8 steps (2 hrs, EURO1).  Hybrid cycle: 4 steps (1 hr).
#
# Inputs:  data/network.rds, data/hub_nodes.rds, output/rtgs_results.rds
# Outputs: output/stress_results.rds, output/mc_stress_results.csv
#
# References:
#   Berndsen (2018); Bech & Garratt (2003); Manning, Nier & Schanz (2009)
# =============================================================================
library(igraph)
set.seed(42)
if (!dir.exists("data"))   stop("Run 01_network.R first.")
if (!dir.exists("output")) stop("Run 02_rtgs.R first.")

# -----------------------------------------------------------------------------
# 1. Load network and parameters
# -----------------------------------------------------------------------------
g            <- readRDS("data/network.rds")
hub_nodes    <- readRDS("data/hub_nodes.rds")
spoke_nodes  <- readRDS("data/spoke_nodes.rds")
n_nodes      <- vcount(g)
deg          <- degree(g, mode = "all")
node_weight  <- deg / sum(deg)
rtgs_results <- readRDS("output/rtgs_results.rds")
n_steps      <- rtgs_results$n_steps      # 32
step_length  <- rtgs_results$step_length  # 0.25 hrs

# -----------------------------------------------------------------------------
# 2. Parameters
# -----------------------------------------------------------------------------
lambda_stress  <- 50000   # peak stress volume (3x baseline)
cycle_length   <- 8       # DNS cycle: 8 steps = 2 hrs (EURO1)
hybrid_cycle   <- 4       # Hybrid offsetting cycle: 4 steps = 1 hr
n_cycles_dns   <- n_steps / cycle_length
n_cycles_hyb   <- n_steps / hybrid_cycle
finality_max   <- 5       # XRPL/wCBDC maximum finality in seconds

default_time   <- 16      # hub default at midpoint (4 hrs in)
default_node   <- hub_nodes[which.max(deg[hub_nodes])]

cat("=======================================================\n")
cat("  STRESS TEST SETUP\n")
cat("=======================================================\n")
cat(sprintf("  n_steps:                 %d (%.2f hrs each)\n",
            n_steps, step_length))
cat(sprintf("  DNS cycle:               %d steps (%.1f hrs)\n",
            cycle_length, cycle_length * step_length))
cat(sprintf("  Hybrid cycle:            %d steps (%.1f hrs)\n",
            hybrid_cycle, hybrid_cycle * step_length))
cat(sprintf("  Stress volume (lambda):  %d\n",   lambda_stress))
cat(sprintf("  Defaulting hub node:     %d  (degree %d)\n",
            default_node, deg[default_node]))
cat(sprintf("  Default time step:       %d (%.1f hrs into day)\n",
            default_time, default_time * step_length))
cat("=======================================================\n\n")

# -----------------------------------------------------------------------------
# 3. EAF computation functions
# -----------------------------------------------------------------------------

# RTGS: full gross exposure for all payments involving the hub from default onward
compute_rtgs_eaf <- function(pays, default_node, default_time, n_nodes) {
  affected <- which(
    (pays$sender == default_node | pays$receiver == default_node) &
      pays$arrival_step >= default_time)
  if (length(affected) == 0) return(NA)
  direct_exposure <- sum(pays$amount[affected])
  lost_inflows <- rep(0, n_nodes)
  for (i in affected) {
    if (pays$sender[i] == default_node) {
      lost_inflows[pays$receiver[i]] <- lost_inflows[pays$receiver[i]] +
        pays$amount[i]
    }
  }
  secondary_exposure <- sum(lost_inflows[lost_inflows > 0])
  list(eaf = secondary_exposure / direct_exposure,
       direct_exposure = direct_exposure,
       secondary_exposure = secondary_exposure,
       affected_nodes = sum(lost_inflows > 0))
}

# Generic netting-based EAF: net receivables vs the hub within the cycle
# containing the default. Used for both DNS (cycle_length = 8) and Hybrid
# (cycle_length = 4).
compute_netting_eaf <- function(pays, default_node, default_time,
                                n_nodes, cycle_length) {
  affected <- which(
    (pays$sender == default_node | pays$receiver == default_node) &
      pays$arrival_step >= default_time)
  if (length(affected) == 0) return(NA)
  direct_exposure <- sum(pays$amount[affected])
  default_cycle <- ceiling(default_time / cycle_length)
  t_start <- (default_cycle - 1) * cycle_length + 1
  t_end   <-  default_cycle * cycle_length
  in_cycle <- which(pays$arrival_step >= t_start &
                      pays$arrival_step <= t_end)
  secondary_exposure <- 0
  affected_nodes     <- 0
  for (node in 1:n_nodes) {
    if (node == default_node) next
    received <- sum(pays$amount[
      in_cycle[pays$sender[in_cycle]   == default_node &
                 pays$receiver[in_cycle] == node]])
    sent <- sum(pays$amount[
      in_cycle[pays$sender[in_cycle]   == node &
                 pays$receiver[in_cycle] == default_node]])
    net_receivable <- received - sent
    if (net_receivable > 0) {
      secondary_exposure <- secondary_exposure + net_receivable
      affected_nodes     <- affected_nodes + 1
    }
  }
  list(eaf = secondary_exposure / direct_exposure,
       direct_exposure = direct_exposure,
       secondary_exposure = secondary_exposure,
       affected_nodes = affected_nodes)
}

# Atomic-finality EAF: only the in-flight fraction (finality_max / day) is
# at risk. Used for both XRPL and wCBDC, which share atomic settlement.
compute_atomic_eaf <- function(pays, default_node, default_time,
                               n_steps, step_length, finality_max) {
  affected <- which(
    (pays$sender == default_node | pays$receiver == default_node) &
      pays$arrival_step >= default_time)
  if (length(affected) == 0) return(NA)
  direct_exposure <- sum(pays$amount[affected])
  total_seconds  <- n_steps * step_length * 3600
  in_flight_frac <- finality_max / total_seconds
  secondary_exposure <- direct_exposure * in_flight_frac
  affected_nodes <- length(unique(c(
    pays$receiver[affected[pays$sender[affected]   == default_node]],
    pays$sender[affected[pays$receiver[affected] == default_node]])))
  list(eaf = secondary_exposure / direct_exposure,
       direct_exposure = direct_exposure,
       secondary_exposure = secondary_exposure,
       affected_nodes = affected_nodes,
       in_flight_frac = in_flight_frac)
}

# Thin dispatchers for clarity in the loop
compute_dns_eaf    <- function(pays) compute_netting_eaf(pays, default_node,
                                                         default_time, n_nodes, cycle_length)
compute_hybrid_eaf <- function(pays) compute_netting_eaf(pays, default_node,
                                                         default_time, n_nodes, hybrid_cycle)
compute_xrpl_eaf   <- function(pays) compute_atomic_eaf(pays, default_node,
                                                        default_time, n_steps, step_length, finality_max)
compute_wcbdc_eaf  <- function(pays) compute_atomic_eaf(pays, default_node,
                                                        default_time, n_steps, step_length, finality_max)

# -----------------------------------------------------------------------------
# 4. Helper to generate a stress-volume payment population
# -----------------------------------------------------------------------------
gen_stress_pays <- function() {
  n_s <- rpois(1, lambda_stress)
  p   <- data.frame(
    arrival_step = sort(sample(1:n_steps, n_s, replace = TRUE)),
    sender       = sample(1:n_nodes, n_s, replace = TRUE, prob = node_weight),
    receiver     = sample(1:n_nodes, n_s, replace = TRUE, prob = node_weight),
    amount       = round(rlnorm(n_s, meanlog = 5, sdlog = 1.5)))
  p[p$sender != p$receiver, ]
}

# -----------------------------------------------------------------------------
# 5. Single-run stress test (seed 42)
# -----------------------------------------------------------------------------
pays_s <- gen_stress_pays()
rtgs_s <- compute_rtgs_eaf(pays_s, default_node, default_time, n_nodes)
dns_s  <- compute_dns_eaf(pays_s)
hyb_s  <- compute_hybrid_eaf(pays_s)
xrpl_s <- compute_xrpl_eaf(pays_s)
wcbdc_s<- compute_wcbdc_eaf(pays_s)

cat("=======================================================\n")
cat("  SINGLE-RUN STRESS RESULTS (seed 42)\n")
cat("=======================================================\n")
cat(sprintf("  In-flight fraction (atomic): %.6f  (= %d sec / %.0f sec/day)\n",
            xrpl_s$in_flight_frac, finality_max, n_steps*step_length*3600))
cat("-------------------------------------------------------\n")
print_row <- function(label, res) {
  loss <- res$secondary_exposure / max(1, res$affected_nodes)
  cat(sprintf("  %-7s EAF: %.6f | Affected: %3d | Avg loss: EUR %s\n",
              label, res$eaf, res$affected_nodes,
              format(round(loss), big.mark=",")))
}
print_row("RTGS",   rtgs_s)
print_row("DNS",    dns_s)
print_row("Hybrid", hyb_s)
print_row("XRPL",   xrpl_s)
print_row("wCBDC",  wcbdc_s)
cat("=======================================================\n\n")

# -----------------------------------------------------------------------------
# 6. Monte Carlo stress test (1,000 runs)
# -----------------------------------------------------------------------------
n_mc <- 1000
mc <- data.frame(
  sim = 1:n_mc,
  rtgs_eaf = NA_real_, dns_eaf = NA_real_, hyb_eaf = NA_real_,
  xrpl_eaf = NA_real_, wcbdc_eaf = NA_real_,
  rtgs_aff = NA_integer_, dns_aff = NA_integer_, hyb_aff = NA_integer_,
  xrpl_aff = NA_integer_, wcbdc_aff = NA_integer_,
  rtgs_loss= NA_real_, dns_loss = NA_real_, hyb_loss = NA_real_,
  xrpl_loss= NA_real_, wcbdc_loss= NA_real_
)

cat("Running Monte Carlo stress test (1,000 runs)...\n")
for (sim in 1:n_mc) {
  set.seed(sim)
  p  <- gen_stress_pays()
  r  <- compute_rtgs_eaf(p, default_node, default_time, n_nodes)
  d  <- compute_dns_eaf(p)
  h  <- compute_hybrid_eaf(p)
  x  <- compute_xrpl_eaf(p)
  w  <- compute_wcbdc_eaf(p)
  fill <- function(res, eaf_col, aff_col, loss_col) {
    if (is.null(res) || is.na(res$eaf)) return(invisible())
    mc[[eaf_col]][sim]  <<- res$eaf
    mc[[aff_col]][sim]  <<- res$affected_nodes
    mc[[loss_col]][sim] <<- res$secondary_exposure / max(1, res$affected_nodes)
  }
  fill(r, "rtgs_eaf",  "rtgs_aff",  "rtgs_loss")
  fill(d, "dns_eaf",   "dns_aff",   "dns_loss")
  fill(h, "hyb_eaf",   "hyb_aff",   "hyb_loss")
  fill(x, "xrpl_eaf",  "xrpl_aff",  "xrpl_loss")
  fill(w, "wcbdc_eaf", "wcbdc_aff", "wcbdc_loss")
}

# -----------------------------------------------------------------------------
# 7. Monte Carlo summary
# -----------------------------------------------------------------------------
m  <- function(x) mean(x, na.rm=TRUE)
q9 <- function(x) quantile(x, 0.95, na.rm=TRUE)
se <- function(x) sd(x, na.rm=TRUE) / sqrt(n_mc)

cat("\n=======================================================\n")
cat("  MONTE CARLO STRESS RESULTS (1,000 runs)\n")
cat("=======================================================\n")
cat(sprintf("  %-20s %10s %10s %10s %12s %12s\n",
            "Metric", "RTGS", "DNS", "Hybrid", "XRPL", "wCBDC"))
cat(sprintf("  %-20s %10.4f %10.4f %10.4f %12.6f %12.6f\n", "Mean EAF",
            m(mc$rtgs_eaf), m(mc$dns_eaf), m(mc$hyb_eaf),
            m(mc$xrpl_eaf), m(mc$wcbdc_eaf)))
cat(sprintf("  %-20s %10.4f %10.4f %10.4f %12.6f %12.6f\n", "95th pct EAF",
            q9(mc$rtgs_eaf), q9(mc$dns_eaf), q9(mc$hyb_eaf),
            q9(mc$xrpl_eaf), q9(mc$wcbdc_eaf)))
cat(sprintf("  %-20s %10.1f %10.1f %10.1f %12.1f %12.1f\n", "Mean affected nodes",
            m(mc$rtgs_aff), m(mc$dns_aff), m(mc$hyb_aff),
            m(mc$xrpl_aff), m(mc$wcbdc_aff)))
cat(sprintf("  %-20s %10.0f %10.0f %10.0f %12.0f %12.0f\n", "Mean loss/node (EUR)",
            m(mc$rtgs_loss), m(mc$dns_loss), m(mc$hyb_loss),
            m(mc$xrpl_loss), m(mc$wcbdc_loss)))
cat("-------------------------------------------------------\n")
se_r <- se(mc$rtgs_eaf);  se_d <- se(mc$dns_eaf);  se_h <- se(mc$hyb_eaf)
se_x <- se(mc$xrpl_eaf);  se_w <- se(mc$wcbdc_eaf)
cat(sprintf("  %-20s %10.6f %10.6f %10.6f %12.8f %12.8f\n", "Monte Carlo SE",
            se_r, se_d, se_h, se_x, se_w))
cat(sprintf("  %-20s %10s %10s %10s %12s %12s\n", "SE < 5% threshold",
            ifelse(se_r<0.05,"PASS","FAIL"), ifelse(se_d<0.05,"PASS","FAIL"),
            ifelse(se_h<0.05,"PASS","FAIL"), ifelse(se_x<0.05,"PASS","FAIL"),
            ifelse(se_w<0.05,"PASS","FAIL")))
cat("=======================================================\n\n")

# -----------------------------------------------------------------------------
# 8. Save results
# -----------------------------------------------------------------------------
stress_results <- list(
  default_node = default_node, default_time = default_time,
  default_hrs  = default_time * step_length,
  lambda_stress= lambda_stress, n_steps = n_steps, step_length = step_length,
  cycle_length = cycle_length, hybrid_cycle = hybrid_cycle,
  rtgs_mean_eaf  = m(mc$rtgs_eaf),  rtgs_95_eaf  = q9(mc$rtgs_eaf),
  rtgs_aff_nodes = m(mc$rtgs_aff),  rtgs_loss_node = m(mc$rtgs_loss),
  dns_mean_eaf   = m(mc$dns_eaf),   dns_95_eaf   = q9(mc$dns_eaf),
  dns_aff_nodes  = m(mc$dns_aff),   dns_loss_node  = m(mc$dns_loss),
  hyb_mean_eaf   = m(mc$hyb_eaf),   hyb_95_eaf   = q9(mc$hyb_eaf),
  hyb_aff_nodes  = m(mc$hyb_aff),   hyb_loss_node  = m(mc$hyb_loss),
  xrpl_mean_eaf  = m(mc$xrpl_eaf),  xrpl_95_eaf  = q9(mc$xrpl_eaf),
  xrpl_aff_nodes = m(mc$xrpl_aff),  xrpl_loss_node = m(mc$xrpl_loss),
  wcbdc_mean_eaf = m(mc$wcbdc_eaf), wcbdc_95_eaf = q9(mc$wcbdc_eaf),
  wcbdc_aff_nodes= m(mc$wcbdc_aff), wcbdc_loss_node= m(mc$wcbdc_loss),
  se_rtgs = se_r, se_dns = se_d, se_hyb = se_h,
  se_xrpl = se_x, se_wcbdc = se_w
)
saveRDS(stress_results, "output/stress_results.rds")
write.csv(mc,           "output/mc_stress_results.csv", row.names = FALSE)
cat("Saved:\n")
cat("  output/stress_results.rds\n")
cat("  output/mc_stress_results.csv\n\n")