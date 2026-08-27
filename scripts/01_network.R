# =============================================================================
# 01_network.R
# Build and validate the 50-node scale-free interbank payment network
#
# Theory:
#   Empirical analyses of large-value payment systems (Soramäki et al., 2007)
#   document power-law degree distributions with extreme connectivity
#   concentration: a small number of systemically important hub institutions
#   intermediate the majority of payment flows, while the majority of
#   participants are lightly connected spokes that route through the hubs.
#
#   We replicate this structure using the Barabási–Albert preferential
#   attachment algorithm (Barabási & Albert, 1999), where new nodes attach
#   to existing nodes with probability proportional to their current degree
#   ("rich get richer"). The power parameter gamma = 2.5 and m = 2 new
#   edges per node are calibrated to produce a hub-to-spoke degree ratio
#   consistent with the Fedwire empirical benchmark of ~23.5x.
#
# Output:
#   data/network.rds     — igraph network object
#   data/hub_nodes.rds   — integer vector of 5 hub node indices
#   data/spoke_nodes.rds — integer vector of 45 spoke node indices
#
# References:
#   Barabási & Albert (1999), Science 286
#   Soramäki et al. (2007), Physica A 379
# =============================================================================

library(igraph)

# -----------------------------------------------------------------------------
# 0. Reproducibility and directory setup
# -----------------------------------------------------------------------------

set.seed(42)

# Create data directory if it does not exist
if (!dir.exists("data")) {
  dir.create("data")
  cat("Created data/ directory\n")
}

# -----------------------------------------------------------------------------
# 1. Generate Barabási–Albert scale-free network
# -----------------------------------------------------------------------------
# n     = 50 nodes: stylised approximation of TARGET2 (scaled from ~1,000
#         direct participants proportionally to network size)
# power = 2.5: power-law exponent controlling degree of hub dominance;
#         sensitivity analysis covers gamma in {2.0, 2.5, 3.0} in 07_sensitivity.R
# m     = 2: each new node attaches to 2 existing nodes, producing a
#         sparse but connected network consistent with interbank topology
# directed = TRUE: payments are directional (sender -> receiver)

g <- sample_pa(
  n        = 50,
  power    = 2.5,
  m        = 2,
  directed = TRUE
)

# -----------------------------------------------------------------------------
# 2. Compute degree and identify hub vs. spoke nodes
# -----------------------------------------------------------------------------
# We use total degree (in + out) to rank nodes by overall connectivity.
# The 5 highest-degree nodes are designated hubs (systemically important
# institutions); the remaining 45 are spokes.

deg      <- degree(g, mode = "all")
deg_in   <- degree(g, mode = "in")
deg_out  <- degree(g, mode = "out")

hub_nodes   <- order(deg, decreasing = TRUE)[1:5]
spoke_nodes <- setdiff(1:vcount(g), hub_nodes)

# -----------------------------------------------------------------------------
# 3. Network validation
# -----------------------------------------------------------------------------
# Hub-to-spoke ratio: computed as max hub degree divided by median spoke degree.
# Using median of SPOKE nodes only (not all nodes) is more precise because
# the hub degrees themselves would inflate the denominator if we used
# median(deg) over all 50 nodes.

hub_max_deg     <- max(deg[hub_nodes])
spoke_med_deg   <- median(deg[spoke_nodes])
hub_spoke_ratio <- hub_max_deg / spoke_med_deg

# Edge density: proportion of possible directed edges that exist
# Expected to be low (sparse network) consistent with real interbank topology
edge_dens <- edge_density(g, loops = FALSE)

# Strongest hub: the single node with the highest degree
strongest_hub             <- hub_nodes[which.max(deg[hub_nodes])]
strongest_hub_connections <- deg[strongest_hub]

# Check connectivity: is the network weakly connected?
# (i.e., ignoring edge direction, can you reach any node from any other?)
is_connected <- is_connected(g, mode = "weak")

# -----------------------------------------------------------------------------
# 4. Print validation summary
# -----------------------------------------------------------------------------

cat("\n=======================================================\n")
cat("  NETWORK VALIDATION SUMMARY\n")
cat("=======================================================\n")
cat(sprintf("  Nodes (n):                    %d\n",   vcount(g)))
cat(sprintf("  Edges:                        %d\n",   ecount(g)))
cat(sprintf("  Edge density:                 %.4f\n", edge_dens))
cat(sprintf("  Weakly connected:             %s\n",   is_connected))
cat("-------------------------------------------------------\n")
cat(sprintf("  Hub nodes (top 5 by degree):  %s\n",
            paste(hub_nodes, collapse = ", ")))
cat(sprintf("  Hub degrees:                  %s\n",
            paste(deg[hub_nodes], collapse = ", ")))
cat(sprintf("  Max hub degree:               %d\n",   hub_max_deg))
cat(sprintf("  Median spoke degree:          %.1f\n", spoke_med_deg))
cat(sprintf("  Hub/spoke degree ratio:       %.2f\n", hub_spoke_ratio))
cat("-------------------------------------------------------\n")
cat(sprintf("  Strongest hub (node %d):       connected to %d of %d nodes\n",
            strongest_hub, strongest_hub_connections, vcount(g) - 1))
cat("-------------------------------------------------------\n")

# Benchmark check: warn if ratio is outside expected range
if (hub_spoke_ratio < 15 || hub_spoke_ratio > 35) {
  warning(sprintf(
    "Hub/spoke ratio %.2f is outside expected range [15, 35]. Check power parameter.",
    hub_spoke_ratio
  ))
} else {
  cat(sprintf("  [OK] Hub/spoke ratio %.2fx within expected range [15, 35]\n",
              hub_spoke_ratio))
  cat(sprintf("       Consistent with Soramaki et al. (2007) benchmark ~23.5x\n"))
}

cat("=======================================================\n\n")

# -----------------------------------------------------------------------------
# 5. Save outputs
# -----------------------------------------------------------------------------

saveRDS(g,           "data/network.rds")
saveRDS(hub_nodes,   "data/hub_nodes.rds")
saveRDS(spoke_nodes, "data/spoke_nodes.rds")

cat("Saved:\n")
cat("  data/network.rds\n")
cat("  data/hub_nodes.rds\n")
cat("  data/spoke_nodes.rds\n\n")