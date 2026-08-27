# =============================================================================
# 07_wcbdc.R
# Fourth architecture: permissioned wholesale CBDC (analytic control)
#
# Purpose:
#   Isolate the "atomic-settlement effect" (low latency, near-zero recognition
#   lag, contagion containment) from the "public-blockchain effect" (slippage,
#   XRP price/volatility exposure, validator/governance risk). wCBDC shares
#   XRPL's atomic settlement but is denominated in central bank money:
#     - liquidity = gross corridor pool MD * P_xrp, fully encumbered (CB money,
#       no market-making offset; treated symmetrically with RTGS collateral)
#     - NO volatility buffer (settlement currency carries no FX risk)
#     - NO slippage (atomic CB-money transfer; no DEX routing)
#     - latency ~ 0, recognition lag ~ 0 (atomic finality)
#
# Inputs : output/rtgs_results.rds, dns_results.rds, xrpl_results.rds
# Outputs: output/wcbdc_results.rds
# =============================================================================
set.seed(42)
if (!dir.exists("output")) stop("Run 02_rtgs.R first.")

rtgs <- readRDS("output/rtgs_results.rds")
dns  <- readRDS("output/dns_results.rds")
xrpl <- readRDS("output/xrpl_results.rds")

MD <- 1971853; P_xrp <- 1.16; sigma_xrp <- 0.05; r_rate <- 0.03
alpha <- 0.1; beta <- 1.8

# Atomic finality, central bank money
L_wcbdc   <- MD * P_xrp                  # gross pool, fully encumbered (no buffer)
D_wcbdc   <- mean(runif(1e5, 3, 5)) / 3600
phi_wcbdc <- alpha * (D_wcbdc ^ beta)
slip_wcbdc<- 0                           # no DEX routing
TCS_wcbdc <- L_wcbdc * r_rate + slip_wcbdc + phi_wcbdc

# XRPL comparators
L_xrpl_gross <- MD * P_xrp
L_xrpl_vbuf  <- MD * sigma_xrp * P_xrp
slip_xrpl    <- xrpl$slippage
TCS_xrpl_gross <- L_xrpl_gross * r_rate + slip_xrpl + phi_wcbdc
TCS_xrpl_vbuf  <- L_xrpl_vbuf  * r_rate + slip_xrpl + phi_wcbdc

cat("================ FOUR-ARCHITECTURE COMPARISON (r=3%) ================\n")
cat(sprintf("  RTGS              TCS = EUR %s\n", format(round(rtgs$TCS), big.mark=",")))
cat(sprintf("  DNS               TCS = EUR %s\n", format(round(dns$TCS),  big.mark=",")))
cat(sprintf("  wCBDC (gross)     TCS = EUR %s\n", format(round(TCS_wcbdc), big.mark=",")))
cat(sprintf("  XRPL  (gross)     TCS = EUR %s\n", format(round(TCS_xrpl_gross), big.mark=",")))
cat(sprintf("  XRPL  (vol-buffer)TCS = EUR %s\n", format(round(TCS_xrpl_vbuf),  big.mark=",")))
cat("--------------------------------------------------------------------\n")
cat(sprintf("  Public-blockchain effect (XRPL_gross - wCBDC) = EUR %s  [slippage]\n",
            format(round(TCS_xrpl_gross - TCS_wcbdc), big.mark=",")))
cat(sprintf("  Vol-buffer accounting gap (wCBDC - XRPL_vbuf) = EUR %s\n",
            format(round(TCS_wcbdc - TCS_xrpl_vbuf), big.mark=",")))
cat("====================================================================\n")

wcbdc_results <- list(L = L_wcbdc, D = D_wcbdc, slippage = slip_wcbdc,
                      TCS = TCS_wcbdc, EAF = 5/28800, herstatt_loss = 0)
saveRDS(wcbdc_results, "output/wcbdc_results.rds")