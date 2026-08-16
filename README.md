# Payment-System-Efficiency-Simulation

**Reengineering Payment System Efficiency: A Simulation-Based Comparison of Traditional, Hybrid, and Atomic Settlement Architectures**
*An XRP Ledger Calibration under the Warehouse of Payments Framework*

Author: Lorenzo Gerundo

Master's Thesis | MSc Economics: Financial Economics | Tilburg University (June 2026)
Supervisor: G. Ourens | Student number (SNR): 2151402

## Project Overview

This repository contains the complete R simulation framework developed for my
Master's Thesis. The study investigates the economic conditions under which
blockchain-based settlement mechanisms (specifically the XRP Ledger) improve
efficiency and risk allocation relative to traditional Financial Market
Infrastructures (FMIs).

Using a Discrete-Time Agent-Based Model, the code compares **five settlement
configurations** within a unified Total Cost of Settlement (TCS) function:

1.  **RTGS**: Real-Time Gross Settlement (modeled after TARGET2).
2.  **DNS**: Deferred Net Settlement (modeled after EURO1).
3.  **Hybrid RTGS–LSM**: a high-frequency multilateral-offsetting benchmark
    that occupies the interior of the latency–liquidity plane between RTGS
    and DNS.
4.  **wCBDC**: a permissioned wholesale CBDC control, used to isolate the
    "atomic-settlement effect" from the "public-blockchain effect."
5.  **XRPL**: Blockchain-based Atomic Settlement (XRP Ledger), reported under
    two liquidity concepts — a gross-capital (primary) specification and a
    risk-adjusted volatility-buffer (alternative) specification.

## Key Research Contributions

  - **Efficiency Frontier Analysis**: Demonstrates that atomic settlement
    relocates the latency frontier unconditionally, but only shifts the
    liquidity frontier under the risk-adjusted volatility-buffer concept.
  - **Exposure Amplification Factor (EAF)**: A novel metric introduced to
    quantify the contagion containment benefit of each architecture during a
    hub-node default, computed here across all five configurations.
  - **Atomic vs. Public-Blockchain Decomposition**: The wCBDC control isolates
    which of XRPL's benefits (latency, contagion containment, Herstatt-risk
    elimination) stem from atomic settlement itself versus the public
    XRP Ledger implementation (slippage, price/volatility exposure).
  - **Cross-Border Extension**: Comparative analysis of Herstatt Risk and
    "Trapped Liquidity" across Correspondent Banking, CLS, XRPL, and wCBDC,
    including the Pool Depth Threshold (MD\*).

## Repository Structure

The simulation is modularized into nine primary scripts plus one supporting
data file, to ensure reproducibility:

  - `01_network.R`: Constructs the scale-free Barabási–Albert topology (50
    nodes: 5 hubs, 45 spokes) and validates the hub-to-spoke degree ratio
    against the empirical benchmark (~23.5x, expected range [15, 35]).
  - `02_rtgs.R`: Implements the RTGS baseline, including FIFO queuing, a
    partial liquidity-recycling delay (0.421), and a 2% collateral haircut.
  - `03_dns.R`: Simulates the 2-hour netting cycles and calculates the 1.6%
    credit risk premium on residual net exposures (Basel II).
  - `04_xrpl.R`: Models atomic settlement, the price slippage function, and
    reports **both** the gross-capital liquidity requirement and the
    volatility-buffer (risk-adjusted) liquidity requirement.
  - `05_stress.R`: Executes the Hub-Default stress test and computes the
    Exposure Amplification Factor (EAF) and affected-node counts for all
    five architectures (RTGS, DNS, Hybrid, wCBDC, XRPL).
  - `06_crossborder.R`: Models Herstatt risk and trapped/gross-pool liquidity
    across Correspondent Banking, CLS, XRPL, and wCBDC, and derives the pool
    depth threshold MD\*.
  - `07_wcbdc.R`: Implements the permissioned wholesale CBDC control (atomic
    finality, gross corridor pool, no slippage, no volatility buffer) and
    decomposes XRPL's total cost into an atomic-settlement effect and a
    public-blockchain effect.
  - `07b_hybrid.R`: Implements the hybrid RTGS–LSM benchmark using a 1-hour
    (4-step) multilateral-offsetting cycle, positioned between RTGS and DNS.
  - `08_sensitivity.R`: Runs the full sensitivity sweep (interest rate,
    volume, delay-cost parameters, slippage parameters, and network
    power-law exponent) across all five architectures under both the
    gross-capital and volatility-buffer specifications, and generates the
    efficiency-frontier plot.

## Data & Calibration

  - **Network**: Scaled 1:20 from TARGET2 benchmarks.
  - **Payment Arrivals**: Modeled via a memoryless Poisson process
    (λ = 17,500 baseline daily payments; sensitivity range 10,000–100,000).
  - **Currency Data**: Realized XRP/EUR exchange rates and volatility derived
    from CoinGecko (April 2024 – April 2026).

## How to Run

1.  Ensure you have R version 4.0+ installed.
2.  Install required packages: `install.packages(c("igraph", "ggplot2", "dplyr", "scales"))`.
3.  Place the provided `Daily_avg_XRP_volatility_2_years.xlsx` in the root
    directory.
4.  Run the scripts in numerical order (`01` → `08`, with `07b_hybrid.R`
    after `07_wcbdc.R`) to replicate the findings presented in the thesis.

## Citation

If you use this framework or the EAF metric in your research, please cite:

> Gerundo, L. (2026). *Reengineering Payment System Efficiency: A
> Simulation-Based Comparison of Traditional, Hybrid, and Atomic Settlement
> Architectures — An XRP Ledger Calibration under the Warehouse of Payments
> Framework.* Master's Thesis, Tilburg University.

**Disclaimer**: This code was developed for academic research purposes. No
generative AI tools were used in the writing of the logic or scripts beyond
standard syntax debugging.

---

## File Descriptions & Simulation Logic

The simulation is designed to be run sequentially. Each script handles a
specific component of the "Warehouse of Payments" framework as extended in
the thesis.

### `01_network.R` — Network Construction and Validation

  - **Purpose**: Generates the interbank payment topology.
  - **Logic**: Implements a scale-free Barabási–Albert network (n = 50,
    power/γ = 2.5, m = 2), directed to reflect sender → receiver flows.
  - **Output**: Creates a hub-and-spoke structure where 5 hub nodes
    intermediate the majority of payment flows. The hub-to-spoke degree
    ratio is validated against an expected range of [15, 35], consistent
    with the empirical ~23.5x benchmark from TARGET2/Fedwire studies.

### `02_rtgs.R` — RTGS Baseline Simulation

  - **Purpose**: Models the Real-Time Gross Settlement scenario (TARGET2
    proxy).
  - **Logic**: Simulates immediate settlement using FIFO queuing, a partial
    liquidity-recycling delay (0.421) representing collateral-mobilisation
    lag, and a 2% collateral haircut on intraday credit.
  - **Key Metric**: Calculates the opportunity cost of total pre-funded
    liquidity (L_RTGS, 30% of daily payment value) and settlement latency.

### `03_dns.R` — DNS Baseline Simulation

  - **Purpose**: Models the Deferred Net Settlement scenario (EURO1 proxy).
  - **Logic**: Implements 2-hour multilateral netting cycles (τ = 8 steps).
  - **Regulatory Calibration**: Applies a 1.6% credit risk premium (Basel II
    Standardised Approach: 20% risk weight × 8% capital ratio) to residual
    net exposures during the settlement gap. It validates the Liquidity
    Saving Ratio (LSR) against the 82% BoF-PSS2 benchmark.

### `04_xrpl.R` — XRPL Atomic Settlement Simulation

  - **Purpose**: Models the blockchain-based atomic settlement scenario.
  - **Logic**: Implements a stylised rollback probability (10⁻⁴ per
    transaction), a non-linear slippage function based on pool depth (MD)
    and transaction size, and reports two distinct liquidity concepts:
    - `L_gross = MD × P_XRP` — the full gross corridor pool (primary
      specification, symmetric with RTGS/DNS treatment of capital).
    - `L_vbuf = MD × σ_XRP × P_XRP` — the volatility buffer, i.e. capital at
      risk during the 3–5 second settlement window (risk-adjusted
      alternative).

### `05_stress.R` — Stress Testing and Contagion Analysis

  - **Purpose**: Quantifies systemic risk during a financial crisis, across
    **all five architectures**.
  - **Logic**: Triggers an exogenous default of the highest-degree hub node
    at peak intraday stress volume (λ = 50,000, t = 16).
  - **Contribution**: Computes the Exposure Amplification Factor (EAF) for
    RTGS, DNS, the hybrid, XRPL, and wCBDC to measure how each settlement
    architecture transforms an idiosyncratic failure into a systemic event,
    via 1,000 Monte Carlo runs.

### `06_crossborder.R` — Cross-Border Extension

  - **Purpose**: Compares Correspondent Banking, CLS, XRPL, and wCBDC.
  - **Logic**: Models Herstatt Risk as an expected-loss function of the
    settlement gap (Δt), via 100,000 Monte Carlo runs (required because
    default events are rare).
  - **Key Metric**: Derives the Pool Depth Threshold (MD\*) — the point at
    which the XRPL gross pool value equals correspondent nostro
    prefunding — and reports trapped-liquidity opportunity costs under both
    the gross-pool and volatility-buffer bases.

### `07_wcbdc.R` — Permissioned Wholesale CBDC Control

  - **Purpose**: Isolates the "atomic-settlement effect" from the
    "public-blockchain effect."
  - **Logic**: Models a wholesale CBDC that shares XRPL's atomic finality
    but settles in central bank money — no FX volatility buffer, no DEX
    slippage — with liquidity endowed at the same gross-pool value as
    XRPL, for a like-for-like comparison.
  - **Output**: Decomposes the RTGS/DNS/wCBDC/XRPL(gross)/XRPL(buffer) cost
    gap into the slippage-driven "public-blockchain" component and the
    buffer-accounting component.

### `07b_hybrid.R` — Hybrid RTGS–LSM Benchmark

  - **Purpose**: Models a Liquidity Saving Mechanism layered on RTGS.
  - **Logic**: Applies the same multilateral-netting mechanism as DNS but at
    a shorter 1-hour (4-step) offsetting cycle, recovering part of the DNS
    liquidity saving while cutting the settlement gap roughly in half.

### `08_sensitivity.R` — Sensitivity Analysis and Efficiency Frontier

  - **Purpose**: Tests the robustness of the thesis findings across all five
    architectures and both XRPL liquidity concepts.
  - **Logic**: Sweeps interest rate (r), transaction volume (λ), delay-cost
    parameters (α, β), slippage parameters (κ, δ), and the network power-law
    exponent (γ).
  - **Output**: Generates the Efficiency Frontier (Latency–Liquidity plane,
    `figures/efficiency_frontier.png`) and reports the gross-capital vs.
    volatility-buffer win-rate across all sensitivity cells.
