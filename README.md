# Payment-System-Efficiency-Simulation

Reengineering Payment System Efficiency: A Simulation-Based Analysis

Author: Lorenzo Gerundo

Master’s Thesis | MSc Economics: Financial Economics | Tilburg University (2026)

Project Overview

This repository contains the complete R simulation framework developed for my
Master's Thesis. The study investigates the economic conditions under which
blockchain-based settlement mechanisms (specifically the XRP Ledger) improve
efficiency and risk allocation relative to traditional Financial Market
Infrastructures (FMIs).

Using a Discrete-Time Agent-Based Model, the code compares three
settlement architectures within a unified Total Cost of Settlement (TCS)
function:

1.  RTGS: Real-Time Gross Settlement (modeled after TARGET2).
2.  DNS: Deferred Net Settlement (modeled after EURO1).
3.  XRPL: Blockchain-based Atomic Settlement (XRP Ledger).

Key Research Contributions

  - Efficiency Frontier Analysis: Demonstrates how atomic settlement shifts the
    latency-liquidity trade-off plane.
  - Exposure Amplification Factor (EAF): A novel metric introduced to quantify
    the contagion containment benefits of different architectures during a
    hub-node default.
  - Cross-Border Extension: Comparative analysis of Herstatt Risk and "Trapped
    Liquidity" between Correspondent Banking, CLS, and XRPL.

Repository Structure

The simulation is modularized into seven primary scripts to ensure
reproducibility:

  - 01_network.R: Constructs the scale-free Barabási–Albert topology (50
    nodes: 5 hubs, 45 spokes) and validates the hub-to-spoke degree ratio.
  - 02_rtgs.R: Implements the RTGS baseline, including FIFO queuing, liquidity
    recycling delays, and 2% collateral haircuts.
  - 03_dns.R: Simulates the 2-hour netting cycles and calculates the 1.6% credit
    risk premium on residual net exposures (Basel II).
  - 04_xrpl.R: Models atomic settlement, price slippage functions, and the
    volatility buffer opportunity cost.
  - 05_stress.R: Executes the Hub-Default stress test to calculate EAF and
    secondary systemic exposure.
  - 06_crossborder.R: Models FX settlement gaps and the MD* (Pool Depth
    Threshold) for on-demand liquidity.
  - 07_sensitivity_clean.R: Runs the 64-parameter combination sweep to test the
    robustness of the qualitative cost rankings.

Data & Calibration

  - Network: Scaled 1:20 from TARGET2 benchmarks.
  - Payment Arrivals: Modeled via a memoryless Poisson process
    (\lambda = 17,500).
  - Currency Data: Realized XRP/EUR exchange rates and volatility derived from
    CoinGecko (2024–2026).

How to Run

1.  Ensure you have R version 4.0+ installed.
2.  Install required packages: install.packages(c("igraph", "ggplot2", "dplyr",
    "scales")).
3.  Place the provided Daily_avg_XRP_volatility_2_years.xlsx in the root
    directory.
4.  Run the scripts in numerical order to replicate the findings presented in
    the thesis.

Citation

If you use this framework or the EAF metric in your research, please cite:

Gerundo, L. (2026). Reengineering Payment System Efficiency: A Simulation-Based
Analysis of RTGS, DNS, and Blockchain-Based Atomic Settlement. Tilburg
University.

Disclaimer: This code was developed for academic research purposes. No
generative AI tools were used in the writing of the logic or scripts beyond
standard syntax debugging.


File Descriptions & Simulation Logic

The simulation is designed to be run sequentially. Each script handles a
specific component of the "Warehouse of Payments" framework as extended in the
thesis.

01_network.R — Network Construction and Validation

  - Purpose: Generates the interbank payment topology.
  - Logic: Implements a scale-free Barabási–Albert network (n = 50, m = 2).
  - Output: Creates a hub-and-spoke structure where 5 hub nodes intermediate the
    majority of payment flows, achieving a hub-to-spoke degree ratio of 23.5,
    consistent with empirical TARGET2/Fedwire studies.

02_rtgs.R — RTGS Baseline Simulation

  - Purpose: Models the Real-Time Gross Settlement scenario (TARGET2 proxy).
  - Logic: Simulates immediate settlement using FIFO queuing and a 2% collateral
    haircut on intraday credit.
  - Key Metric: Calculates the opportunity cost of total pre-funded liquidity
    (L_{RTGS}) and settlement latency.

03_dns.R — DNS Baseline Simulation

  - Purpose: Models the Deferred Net Settlement scenario (EURO1 proxy).
  - Logic: Implements 2-hour multilateral netting cycles (\tau = 8 steps).
  - Regulatory Calibration: Applies a 1.6% credit risk premium (Basel II
    Standardised Approach) to residual net exposures during the settlement gap.
    It validates the Liquidity Saving Ratio (LSR) against the 82% benchmark.

04_xrpl.R — XRPL Atomic Settlement Simulation

  - Purpose: Models the blockchain-based atomic settlement scenario.
  - Logic: Implements On-Demand Liquidity (ODL) logic. It replaces pre-funding
    with a Volatility Buffer (L_{XRPL}) and a non-linear Slippage Function based
    on pool depth (MD) and transaction size, calibrated to recent empirical DEX
    data.

05_stress.R — Stress Testing and Contagion Analysis

  - Purpose: Quantifies systemic risk during a financial crisis.
  - Logic: Triggers an exogenous default of the highest-degree hub node at peak
    intraday volume (t=16).
  - Contribution: Computes the Exposure Amplification Factor (EAF) to measure
    how settlement architecture transforms idiosyncratic failures into systemic
    events.

06_crossborder.R — Cross-Border Extension

  - Purpose: Compares traditional Correspondent Banking and CLS against XRPL.
  - Logic: Models Herstatt Risk as an expected loss function of the settlement
    gap (\Delta t).
  - Key Metric: Identifies the Pool Depth Threshold (MD^*) at which on-demand
    liquidity becomes more capital-efficient than maintaining pre-funded nostro
    balances.

07_sensitivity_clean.R — Sensitivity Analysis and Efficiency Frontier

  - Purpose: Tests the robustness of the thesis findings.
  - Logic: Runs a 64-parameter combination sweep across interest rates (r),
    volumes (\lambda), and slippage coefficients (\kappa, \delta).
  - Output: Generates the Efficiency Frontier (Latency–Liquidity plane) and
    identifies the threshold interest rate r^* at which XRPL becomes the
    dominant architecture.

