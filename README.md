# Financial Strength and the Distress Anomaly: Altman Z-Score vs Piotroski F-Score

**Authors:** Mirko Balli, Lorenzo Attanasio  
**Institution:** WU Vienna (Asset & Risk Management I)

An empirical cross-sectional analysis of financial-quality factors using the [Global Factor Data](https://jkpfactors.com) universe (Jensen, Kelly & Pedersen, 2023). This project investigates the "distress anomaly" by contrasting two fundamental lenses of corporate health: the **Altman Z-score** (distance to default, representing the *level* of safety) and the **Piotroski F-score** (fundamental momentum, representing the *direction* of improvement).

## Executive Summary & Investment Thesis

Contrary to the classical risk-reward tradeoff, the market does not systematically reward distress risk. Our analysis confirms the distress anomaly and highlights the superiority of fundamental improvement over static safety:

1. **The Distress Anomaly in Action:** The Z-Score premium is statistically indistinguishable from zero ($t = 0.21$). Going long safe firms and short distressed firms yields no excess return, as both legs perform identically over the long term. 
2. **Direction Beats Level:** The F-Score delivers a robust, defensive premium ($t = 3.28$, clearing the Harvey-Liu-Zhu threshold of $t>3.0$). However, the two factors are negatively correlated ($\rho = -0.36$), indicating they capture opposing dimensions of "quality".
3. **Alpha Decay & Spanning:** Post-publication (McLean & Pontiff, 2016 framework), the F-Score's efficacy decays significantly (t-stat drops from 4.48 to 1.18). Furthermore, spanning regressions and the Gibbons-Ross-Shanken (GRS) joint test reveal that the F-Score's premium is fully subsumed by the profitability (ROE) factor in the Hou et al. (2021) **q5 model**.
4. **Regime Behavior:** Both factor premia are highly time-varying. Overlays with ICE BofA US High Yield OAS spreads and NBER recessions show long flat or negative stretches, emphasizing the need for regime-conditional risk management.

## Methodology & Tech Stack

This codebase is built for computational efficiency and statistical rigor, suitable for processing large cross-sectional datasets.

* **Data Processing:** Fully vectorized data wrangling and rolling-window aggregations utilizing R's `data.table` and `zoo`.
* **Robust Econometrics:** Performance metrics and spanning regressions are evaluated using Newey-West HAC (Heteroskedasticity and Autocorrelation Consistent) standard errors to correct for overlapping observations and serial correlation.
* **Avoidance of Look-Ahead Bias:** In the supplementary Risk-Parity portfolio construction, rolling volatility weights are explicitly lagged by one month (`t-1`) to ensure strictly out-of-sample capital allocation.

##  Repository Structure

    .
    ├── README.md
    ├── 25_code.R                   # Core research script (data ingestion, regressions, visualization)
    ├── 25_data.RData               # Pre-processed and merged dataset (generated dynamically)
    ├── data/                       # Raw input data directory (see Data Governance note below)
    ├── docs/                       # Assignment instructions 
    ├── plots/                      # Output directory for rolling metrics, cumulative returns, and regime overlays
    └── slides/                     # Executive presentation deck (.pdf and .pptx)

## Data Governance & Compliance Policy

To strictly comply with vendor data redistribution policies (specifically Bloomberg L.P. Terms of Service), the raw benchmark dataset (`bloomberg_benchmarks.xlsx`) containing proprietary Total Return and OAS spread indices has been omitted from this public repository. 

The `data/` folder contains only publicly available factor data from:
* **Global Factor Data:** JKP Factor and Portfolio returns (jkpfactors.com)
* **global-q.org:** Hou et al. (2021) q5 factors
* **Ken French Data Library:** FF3, FF5, and Momentum factors (downloaded dynamically via API in the script)
* **FRED:** Macro regime indicators (NFCI, USRECD, VIX, Term Spread)

*Note: Running `25_code.R` without the Bloomberg file will bypass the benchmark comparison section (Table 5 & 6, Fig 7) unless a dummy dataset of identical structure is provided.*

## How to Run (Reproducibility)

**Prerequisites:** R ($\ge$ 4.0) and an active internet connection (for dynamic Ken French data fetching). Missing CRAN packages (`data.table`, `zoo`, `ggplot2`, `sandwich`, `lmtest`, `readxl`, `scales`) will auto-install on the first run.

1. Clone the repository and set your R working directory to the repository root.
2. Execute `25_code.R`.
3. The script features an intelligent caching mechanism:
   * **First Run:** It ingests raw CSVs/ZIPs from `data/`, downloads external FF data, executes the full econometric pipeline, outputs charts to `plots/`, and caches the master dataset as `25_data.RData`.
   * **Subsequent Runs:** It detects `25_data.RData`, bypassing the raw data processing phase and jumping directly to the analysis and visualization generation.
