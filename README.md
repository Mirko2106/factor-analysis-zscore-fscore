# Global Factor Data Analysis: Altman Z-score & Piotroski F-score

**Course:** 5376 Asset/Risk Management I
**Authors:** Mirko Balli, Lorenzo Attanasio

Factor performance analysis of two financial-quality factors from the
[Global Factor Data](https://jkpfactors.com) repository (Jensen, Kelly &
Pedersen, 2023): the **Altman Z-score** (Altman, 1968) and the
**Piotroski F-score** (Piotroski, 2000).

---

## What the project does

1. **Descriptive stats** — risk/return, Newey-West t-stats, correlations.
2. **Long vs short leg decomposition** — where the factor premium comes from.
3. **Rolling-window analysis** — Sharpe, CAPM beta/alpha, rolling correlation,
   regime overlay (HY OAS spread + NBER recessions).
4. **Pre/post publication** — McLean & Pontiff (2016) decay framework.
5. **Spanning regressions** — CAPM, Fama-French 3, Hou et al. (2021) q5,
   plus FF5/FF6 as alternatives, with the GRS joint test.
6. **Supplementary** — combined factor (50/50 + risk-parity), benchmark and
   alternative-asset comparison.

---

## Repository structure

```
.
├── README.md
├── .gitignore
├── ARM_Assignment.Rproj        # open this in RStudio (sets the working dir)
├── ARM_ZScore_FScore.R                    # the full analysis script
├── data/                       # raw input data (read by the script)
│   ├── [usa]_[z_score]_[monthly]_[vw_cap]_[factor].zip
│   ├── [usa]_[f_score]_[monthly]_[vw_cap]_[factor].zip
│   ├── [usa]_[z_score]_[monthly]_[vw_cap]_[portfolio].zip
│   ├── [usa]_[f_score]_[monthly]_[vw_cap]_[portfolio].zip
│   ├── [usa]_[mkt]_[monthly]_[vw]_[factor].zip
│   ├── [usa]_[mkt]_[monthly]_[ew]_[factor].zip
│   ├── [usa]_[quality]_[monthly]_[vw_cap]_[factor].zip
│   ├── q5_factors_monthly_2024.csv
│   ├── Copia di SPX.xlsx        # Bloomberg export (PRIVATE — do not share)
│   ├── NFCI.csv
│   ├── T10Y2Y.csv
│   ├── USRECD.csv
│   └── VIXCLS.csv
├── docs/                       # guide / reference material
└── slides/                     # final presentation (.pdf)
```

Generated automatically by the script and **not tracked** by git:
`groupXX_data.RData` (the merged dataset) and the `plots/` folder.

---

## How to run

**Prerequisites:** R (>= 4.0) and an internet connection on the first run
(the script downloads Fama-French factors from the Ken French Data Library).

1. Clone the repo and open **`ARM_Assignment.Rproj`** in RStudio.
   This sets the working directory to the repo root, so the relative paths
   (`data/`, `plots/`) work automatically — no `setwd()` needed.
2. Open `ARM_ZScore_FScore.R` and run it (Source, or Ctrl+Shift+Enter).
3. On the first run the script reads `data/`, downloads the FF factors, builds
   `groupXX_data.RData`, and produces all tables (console) and figures
   (`plots/`). Later runs load the `.RData` directly and skip the slow part.

The required CRAN packages auto-install if missing:
`data.table, zoo, ggplot2, sandwich, lmtest, readxl, scales`.

---

## Data sources

| Data | Source |
|------|--------|
| Factor & portfolio returns | jkpfactors.com (Global Factor Data) |
| q5 factors (Hou et al. 2021) | global-q.org |
| FF3 / FF5 / Momentum | Ken French Data Library (downloaded in-script) |
| Equity / bond / commodity / HY OAS | Bloomberg terminal |
| Macro regime variables | FRED (St. Louis Fed) |

---

## Key findings (short)

- **Z-score has no premium** (t ≈ 0.2): safe and distressed firms performed
  alike — the distress anomaly.
- **F-score works** (t ≈ 3.3) but its premium is fully explained by the q5
  ROE/profitability factor (alpha vanishes under q5; GRS not rejected only for q5).
- The two factors are **negatively correlated** (level vs direction of quality).

---

## Note

This repository is **private**. The Bloomberg data in `data/Copia di SPX.xlsx`
is licensed and must not be redistributed publicly.
