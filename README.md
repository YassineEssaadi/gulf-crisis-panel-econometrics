# 🛢️ Macroeconomic Impact of the 2026 Gulf Crisis
### A Panel Data Analysis of EU3 Countries (France, Germany, Italy)

[![R](https://img.shields.io/badge/Language-R-276DC3?style=flat-square&logo=r)](https://www.r-project.org/)
[![Panel Data](https://img.shields.io/badge/Method-Panel%20Data%20Econometrics-blue?style=flat-square)](https://en.wikipedia.org/wiki/Panel_data)
[![ARIMA](https://img.shields.io/badge/Forecasting-ARIMA-orange?style=flat-square)](https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average)
[![Academic](https://img.shields.io/badge/Institution-Tunis%20Business%20School-red?style=flat-square)](https://www.tbs.u-tunis.tn/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg?style=flat-square)](https://opensource.org/licenses/MIT)

> **Course:** Advanced Econometrics | **Institution:** Tunis Business School | **Date:** May 2026

---

## 📋 Table of Contents

- [Overview](#overview)
- [Research Questions](#research-questions)
- [Dataset](#dataset)
- [Methodology](#methodology)
- [Key Results](#key-results)
- [Repository Structure](#repository-structure)
- [How to Reproduce](#how-to-reproduce)
- [Authors](#authors)

---

## 📌 Overview

This project investigates the **macroeconomic transmission channels** of the 2026 Gulf Crisis on the three largest euro-area economies: **France, Germany, and Italy (EU3)**. The crisis, which erupted on **February 27, 2026**, caused Brent crude oil to surge from ~$75/barrel to over $100 within weeks — the most significant supply shock since the 1990–91 Gulf War.

Using a **balanced panel dataset of 126 observations** (3 countries × 42 months, January 2023 – June 2026), we disentangle the effects of oil-price shocks and exchange-rate movements on **inflation (CPI)** and **industrial production (IPI)** across the EU3.

The three post-crisis months (March–May 2026) are observed data; the subsequent three months (April–June 2026) are extended via **ARIMA forecasting**, with policy rates projected via **Last Observation Carry-Forward (LOCF)**.

---

## ❓ Research Questions

1. To what extent do **oil-price changes pass through to consumer prices** in the EU3, and does this differ from the exchange-rate channel?
2. How does the Gulf Crisis affect **industrial production**, and does seasonality play a confounding role?
3. Are there significant **cross-country differences** in transmission mechanisms? Do supplementary economies (US, Saudi Arabia, Turkey) exhibit qualitatively different patterns?

---

## 📊 Dataset

| Feature | Details |
|---|---|
| **Countries (Main Panel)** | France, Germany, Italy (EU3) |
| **Countries (Supplementary)** | USA, Saudi Arabia, Turkey |
| **Time Period** | January 2023 – June 2026 (42 months) |
| **Observations** | 126 (EU3 panel) / 252 (full dataset) |
| **Frequency** | Monthly |
| **Crisis Onset** | February 27, 2026 |

### Variables

| Variable | Transformation | Source | Description |
|---|---|---|---|
| `ln_cpi` | log(CPI) | Eurostat | Consumer Price Index (2023=100) |
| `ln_ipi` | log(IPI) | Eurostat | Industrial Production Index (2023=100) |
| `oil_pct_change` | % change | EIA | Month-on-month Brent crude change (USD/bbl) |
| `ln_neer` | log(NEER) | BIS | Nominal Effective Exchange Rate |
| `policy_rate` | Level (%) | ECB / FRED | ECB main refinancing rate |
| `vix` | Level | CBOE / FRED | Volatility Index (global risk sentiment) |
| `post_crisis` | Dummy | — | 1 if date ≥ 2026-03-01 |

**Data Sources:** OECD, Eurostat, EIA, BIS, FRED

---

## 🔬 Methodology

### Econometric Models

We estimate a **hybrid panel data framework** combining observed data with ARIMA-forecasted extensions:

```
y_it = α_i + β₁·oil_pct_change_t + β₂·ln_neer_it + β₃·policy_rate_t
       + β₄·vix_t + β₅·post_crisis_t + γ_m·month_m + ε_it
```

Where `α_i` captures country fixed effects and `γ_m` are month dummies.

### Estimators Used

| Estimator | Purpose |
|---|---|
| **Pooled OLS (POLS)** | Naive benchmark |
| **Fixed Effects (FE)** | Controls for unobserved time-invariant heterogeneity |
| **Random Effects (RE)** | Wallace-Hussain transform; efficiency comparison |
| **Two-Way Fixed Effects (TWFE)** | Controls for common time shocks |
| **FE + Month Dummies** | Captures strong seasonal patterns in IPI |

### Diagnostic Tests

- **Hausman Test** — FE vs. RE selection (CPI: χ² = 0.033, p = 1.000; IPI: χ² = 5.671, p = 0.340)
- **Panel Unit Root Tests** — Levin-Lin-Chu (LLC) and Im-Pesaran-Shin (IPS)
- **Robust Standard Errors** — Arellano HC3 and double-clustered (country × time) via `vcovDC`

### ARIMA Forecasting

- Variables forecasted: CPI, IPI, Brent, VIX, NEER (via `auto.arima()`)
- Policy rate: Last Observation Carry-Forward (LOCF) — step-function unsuitable for ARIMA

---

## 📈 Key Results

### Inflation Model (CPI) — R² = 0.774 (FE)

| Finding | Value | Significance |
|---|---|---|
| **Exchange-rate pass-through** (ln_neer) | **0.908** | p < 2e-16 ⭐⭐⭐ |
| Oil-price pass-through (oil_pct_change) | 2.58e-06 | p = 0.982 (insignificant) |
| VIX financial-conditions channel | 0.0003 | p < 2e-16 ⭐⭐⭐ |
| Post-crisis dummy | -0.006 | p = 0.372 (insignificant) |

> 💡 **Key insight:** A 1% appreciation of the NEER reduces log(CPI) by ~0.91%. Direct oil-to-inflation pass-through is statistically undetectable.

### Industrial Production Model (IPI) — R² = 0.649 (FE + Month Dummies)

| Finding | Value | Significance |
|---|---|---|
| **August shutdown dummy** | **-0.245** | p = 0.003 ⭐⭐⭐ |
| March dummy | +0.082 | p < 0.001 ⭐⭐⭐ |
| Simple FE (no month dummies) | R² = 0.071 | — |
| FE + Month Dummies | R² = 0.649 | — |

> 💡 **Key insight:** Seasonality (European summer shutdown) is the dominant driver of IPI variation — far more important than the crisis itself.

### Country-Level Heterogeneity (NEER Sensitivity)

| Country | NEER Coefficient | Interpretation |
|---|---|---|
| 🇩🇪 Germany | **1.14** | Most sensitive to euro appreciation |
| 🇫🇷 France | 1.02 | Intermediate |
| 🇮🇹 Italy | 0.62 | Least sensitive |

### ARIMA Forecast (April–June 2026)

- Brent crude stabilizes around **$103/barrel**
- CPI trend continues upward with no dramatic break
- IPI forecast reflects seasonal recovery post-August

---

## 📁 Repository Structure

```
gulf-crisis-panel-econometrics/
│
├── 📄 README.md
├── 📄 report/
│   └── econometrics_report.pdf          # Full academic report
│
├── 📊 data/
│   ├── raw/
│   │   ├── cpi_eu3.csv                  # CPI data (Eurostat)
│   │   ├── ipi_eu3.csv                  # IPI data (Eurostat)
│   │   ├── brent_oil.csv                # Brent crude (EIA)
│   │   ├── neer_bis.csv                 # NEER (BIS)
│   │   ├── ecb_policy_rate.csv          # ECB rate (FRED)
│   │   └── vix.csv                      # VIX (FRED)
│   └── processed/
│       ├── panel_eu3.csv                # Main balanced panel (126 obs)
│       └── panel_full.csv               # Full dataset (252 obs)
│
├── 📝 scripts/
│   ├── 01_data_collection.R             # Data download & merging
│   ├── 02_eda.R                         # Exploratory data analysis
│   ├── 03_unit_root_tests.R             # LLC and IPS tests
│   ├── 04_panel_regressions.R           # POLS, FE, RE, TWFE models
│   ├── 05_robust_se.R                   # Arellano HC3 & vcovDC
│   ├── 06_arima_forecasting.R           # ARIMA models & LOCF
│   ├── 07_country_heterogeneity.R       # Country-level OLS
│   └── 08_plots.R                       # All figures
│
├── 📊 figures/
│   ├── fig01_brent_oil_price.png
│   ├── fig02_log_cpi_faceted.png
│   ├── fig03_log_cpi_overlaid.png
│   ├── fig04_log_ipi_faceted.png
│   ├── fig05_ecb_policy_rate.png
│   ├── fig06_vix_index.png
│   ├── fig07_log_neer.png
│   ├── fig08_oil_pct_change.png
│   ├── fig09_correlation_matrix.png
│   ├── fig10_boxplot_cpi.png
│   ├── fig11_boxplot_ipi.png
│   ├── fig12_pre_post_crisis_cpi.png
│   ├── fig13_scatter_cpi_oil.png
│   └── fig18_coef_plot_fe_cpi.png
│
└── 📋 tables/
    ├── table1_variable_definitions.csv
    ├── table2_descriptive_stats.csv
    ├── table3_unit_root_tests.csv
    ├── table4_cpi_regression.csv
    └── table5_ipi_regression.csv
```

---

## ▶️ How to Reproduce

### Prerequisites

```r
# Install required R packages
install.packages(c(
  "plm",        # Panel data models
  "lmtest",     # Hypothesis testing
  "sandwich",   # Robust standard errors
  "forecast",   # ARIMA forecasting
  "ggplot2",    # Visualization
  "dplyr",      # Data manipulation
  "tidyr",      # Data reshaping
  "zoo",        # LOCF imputation
  "corrplot"    # Correlation matrix
))
```

### Run the Analysis

```bash
# Clone the repository
git clone https://github.com/YOUR_USERNAME/gulf-crisis-panel-econometrics.git
cd gulf-crisis-panel-econometrics

# Run scripts in order
Rscript scripts/01_data_collection.R
Rscript scripts/02_eda.R
Rscript scripts/03_unit_root_tests.R
Rscript scripts/04_panel_regressions.R
Rscript scripts/05_robust_se.R
Rscript scripts/06_arima_forecasting.R
Rscript scripts/07_country_heterogeneity.R
Rscript scripts/08_plots.R
```

---

## 👥 Authors

| Name | Contribution |
|---|---|
| **Yassine Essaadi** | Data collection, ARIMA forecasting, panel regressions, report writing |
| **Amine Bessaad** | Data cleaning, Fixed Effects models, robustness checks, report writing |
| **Atef Hammami** | EDA, diagnostic testing, country heterogeneity analysis, report writing |
| **Ayoub El Atri** | Literature review, TWFE models, visualization, report writing |

> All authors contributed equally to every stage of this project.

**Institution:** Tunis Business School  
**Course:** Advanced Econometrics  
**Academic Year:** 2025–2026

---

## 📚 References

- Hamilton, J.D. (1983). Oil and the macroeconomy since World War II. *Journal of Political Economy*, 91(2), 228–248.
- Hooker, M.A. (2002). Are oil shocks inflationary? *Journal of Money, Credit and Banking*, 34(2), 540–561.
- Kilian, L. (2008). The economic effects of energy price shocks. *Journal of Economic Literature*, 46(4), 871–909.
- Pesaran, M.H. (2006). Estimation and inference in large heterogeneous panels. *Econometrica*, 74(4), 967–1012.
- Baltagi, B.H. (2021). *Econometric Analysis of Panel Data* (6th ed.). Springer.

---

*This project was conducted as part of the Advanced Econometrics course at Tunis Business School (May 2026).*
