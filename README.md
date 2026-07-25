# The Geopolitics of Oil: Assessing the Macroeconomic Impact of the 2026 Gulf Crisis

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Econometrics](https://img.shields.io/badge/Econometrics-Panel_Data-blue?style=for-the-badge)
![Time Series](https://img.shields.io/badge/Time_Series-ARIMA-green?style=for-the-badge)

## 📌 Project Overview
This project investigates the macroeconomic transmission of the **February 2026 Gulf Crisis** (a major geopolitical shock causing a 25% spike in Brent crude prices) on three advanced net-oil-importing economies: **France, Germany, and Italy (EU3)**. 

Using a **Panel Data Econometrics** approach combined with **ARIMA forecasting**, the study isolates the "crisis effect" from underlying economic trends, focusing on how oil price shocks pass through to inflation (CPI) and industrial production (IPI), and how monetary policy (interest rates) responds.

## 🎯 Key Objectives
1. **Quantify the Oil Pass-Through:** Measure how the 25% Brent price shock translates into domestic inflation across the EU3.
2. **Assess Industrial Impact:** Evaluate the contraction in Industrial Production Indices (IPI) due to higher energy input costs.
3. **Evaluate Policy Response:** Analyze the European Central Bank's (ECB) monetary policy reaction function to imported inflation.
4. **ARIMA Forecasting:** To generate the non-crisis counterfactual baseline for Q2 2026.

## 📈 Data & Methodology

### Dataset
- **Scope:** France, Germany, Italy (Main Panel) + US, Saudi Arabia, Turkey (Supplementary Cross-Group Comparison).
- **Timeframe:** January 2023 – June 2026 (42 months).
- **Frequency:** Monthly.
- **Variables:**
  - `cpi`: Consumer Price Index (Inflation proxy)
  - `ipi`: Industrial Production Index (Growth proxy)
  - `brent`: Brent Crude Oil Price (USD/barrel)
  - `policy_rate`: Central Bank Policy Rates (%)
  - `neer`: Nominal Effective Exchange Rate
  - `vix`: Volatility Index (Global uncertainty proxy)

### Econometric Models
1. **Panel Unit Root Tests:** Im-Pesaran-Shin (IPS) tests to check for stationarity.
2. **Fixed Effects (FE) & Random Effects (RE) Models:** To control for unobserved country-specific heterogeneity.
3. **Hausman Test:** To select the appropriate panel specification (FE vs. RE).
4. **ARIMA Forecasting:** To generate the non-crisis counterfactual baseline for Q2 2026.

## 🚀 Key Findings
- **Strong Oil-to-Inflation Link:** The Fixed Effects model reveals a highly significant positive relationship between Brent prices and CPI (R² = 0.774).
- **Exchange Rate Buffer:** The Nominal Effective Exchange Rate (NEER) acts as a crucial shock absorber (β = 0.908, p < 0.01).
- **Industrial Contraction:** A statistically significant negative impact of oil prices on IPI was observed, confirming the cost-push shock on manufacturing.
- **Policy Dilemma:** The models highlight the central bank's trade-off between fighting imported inflation and supporting contracting industrial output.

## 📂 Repository Structure
```text
├── data/
│   ├── main_panel_full.csv         # EU3 dataset (France, Germany, Italy)
│   └── panel_data_6country.csv     # Supplementary dataset (EU3 + US, SA, TR)
├── scripts/
│   └── eu3_analysis_hybrid_FINAL.R # Main R script (Panel regressions, ARIMA, plots)
├── report/
│   ├── EU3_Methodology_Report_FINAL.docx # Detailed methodology document
│   └── README.md                  # Link to download the full PDF report
├── README.md                       # Project overview
└── LICENSE                         # MIT License
```

## 🛠️ How to Run the Analysis
1. Clone the repository:
   ```bash
   git clone https://github.com/YassineEssaadi/gulf-crisis-panel-econometrics.git
   ```
2. Open `scripts/eu3_analysis_hybrid_FINAL.R` in RStudio.
3. Ensure the required packages are installed (`dplyr`, `plm`, `forecast`, `ggplot2`, `stargazer`, etc.). *Note: Do not load `plyr` as it conflicts with `dplyr`.*
4. Set your working directory to the project root.
5. Run the script to reproduce the panel regressions, ARIMA forecasts, and generate the 23 analytical figures.

## 📄 Full Report
The comprehensive 25-page econometric report containing all theoretical frameworks, regression tables, and 23 figures is available in the report folder.

