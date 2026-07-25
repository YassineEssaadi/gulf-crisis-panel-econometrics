# =============================================================================
# Project: The Geopolitics of Oil — Assessing the Macroeconomic Impact
#          of the 2026 Gulf Crisis
# Panel Data Econometrics: Oil Price Pass-Through to Inflation
# =============================================================================
# HYBRID VERSION — Merges Version A (6-country structure, full diagnostics,
#   supplementary OLS, cross-group comparison) with Version B (clean data
#   scoping, reproducible code, IPS unit root tests, sink output).
#
# MAIN PANEL:  France, Germany, Italy (Advanced Net-Importers)
# SUPPLEMENTARY: United States (Advanced Net-Exporter),
#                Saudi Arabia (Emerging Net-Exporter),
#                Turkey (Emerging Net-Importer)
# Time Frame: January 2023 – June 2026 (observed through March 2026;
#             Apr–Jun 2026 forecasted via ARIMA — exactly 3 months)
# Crisis Date: February 27, 2026
# post_crisis dummy: 1 if date >= 2026-03-01 (March 2026 onward)
#
# Per professor's guidance (May 2026): The core panel dataset must come from
# ONE predefined group to ensure comparability of macroeconomic structures
# and policy responses. Mixing countries from different groups introduces
# confounding variables (exchange rate regimes, capital mobility, income
# elasticities) that undermine scientific validity. Cross-group comparisons
# are conducted via separate analyses and discussed qualitatively.
# =============================================================================
# IMPORTANT: Do NOT load plyr — it conflicts with dplyr.
#            This script uses dplyr exclusively with explicit namespacing.
# =============================================================================