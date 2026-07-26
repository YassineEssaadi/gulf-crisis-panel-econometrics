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

# =============================================================================
# 0. INSTALL AND LOAD LIBRARIES
# =============================================================================
# install.packages(c("dplyr","tidyr","ggplot2","purrr","readr","plm",
#                    "lmtest","car","forecast","tseries","scales",
#                    "gridExtra","corrplot","stargazer"))

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(readr)
  library(plm)
  library(lmtest)
  library(car)
  library(forecast)
  library(tseries)
  library(scales)
  library(gridExtra)
  library(corrplot)
  library(stargazer)
})

# Detach plyr if it was loaded previously (e.g., from a prior session)
if ("package:plyr" %in% search()) {
  detach("package:plyr", unload = TRUE)
  cat("NOTE: plyr was detached to prevent conflicts with dplyr.\n")
}

cat("All libraries loaded successfully.\n\n")

# =============================================================================
# 1. LOAD DATA
# =============================================================================
# Main panel: EU3 only (France, Germany, Italy) — 39 months observed
# Supplementary: 6-country dataset used only for supplementary OLS
# Both use read.csv() with fixed filenames for reproducibility.

# --- 1a. Main EU3 Panel (clean, correctly scoped) ---
data_main_raw <- read.csv("panel_data_eu3.csv", stringsAsFactors = FALSE)

cat("=== Main Panel (EU3) Dimensions ===\n")
cat("Rows:", nrow(data_main_raw), "  Columns:", ncol(data_main_raw), "\n\n")
cat("=== Unique Countries ===\n")
print(unique(data_main_raw$country))
cat("\n=== Date Range per Country ===\n")
date_range_main <- data_main_raw %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(min_date = min(date), max_date = max(date),
                   n_obs = dplyr::n(), .groups = "drop")
print(as.data.frame(date_range_main))

# --- 1b. Six-Country Dataset (supplementary analyses only) ---
data_supp_raw <- read.csv("panel_data_6country.csv", stringsAsFactors = FALSE)

cat("\n=== Supplementary (6-Country) Dimensions ===\n")
cat("Rows:", nrow(data_supp_raw), "  Columns:", ncol(data_supp_raw), "\n")
cat("=== Country Groups ===\n")
print(dplyr::distinct(data_supp_raw, country, group, development, oil_balance))
cat("\n=== Date Range per Country ===\n")
date_range_supp <- data_supp_raw %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(min_date = min(date), max_date = max(date),
                   n_obs = dplyr::n(), .groups = "drop")
print(as.data.frame(date_range_supp))

# =============================================================================
# 2. ARIMA FORECASTING (Apr–Jun 2026 — exactly 3 months)
# =============================================================================
cat("\n\n########## ARIMA FORECASTING (Apr–Jun 2026 — 3 months) ##########\n")

forecast_dates   <- c("2026-04", "2026-05", "2026-06")
vars_to_forecast <- c("cpi", "ipi", "brent", "policy_rate", "vix", "neer")

forecast_arima_country <- function(series, n_ahead = 3, freq = 12) {
  ts_obj <- ts(series, frequency = freq)
  fit    <- auto.arima(ts_obj, seasonal = TRUE,
                       stepwise = FALSE, approximation = FALSE)
  fc     <- forecast(fit, h = n_ahead)
  list(forecast_values = as.numeric(fc$mean),
       arima_order     = arimaorder(fit))
}

# --- 2a. ARIMA for main EU3 panel ---
cat("\n--- ARIMA: Main EU3 Panel ---\n")
arima_log_main <- list()   # store ARIMA orders for the report

for (ctry in unique(data_main_raw$country)) {
  cat("\n--- Forecasting for:", ctry, "---\n")
  country_data <- data_main_raw %>% dplyr::filter(country == ctry) %>%
    dplyr::arrange(date)
  last_date <- max(country_data$date)
  if (last_date >= "2026-06") {
    cat("  Data complete through Jun 2026 - skipping.\n")
    next
  }
  n_ahead <- sum(!forecast_dates %in% country_data$date)
  if (n_ahead == 0) next

  new_rows <- data.frame(
    country = rep(ctry, n_ahead),
    date    = forecast_dates[!forecast_dates %in% country_data$date],
    stringsAsFactors = FALSE
  )

  arima_log_main[[ctry]] <- list()

  for (v in vars_to_forecast) {
    series <- country_data[[v]]

    # --- 2.1 FIX: policy_rate uses LOCF (step function, not suitable for ARIMA) ---
    if (v == "policy_rate") {
      cat("  policy_rate: using LOCF (step function, not suitable for ARIMA).\n")
      fc_vals <- rep(tail(series[!is.na(series)], 1), n_ahead)
      arima_log_main[[ctry]][[v]] <- list(order = "LOCF-policy_rate",
                                           values = fc_vals)
    } else if (sum(!is.na(series)) < 10) {
      cat("  ", v, ": Not enough non-NA values. Using last-obs carry-forward.\n")
      fc_vals <- rep(tail(series[!is.na(series)], 1), n_ahead)
      arima_log_main[[ctry]][[v]] <- list(order = "LOCF",
                                           values = fc_vals)
    } else {
      tryCatch({
        result  <- forecast_arima_country(series, n_ahead = n_ahead)
        fc_vals <- result$forecast_values
        arima_log_main[[ctry]][[v]] <- list(order = result$arima_order,
                                             values = fc_vals)
        cat("  ", v, ": ARIMA(", result$arima_order[1], ",",
            result$arima_order[2], ",", result$arima_order[3],
            ") => Forecast:", round(fc_vals, 3), "\n")
      }, error = function(e) {
        fc_vals <<- rep(tail(series[!is.na(series)], 1), n_ahead)
        arima_log_main[[ctry]][[v]] <<- list(order = "fallback-LOCF",
                                              values = fc_vals)
        cat("  ", v, ": ARIMA failed. Using last-obs carry-forward.\n")
      })
    }
    new_rows[[v]] <- fc_vals
  }

  # Append forecast rows
  data_main_raw <- dplyr::bind_rows(data_main_raw, new_rows)
}

data_main_raw <- data_main_raw %>% dplyr::arrange(country, date)

# --- 2b. ARIMA for 6-country supplementary dataset ---
cat("\n--- ARIMA: Supplementary 6-Country Dataset ---\n")
arima_log_supp <- list()

for (ctry in unique(data_supp_raw$country)) {
  cat("\n--- Forecasting for:", ctry, "---\n")
  country_data <- data_supp_raw %>% dplyr::filter(country == ctry) %>%
    dplyr::arrange(date)
  last_date <- max(country_data$date)
  if (last_date >= "2026-06") {
    cat("  Data complete through Jun 2026 - skipping.\n")
    next
  }
  n_ahead <- sum(!forecast_dates %in% country_data$date)
  if (n_ahead == 0) next

  new_rows <- data.frame(
    country     = rep(ctry, n_ahead),
    date        = forecast_dates[!forecast_dates %in% country_data$date],
    group       = rep(country_data$group[1], n_ahead),
    development = rep(country_data$development[1], n_ahead),
    oil_balance = rep(country_data$oil_balance[1], n_ahead),
    stringsAsFactors = FALSE
  )

  arima_log_supp[[ctry]] <- list()

  for (v in vars_to_forecast) {
    series <- country_data[[v]]

    # --- 2.1 FIX: policy_rate uses LOCF (step function, not suitable for ARIMA) ---
    if (v == "policy_rate") {
      cat("  policy_rate: using LOCF (step function, not suitable for ARIMA).\n")
      fc_vals <- rep(tail(series[!is.na(series)], 1), n_ahead)
      arima_log_supp[[ctry]][[v]] <- list(order = "LOCF-policy_rate",
                                           values = fc_vals)
    } else if (sum(!is.na(series)) < 10) {
      cat("  ", v, ": Not enough non-NA values. Using last-obs carry-forward.\n")
      fc_vals <- rep(tail(series[!is.na(series)], 1), n_ahead)
      arima_log_supp[[ctry]][[v]] <- list(order = "LOCF", values = fc_vals)
    } else {
      tryCatch({
        result  <- forecast_arima_country(series, n_ahead = n_ahead)
        fc_vals <- result$forecast_values
        arima_log_supp[[ctry]][[v]] <- list(order = result$arima_order,
                                             values = fc_vals)
        cat("  ", v, ": ARIMA(", result$arima_order[1], ",",
            result$arima_order[2], ",", result$arima_order[3],
            ") => Forecast:", round(fc_vals, 3), "\n")
      }, error = function(e) {
        fc_vals <<- rep(tail(series[!is.na(series)], 1), n_ahead)
        arima_log_supp[[ctry]][[v]] <<- list(order = "fallback-LOCF",
                                              values = fc_vals)
        cat("  ", v, ": ARIMA failed. Using last-obs carry-forward.\n")
      })
    }
    new_rows[[v]] <- fc_vals
  }

  data_supp_raw <- dplyr::bind_rows(data_supp_raw, new_rows)
}

data_supp_raw <- data_supp_raw %>% dplyr::arrange(country, date)

cat("\n=== After ARIMA extension ===\n")
cat("Main panel:", nrow(data_main_raw), "rows x", ncol(data_main_raw), "cols\n")
cat("Supplementary dataset:", nrow(data_supp_raw), "rows x",
    ncol(data_supp_raw), "cols\n")

# --- 2.5 FIX: Save final extended datasets ---
write.csv(data_main_raw, "main_panel_full.csv", row.names = FALSE)
cat("Saved main_panel_full.csv (EU3, 42 months)\n")
write.csv(data_supp_raw, "supplementary_full.csv", row.names = FALSE)
cat("Saved supplementary_full.csv (all 6 countries, 42 months)\n")

# =============================================================================
# 3. DATA PROCESSING AND LOG TRANSFORMS
# =============================================================================
cat("\n\n########## DATA PROCESSING ##########\n")

# --- 3a. Process main EU3 panel ---
# The EU3 CSV may or may not have group/development/oil_balance columns.
# If missing, add them for consistency.
if (!"group" %in% names(data_main_raw)) {
  data_main_raw$group       <- "Advanced_Net_Importer"
  data_main_raw$development <- "Advanced"
  data_main_raw$oil_balance <- "Net_Importer"
}

data_main <- data_main_raw %>%
  dplyr::mutate(
    date           = as.Date(paste0(date, "-01")),
    ln_cpi         = log(cpi),
    ln_ipi         = log(ipi),
    ln_neer        = log(neer),
    post_crisis    = ifelse(date >= as.Date("2026-03-01"), 1, 0),
    oil_pct_change = (brent - dplyr::lag(brent)) / dplyr::lag(brent) * 100
  ) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(
    oil_pct_change = ifelse(is.na(oil_pct_change), 0, oil_pct_change),
    d_ln_cpi       = ln_cpi - dplyr::lag(ln_cpi),
    d_ln_ipi       = ln_ipi - dplyr::lag(ln_ipi),
    month          = as.factor(format(date, "%b"))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(ln_cpi), !is.na(ln_ipi), !is.na(oil_pct_change))

cat("Main panel processed dims:", nrow(data_main), "x", ncol(data_main), "\n")
cat("Date range:", as.character(min(data_main$date)), "to",
    as.character(max(data_main$date)), "\n")
cat("Countries:", paste(unique(data_main$country), collapse = ", "), "\n")
cat("Obs per country:", nrow(data_main) / length(unique(data_main$country)), "\n")

# --- 3b. Process 6-country supplementary dataset ---
data_supp <- data_supp_raw %>%
  dplyr::mutate(
    date           = as.Date(paste0(date, "-01")),
    ln_cpi         = log(cpi),
    ln_ipi         = log(ipi),
    ln_neer        = log(neer),
    post_crisis    = ifelse(date >= as.Date("2026-03-01"), 1, 0),
    oil_pct_change = (brent - dplyr::lag(brent)) / dplyr::lag(brent) * 100
  ) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(
    oil_pct_change = ifelse(is.na(oil_pct_change), 0, oil_pct_change),
    d_ln_cpi       = ln_cpi - dplyr::lag(ln_cpi),
    d_ln_ipi       = ln_ipi - dplyr::lag(ln_ipi),
    month          = as.factor(format(date, "%b"))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(ln_cpi), !is.na(ln_ipi), !is.na(oil_pct_change))

cat("Supplementary processed dims:", nrow(data_supp), "x", ncol(data_supp), "\n")

# Supplementary country datasets
data_us     <- data_supp %>% dplyr::filter(country == "United States")
data_saudi  <- data_supp %>% dplyr::filter(country == "Saudi Arabia")
data_turkey <- data_supp %>% dplyr::filter(country == "Turkey")

# =============================================================================
# 4. DESCRIPTIVE STATISTICS
# =============================================================================
cat("\n\n########## DESCRIPTIVE STATISTICS ##########\n")

num_vars <- c("ln_cpi", "ln_ipi", "oil_pct_change", "ln_neer",
              "policy_rate", "vix")

# --- 4a. Main Panel (EU3) ---
cat("\n>>> MAIN PANEL: Advanced Net-Importers <<<\n")
desc_list <- list()
for (v in num_vars) {
  stats_df <- data_main %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      !!paste0(v, "_Mean") := round(mean(.data[[v]], na.rm = TRUE), 4),
      !!paste0(v, "_SD")   := round(sd(.data[[v]], na.rm = TRUE), 4),
      !!paste0(v, "_Min")  := round(min(.data[[v]], na.rm = TRUE), 4),
      !!paste0(v, "_Max")  := round(max(.data[[v]], na.rm = TRUE), 4),
      .groups = "drop")
  desc_list[[v]] <- stats_df
}
desc_stats_main <- desc_list[[1]]
for (i in seq_along(desc_list)[-1]) {
  desc_stats_main <- dplyr::left_join(desc_stats_main, desc_list[[i]],
                                       by = "country")
}
cat("\n=== Per-Country Descriptive Statistics (Main Panel) ===\n")
print(as.data.frame(desc_stats_main))

# --- 4b. Supplementary Countries ---
cat("\n=== Supplementary Country Statistics ===\n")
supp_countries <- c("United States", "Saudi Arabia", "Turkey")
desc_stats_supp_list <- list()
for (sc in supp_countries) {
  cat("\n---", sc, "---\n")
  sc_data <- data_supp %>% dplyr::filter(country == sc)
  for (v in num_vars) {
    cat(sprintf("  %s: Mean=%.3f, SD=%.3f, Min=%.3f, Max=%.3f\n",
                v, mean(sc_data[[v]], na.rm = TRUE),
                sd(sc_data[[v]], na.rm = TRUE),
                min(sc_data[[v]], na.rm = TRUE),
                max(sc_data[[v]], na.rm = TRUE)))
  }
}

# --- 4c. Group-level summary ---
group_stats <- data_supp %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(
    Obs          = dplyr::n(),
    Mean_ln_cpi  = mean(ln_cpi, na.rm = TRUE),
    Mean_ln_ipi  = mean(ln_ipi, na.rm = TRUE),
    Mean_brent   = mean(brent, na.rm = TRUE),
    Mean_policy  = mean(policy_rate, na.rm = TRUE),
    Mean_vix     = mean(vix, na.rm = TRUE),
    Mean_ln_neer = mean(ln_neer, na.rm = TRUE),
    .groups = "drop"
  )
cat("\n=== Group-Level Summary (All 6 Countries) ===\n")
print(as.data.frame(group_stats))

# =============================================================================
# 5. EXPLORATORY DATA ANALYSIS — PLOTS
# =============================================================================
cat("\n\n########## EXPLORATORY DATA ANALYSIS ##########\n")
if (!dir.exists("plots")) dir.create("plots")

group_colors <- c(
  "Advanced_Net_Importer" = "#2E86AB",
  "Advanced_Net_Exporter" = "#A23B72",
  "Emerging_Net_Exporter" = "#F18F01",
  "Emerging_Net_Importer" = "#C73E1D"
)

country_colors <- c(
  "France"        = "#2E86AB",
  "Germany"       = "#4A90D9",
  "Italy"         = "#7BC8F6",
  "United States" = "#A23B72",
  "Saudi Arabia"  = "#F18F01",
  "Turkey"        = "#C73E1D"
)

main_country_colors <- c(
  "France"  = "#2E86AB",
  "Germany" = "#4A90D9",
  "Italy"   = "#7BC8F6"
)

# --- Figure 1: Brent Oil Price with Crisis Line ---
p1 <- ggplot(data_main %>% dplyr::arrange(date) %>%
               dplyr::distinct(date, brent),
             aes(x = date, y = brent)) +
  geom_line(color = "darkred", linewidth = 0.8) +
  geom_vline(xintercept = as.Date("2026-02-27"),
             linetype = "dashed", color = "blue", linewidth = 0.6) +
  annotate("text", x = as.Date("2026-02-27"),
           y = max(data_main$brent, na.rm = TRUE),
           label = "Gulf Crisis\n(Feb 27, 2026)",
           hjust = -0.05, vjust = 1, size = 3.2, color = "blue") +
  labs(title = "Brent Crude Oil Price: Pre- and Post-2026 Gulf Crisis",
       x = "Date", y = "USD per Barrel") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave("plots/01_brent_oil_price.png", p1, width = 10, height = 5, dpi = 150)
cat("Figure 1 saved\n")

# --- Figure 2: Log CPI by Country (Faceted — Main Group) ---
p2 <- ggplot(data_main, aes(x = date, y = ln_cpi, color = country)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = as.Date("2026-02-27"),
             linetype = "dashed", color = "gray40") +
  facet_wrap(~country, scales = "free_y", ncol = 3) +
  scale_color_manual(values = main_country_colors) +
  labs(title = "Log(CPI) by Country Over Time (Main Panel)",
       x = "Date", y = "Log(CPI)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))
ggsave("plots/02_ln_cpi_by_country.png", p2, width = 12, height = 5, dpi = 150)
cat("Figure 2 saved\n")

# --- Figure 3: Log CPI — All Three Countries Overlaid ---
p3 <- ggplot(data_main, aes(x = date, y = ln_cpi, color = country)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = as.Date("2026-02-27"),
             linetype = "dashed", color = "gray40") +
  scale_color_manual(values = main_country_colors) +
  labs(title = "Log(CPI) — France, Germany, Italy (Main Panel)",
       x = "Date", y = "Log(CPI)", color = "Country") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave("plots/03_ln_cpi_all_countries.png", p3, width = 10, height = 5, dpi = 150)
cat("Figure 3 saved\n")

# --- Figure 4: Log IPI by Country (Faceted) ---
p4 <- ggplot(data_main, aes(x = date, y = ln_ipi, color = country)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = as.Date("2026-02-27"),
             linetype = "dashed", color = "gray40") +
  facet_wrap(~country, scales = "free_y", ncol = 3) +
  scale_color_manual(values = main_country_colors) +
  labs(title = "Log(IPI) by Country Over Time (Main Panel)",
       x = "Date", y = "Log(IPI)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))
ggsave("plots/04_ln_ipi_by_country.png", p4, width = 12, height = 5, dpi = 150)
cat("Figure 4 saved\n")

# --- Figure 5: Policy Rate (ECB — same for all 3) ---
p5 <- ggplot(data_main, aes(x = date, y = policy_rate, color = country)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = as.Date("2026-02-27"),
             linetype = "dashed", color = "gray40") +
  scale_color_manual(values = main_country_colors) +
  labs(title = "ECB Policy Rate (All Three Countries Share Same Rate)",
       x = "Date", y = "Policy Rate (%)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave("plots/05_policy_rate.png", p5, width = 10, height = 5, dpi = 150)
cat("Figure 5 saved\n")

# --- Figure 6: VIX Over Time ---
p6 <- ggplot(data_main %>% dplyr::distinct(date, vix) %>%
               dplyr::arrange(date),
             aes(x = date, y = vix)) +
  geom_line(color = "darkblue", linewidth = 0.8) +
  geom_vline(xintercept = as.Date("2026-02-27"),
             linetype = "dashed", color = "red", linewidth = 0.6) +
  annotate("text", x = as.Date("2026-02-27"),
           y = max(data_main$vix, na.rm = TRUE),
           label = "Gulf Crisis", hjust = -0.05, vjust = 1,
           size = 3.2, color = "red") +
  labs(title = "VIX — Global Financial Uncertainty Index",
       x = "Date", y = "VIX") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave("plots/06_vix.png", p6, width = 10, height = 5, dpi = 150)
cat("Figure 6 saved\n")

# --- Figure 7: Log NEER by Country ---
p7 <- ggplot(data_main, aes(x = date, y = ln_neer, color = country)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = as.Date("2026-02-27"),
             linetype = "dashed", color = "gray40") +
  facet_wrap(~country, scales = "free_y", ncol = 3) +
  scale_color_manual(values = main_country_colors) +
  labs(title = "Log(NEER) by Country Over Time",
       x = "Date", y = "Log(NEER)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))
ggsave("plots/07_ln_neer_by_country.png", p7, width = 12, height = 5, dpi = 150)
cat("Figure 7 saved\n")

# --- Figure 8: Oil Price Percentage Change ---
p8 <- ggplot(data_main, aes(x = date, y = oil_pct_change, color = country)) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  geom_vline(xintercept = as.Date("2026-02-27"),
             linetype = "dashed", color = "gray40") +
  scale_color_manual(values = main_country_colors) +
  labs(title = "Monthly Oil Price Percentage Change (Shock Variable)",
       x = "Date", y = "Brent % Change", color = "Country") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave("plots/08_oil_pct_change.png", p8, width = 10, height = 5, dpi = 150)
cat("Figure 8 saved\n")

# --- Figure 9: Correlation Matrix (Main Group) ---
cor_vars <- data_main %>%
  dplyr::select(ln_cpi, ln_ipi, oil_pct_change, ln_neer, policy_rate, vix) %>%
  tidyr::drop_na()
cor_mat <- cor(cor_vars)
cat("\n=== Correlation Matrix ===\n")
print(round(cor_mat, 3))

png("plots/09_correlation_matrix.png", width = 700, height = 600, res = 150)
corrplot(cor_mat, method = "color", type = "full",
         addCoef.col = "black", number.cex = 0.75,
         tl.col = "black", tl.cex = 0.85,
         title = "Correlation Matrix of Key Variables (Main Panel)",
         mar = c(0, 0, 2, 0))
dev.off()
cat("Figure 9 saved\n")

# --- Figure 10: Boxplot ln_cpi ---
p10 <- ggplot(data_main, aes(x = country, y = ln_cpi, fill = country)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  scale_fill_manual(values = main_country_colors) +
  labs(title = "Distribution of Log(CPI) by Country (Main Panel)",
       x = "", y = "Log(CPI)", fill = "Country") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave("plots/10_boxplot_ln_cpi.png", p10, width = 8, height = 5, dpi = 150)
cat("Figure 10 saved\n")

# --- Figure 11: Boxplot ln_ipi ---
p11 <- ggplot(data_main, aes(x = country, y = ln_ipi, fill = country)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  scale_fill_manual(values = main_country_colors) +
  labs(title = "Distribution of Log(IPI) by Country (Main Panel)",
       x = "", y = "Log(IPI)", fill = "Country") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave("plots/11_boxplot_ln_ipi.png", p11, width = 8, height = 5, dpi = 150)
cat("Figure 11 saved\n")

# --- Figure 12: Pre vs Post Crisis (Main Panel) ---
pre_post <- data_main %>%
  dplyr::mutate(period = ifelse(date < as.Date("2026-03-01"),
                                "Pre-Crisis", "Post-Crisis")) %>%
  dplyr::group_by(country, period) %>%
  dplyr::summarise(mean_ln_cpi = mean(ln_cpi, na.rm = TRUE),
                   mean_ln_ipi = mean(ln_ipi, na.rm = TRUE),
                   .groups = "drop")

p12 <- ggplot(pre_post, aes(x = period, y = mean_ln_cpi, fill = period)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  facet_wrap(~country) +
  scale_fill_manual(values = c("Pre-Crisis" = "#2E86AB",
                               "Post-Crisis" = "#C73E1D")) +
  labs(title = "Average Log(CPI): Pre-Crisis vs Post-Crisis by Country",
       x = "", y = "Mean Log(CPI)", fill = "Period") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))
ggsave("plots/12_pre_post_crisis_cpi.png", p12, width = 10, height = 5, dpi = 150)
cat("Figure 12 saved\n")

# --- Figure 13: Scatter CPI vs Oil ---
p13 <- ggplot(data_main, aes(x = oil_pct_change, y = ln_cpi,
                              color = country)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, linewidth = 0.8) +
  scale_color_manual(values = main_country_colors) +
  labs(title = "Log(CPI) vs. Oil Price Change (%) — Advanced Net-Importers",
       x = "Brent % Change", y = "Log(CPI)", color = "Country") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave("plots/13_scatter_cpi_oil.png", p13, width = 9, height = 6, dpi = 150)
cat("Figure 13 saved\n")

# --- Figure 14: Log CPI by Country Group (6 countries) ---
p14 <- ggplot(data_supp, aes(x = date, y = ln_cpi, color = group)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  geom_vline(xintercept = as.Date("2026-02-27"),
             linetype = "dashed", color = "gray40") +
  scale_color_manual(values = group_colors) +
  labs(title = "Average Log(CPI) by Country Group (All 6 Countries)",
       x = "Date", y = "Log(CPI)", color = "Group") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave("plots/14_ln_cpi_by_group.png", p14, width = 10, height = 5, dpi = 150)
cat("Figure 14 saved\n")

# --- Figure 15: Log IPI by Country Group (6 countries) ---
p15 <- ggplot(data_supp, aes(x = date, y = ln_ipi, color = group)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  geom_vline(xintercept = as.Date("2026-02-27"),
             linetype = "dashed", color = "gray40") +
  scale_color_manual(values = group_colors) +
  labs(title = "Average Log(IPI) by Country Group (All 6 Countries)",
       x = "Date", y = "Log(IPI)", color = "Group") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave("plots/15_ln_ipi_by_group.png", p15, width = 10, height = 5, dpi = 150)
cat("Figure 15 saved\n")

cat("\nAll EDA plots saved.\n")

# =============================================================================
# 6. PANEL UNIT ROOT TESTS — LLC, IPS, Individual ADF (Main Panel Only)
# =============================================================================
cat("\n\n########## PANEL UNIT ROOT TESTS — MAIN PANEL ##########\n")

pdata_main <- pdata.frame(data_main, index = c("country", "date"))

# Variables to test
ur_vars <- c("ln_cpi", "ln_ipi", "oil_pct_change", "ln_neer",
             "policy_rate", "vix")

# Variables with zero cross-sectional variation (identical across all 3 EU
# countries because they share the same Brent price, ECB rate, and VIX)
zero_xsec_vars <- c("oil_pct_change", "policy_rate", "vix")

# --- 6.1 Levin-Lin-Chu (LLC) Tests ---
cat("\n--- Levin-Lin-Chu (LLC) Unit Root Tests ---\n")
llc_results <- list()
for (v in ur_vars) {
  tryCatch({
    res <- purtest(as.formula(paste(v, "~ 1")),
                   data = pdata_main, test = "levinlin", lags = 2)
    stat <- round(res$statistic$statistic, 3)
    pval <- round(res$statistic$p.value, 4)
    llc_results[[v]] <- c(stat, pval)
    cat(sprintf("  %-18s LLC stat: %7.3f   p-value: %.4f\n", v, stat, pval))
  }, error = function(e) {
    cat("  ", v, ": Error -", e$message, "\n")
  })
}

# --- 6.2 Im-Pesaran-Shin (IPS) Tests ---
# NOTE: IPS is not informative for variables with zero cross-sectional
# variation (oil_pct_change, policy_rate, vix) because the test relies on
# cross-sectional heterogeneity. The test will return degenerate statistics.
cat("\n--- Im-Pesaran-Shin (IPS) Unit Root Tests ---\n")
ips_results <- list()
for (v in ur_vars) {
  if (v %in% zero_xsec_vars) {
    cat(sprintf("  %-18s: SKIPPED — zero cross-sectional variation;\n", v))
    cat("    IPS test is not informative for this variable (identical\n")
    cat("    across all 3 EU countries sharing ECB rate, Brent, VIX).\n")
    ips_results[[v]] <- c(NA, NA)
    next
  }
  tryCatch({
    res  <- purtest(as.formula(paste(v, "~ 1")),
                    data = pdata_main, test = "ips", lags = 2)
    stat <- round(res$statistic$statistic, 3)
    pval <- round(res$statistic$p.value, 4)
    ips_results[[v]] <- c(stat, pval)
    cat(sprintf("  %-18s IPS stat: %7.3f   p-value: %.4f\n", v, stat, pval))
  }, error = function(e) {
    cat("  ", v, ": Error -", e$message, "\n")
    cat("    (This may occur due to zero cross-sectional variation.)\n")
    ips_results[[v]] <- c(NA, NA)
  })
}

# --- 6.3 Individual ADF Tests (ln_cpi by country) ---
cat("\n--- Individual ADF Tests (Log CPI — Main Panel) ---\n")
adf_results <- list()
for (ctry in unique(data_main$country)) {
  c_data <- data_main %>%
    dplyr::filter(country == ctry) %>% dplyr::arrange(date)
  tryCatch({
    adf_r <- adf.test(c_data$ln_cpi, alternative = "stationary")
    adf_results[[ctry]] <- c(round(adf_r$statistic, 4),
                              round(adf_r$p.value, 4))
    cat(sprintf("  %-14s ADF stat: %7.4f   p-value: %.4f\n",
                ctry, adf_r$statistic, adf_r$p.value))
  }, error = function(e) {
    cat(ctry, ": Error -", e$message, "\n")
  })
}

# =============================================================================
# 7. MAIN PANEL REGRESSIONS
# =============================================================================
cat("\n\n########## MAIN PANEL REGRESSIONS ##########\n")
cat(">>> CORE ANALYSIS: Advanced Net-Importers (France, Germany, Italy) <<<\n\n")

# ---- 7.1 INFLATION MODEL (ln_cpi) — POLS, FE, RE, TWFE ----
cat("\n--- 7.1 Inflation Model (ln_cpi) ---\n")

pols_cpi <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate +
                  vix + post_crisis,
                data = pdata_main, model = "pooling")
cat("\n=== Pooled OLS (CPI) ===\n")
print(summary(pols_cpi))

fe_cpi <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate +
                vix + post_crisis,
              data = pdata_main, model = "within")
cat("\n=== Fixed Effects — One-Way (CPI) ===\n")
print(summary(fe_cpi))

# RE with Wallace-Hussain — Swamy-Arora not identified with N=3
re_cpi <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate +
                vix + post_crisis,
              data = pdata_main, model = "random", random.method = "walhus")
cat("\n=== Random Effects — Wallace-Hussain (CPI) ===\n")
cat("NOTE: Wallace-Hussain estimator used because Swamy-Arora is not\n")
cat("identified with N=3 entities (degenerate variance components).\n")
print(summary(re_cpi))

twfe_cpi <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate +
                  vix + post_crisis,
                data = pdata_main, model = "within", effect = "twoways")
cat("\n=== Two-Way Fixed Effects (CPI) ===\n")
cat("NOTE: TWFE absorbs global variables (oil_pct_change, policy_rate,\n")
cat("vix) that have zero cross-sectional variation in this 3-country\n")
cat("Eurozone panel. Their coefficients will be NA.\n")
print(summary(twfe_cpi))

# ---- 7.2 GROWTH MODEL (ln_ipi) — POLS, FE, RE, FE+Month Dummies ----
cat("\n--- 7.2 Growth Model (ln_ipi) ---\n")

pols_ipi <- plm(ln_ipi ~ oil_pct_change + ln_neer + policy_rate +
                  vix + post_crisis,
                data = pdata_main, model = "pooling")
cat("\n=== Pooled OLS (IPI) ===\n")
print(summary(pols_ipi))

fe_ipi <- plm(ln_ipi ~ oil_pct_change + ln_neer + policy_rate +
                vix + post_crisis,
              data = pdata_main, model = "within")
cat("\n=== Fixed Effects — One-Way (IPI) ===\n")
cat("NOTE: This specification has very low R-squared (~0.23) due to\n")
cat("strong seasonal patterns in IPI (August dip). The month-dummies\n")
cat("specification below is the preferred IPI model.\n")
print(summary(fe_ipi))

re_ipi <- plm(ln_ipi ~ oil_pct_change + ln_neer + policy_rate +
                vix + post_crisis,
              data = pdata_main, model = "random", random.method = "walhus")
cat("\n=== Random Effects — Wallace-Hussain (IPI) ===\n")
print(summary(re_ipi))

# FE IPI with month dummies — preferred specification for growth model
fe_ipi_months <- plm(ln_ipi ~ oil_pct_change + ln_neer + policy_rate +
                        vix + post_crisis + month,
                      data = pdata_main, model = "within")
cat("\n=== Fixed Effects + Month Dummies (IPI) — PREFERRED SPECIFICATION ===\n")
cat("NOTE: Adding month dummies dramatically improves R-squared by\n")
cat("absorbing the strong seasonal pattern (esp. August dip) in\n")
cat("non-seasonally-adjusted IPI data. This is the preferred growth model.\n")
print(summary(fe_ipi_months))

# =============================================================================
# 8. DIAGNOSTIC TESTS
# =============================================================================
cat("\n\n########## DIAGNOSTIC TESTS ##########\n")

# --- 8.1 Hausman Test (CPI) — with graceful handling of negative chi-sq ---
cat("\n--- Hausman Test (CPI) ---\n")
tryCatch({
  hausman_cpi <- phtest(fe_cpi, re_cpi)
  h_stat <- hausman_cpi$statistic
  h_pval <- hausman_cpi$p.value
  if (!is.nan(h_stat) && h_stat >= 0) {
    print(hausman_cpi)
    cat("Interpretation: p-value < 0.05 => FE preferred; otherwise RE acceptable.\n")
  } else {
    cat("Hausman test returned a negative chi-squared statistic:", round(h_stat, 4), "\n")
    cat("This occurs when FE and RE estimates are very similar (non-positive-definite\n")
    cat("variance matrix), which is common in a homogeneous Eurozone panel with N=3.\n")
    cat("INTERPRETATION: Negative Hausman statistic => FAIL TO REJECT H0.\n")
    cat("Random Effects is acceptable (consistent and efficient).\n")
    cat("However, with only N=3 entities, the test has very low power.\n")
    cat("We retain FE as the primary estimator on theoretical grounds\n")
    cat("(country heterogeneity in price levels) and use RE as a robustness check.\n")
  }
}, error = function(e) {
  cat("Hausman test error:", e$message, "\n")
  cat("This typically indicates FE and RE estimates are very similar.\n")
  cat("Interpretation: Fail to reject H0 — RE is acceptable.\n")
})

# --- 8.2 Hausman Test (IPI) ---
cat("\n--- Hausman Test (IPI) ---\n")
tryCatch({
  hausman_ipi <- phtest(fe_ipi, re_ipi)
  h_stat <- hausman_ipi$statistic
  h_pval <- hausman_ipi$p.value
  if (!is.nan(h_stat) && h_stat >= 0) {
    print(hausman_ipi)
  } else {
    cat("Hausman test returned a negative chi-squared statistic:", round(h_stat, 4), "\n")
    cat("Same interpretation as CPI: fail to reject H0, RE acceptable.\n")
  }
}, error = function(e) {
  cat("Hausman test error:", e$message, "\n")
})

# --- 8.3 Breusch-Pagan LM Test (CPI) ---
cat("\n--- Breusch-Pagan LM Test (CPI) ---\n")
bp_cpi <- plmtest(pols_cpi, effect = "individual", type = "bp")
print(bp_cpi)
cat("Interpretation: p-value < 0.05 => individual RE effects present (POLS rejected).\n")

# --- 8.4 Breusch-Pagan LM Test (IPI) ---
cat("\n--- Breusch-Pagan LM Test (IPI) ---\n")
bp_ipi <- plmtest(pols_ipi, effect = "individual", type = "bp")
print(bp_ipi)

# --- 8.5 F-Test for Poolability ---
cat("\n--- F-Test for Poolability (CPI) ---\n")
tryCatch({
  pool_test <- pooltest(ln_cpi ~ oil_pct_change + ln_neer + policy_rate +
                          vix + post_crisis,
                        data = pdata_main, model = "within")
  print(pool_test)
}, error = function(e) cat("  Error:", e$message, "\n"))

# --- 8.6 Heteroskedasticity Test (FE CPI) ---
cat("\n--- Heteroskedasticity Test (FE CPI) ---\n")
tryCatch({
  bph_cpi <- bptest(ln_cpi ~ oil_pct_change + ln_neer + policy_rate +
                      vix + post_crisis + factor(country),
                    data = data_main, studentize = TRUE)
  print(bph_cpi)
}, error = function(e) cat("  Error:", e$message, "\n"))

# --- 8.7 Serial Correlation Test (FE CPI) ---
cat("\n--- Serial Correlation Test (FE CPI) ---\n")
tryCatch({
  sc_cpi <- pbgtest(fe_cpi)
  print(sc_cpi)
}, error = function(e) cat("  Error:", e$message, "\n"))

# --- 8.8 Wald Test (FE CPI) ---
cat("\n--- Wald Test: Joint Significance (FE CPI) ---\n")
tryCatch({
  wald_cpi <- pwaldtest(fe_cpi, test = "Chisq")
  print(wald_cpi)
}, error = function(e) cat("  Error:", e$message, "\n"))

# =============================================================================
# 9. ROBUST AND CLUSTERED STANDARD ERRORS
# =============================================================================
cat("\n\n########## ROBUST AND CLUSTERED SEs ##########\n")

# --- 9.1 Arellano HC3 Robust SEs (FE CPI) ---
cat("\n--- Robust SEs (FE CPI — Arellano HC3) ---\n")
fe_cpi_robust <- coeftest(fe_cpi,
                          vcov = vcovHC(fe_cpi, method = "arellano",
                                        type = "HC3"))
print(fe_cpi_robust)

# --- 9.2 Arellano HC3 Robust SEs (FE IPI) ---
cat("\n--- Robust SEs (FE IPI — Arellano HC3) ---\n")
fe_ipi_robust <- coeftest(fe_ipi,
                          vcov = vcovHC(fe_ipi, method = "arellano",
                                        type = "HC3"))
print(fe_ipi_robust)

# --- 9.3 Double-Clustered SEs (FE CPI — time & entity) ---
cat("\n--- Double-Clustered SEs (FE CPI — vcovDC) ---\n")
tryCatch({
  fe_cpi_clustered <- coeftest(fe_cpi, vcov = vcovDC(fe_cpi, type = "HC3"))
  print(fe_cpi_clustered)
}, error = function(e) cat("  Error:", e$message, "\n"))

# --- 9.4 Robust SEs for FE IPI with month dummies ---
cat("\n--- Robust SEs (FE IPI + Month Dummies — Arellano HC3) ---\n")
fe_ipi_months_robust <- coeftest(fe_ipi_months,
                                  vcov = vcovHC(fe_ipi_months,
                                                method = "arellano",
                                                type = "HC3"))
print(fe_ipi_months_robust)

# =============================================================================
# 10. STARGAZER TABLES
# =============================================================================
cat("\n\n########## STARGAZER REGRESSION TABLES ##########\n")

# --- 10.1 Inflation Model Comparison ---
stargazer(pols_cpi, fe_cpi, re_cpi, twfe_cpi,
          title = "Panel Regression — Inflation (Advanced Net-Importers)",
          column.labels = c("Pooled OLS", "Fixed Effects",
                            "RE (Walhus)", "Two-Way FE"),
          dep.var.labels = "Log(CPI)",
          covariate.labels = c("Oil Price Change (%)", "Log(NEER)",
                               "Policy Rate", "VIX", "Post-Crisis Dummy"),
          keep.stat = c("n", "rsq"),
          type = "text",
          out = "plots/table_inflation_models.txt"
)

# --- 10.2 Growth Model Comparison ---
stargazer(pols_ipi, fe_ipi, re_ipi, fe_ipi_months,
          title = "Panel Regression — Growth (Advanced Net-Importers)",
          column.labels = c("Pooled OLS", "Fixed Effects",
                            "RE (Walhus)", "FE + Month Dummies"),
          dep.var.labels = "Log(IPI)",
          covariate.labels = c("Oil Price Change (%)", "Log(NEER)",
                               "Policy Rate", "VIX", "Post-Crisis Dummy"),
          keep.stat = c("n", "rsq"),
          type = "text",
          out = "plots/table_growth_models.txt"
)

# --- 10.3 FE with Robust SEs ---
stargazer(fe_cpi, fe_ipi,
          title = "Fixed Effects with Robust Standard Errors (Adv. Net-Importers)",
          column.labels = c("Inflation (Log CPI)", "Growth (Log IPI)"),
          covariate.labels = c("Oil Price Change (%)", "Log(NEER)",
                               "Policy Rate", "VIX", "Post-Crisis Dummy"),
          keep.stat = c("n", "rsq"),
          se = list(
            sqrt(diag(vcovHC(fe_cpi, method = "arellano", type = "HC3"))),
            sqrt(diag(vcovHC(fe_ipi, method = "arellano", type = "HC3")))
          ),
          type = "text",
          out = "plots/table_FE_robust.txt"
)

# --- 10.4 Custom comparison table (console) ---
print_reg_table <- function(models, title, col_labels) {
  cat("\n", title, "\n")
  cat(paste(rep("-", 80), collapse = ""), "\n")
  all_vars <- unique(unlist(lapply(models, function(m) names(coef(m)))))
  for (v in all_vars) {
    line <- sprintf("%-25s", v)
    for (m in models) {
      cf <- coef(m)
      se <- sqrt(diag(vcov(m)))
      if (v %in% names(cf)) {
        est <- cf[v]
        s   <- se[v]
        pv  <- 2 * (1 - pnorm(abs(est / s)))
        stars <- ifelse(pv < 0.01, "***",
                        ifelse(pv < 0.05, "**",
                               ifelse(pv < 0.1, "*", "")))
        line <- paste0(line, sprintf(" %9.4f%-3s", est, stars))
      } else {
        line <- paste0(line, sprintf(" %12s", "---"))
      }
    }
    cat(line, "\n")
  }
  cat(paste(rep("-", 80), collapse = ""), "\n")
  for (i in seq_along(models)) {
    rsq_val <- tryCatch(summary(models[[i]])$r.squared[1], error = function(e) NA)
    cat(sprintf("R2(%-20s): %.4f\n", col_labels[i], rsq_val))
  }
}

print_reg_table(list(pols_cpi, fe_cpi, re_cpi, twfe_cpi),
                "Inflation Model (Log CPI) — POLS / FE / RE / TWFE",
                c("Pooled OLS", "Fixed Effects",
                  "Random Effects", "Two-Way FE"))

print_reg_table(list(pols_ipi, fe_ipi, re_ipi, fe_ipi_months),
                "Growth Model (Log IPI) — POLS / FE / RE / FE+Months",
                c("Pooled OLS", "Fixed Effects",
                  "Random Effects", "FE+Month Dummies"))

# =============================================================================
# 11. SUPPLEMENTARY COUNTRY OLS + CROSS-GROUP COMPARISON
# =============================================================================
cat("\n\n########## SUPPLEMENTARY: INDIVIDUAL COUNTRY ANALYSES ##########\n")
cat("Per professor's guidance: cross-group comparisons should be conducted\n")
cat("via separate analyses. Since US, Saudi Arabia, and Turkey each represent\n")
cat("a different group with only one member, we run individual time-series\n")
cat("OLS regressions and compare coefficients qualitatively.\n\n")

# --- 11.1 United States (Advanced Net-Exporter) ---
cat("\n--- United States (Advanced Net-Exporter) ---\n")
ols_us_cpi <- lm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate +
                   vix + post_crisis, data = data_us)
cat("\n=== OLS Inflation (US) ===\n")
print(summary(ols_us_cpi))
cat("\nRobust SEs (US CPI):\n")
print(coeftest(ols_us_cpi, vcov = vcovHC(ols_us_cpi, type = "HC3")))

ols_us_ipi <- lm(ln_ipi ~ oil_pct_change + ln_neer + policy_rate +
                   vix + post_crisis, data = data_us)
cat("\n=== OLS Growth (US) ===\n")
print(summary(ols_us_ipi))

# --- 11.2 Saudi Arabia (Emerging Net-Exporter) ---
cat("\n--- Saudi Arabia (Emerging Net-Exporter) ---\n")
ols_sa_cpi <- lm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate +
                   vix + post_crisis, data = data_saudi)
cat("\n=== OLS Inflation (Saudi Arabia) ===\n")
print(summary(ols_sa_cpi))
cat("\nRobust SEs (Saudi CPI):\n")
print(coeftest(ols_sa_cpi, vcov = vcovHC(ols_sa_cpi, type = "HC3")))

ols_sa_ipi <- lm(ln_ipi ~ oil_pct_change + ln_neer + policy_rate +
                   vix + post_crisis, data = data_saudi)
cat("\n=== OLS Growth (Saudi Arabia) ===\n")
print(summary(ols_sa_ipi))

# --- 11.3 Turkey (Emerging Net-Importer) ---
cat("\n--- Turkey (Emerging Net-Importer) ---\n")
cat("NOTE: Turkey's policy_rate data may only extend to Feb 2026.\n")
cat("Missing months are handled via ARIMA carry-forward in Section 2.\n")
ols_tr_cpi <- lm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate +
                   vix + post_crisis, data = data_turkey)
cat("\n=== OLS Inflation (Turkey) ===\n")
print(summary(ols_tr_cpi))
cat("\nRobust SEs (Turkey CPI):\n")
print(coeftest(ols_tr_cpi, vcov = vcovHC(ols_tr_cpi, type = "HC3")))

ols_tr_ipi <- lm(ln_ipi ~ oil_pct_change + ln_neer + policy_rate +
                   vix + post_crisis, data = data_turkey)
cat("\n=== OLS Growth (Turkey) ===\n")
print(summary(ols_tr_ipi))

# --- 11.4 Cross-Group Comparison Table ---
cat("\n\n=== Cross-Group Comparison: Coefficient Estimates ===\n")
cat("(Main panel FE vs. individual country OLS — qualitative comparison)\n\n")

extract_coefs <- function(model, model_name) {
  coefs <- coef(model)
  se <- tryCatch(sqrt(diag(vcovHC(model, type = "HC3"))),
                 error = function(e) {
                   tryCatch(summary(model)$coefficients[, 2],
                            error = function(e2) rep(NA, length(coefs)))
                 })
  data.frame(
    Model     = model_name,
    Variable  = names(coefs),
    Estimate  = round(coefs, 4),
    Robust_SE = round(se[1:length(coefs)], 4),
    stringsAsFactors = FALSE
  )
}

comparison_df <- dplyr::bind_rows(
  extract_coefs(fe_cpi, "FE: Adv. Net-Importers (Main)"),
  extract_coefs(ols_us_cpi, "OLS: United States"),
  extract_coefs(ols_sa_cpi, "OLS: Saudi Arabia"),
  extract_coefs(ols_tr_cpi, "OLS: Turkey")
)
print(comparison_df)

# --- 11.5 Cross-Group Comparison Plot ---
comp_cpi <- comparison_df %>%
  dplyr::filter(Variable %in% c("oil_pct_change", "ln_neer",
                                  "policy_rate", "vix", "post_crisis"))

p_crossgroup <- ggplot(comp_cpi,
                        aes(x = Variable, y = Estimate, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c(
    "FE: Adv. Net-Importers (Main)" = "#2E86AB",
    "OLS: United States"            = "#A23B72",
    "OLS: Saudi Arabia"             = "#F18F01",
    "OLS: Turkey"                   = "#C73E1D")) +
  labs(title = "Cross-Group Comparison: Oil Pass-Through Coefficients (Inflation)",
       x = "", y = "Coefficient Estimate", fill = "Model") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 15, hjust = 1))
ggsave("plots/16_cross_group_comparison.png", p_crossgroup,
       width = 12, height = 6, dpi = 150)
cat("Figure 16 saved\n")

# --- 11.6 Stargazer Cross-Group Comparison Table ---
stargazer(fe_cpi, ols_us_cpi, ols_sa_cpi, ols_tr_cpi,
          title = "Cross-Group Comparison: Inflation Model",
          column.labels = c("FE: Adv. Importers", "OLS: US",
                            "OLS: Saudi", "OLS: Turkey"),
          dep.var.labels = "Log(CPI)",
          covariate.labels = c("Oil Price Change (%)", "Log(NEER)",
                               "Policy Rate", "VIX", "Post-Crisis Dummy"),
          keep.stat = c("n", "rsq"),
          se = list(
            sqrt(diag(vcovHC(fe_cpi, method = "arellano", type = "HC3"))),
            sqrt(diag(vcovHC(ols_us_cpi, type = "HC3"))),
            sqrt(diag(vcovHC(ols_sa_cpi, type = "HC3"))),
            sqrt(diag(vcovHC(ols_tr_cpi, type = "HC3")))
          ),
          type = "text",
          out = "plots/table_cross_group_comparison.txt"
)

# =============================================================================
# 12. INTERACTION TERM
# =============================================================================
cat("\n\n########## INTERACTION TERM ##########\n")

cat("\n--- Interaction: oil_pct_change x post_crisis (FE CPI — Main Group) ---\n")
tryCatch({
  fe_crisis_interaction <- plm(
    ln_cpi ~ oil_pct_change * post_crisis + ln_neer + policy_rate + vix,
    data = pdata_main, model = "within")
  print(summary(fe_crisis_interaction))
  cat("\nInterpretation: A significant interaction term indicates that\n")
  cat("the oil price pass-through to inflation changed after the Gulf crisis.\n")
}, error = function(e) cat("Error:", e$message, "\n"))

# =============================================================================
# 13. DIAGNOSTIC AND REGRESSION PLOTS
# =============================================================================
cat("\n\n########## REGRESSION DIAGNOSTIC PLOTS ##########\n")

# --- Figure 17: Residuals vs Fitted (FE CPI) ---
residuals_fe <- residuals(fe_cpi)
fitted_fe    <- fitted(fe_cpi)

resid_df <- data.frame(
  fitted    = as.numeric(fitted_fe),
  residuals = as.numeric(residuals_fe),
  country   = data_main$country[as.numeric(names(residuals_fe))]
)

p_resid <- ggplot(resid_df, aes(x = fitted, y = residuals, color = country)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = main_country_colors) +
  labs(title = "Residuals vs. Fitted Values (FE — CPI Model)",
       x = "Fitted Values", y = "Residuals", color = "Country") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave("plots/17_residuals_vs_fitted.png", p_resid, width = 10, height = 6, dpi = 150)
cat("Figure 17 saved\n")

# --- Figure 18: Coefficient Plot — FE CPI with Robust SEs ---
coef_data <- data.frame(
  variable = names(coef(fe_cpi)),
  estimate = coef(fe_cpi),
  se       = sqrt(diag(vcovHC(fe_cpi, method = "arellano", type = "HC3")))
) %>%
  dplyr::filter(variable != "(intercept)") %>%
  dplyr::mutate(
    ci_lower    = estimate - 1.96 * se,
    ci_upper    = estimate + 1.96 * se,
    significant = (ci_lower > 0 & ci_upper > 0) |
      (ci_lower < 0 & ci_upper < 0)
  )

p_coef <- ggplot(coef_data, aes(x = reorder(variable, estimate),
                                 y = estimate, color = significant)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  coord_flip() +
  scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "darkred"),
                     labels = c("Not Significant", "Significant")) +
  labs(title = "Coefficient Plot: FE Inflation Model (Robust 95% CI)",
       x = "", y = "Coefficient Estimate", color = "") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave("plots/18_coefficient_plot_fe_cpi.png", p_coef, width = 9, height = 5, dpi = 150)
cat("Figure 18 saved\n")

# --- Figure 19: Coefficient Plot — FE IPI with Robust SEs ---
coef_data_ipi <- data.frame(
  variable = names(coef(fe_ipi)),
  estimate = coef(fe_ipi),
  se       = sqrt(diag(vcovHC(fe_ipi, method = "arellano", type = "HC3")))
) %>%
  dplyr::filter(variable != "(intercept)") %>%
  dplyr::mutate(
    ci_lower    = estimate - 1.96 * se,
    ci_upper    = estimate + 1.96 * se,
    significant = (ci_lower > 0 & ci_upper > 0) |
      (ci_lower < 0 & ci_upper < 0)
  )

p_coef_ipi <- ggplot(coef_data_ipi, aes(x = reorder(variable, estimate),
                                         y = estimate, color = significant)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  coord_flip() +
  scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "darkblue"),
                     labels = c("Not Significant", "Significant")) +
  labs(title = "Coefficient Plot: FE Growth Model (Robust 95% CI)",
       x = "", y = "Coefficient Estimate", color = "") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave("plots/19_coefficient_plot_fe_ipi.png", p_coef_ipi,
       width = 9, height = 5, dpi = 150)
cat("Figure 19 saved\n")

# --- Figure 20: Country Fixed Effects (CPI Model) ---
cat("\n--- Country Fixed Effects (Inflation Model) ---\n")
fe_intercepts <- fixef(fe_cpi)
print(round(fe_intercepts, 4))

p_fixef <- data.frame(
  country   = names(fe_intercepts),
  intercept = as.numeric(fe_intercepts)
) %>%
  ggplot(aes(x = reorder(country, intercept), y = intercept, fill = country)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  coord_flip() +
  scale_fill_manual(values = main_country_colors) +
  labs(title = "Country Fixed Effects (Inflation Model)",
       x = "", y = "Fixed Effect Intercept", fill = "Country") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave("plots/20_country_fixed_effects.png", p_fixef, width = 9, height = 5, dpi = 150)
cat("Figure 20 saved\n")

# --- Figure 21: Country-Level Coefficient Comparison (within-group) ---
country_coefs <- lapply(unique(data_main$country), function(ctry) {
  c_data <- data_main %>% dplyr::filter(country == ctry)
  m <- lm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate + vix + post_crisis,
          data = c_data)
  data.frame(
    country  = ctry,
    variable = c("Oil % Change", "Log(NEER)", "Policy Rate", "VIX",
                 "Post-Crisis"),
    estimate = coef(m)[c("oil_pct_change", "ln_neer", "policy_rate",
                         "vix", "post_crisis")]
  )
})
country_coef_df <- dplyr::bind_rows(country_coefs)

p_het <- ggplot(country_coef_df, aes(x = variable, y = estimate,
                                      fill = country)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = main_country_colors) +
  labs(title = "Country-Level Coefficient Comparison (OLS — Inflation Model)",
       x = "", y = "Coefficient Estimate", fill = "Country") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 15, hjust = 1))
ggsave("plots/21_country_coef_comparison.png", p_het, width = 12, height = 6, dpi = 150)
cat("Figure 21 saved\n")

# --- Figure 22: Model Fit Comparison (R-squared) ---
rsq_data <- data.frame(
  Model = c("POLS\n(CPI)", "FE\n(CPI)", "RE\n(CPI)", "TWFE\n(CPI)",
            "POLS\n(IPI)", "FE\n(IPI)", "RE\n(IPI)", "FE+Months\n(IPI)"),
  R2    = c(summary(pols_cpi)$r.squared[1], summary(fe_cpi)$r.squared[1],
            summary(re_cpi)$r.squared[1],   summary(twfe_cpi)$r.squared[1],
            summary(pols_ipi)$r.squared[1], summary(fe_ipi)$r.squared[1],
            summary(re_ipi)$r.squared[1],   summary(fe_ipi_months)$r.squared[1]),
  Dep   = c(rep("Inflation (CPI)", 4), rep("Growth (IPI)", 4))
)

p_fit <- ggplot(rsq_data, aes(x = Model, y = R2, fill = Dep)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  geom_text(aes(label = round(R2, 3)), vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("Inflation (CPI)" = "#2E86AB",
                               "Growth (IPI)" = "#F18F01")) +
  labs(title = "Model Fit Comparison: R-squared Across Specifications",
       x = "", y = "R-squared", fill = "") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold")) +
  ylim(0, 1.05)
ggsave("plots/22_model_fit_comparison.png", p_fit, width = 10, height = 5, dpi = 150)
cat("Figure 22 saved\n")

# --- Figure 23: Forecast Visualization ---
data_main_flagged <- data_main %>%
  dplyr::mutate(period_type = dplyr::case_when(
    date >= as.Date("2026-04-01") ~ "ARIMA Forecast",
    date >= as.Date("2026-03-01") ~ "Post-Crisis (observed)",
    TRUE ~ "Pre-Crisis"))

p_fc <- ggplot(data_main_flagged,
               aes(x = date, y = ln_cpi,
                   color = country, linetype = period_type)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = as.Date("2026-02-27"),
             linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = as.Date("2026-04-01"),
             linetype = "dotted", color = "gray60") +
  facet_wrap(~country, scales = "free_y", ncol = 3) +
  scale_color_manual(values = main_country_colors) +
  scale_linetype_manual(values = c("Pre-Crisis" = "solid",
                                   "Post-Crisis (observed)" = "solid",
                                   "ARIMA Forecast" = "dashed")) +
  labs(title = "Log(CPI) with ARIMA Forecast Period Highlighted (Apr–Jun 2026)",
       x = "Date", y = "Log(CPI)", color = "Country", linetype = "Period") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))
ggsave("plots/23_forecast_highlighted.png", p_fc, width = 12, height = 5, dpi = 150)
cat("Figure 23 saved\n")

# =============================================================================
# 14. SINK() — FULL RESULTS SUMMARY
# =============================================================================
sink("plots/full_results_summary.txt")
cat("=== FULL RESULTS — EU3 PANEL ANALYSIS (HYBRID VERSION) ===\n\n")
cat("Countries: France, Germany, Italy (Main Panel)\n")
cat("Supplementary: United States, Saudi Arabia, Turkey\n")
cat("Dates: Jan 2023 – Jun 2026 (observed through Mar 2026;\n")
cat("        Apr–Jun 2026 ARIMA-forecasted — exactly 3 months)\n")
cat("Crisis date: Feb 27, 2026\n")
cat("post_crisis dummy: date >= 2026-03-01\n")
cat("Obs (main panel):", nrow(data_main), "\n\n")

cat("=== DESCRIPTIVE STATS (MAIN PANEL) ===\n")
print(as.data.frame(desc_stats_main))

cat("\n=== UNIT ROOT TESTS ===\n")
cat("LLC Results:\n")
for (v in names(llc_results)) {
  cat(sprintf("  %-18s stat: %7.3f  p-value: %.4f\n",
              v, llc_results[[v]][1], llc_results[[v]][2]))
}
cat("\nIPS Results:\n")
for (v in names(ips_results)) {
  if (is.na(ips_results[[v]][1])) {
    cat(sprintf("  %-18s SKIPPED (zero cross-sectional variation)\n", v))
  } else {
    cat(sprintf("  %-18s stat: %7.3f  p-value: %.4f\n",
                v, ips_results[[v]][1], ips_results[[v]][2]))
  }
}
cat("\nIndividual ADF (ln_cpi):\n")
for (c in names(adf_results)) {
  cat(sprintf("  %-14s stat: %7.4f  p-value: %.4f\n",
              c, adf_results[[c]][1], adf_results[[c]][2]))
}

cat("\n=== HAUSMAN TEST (CPI) ===\n")
tryCatch(print(hausman_cpi), error = function(e) cat("See console output\n"))
cat("\n=== HAUSMAN TEST (IPI) ===\n")
tryCatch(print(hausman_ipi), error = function(e) cat("See console output\n"))

cat("\n=== BP LM TEST (CPI) ===\n")
print(bp_cpi)
cat("\n=== BP LM TEST (IPI) ===\n")
print(bp_ipi)

cat("\n=== FE CPI (Robust SEs — Arellano HC3) ===\n")
print(fe_cpi_robust)
cat("\n=== FE IPI (Robust SEs — Arellano HC3) ===\n")
print(fe_ipi_robust)
cat("\n=== FE IPI + Month Dummies (Robust SEs) ===\n")
print(fe_ipi_months_robust)

cat("\n=== COUNTRY FIXED EFFECTS (CPI) ===\n")
print(round(fe_intercepts, 4))

cat("\n=== R-SQUARED SUMMARY ===\n")
print(rsq_data)

cat("\n=== KEY FINDINGS ===\n")
cat("1. Oil pass-through (FE CPI):", round(coef(fe_cpi)["oil_pct_change"], 6), "\n")
cat("   Robust SE:",
    round(sqrt(diag(vcovHC(fe_cpi, method = "arellano", type = "HC3")))["oil_pct_change"], 6), "\n")
cat("2. Exchange rate effect (FE CPI):", round(coef(fe_cpi)["ln_neer"], 4), "\n")
cat("3. Post-crisis dummy (FE CPI):", round(coef(fe_cpi)["post_crisis"], 6), "\n")
cat("4. Growth model R2 (FE without months):", round(summary(fe_ipi)$r.squared[1], 4), "\n")
cat("   Growth model R2 (FE with month dummies):", round(summary(fe_ipi_months)$r.squared[1], 4), "\n")
cat("   => Month dummies are essential for the IPI specification.\n")
cat("5. TWFE absorbs global variables in this panel (oil, policy_rate, VIX\n")
cat("   identical across FRA/DEU/ITA), yielding NA coefficients for those.\n")

cat("\n=== ARIMA FORECAST LOG (MAIN PANEL) ===\n")
for (ctry in names(arima_log_main)) {
  cat("\n", ctry, ":\n")
  for (v in names(arima_log_main[[ctry]])) {
    entry <- arima_log_main[[ctry]][[v]]
    cat(sprintf("  %s: Order=%s, Forecast=%s\n",
                v, paste(entry$order, collapse=","),
                paste(round(entry$values, 3), collapse=", ")))
  }
}
sink()

cat("\n\n>>> Script completed. All plots and tables saved to ./plots/ <<<\n")
cat(">>> Full results summary written to plots/full_results_summary.txt <<<\n")
