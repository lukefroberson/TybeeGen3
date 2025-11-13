# ============================================================================
# ANALYZE MODEL PERFORMANCE AND TRAINING DATA
# Diagnose why the model is conservative and has low confidence
# ============================================================================

library(readxl)
library(dplyr)
library(randomForest)
library(lubridate)

cat("=== MODEL PERFORMANCE ANALYSIS ===\n\n")

# ============================================================================
# 1. LOAD AND ANALYZE TRAINING DATA
# ============================================================================

cat("1. LOADING AND ANALYZING TRAINING DATA\n")
cat("=" , rep("=", 50), "\n", sep="")

# Load data files
data_files <- c(
  "NorthBeach.xlsx",
  "MiddleBeach.xlsx",
  "SouthBeach.xlsx",
  "PolkStreet.xlsx",
  "StrandStreet.xlsx"
)

all_data <- data.frame()

for (file in data_files) {
  if (file.exists(file)) {
    beach_data <- read_excel(file)
    all_data <- bind_rows(all_data, beach_data)
  }
}

cat(sprintf("Total rows loaded: %d\n\n", nrow(all_data)))

# Analyze enterococcus distribution
cat("ENTEROCOCCUS DISTRIBUTION:\n")
cat(sprintf("  Mean: %.1f CFU/100mL\n", mean(all_data$entero, na.rm=TRUE)))
cat(sprintf("  Median: %.1f CFU/100mL\n", median(all_data$entero, na.rm=TRUE)))
cat(sprintf("  Min: %.1f CFU/100mL\n", min(all_data$entero, na.rm=TRUE)))
cat(sprintf("  Max: %.1f CFU/100mL\n", max(all_data$entero, na.rm=TRUE)))
cat(sprintf("  SD: %.1f CFU/100mL\n\n", sd(all_data$entero, na.rm=TRUE)))

# Advisory rate
advisory_count <- sum(all_data$entero > 70, na.rm=TRUE)
total_count <- sum(!is.na(all_data$entero))
advisory_rate <- 100 * advisory_count / total_count

cat("ADVISORY STATUS:\n")
cat(sprintf("  Samples > 70 CFU/100mL: %d (%.1f%%)\n", advisory_count, advisory_rate))
cat(sprintf("  Samples ≤ 70 CFU/100mL: %d (%.1f%%)\n\n",
            total_count - advisory_count, 100 - advisory_rate))

# Percentiles
cat("PERCENTILES:\n")
percentiles <- quantile(all_data$entero, probs = c(0.25, 0.50, 0.75, 0.90, 0.95, 0.99), na.rm=TRUE)
for (i in seq_along(percentiles)) {
  cat(sprintf("  %s: %.1f CFU/100mL\n", names(percentiles)[i], percentiles[i]))
}
cat("\n")

# ============================================================================
# 2. ANALYZE EXTREME CONDITIONS
# ============================================================================

cat("2. ANALYZING HIGH BACTERIA CONDITIONS\n")
cat("=" , rep("=", 50), "\n", sep="")

# Find samples with high bacteria (>100 CFU/100mL)
high_bacteria <- all_data %>%
  filter(!is.na(entero), entero > 100) %>%
  arrange(desc(entero))

cat(sprintf("Samples with >100 CFU/100mL: %d\n", nrow(high_bacteria)))
cat(sprintf("Samples with >200 CFU/100mL: %d\n", sum(all_data$entero > 200, na.rm=TRUE)))
cat(sprintf("Samples with >500 CFU/100mL: %d\n\n", sum(all_data$entero > 500, na.rm=TRUE)))

if (nrow(high_bacteria) > 0) {
  cat("TOP 10 HIGHEST READINGS:\n")
  top_10 <- head(high_bacteria, 10)
  for (i in 1:nrow(top_10)) {
    cat(sprintf("  %d. %.1f CFU/100mL (Rain: %.2f in, Water Temp: %.1f°F, Salinity: %.1f ppt)\n",
                i,
                top_10$entero[i],
                ifelse(is.na(top_10$rain3day[i]), 0, top_10$rain3day[i]),
                ifelse(is.na(top_10$water_temp_avg_f[i]), NA, top_10$water_temp_avg_f[i]),
                ifelse(is.na(top_10$salinity[i]), NA, top_10$salinity[i])))
  }
  cat("\n")
}

# Analyze correlation with rainfall
cat("RAINFALL VS BACTERIA:\n")
with_rain <- all_data %>% filter(!is.na(entero), !is.na(rain3day))
cat(sprintf("  Correlation: %.3f\n", cor(with_rain$rain3day, with_rain$entero)))
cat(sprintf("  Mean entero with >2 in rain: %.1f CFU/100mL\n",
            mean(with_rain$entero[with_rain$rain3day > 2], na.rm=TRUE)))
cat(sprintf("  Mean entero with <0.5 in rain: %.1f CFU/100mL\n\n",
            mean(with_rain$entero[with_rain$rain3day < 0.5], na.rm=TRUE)))

# ============================================================================
# 3. LOAD AND ANALYZE TRAINED MODEL
# ============================================================================

cat("3. ANALYZING TRAINED MODEL\n")
cat("=" , rep("=", 50), "\n", sep="")

if (file.exists("tybee_advisory_model.rds")) {
  model <- readRDS("tybee_advisory_model.rds")

  cat("MODEL INFORMATION:\n")
  cat(sprintf("  Type: %s\n", class(model)[1]))
  cat(sprintf("  Number of trees: %d\n", model$ntree))
  cat(sprintf("  Variables tried at each split: %d\n", model$mtry))
  cat(sprintf("  Out-of-bag MSE: %.2f\n", tail(model$mse, 1)))
  cat(sprintf("  Out-of-bag RMSE: %.2f CFU/100mL\n", sqrt(tail(model$mse, 1))))
  cat(sprintf("  Percent variance explained: %.1f%%\n\n",
              tail(model$rsq, 1) * 100))

  # Variable importance
  cat("VARIABLE IMPORTANCE (Top 10):\n")
  importance_scores <- importance(model)[, 1]
  importance_sorted <- sort(importance_scores, decreasing = TRUE)
  for (i in 1:min(10, length(importance_sorted))) {
    cat(sprintf("  %d. %s: %.2f\n", i, names(importance_sorted)[i], importance_sorted[i]))
  }
  cat("\n")

  # Analyze prediction range
  cat("MODEL PREDICTION RANGE:\n")
  cat(sprintf("  Minimum predicted value: %.1f CFU/100mL\n", min(model$predicted)))
  cat(sprintf("  Maximum predicted value: %.1f CFU/100mL\n", max(model$predicted)))
  cat(sprintf("  Mean predicted value: %.1f CFU/100mL\n", mean(model$predicted)))
  cat(sprintf("  Predictions >70 CFU/100mL: %d (%.1f%%)\n\n",
              sum(model$predicted > 70),
              100 * sum(model$predicted > 70) / length(model$predicted)))

} else {
  cat("❌ Model file not found!\n\n")
}

# ============================================================================
# 4. RECOMMENDATIONS
# ============================================================================

cat("4. RECOMMENDATIONS\n")
cat("=" , rep("=", 50), "\n", sep="")

if (advisory_rate < 10) {
  cat("⚠️  ISSUE: Very low advisory rate (", advisory_rate, "%)\n", sep="")
  cat("  → Model has few positive examples to learn from\n")
  cat("  → Consider:\n")
  cat("     • Using classification instead of regression\n")
  cat("     • Applying class weights to emphasize rare advisories\n")
  cat("     • Using a lower threshold or different approach\n\n")
}

if (exists("model") && tail(model$rsq, 1) < 0.3) {
  cat("⚠️  ISSUE: Low R² (", round(tail(model$rsq, 1), 3), ")\n", sep="")
  cat("  → Model explains little variance in the data\n")
  cat("  → Consider:\n")
  cat("     • Adding more relevant predictors\n")
  cat("     • Engineering interaction features\n")
  cat("     • Using a different modeling approach\n\n")
}

cat("SUGGESTED IMPROVEMENTS:\n")
cat("  1. Train a CLASSIFICATION model (advisory yes/no)\n")
cat("  2. Use class weights to handle imbalanced data\n")
cat("  3. Tune hyperparameters (ntree, mtry, node size)\n")
cat("  4. Consider ensemble of models\n")
cat("  5. Add feature engineering (interactions, lags)\n\n")

cat("=== ANALYSIS COMPLETE ===\n")
