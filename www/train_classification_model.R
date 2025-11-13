# ============================================================================
# CLASSIFICATION MODEL FOR TYBEE ISLAND BACTERIA ADVISORIES
# Predicts binary outcome: Advisory (>70) vs No Advisory (≤70)
# Uses class weights to handle the 3.5% imbalance
# ============================================================================

cat("=== TYBEE ISLAND ADVISORY CLASSIFICATION MODEL ===\n\n")

# Load required libraries
library(readxl)
library(dplyr)
library(randomForest)
library(lubridate)

# ============================================================================
# 1. LOAD DATA
# ============================================================================

cat("1. LOADING DATA FROM ALL BEACHES...\n")

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
    cat(sprintf("  ✓ %s: %d rows\n", file, nrow(beach_data)))
  }
}

cat(sprintf("✓ Total loaded: %d rows\n\n", nrow(all_data)))

# ============================================================================
# 2. DATA PREPARATION
# ============================================================================

cat("2. PREPARING DATA...\n")

model_data <- all_data %>%
  mutate(
    # Beach names
    beach = case_when(
      grepl("NORTH", beach_name, ignore.case = TRUE) ~ "North",
      grepl("MIDDLE", beach_name, ignore.case = TRUE) ~ "Middle",
      grepl("SOUTH", beach_name, ignore.case = TRUE) ~ "South",
      grepl("POLK", beach_name, ignore.case = TRUE) ~ "Polk",
      grepl("STRAND", beach_name, ignore.case = TRUE) ~ "Strand",
      TRUE ~ as.character(beach_name)
    ),
    beach = factor(beach, levels = c("North", "Middle", "South", "Polk", "Strand")),

    # Temporal variables
    date = as.Date(date_collected),
    month = as.numeric(month(date)),

    # Season
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall"
    ),
    season_f = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),

    # Numeric variables
    rain_3day = as.numeric(rain3day),
    maxtemp_f = as.numeric(maxtemp_f),
    water_temp_avg_f = as.numeric(water_temp_avg_f),
    conductivity = as.numeric(conductivity),
    do = as.numeric(do),
    ph = as.numeric(ph),
    salinity = as.numeric(salinity),
    turbidity = as.numeric(turbidity),
    entero = as.numeric(entero),

    # Derived variable
    air_water_diff = maxtemp_f - water_temp_avg_f,

    # BINARY OUTCOME: Advisory status
    advisory = factor(ifelse(entero > 70, "Yes", "No"), levels = c("No", "Yes"))
  ) %>%
  select(
    beach, date, month, season_f,
    rain_3day, maxtemp_f, water_temp_avg_f, air_water_diff,
    conductivity, do, ph, salinity, turbidity,
    entero, advisory
  ) %>%
  filter(
    !is.na(beach),
    !is.na(season_f),
    !is.na(advisory),
    !is.na(rain_3day),
    !is.na(maxtemp_f),
    !is.na(water_temp_avg_f),
    !is.na(air_water_diff),
    !is.na(month),
    !is.na(conductivity),
    !is.na(do),
    !is.na(ph),
    !is.na(salinity),
    !is.na(turbidity)
  )

cat(sprintf("✓ Clean dataset: %d rows\n", nrow(model_data)))

# Check class balance
advisory_counts <- table(model_data$advisory)
cat(sprintf("\nClass distribution:\n"))
cat(sprintf("  No Advisory: %d (%.1f%%)\n",
            advisory_counts["No"],
            100 * advisory_counts["No"] / sum(advisory_counts)))
cat(sprintf("  Advisory: %d (%.1f%%)\n\n",
            advisory_counts["Yes"],
            100 * advisory_counts["Yes"] / sum(advisory_counts)))

# ============================================================================
# 3. TRAIN-TEST SPLIT (STRATIFIED)
# ============================================================================

cat("3. STRATIFIED TRAIN-TEST SPLIT...\n")

set.seed(42)

# Stratified split to maintain class balance
advisory_idx <- which(model_data$advisory == "Yes")
no_advisory_idx <- which(model_data$advisory == "No")

# Sample 80% from each class
train_advisory <- sample(advisory_idx, floor(0.8 * length(advisory_idx)))
train_no_advisory <- sample(no_advisory_idx, floor(0.8 * length(no_advisory_idx)))

train_indices <- c(train_advisory, train_no_advisory)
test_indices <- setdiff(1:nrow(model_data), train_indices)

train_data <- model_data[train_indices, ]
test_data <- model_data[test_indices, ]

cat(sprintf("Training set: %d rows\n", nrow(train_data)))
cat(sprintf("  Advisory: %d (%.1f%%)\n",
            sum(train_data$advisory == "Yes"),
            100 * sum(train_data$advisory == "Yes") / nrow(train_data)))
cat(sprintf("Testing set: %d rows\n", nrow(test_data)))
cat(sprintf("  Advisory: %d (%.1f%%)\n\n",
            sum(test_data$advisory == "Yes"),
            100 * sum(test_data$advisory == "Yes") / nrow(test_data)))

# ============================================================================
# 4. TRAIN CLASSIFICATION RANDOM FOREST
# ============================================================================

cat("4. TRAINING CLASSIFICATION RANDOM FOREST...\n")

# Define predictors
predictor_cols <- c("beach", "rain_3day", "maxtemp_f", "water_temp_avg_f",
                    "air_water_diff", "month", "season_f",
                    "conductivity", "do", "ph", "salinity", "turbidity")

X_train <- as.data.frame(train_data[, predictor_cols])
y_train <- train_data$advisory

# Calculate class weights to handle imbalance
# Give EXTREMELY high weight to the minority class (advisory)
n_no <- sum(y_train == "No")
n_yes <- sum(y_train == "Yes")
base_weight_ratio <- n_no / n_yes

# MULTIPLY by 10 to be EXTREMELY aggressive (much higher than before!)
weight_ratio <- base_weight_ratio * 10

cat(sprintf("Base class weight ratio: %.1f\n", base_weight_ratio))
cat(sprintf("Adjusted weight ratio: %.1f (giving %.1fx weight to Advisory)\n\n",
            weight_ratio, weight_ratio))

# Train classification Random Forest with EXTREMELY aggressive settings
# Note: With R²=4%, environmental variables barely predict bacteria
# So we compensate by being very trigger-happy on advisories
set.seed(123)
rf_model <- randomForest(
  x = X_train,
  y = y_train,
  ntree = 500,
  mtry = 4,
  importance = TRUE,
  classwt = c("No" = 1, "Yes" = weight_ratio),  # Extreme weight for minority class
  cutoff = c("No" = 0.92, "Yes" = 0.08),  # EXTREMELY low threshold (8% probability triggers advisory!)
  keep.forest = TRUE
)

cat("✓ Training complete\n\n")
print(rf_model)
cat("\n")

# ============================================================================
# 5. EVALUATE MODEL
# ============================================================================

cat("5. MODEL EVALUATION...\n")

# Predictions on test set
X_test <- as.data.frame(test_data[, predictor_cols])
test_pred_class <- predict(rf_model, X_test, type = "response")
test_pred_prob <- predict(rf_model, X_test, type = "prob")

# Confusion matrix
conf_matrix <- table(Predicted = test_pred_class, Actual = test_data$advisory)
cat("\nCONFUSION MATRIX:\n")
print(conf_matrix)
cat("\n")

# Calculate metrics safely (handle division by zero)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Sensitivity: Of all real advisories, how many did we catch?
if (sum(conf_matrix[, "Yes"]) > 0) {
  sensitivity <- conf_matrix["Yes", "Yes"] / sum(conf_matrix[, "Yes"])
} else {
  sensitivity <- 0
}

# Specificity: Of all real non-advisories, how many did we correctly identify?
if (sum(conf_matrix[, "No"]) > 0) {
  specificity <- conf_matrix["No", "No"] / sum(conf_matrix[, "No"])
} else {
  specificity <- 0
}

# Precision: Of all our advisory predictions, how many were correct?
if (sum(conf_matrix["Yes", ]) > 0) {
  precision <- as.numeric(conf_matrix["Yes", "Yes"]) / as.numeric(sum(conf_matrix["Yes", ]))
} else {
  precision <- NA
}

# F1 score (harmonic mean of precision and recall)
if (!is.na(precision) && !is.na(sensitivity) && (precision + sensitivity) > 0) {
  f1 <- 2 * (as.numeric(precision) * as.numeric(sensitivity)) / (as.numeric(precision) + as.numeric(sensitivity))
} else {
  f1 <- NA
}

cat("PERFORMANCE METRICS:\n")
cat(sprintf("  Accuracy: %.1f%%\n", as.numeric(accuracy) * 100))
cat(sprintf("  Sensitivity (catches real advisories): %.1f%%\n", as.numeric(sensitivity) * 100))
cat(sprintf("  Specificity (correct non-advisories): %.1f%%\n", as.numeric(specificity) * 100))

# Safe precision output
if (!is.na(precision) && length(precision) == 1) {
  cat(sprintf("  Precision (% of advisory predictions that are correct): %.1f%%\n", as.numeric(precision) * 100))
} else {
  cat("  Precision: N/A (no advisory predictions made)\n")
}

# Safe F1 output
if (!is.na(f1) && length(f1) == 1) {
  cat(sprintf("  F1 Score: %.3f\n\n", as.numeric(f1)))
} else {
  cat("  F1 Score: N/A\n\n")
}

# Variable importance
cat("VARIABLE IMPORTANCE:\n")
importance_scores <- importance(rf_model)[, "MeanDecreaseGini"]
importance_sorted <- sort(importance_scores, decreasing = TRUE)
for (i in 1:min(10, length(importance_sorted))) {
  cat(sprintf("  %d. %s: %.2f\n", i, names(importance_sorted)[i], importance_sorted[i]))
}
cat("\n")

# ============================================================================
# 6. SAVE MODEL
# ============================================================================

cat("6. SAVING MODEL...\n")

saveRDS(rf_model, "tybee_advisory_classification_model.rds")
cat("✓ Saved: tybee_advisory_classification_model.rds\n")

metadata <- list(
  model_type = "randomForest_classification",
  training_date = Sys.Date(),
  n_train = nrow(train_data),
  n_test = nrow(test_data),
  predictors = predictor_cols,
  response = "advisory (binary)",
  class_weight_ratio = weight_ratio,
  performance = list(
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    f1_score = f1
  )
)

saveRDS(metadata, "tybee_advisory_classification_metadata.rds")
cat("✓ Saved: tybee_advisory_classification_metadata.rds\n\n")

# ============================================================================
# 7. RECOMMENDATIONS
# ============================================================================

cat("=== MODEL TRAINING COMPLETE ===\n\n")

cat("INTERPRETATION:\n")
if (sensitivity < 0.5) {
  cat("  ⚠️  Low sensitivity - model misses many real advisories\n")
  cat("  Consider: Increase class weight or lower cutoff threshold\n\n")
} else if (sensitivity >= 0.7) {
  cat("  ✓ Good sensitivity - catches most real advisories\n\n")
}

if (specificity < 0.8) {
  cat("  ⚠️  Low specificity - too many false positives\n")
  cat("  Consider: Decrease class weight or raise cutoff threshold\n\n")
} else {
  cat("  ✓ Good specificity - few false alarms\n\n")
}

cat("NEXT STEPS:\n")
cat("  1. Copy model to models/ directory:\n")
cat("     cp tybee_advisory_classification_model.rds ../models/\n")
cat("  2. Update Tab 4 to use classification predictions\n")
cat("  3. Test different cutoff thresholds if needed\n\n")
