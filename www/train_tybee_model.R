# ============================================================================
# TYBEE ISLAND WATER QUALITY MODEL TRAINING
# Comprehensive Random Forest Model with Proper Variable Types
# ============================================================================

cat("=== TYBEE ISLAND ENTEROCOCCUS PREDICTION MODEL ===\n\n")

# Load required libraries
library(readxl)
library(dplyr)
library(randomForest)
library(lubridate)
library(ggplot2)

# ============================================================================
# 1. LOAD AND COMBINE DATA
# ============================================================================

cat("1. LOADING DATA FROM ALL BEACHES...\n")

# Define file paths
data_files <- c(
  "NorthBeach.xlsx",
  "MiddleBeach.xlsx", 
  "SouthBeach.xlsx",
  "PolkStreet.xlsx",
  "StrandStreet.xlsx"
)

# Load all data
all_data <- data.frame()

for (file in data_files) {
  cat(sprintf("  Loading %s...", file))
  
  tryCatch({
    beach_data <- read_excel(file)
    all_data <- bind_rows(all_data, beach_data)
    cat(sprintf(" %d rows\n", nrow(beach_data)))
  }, error = function(e) {
    cat(sprintf(" ERROR: %s\n", e$message))
  })
}

cat(sprintf("✓ Total data loaded: %d rows\n\n", nrow(all_data)))

# ============================================================================
# 2. DATA PREPARATION AND CLEANING
# ============================================================================

cat("2. PREPARING DATA...\n")

# Create clean dataset with proper variable types
model_data <- all_data %>%
  mutate(
    # === BEACH NAME: Convert to simple format ===
    beach = case_when(
      grepl("NORTH", beach_name, ignore.case = TRUE) ~ "North",
      grepl("MIDDLE", beach_name, ignore.case = TRUE) ~ "Middle",
      grepl("SOUTH", beach_name, ignore.case = TRUE) ~ "South",
      grepl("POLK", beach_name, ignore.case = TRUE) ~ "Polk",
      grepl("STRAND", beach_name, ignore.case = TRUE) ~ "Strand",
      TRUE ~ as.character(beach_name)
    ),
    
    # === TEMPORAL VARIABLES ===
    date = as.Date(date_collected),
    month = as.numeric(month(date)),
    day_of_year = as.numeric(yday(date)),
    year = year(date),
    
    # === SEASON: Create factor variable ===
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall"
    ),
    season_f = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
    
    # === NUMERIC VARIABLES: Ensure they're numeric ===
    rain_3day = as.numeric(rain3day),
    maxtemp_f = as.numeric(maxtemp_f),
    water_temp_avg_f = as.numeric(water_temp_avg_f),
    conductivity = as.numeric(conductivity),
    do = as.numeric(do),
    ph = as.numeric(ph),
    salinity = as.numeric(salinity),
    turbidity = as.numeric(turbidity),
    entero = as.numeric(entero),
    
    # === DERIVED VARIABLE: Air-Water Temperature Difference ===
    air_water_diff = maxtemp_f - water_temp_avg_f,
    
    # === TARGET VARIABLE: Advisory status (>70 CFU/100mL) ===
    advisory = ifelse(entero > 70, 1, 0)
  ) %>%
  # === CATEGORICAL VARIABLES: Convert to factors ===
  mutate(
    beach = factor(beach, levels = c("North", "Middle", "South", "Polk", "Strand")),
    tide_stage = factor(tide_stage),
    wind_direction = factor(wind_direction)
  ) %>%
  # Select only the variables we need
  select(
    beach, date, month, day_of_year, season_f,
    rain_3day, maxtemp_f, water_temp_avg_f, air_water_diff,
    conductivity, do, ph, salinity, turbidity,
    tide_stage, wind_direction,
    entero, advisory
  ) %>%
  # Remove rows with missing critical variables
  filter(
    !is.na(beach),
    !is.na(entero),
    !is.na(rain_3day),
    !is.na(maxtemp_f),
    !is.na(water_temp_avg_f),
    !is.na(season_f)
  )

cat(sprintf("✓ Clean dataset: %d rows\n", nrow(model_data)))
cat(sprintf("✓ Date range: %s to %s\n", min(model_data$date), max(model_data$date)))
cat(sprintf("✓ Advisory rate: %.1f%%\n\n", mean(model_data$advisory) * 100))

# ============================================================================
# 3. VERIFY DATA TYPES
# ============================================================================

cat("3. VERIFYING VARIABLE TYPES...\n")

# Check factor variables
factor_vars <- c("beach", "season_f", "tide_stage", "wind_direction")
cat("\nFACTOR VARIABLES:\n")
for (var in factor_vars) {
  if (var %in% names(model_data)) {
    cat(sprintf("  %s: %s (levels: %s)\n", 
                var, 
                class(model_data[[var]]),
                paste(levels(model_data[[var]]), collapse = ", ")))
  }
}

# Check numeric variables
numeric_vars <- c("rain_3day", "maxtemp_f", "water_temp_avg_f", "air_water_diff",
                  "conductivity", "do", "ph", "salinity", "turbidity", 
                  "month", "day_of_year")
cat("\nNUMERIC VARIABLES:\n")
for (var in numeric_vars) {
  if (var %in% names(model_data)) {
    cat(sprintf("  %s: %s (range: %.2f to %.2f)\n",
                var,
                class(model_data[[var]]),
                min(model_data[[var]], na.rm = TRUE),
                max(model_data[[var]], na.rm = TRUE)))
  }
}
cat("\n")

# ============================================================================
# 4. SPLIT DATA INTO TRAINING AND TESTING
# ============================================================================

cat("4. SPLITTING DATA (80% TRAIN / 20% TEST)...\n")

set.seed(42)  # For reproducibility

# Stratified split to maintain advisory rate balance
train_indices <- c()
test_indices <- c()

# Split within each beach to ensure representation
for (beach_name in levels(model_data$beach)) {
  beach_indices <- which(model_data$beach == beach_name)
  n_beach <- length(beach_indices)
  n_train <- floor(n_beach * 0.8)
  
  sampled_indices <- sample(beach_indices, size = n_beach)
  train_indices <- c(train_indices, sampled_indices[1:n_train])
  test_indices <- c(test_indices, sampled_indices[(n_train+1):n_beach])
}

train_data <- model_data[train_indices, ]
test_data <- model_data[test_indices, ]

cat(sprintf("✓ Training set: %d rows (%.1f%% advisory)\n", 
            nrow(train_data), mean(train_data$advisory) * 100))
cat(sprintf("✓ Testing set: %d rows (%.1f%% advisory)\n\n", 
            nrow(test_data), mean(test_data$advisory) * 100))

# ============================================================================
# 5. TRAIN RANDOM FOREST MODEL
# ============================================================================

cat("5. TRAINING RANDOM FOREST MODEL...\n")

# Define formula with all predictors
model_formula <- entero ~ beach + rain_3day + maxtemp_f + water_temp_avg_f + 
                          air_water_diff + month + season_f + 
                          conductivity + do + ph + salinity + turbidity

# Train Random Forest
set.seed(123)
rf_model <- randomForest(
  model_formula,
  data = train_data,
  ntree = 500,              # Number of trees
  mtry = 4,                 # Number of variables at each split
  importance = TRUE,        # Calculate variable importance
  na.action = na.omit,      # Handle missing values
  keep.forest = TRUE        # Keep the forest for prediction
)

cat("\n")
print(rf_model)
cat("\n")

# ============================================================================
# 6. VALIDATE MODEL PERFORMANCE
# ============================================================================

cat("6. VALIDATING MODEL PERFORMANCE...\n\n")

# Predictions on training data
train_pred <- predict(rf_model, train_data)
train_rmse <- sqrt(mean((train_pred - train_data$entero)^2, na.rm = TRUE))
train_mae <- mean(abs(train_pred - train_data$entero), na.rm = TRUE)
train_r2 <- cor(train_pred, train_data$entero, use = "complete.obs")^2

# Predictions on test data
test_pred <- predict(rf_model, test_data)
test_rmse <- sqrt(mean((test_pred - test_data$entero)^2, na.rm = TRUE))
test_mae <- mean(abs(test_pred - test_data$entero), na.rm = TRUE)
test_r2 <- cor(test_pred, test_data$entero, use = "complete.obs")^2

cat("REGRESSION METRICS:\n")
cat(sprintf("  Training RMSE: %.2f CFU/100mL\n", train_rmse))
cat(sprintf("  Testing RMSE:  %.2f CFU/100mL\n", test_rmse))
cat(sprintf("  Training MAE:  %.2f CFU/100mL\n", train_mae))
cat(sprintf("  Testing MAE:   %.2f CFU/100mL\n", test_mae))
cat(sprintf("  Training R²:   %.3f\n", train_r2))
cat(sprintf("  Testing R²:    %.3f\n\n", test_r2))

# Advisory classification metrics
train_pred_advisory <- ifelse(train_pred > 70, 1, 0)
test_pred_advisory <- ifelse(test_pred > 70, 1, 0)

train_accuracy <- mean(train_pred_advisory == train_data$advisory, na.rm = TRUE)
test_accuracy <- mean(test_pred_advisory == test_data$advisory, na.rm = TRUE)

# Confusion matrix for test set
conf_matrix <- table(Predicted = test_pred_advisory, Actual = test_data$advisory)
sensitivity <- conf_matrix[2,2] / sum(conf_matrix[,2])
specificity <- conf_matrix[1,1] / sum(conf_matrix[,1])

cat("CLASSIFICATION METRICS (Advisory vs No Advisory):\n")
cat(sprintf("  Training Accuracy: %.1f%%\n", train_accuracy * 100))
cat(sprintf("  Testing Accuracy:  %.1f%%\n", test_accuracy * 100))
cat(sprintf("  Sensitivity:       %.1f%% (true positives)\n", sensitivity * 100))
cat(sprintf("  Specificity:       %.1f%% (true negatives)\n\n", specificity * 100))

cat("CONFUSION MATRIX (Test Set):\n")
print(conf_matrix)
cat("\n")

# Variable importance
cat("VARIABLE IMPORTANCE:\n")
importance_df <- data.frame(
  Variable = rownames(importance(rf_model)),
  Importance = importance(rf_model)[, 1]
) %>%
  arrange(desc(Importance)) %>%
  mutate(Importance = round(Importance, 2))

print(importance_df)
cat("\n")

# ============================================================================
# 7. CREATE VALIDATION PLOTS
# ============================================================================

cat("7. CREATING VALIDATION PLOTS...\n")

# Create validation data frame
validation_data <- data.frame(
  Beach = test_data$beach,
  Date = test_data$date,
  Actual = test_data$entero,
  Predicted = test_pred,
  Residual = test_data$entero - test_pred,
  Advisory_Actual = test_data$advisory,
  Advisory_Predicted = test_pred_advisory
)

# Plot 1: Predicted vs Actual
p1 <- ggplot(validation_data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, aes(color = Beach)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 70, linetype = "dotted", color = "blue") +
  geom_vline(xintercept = 70, linetype = "dotted", color = "blue") +
  labs(
    title = "Model Predictions vs Actual Values",
    subtitle = sprintf("Test Set (n=%d, R² = %.3f)", nrow(validation_data), test_r2),
    x = "Actual Enterococcus (CFU/100mL)",
    y = "Predicted Enterococcus (CFU/100mL)"
  ) +
  theme_minimal() +
  annotate("text", x = 300, y = 70, label = "Advisory Threshold", 
           color = "blue", hjust = 0, vjust = -0.5)

ggsave("validation_predicted_vs_actual.png", p1, width = 10, height = 6, dpi = 300)

# Plot 2: Residuals by Beach
p2 <- ggplot(validation_data, aes(x = Beach, y = Residual, fill = Beach)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Model Residuals by Beach",
    subtitle = "Test Set Performance",
    x = "Beach",
    y = "Residual (Actual - Predicted)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("validation_residuals_by_beach.png", p2, width = 10, height = 6, dpi = 300)

# Plot 3: Time series of predictions
p3 <- ggplot(validation_data %>% filter(Beach == "South"), 
             aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), alpha = 0.7) +
  geom_line(aes(y = Predicted, color = "Predicted"), alpha = 0.7) +
  geom_hline(yintercept = 70, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "Predictions Over Time (South Beach Example)",
    subtitle = "Test Set",
    x = "Date",
    y = "Enterococcus (CFU/100mL)",
    color = "Type"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "orange"))

ggsave("validation_time_series.png", p3, width = 12, height = 6, dpi = 300)

cat("✓ Validation plots saved\n\n")

# ============================================================================
# 8. SAVE MODEL AND METADATA
# ============================================================================

cat("8. SAVING MODEL AND METADATA...\n")

# Save the Random Forest model
saveRDS(rf_model, "tybee_advisory_model.rds")
cat("✓ Saved: tybee_advisory_model.rds\n")

# Create and save metadata
metadata <- list(
  model_type = "randomForest",
  training_date = Sys.Date(),
  n_observations_train = nrow(train_data),
  n_observations_test = nrow(test_data),
  date_range = c(min(model_data$date), max(model_data$date)),
  features_used = rownames(importance(rf_model)),
  performance = list(
    train_rmse = train_rmse,
    test_rmse = test_rmse,
    train_r2 = train_r2,
    test_r2 = test_r2,
    test_accuracy = test_accuracy,
    sensitivity = sensitivity,
    specificity = specificity
  ),
  variable_importance = importance_df,
  beach_levels = levels(model_data$beach),
  season_levels = levels(model_data$season_f),
  ntree = 500,
  mtry = 4
)

saveRDS(metadata, "tybee_advisory_metadata.rds")
cat("✓ Saved: tybee_advisory_metadata.rds\n")

# Save validation dataset
saveRDS(validation_data, "validation_data.rds")
cat("✓ Saved: validation_data.rds\n\n")

# ============================================================================
# 9. SUMMARY
# ============================================================================

cat("=== MODEL TRAINING COMPLETE ===\n\n")

cat("📊 FINAL MODEL SUMMARY:\n")
cat(sprintf("  • Model Type: Random Forest (%d trees)\n", 500))
cat(sprintf("  • Training Data: %d observations\n", nrow(train_data)))
cat(sprintf("  • Testing Data: %d observations\n", nrow(test_data)))
cat(sprintf("  • Test RMSE: %.2f CFU/100mL\n", test_rmse))
cat(sprintf("  • Test R²: %.3f\n", test_r2))
cat(sprintf("  • Advisory Accuracy: %.1f%%\n\n", test_accuracy * 100))

cat("📁 FILES CREATED:\n")
cat("  • tybee_advisory_model.rds - Random Forest model\n")
cat("  • tybee_advisory_metadata.rds - Model metadata and performance\n")
cat("  • validation_data.rds - Test set predictions\n")
cat("  • validation_predicted_vs_actual.png - Scatter plot\n")
cat("  • validation_residuals_by_beach.png - Residual analysis\n")
cat("  • validation_time_series.png - Time series example\n\n")

cat("✅ MODEL READY FOR USE IN TAB 4!\n")
cat("Copy tybee_advisory_model.rds to your models/ directory\n\n")

cat("🎯 TOP 5 MOST IMPORTANT VARIABLES:\n")
for (i in 1:min(5, nrow(importance_df))) {
  cat(sprintf("  %d. %s (%.2f)\n", i, 
              importance_df$Variable[i], 
              importance_df$Importance[i]))
}

cat("\n=== SCRIPT COMPLETE ===\n")
