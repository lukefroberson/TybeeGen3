# ============================================================================
# DIAGNOSTIC MODEL TRAINING - WITH DETAILED CHECKS
# This version shows you what's happening at each step
# ============================================================================

cat("=== TYBEE ISLAND MODEL TRAINING - DIAGNOSTIC MODE ===\n\n")

# Load required libraries
library(readxl)
library(dplyr)
library(randomForest)
library(lubridate)

# ============================================================================
# STEP 1: LOAD DATA
# ============================================================================

cat("STEP 1: LOADING DATA\n")
cat("====================\n")

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
  } else {
    cat(sprintf("  ✗ %s: NOT FOUND\n", file))
  }
}

cat(sprintf("\n✓ Total loaded: %d rows\n", nrow(all_data)))

# Check initial column types
cat("\nInitial column types (first 10):\n")
for (i in 1:min(10, ncol(all_data))) {
  col_name <- names(all_data)[i]
  col_type <- class(all_data[[col_name]])[1]
  cat(sprintf("  %s: %s\n", col_name, col_type))
}

# ============================================================================
# STEP 2: DATA PREPARATION
# ============================================================================

cat("\nSTEP 2: DATA PREPARATION\n")
cat("========================\n")

cat("\nCreating model_data with explicit type conversions...\n")

# Do conversion in steps to see where it might fail
model_data <- all_data

# Step 2a: Beach names
cat("\n2a. Converting beach names...\n")
model_data$beach <- case_when(
  grepl("NORTH", toupper(model_data$beach_name)) ~ "North",
  grepl("MIDDLE", toupper(model_data$beach_name)) ~ "Middle",
  grepl("SOUTH", toupper(model_data$beach_name)) ~ "South",
  grepl("POLK", toupper(model_data$beach_name)) ~ "Polk",
  grepl("STRAND", toupper(model_data$beach_name)) ~ "Strand",
  TRUE ~ as.character(model_data$beach_name)
)
cat(sprintf("  Unique beaches: %s\n", paste(unique(model_data$beach), collapse = ", ")))

# Step 2b: Temporal variables
cat("\n2b. Creating temporal variables...\n")
model_data$date <- as.Date(model_data$date_collected)
model_data$month <- as.numeric(month(model_data$date))
model_data$day_of_year <- as.numeric(yday(model_data$date))
cat(sprintf("  Month range: %d to %d\n", min(model_data$month, na.rm=TRUE), max(model_data$month, na.rm=TRUE)))

# Step 2c: Season
cat("\n2c. Creating season...\n")
model_data$season <- case_when(
  model_data$month %in% c(12, 1, 2) ~ "Winter",
  model_data$month %in% c(3, 4, 5) ~ "Spring",
  model_data$month %in% c(6, 7, 8) ~ "Summer",
  model_data$month %in% c(9, 10, 11) ~ "Fall"
)
cat(sprintf("  Seasons: %s\n", paste(unique(model_data$season), collapse = ", ")))

# Step 2d: CRITICAL - Numeric conversions
cat("\n2d. Converting numeric variables...\n")
cat("  BEFORE conversion - checking types:\n")

# Check rain3day before conversion
cat(sprintf("    rain3day: %s (sample: %s)\n", 
            class(all_data$rain3day)[1],
            paste(head(all_data$rain3day, 3), collapse = ", ")))

# Now convert
model_data$rain_3day <- as.numeric(model_data$rain3day)
model_data$maxtemp_f <- as.numeric(model_data$maxtemp_f)
model_data$water_temp_avg_f <- as.numeric(model_data$water_temp_avg_f)
model_data$conductivity <- as.numeric(model_data$conductivity)
model_data$do <- as.numeric(model_data$do)
model_data$ph <- as.numeric(model_data$ph)
model_data$salinity <- as.numeric(model_data$salinity)
model_data$turbidity <- as.numeric(model_data$turbidity)
model_data$entero <- as.numeric(model_data$entero)

cat("  AFTER conversion - checking types:\n")
cat(sprintf("    rain_3day: %s (sample: %s)\n", 
            class(model_data$rain_3day)[1],
            paste(head(model_data$rain_3day, 3), collapse = ", ")))
cat(sprintf("    maxtemp_f: %s (sample: %s)\n", 
            class(model_data$maxtemp_f)[1],
            paste(head(model_data$maxtemp_f, 3), collapse = ", ")))
cat(sprintf("    water_temp_avg_f: %s (sample: %s)\n", 
            class(model_data$water_temp_avg_f)[1],
            paste(head(model_data$water_temp_avg_f, 3), collapse = ", ")))

# Step 2e: Derived variable
cat("\n2e. Creating air_water_diff...\n")
model_data$air_water_diff <- model_data$maxtemp_f - model_data$water_temp_avg_f
cat(sprintf("  Type: %s (sample: %s)\n", 
            class(model_data$air_water_diff)[1],
            paste(head(model_data$air_water_diff, 3), collapse = ", ")))

# Step 2f: Create factors
cat("\n2f. Creating factor variables...\n")
model_data$beach <- factor(model_data$beach, levels = c("North", "Middle", "South", "Polk", "Strand"))
model_data$season_f <- factor(model_data$season, levels = c("Winter", "Spring", "Summer", "Fall"))

cat(sprintf("  beach: %s with %d levels\n", class(model_data$beach)[1], length(levels(model_data$beach))))
cat(sprintf("  season_f: %s with %d levels\n", class(model_data$season_f)[1], length(levels(model_data$season_f))))

# Step 2g: Select and clean
cat("\n2g. Selecting final columns...\n")
model_data_clean <- model_data %>%
  select(
    beach, date, month, day_of_year, season_f,
    rain_3day, maxtemp_f, water_temp_avg_f, air_water_diff,
    conductivity, do, ph, salinity, turbidity,
    entero
  ) %>%
  # Remove rows with NA in ANY predictor or response variable
  filter(
    !is.na(beach),
    !is.na(season_f),
    !is.na(entero),
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

cat(sprintf("  Clean dataset: %d rows\n", nrow(model_data_clean)))
cat(sprintf("  Rows removed due to missing values: %d\n", nrow(model_data) - nrow(model_data_clean)))

# ============================================================================
# STEP 3: VERIFY DATA TYPES BEFORE TRAINING
# ============================================================================

cat("\nSTEP 3: VERIFY DATA TYPES BEFORE TRAINING\n")
cat("==========================================\n")

numeric_vars <- c("rain_3day", "maxtemp_f", "water_temp_avg_f", "air_water_diff",
                  "conductivity", "do", "ph", "salinity", "turbidity", "month")
factor_vars <- c("beach", "season_f")

cat("\nNumeric variables (MUST be numeric):\n")
all_numeric <- TRUE
for (var in numeric_vars) {
  var_class <- class(model_data_clean[[var]])[1]
  is_numeric <- var_class %in% c("numeric", "integer")
  cat(sprintf("  %s: %s %s\n", var, var_class, ifelse(is_numeric, "✓", "✗ PROBLEM!")))
  if (!is_numeric) all_numeric <- FALSE
}

cat("\nFactor variables (MUST be factor):\n")
all_factors <- TRUE
for (var in factor_vars) {
  var_class <- class(model_data_clean[[var]])[1]
  is_factor <- var_class == "factor"
  cat(sprintf("  %s: %s %s\n", var, var_class, ifelse(is_factor, "✓", "✗ PROBLEM!")))
  if (!is_factor) all_factors <- FALSE
}

if (!all_numeric || !all_factors) {
  cat("\n❌ DATA TYPES ARE WRONG! Cannot proceed with training.\n")
  cat("Please check what went wrong above.\n")
  stop("Data types incorrect before training")
}

cat("\n✅ ALL DATA TYPES CORRECT! Ready to train.\n")

# ============================================================================
# STEP 4: TRAIN MODEL
# ============================================================================

cat("\nSTEP 4: TRAIN-TEST SPLIT\n")
cat("========================\n")

set.seed(42)
train_indices <- sample(1:nrow(model_data_clean), size = floor(0.8 * nrow(model_data_clean)))
train_data <- model_data_clean[train_indices, ]
test_data <- model_data_clean[-train_indices, ]

cat(sprintf("Training: %d rows\n", nrow(train_data)))
cat(sprintf("Testing: %d rows\n", nrow(test_data)))

cat("\nSTEP 5: TRAIN RANDOM FOREST\n")
cat("===========================\n")

cat("Preparing predictor variables and response...\n")

# Define predictor columns (factors and numeric)
predictor_cols <- c("beach", "rain_3day", "maxtemp_f", "water_temp_avg_f",
                    "air_water_diff", "month", "season_f",
                    "conductivity", "do", "ph", "salinity", "turbidity")

cat(sprintf("  Predictors: %s\n", paste(predictor_cols, collapse = ", ")))
cat(sprintf("  Response: entero\n"))

# Extract predictors and response
X_train <- train_data[, predictor_cols]
y_train <- train_data$entero

# Verify types one more time
cat("\nFinal type verification before training:\n")
cat("  Factors in X_train:\n")
for (col in predictor_cols) {
  if (is.factor(X_train[[col]])) {
    cat(sprintf("    %s: factor with %d levels\n", col, length(levels(X_train[[col]]))))
  }
}
cat("  Numeric in X_train:\n")
for (col in predictor_cols) {
  if (is.numeric(X_train[[col]])) {
    cat(sprintf("    %s: numeric\n", col))
  }
}

cat("\nDebugging data structure before training...\n")
cat(sprintf("  X_train class: %s\n", paste(class(X_train), collapse = ", ")))
cat(sprintf("  X_train dimensions: %d rows x %d cols\n", nrow(X_train), ncol(X_train)))
cat(sprintf("  y_train class: %s (length: %d)\n", class(y_train), length(y_train)))

# Check for any NA values that might have snuck through
cat("\nChecking for NAs in each column:\n")
for (col in predictor_cols) {
  n_na <- sum(is.na(X_train[[col]]))
  if (n_na > 0) {
    cat(sprintf("  WARNING: %s has %d NA values!\n", col, n_na))
  }
}
cat(sprintf("  y_train NAs: %d\n", sum(is.na(y_train))))

# Sample values from numeric columns
cat("\nSample values from first numeric column (rain_3day):\n")
cat(sprintf("  Values: %s\n", paste(head(X_train$rain_3day, 10), collapse = ", ")))
cat(sprintf("  Class: %s\n", class(X_train$rain_3day)))
cat(sprintf("  Is numeric: %s\n", is.numeric(X_train$rain_3day)))
cat(sprintf("  Is factor: %s\n", is.factor(X_train$rain_3day)))

# Explicitly convert to base data.frame to avoid tibble issues
cat("\nConverting to base data.frame to avoid tibble issues...\n")
X_train <- as.data.frame(X_train)
cat(sprintf("  X_train class after conversion: %s\n", paste(class(X_train), collapse = ", ")))

cat("\nTraining Random Forest using x/y interface...\n")
set.seed(123)
rf_model <- randomForest(
  x = X_train,
  y = y_train,
  ntree = 500,
  mtry = 4,
  importance = TRUE,
  keep.forest = TRUE
  # Removed na.action since we pre-filtered all NAs
)

cat("✓ Training complete\n")

# DEBUG: Check the actual structure of xlevels
cat("\nDEBUG: Examining xlevels structure:\n")
if (!is.null(rf_model$forest$xlevels)) {
  for (var_name in names(rf_model$forest$xlevels)) {
    levels_data <- rf_model$forest$xlevels[[var_name]]
    cat(sprintf("  %s: class=%s, length=%d, values=%s\n",
                var_name,
                class(levels_data),
                length(levels_data),
                paste(head(levels_data, 3), collapse=", ")))
  }
}

# ============================================================================
# STEP 6: VERIFY MODEL IMMEDIATELY
# ============================================================================

cat("\nSTEP 6: IMMEDIATE MODEL VERIFICATION\n")
cat("====================================\n")

cat("\nChecking xlevels structure:\n")
if (!is.null(rf_model$forest$xlevels)) {
  factor_in_model <- names(rf_model$forest$xlevels)
  cat(sprintf("  Variables in xlevels: %s\n", paste(factor_in_model, collapse = ", ")))

  # Check if any numeric variables have xlevels entries
  # NOTE: randomForest stores placeholder "0" values for numeric variables
  # This is a quirk/bug in randomForest but doesn't affect predictions
  numeric_in_xlevels <- intersect(factor_in_model, numeric_vars)
  if (length(numeric_in_xlevels) > 0) {
    cat("\n⚠ NOTE: These numeric variables have xlevels entries:\n")
    real_problems <- c()
    for (var in numeric_in_xlevels) {
      levels <- rf_model$forest$xlevels[[var]]
      # Check if it's the harmless "0" placeholder (length=1, value=0)
      if (length(levels) == 1 && is.numeric(levels) && levels[1] == 0) {
        cat(sprintf("  • %s: placeholder only (harmless randomForest quirk)\n", var))
      } else {
        cat(sprintf("  • %s: %d levels (%s) - ACTUAL PROBLEM!\n",
                    var, length(levels), paste(head(levels, 3), collapse=", ")))
        real_problems <- c(real_problems, var)
      }
    }
    if (length(real_problems) > 0) {
      cat("\n❌ REAL PROBLEMS found - model may not work correctly!\n")
    } else {
      cat("\n✅ All numeric xlevels are harmless placeholders - model is OK!\n")
    }
  } else {
    cat("✅ No numeric variables in xlevels - PERFECT!\n")
  }
} else {
  cat("⚠ Cannot check xlevels\n")
}

# Test prediction on a single observation
cat("\nTesting prediction capability on single observation...\n")
test_obs <- test_data[1, ]
test_pred_single <- predict(rf_model, test_obs)
cat(sprintf("  Single test prediction: %.2f CFU/100mL\n", test_pred_single))

# Test prediction on entire test set
cat("\nTesting prediction on entire test set...\n")
X_test <- test_data[, predictor_cols]
X_test <- as.data.frame(X_test)  # Ensure it's a data.frame
test_pred_all <- predict(rf_model, X_test)
cat(sprintf("  Predictions generated: %d\n", length(test_pred_all)))
cat(sprintf("  Sample predictions: %s\n", paste(round(head(test_pred_all, 5), 2), collapse=", ")))

# Calculate simple RMSE
test_rmse <- sqrt(mean((test_pred_all - test_data$entero)^2))
cat(sprintf("  Test RMSE: %.2f CFU/100mL\n", test_rmse))

# Check if predictions vary (if all same, model is broken)
pred_var <- var(test_pred_all)
cat(sprintf("  Prediction variance: %.2f\n", pred_var))
if (pred_var < 0.01) {
  cat("  ❌ WARNING: Predictions have near-zero variance - model may not be working!\n")
} else {
  cat("  ✓ Predictions vary appropriately\n")
}

# ============================================================================
# STEP 7: SAVE MODEL
# ============================================================================

cat("\nSTEP 7: SAVING MODEL\n")
cat("====================\n")

saveRDS(rf_model, "tybee_advisory_model.rds")
cat("✓ Saved: tybee_advisory_model.rds\n")

# Create metadata
metadata <- list(
  training_date = Sys.Date(),
  n_train = nrow(train_data),
  n_test = nrow(test_data),
  predictors = predictor_cols,
  response = "entero"
)
saveRDS(metadata, "tybee_advisory_metadata.rds")
cat("✓ Saved: tybee_advisory_metadata.rds\n")

# ============================================================================
# FINAL VERIFICATION
# ============================================================================

cat("\nFINAL VERIFICATION\n")
cat("==================\n")

cat("Re-loading model to verify...\n")
loaded_model <- readRDS("tybee_advisory_model.rds")

# Test prediction with loaded model
cat("\nTesting loaded model predictions...\n")
test_sample <- test_data[1:5, predictor_cols]
test_sample <- as.data.frame(test_sample)
loaded_predictions <- predict(loaded_model, test_sample)
cat(sprintf("  Predictions: %s\n", paste(round(loaded_predictions, 2), collapse=", ")))

# Check xlevels structure
if (!is.null(loaded_model$forest$xlevels)) {
  numeric_in_xlevels <- intersect(names(loaded_model$forest$xlevels), numeric_vars)

  if (length(numeric_in_xlevels) > 0) {
    # Check if they're the harmless placeholders
    all_placeholders <- TRUE
    for (var in numeric_in_xlevels) {
      levels <- loaded_model$forest$xlevels[[var]]
      if (!(length(levels) == 1 && is.numeric(levels) && levels[1] == 0)) {
        all_placeholders <- FALSE
        break
      }
    }

    if (all_placeholders) {
      cat("\n✅ SAVED MODEL IS WORKING CORRECTLY!\n")
      cat("Note: Numeric variables have harmless placeholder values in xlevels.\n")
      cat("This is a known randomForest quirk and does not affect predictions.\n")
    } else {
      cat("\n❌ SAVED MODEL HAS ISSUES!\n")
      cat("Some numeric variables are stored as actual factors.\n")
    }
  } else {
    cat("\n✅ SAVED MODEL IS PERFECT!\n")
    cat("No numeric variables in xlevels.\n")
  }
} else {
  cat("⚠ Cannot verify saved model xlevels\n")
}

cat("\n=== DIAGNOSTIC TRAINING COMPLETE ===\n")
cat("\n📊 SUMMARY:\n")
cat(sprintf("  • Training samples: %d\n", nrow(train_data)))
cat(sprintf("  • Test samples: %d\n", nrow(test_data)))
cat(sprintf("  • Test RMSE: %.2f CFU/100mL\n", test_rmse))
cat(sprintf("  • Prediction variance: %.2f\n", pred_var))
cat("\nIf you see '✓ SAVED MODEL IS WORKING CORRECTLY!' above, the model is ready to use!\n")
