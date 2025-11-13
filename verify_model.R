# ============================================================================
# MODEL VERIFICATION SCRIPT
# Run this to check if your model was trained correctly
# ============================================================================

cat("=== TYBEE ISLAND MODEL VERIFICATION ===\n\n")

# Check if model file exists
model_file <- "tybee_advisory_model.rds"

if (!file.exists(model_file)) {
  cat("❌ ERROR: Model file not found!\n")
  cat("Looking for:", model_file, "\n")
  cat("Current directory:", getwd(), "\n")
  cat("\nPlease run train_tybee_model.R first to create the model.\n")
  stop("Model file not found")
}

cat("✓ Model file found:", model_file, "\n")
cat("  File size:", file.info(model_file)$size / 1024, "KB\n")
cat("  Last modified:", format(file.info(model_file)$mtime, "%Y-%m-%d %H:%M:%S"), "\n\n")

# Load the model
cat("Loading model...\n")
model <- readRDS(model_file)
cat("✓ Model loaded successfully\n\n")

# Check model type
cat("MODEL TYPE:\n")
cat("  Class:", class(model)[1], "\n")
if ("randomForest" %in% class(model)) {
  cat("  ✓ Random Forest model detected\n")
} else {
  cat("  ⚠ Warning: Not a Random Forest model\n")
}
cat("\n")

# Check what the model thinks are factors vs numeric
cat("VARIABLE TYPE ANALYSIS:\n")
cat("------------------------\n\n")

if ("randomForest" %in% class(model) && !is.null(model$forest$xlevels)) {
  factor_vars <- names(model$forest$xlevels)
  
  cat("Variables stored as FACTORS in model:\n")
  if (length(factor_vars) > 0) {
    for (var in factor_vars) {
      levels <- model$forest$xlevels[[var]]
      cat(sprintf("  • %s: %d levels\n", var, length(levels)))
      
      # Check if it's a numeric variable stored as factor (BAD!)
      numeric_var_names <- c("rain_3day", "rain3day", "maxtemp_f", "water_temp_avg_f", 
                            "air_water_diff", "conductivity", "do", "ph", 
                            "salinity", "turbidity", "month", "day_of_year")
      
      if (var %in% numeric_var_names) {
        cat("    ⚠️ WARNING: This should be NUMERIC, not a factor!\n")
        cat("    Levels:", paste(levels[1:min(5, length(levels))], collapse = ", "), 
            ifelse(length(levels) > 5, "...", ""), "\n")
        
        # Check if all levels are just "0" (very bad!)
        if (length(levels) == 1 && levels[1] == "0") {
          cat("    ❌ CRITICAL: Only has level '0' - model is BROKEN!\n")
        }
      }
    }
  } else {
    cat("  None found\n")
  }
  cat("\n")
  
  # Check for variables that SHOULD be factors
  cat("Expected FACTOR variables:\n")
  expected_factors <- c("beach", "season_f", "season", "tide_stage", "wind_direction")
  for (var in expected_factors) {
    if (var %in% factor_vars) {
      cat(sprintf("  ✓ %s (correctly stored as factor)\n", var))
    } else {
      cat(sprintf("  ⚠ %s (should be factor but not found)\n", var))
    }
  }
  cat("\n")
  
  # Check for variables that should be NUMERIC
  cat("Expected NUMERIC variables:\n")
  expected_numeric <- c("rain_3day", "maxtemp_f", "water_temp_avg_f", 
                       "air_water_diff", "conductivity", "do", "ph", 
                       "salinity", "turbidity", "month")
  
  problem_count <- 0
  for (var in expected_numeric) {
    if (var %in% factor_vars) {
      cat(sprintf("  ❌ %s (stored as FACTOR - should be NUMERIC!)\n", var))
      problem_count <- problem_count + 1
    } else {
      cat(sprintf("  ✓ %s (not in factor list - likely numeric)\n", var))
    }
  }
  cat("\n")
  
  # Overall verdict
  cat("=== VERDICT ===\n")
  if (problem_count == 0) {
    cat("✅ MODEL IS CORRECT!\n")
    cat("All numeric variables are stored properly.\n")
    cat("This model should work in Tab 4.\n\n")
    cat("If Tab 4 still shows errors, check:\n")
    cat("1. Did you copy this model to your Shiny app's models/ directory?\n")
    cat("2. Did you restart the Shiny app after copying?\n")
    cat("3. Is the app loading the correct model file?\n")
  } else {
    cat("❌ MODEL IS BROKEN!\n")
    cat(sprintf("Found %d numeric variables incorrectly stored as factors.\n", problem_count))
    cat("\nTHIS MODEL WILL NOT WORK FOR PREDICTIONS.\n")
    cat("\nYou need to:\n")
    cat("1. Check your train_tybee_model.R script\n")
    cat("2. Make sure numeric variables use as.numeric()\n")
    cat("3. Re-run the training script\n")
    cat("4. Verify the new model with this script again\n")
  }
  
} else {
  cat("⚠ Cannot determine variable types from this model\n")
}

cat("\n")

# Show variable importance
if ("randomForest" %in% class(model) && !is.null(model$importance)) {
  cat("VARIABLE IMPORTANCE (Top 10):\n")
  importance_sorted <- sort(model$importance[,1], decreasing = TRUE)
  for (i in 1:min(10, length(importance_sorted))) {
    cat(sprintf("  %2d. %s: %.4f\n", i, names(importance_sorted)[i], importance_sorted[i]))
  }
  cat("\n")
}

# Test prediction capability
cat("TESTING PREDICTION CAPABILITY:\n")
cat("Creating a test observation...\n")

test_data <- data.frame(
  beach = factor("South", levels = c("North", "Middle", "South", "Polk", "Strand")),
  rain_3day = 0.5,
  maxtemp_f = 78,
  water_temp_avg_f = 75,
  air_water_diff = 3,
  month = 6,
  season_f = factor("Summer", levels = c("Winter", "Spring", "Summer", "Fall")),
  conductivity = 40000,
  do = 7,
  ph = 7.5,
  salinity = 28,
  turbidity = 10
)

cat("Test input:\n")
print(test_data)
cat("\n")

tryCatch({
  prediction <- predict(model, test_data)
  cat("✅ PREDICTION SUCCESSFUL!\n")
  cat(sprintf("Predicted bacteria level: %.2f CFU/100mL\n", prediction))
  cat(sprintf("Advisory status: %s\n", ifelse(prediction > 70, "Advisory", "No Advisory")))
  cat("\nThe model can make predictions. If Tab 4 isn't working,\n")
  cat("the problem is with file location or app configuration.\n")
}, error = function(e) {
  cat("❌ PREDICTION FAILED!\n")
  cat("Error:", e$message, "\n")
  cat("\nThis confirms the model is broken.\n")
  cat("Re-train the model with correct variable types.\n")
})

cat("\n=== VERIFICATION COMPLETE ===\n")
