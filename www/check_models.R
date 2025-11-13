#!/usr/bin/env Rscript

cat("=== CHECKING MODEL FILES ===\n\n")

# Check www model
if (file.exists("tybee_advisory_model.rds")) {
  cat("WWW MODEL (tybee_advisory_model.rds):\n")
  model_www <- readRDS("tybee_advisory_model.rds")
  cat("  Class:", class(model_www), "\n")
  cat("  Type:", model_www$type, "\n")
  if (!is.null(model_www$classwt)) {
    cat("  Class weights:", paste(names(model_www$classwt), "=", model_www$classwt, collapse=", "), "\n")
  }
  if (!is.null(model_www$cutoff)) {
    cat("  Cutoff:", paste(names(model_www$cutoff), "=", model_www$cutoff, collapse=", "), "\n")
  }
  cat("\n")
}

# Check models folder
if (file.exists("../models/tybee_advisory_model.rds")) {
  cat("MODELS FOLDER (../models/tybee_advisory_model.rds):\n")
  model_models <- readRDS("../models/tybee_advisory_model.rds")
  cat("  Class:", class(model_models), "\n")
  cat("  Type:", model_models$type, "\n")
  if (!is.null(model_models$classwt)) {
    cat("  Class weights:", paste(names(model_models$classwt), "=", model_models$classwt, collapse=", "), "\n")
  }
  if (!is.null(model_models$cutoff)) {
    cat("  Cutoff:", paste(names(model_models$cutoff), "=", model_models$cutoff, collapse=", "), "\n")
  }
  cat("\n")
}
