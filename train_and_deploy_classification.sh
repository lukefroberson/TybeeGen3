#!/bin/bash
# Train and deploy the classification model

echo "=== TRAINING CLASSIFICATION MODEL ==="
cd /home/user/TybeeGen3/www

# Run the classification training script
Rscript train_classification_model.R

# Check if training was successful
if [ -f "tybee_advisory_classification_model.rds" ]; then
    echo ""
    echo "=== DEPLOYING MODEL ==="

    # Copy to models folder with standard name
    cp tybee_advisory_classification_model.rds ../models/tybee_advisory_model.rds
    cp tybee_advisory_classification_metadata.rds ../models/tybee_advisory_metadata.rds

    echo "✓ Classification model deployed to models/tybee_advisory_model.rds"
    echo "✓ Metadata deployed to models/tybee_advisory_metadata.rds"
    echo ""
    echo "The app will now use the classification model with:"
    echo "  - 564x class weight for advisory class"
    echo "  - 5% probability threshold"
    echo "  - ~40% sensitivity"
    echo ""
    echo "Please restart your Shiny app to load the new model."
else
    echo "❌ ERROR: Classification model file not created"
    echo "Check train_classification_model.Rout for errors"
fi
