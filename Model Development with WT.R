# ============================================================================
# TESTING VARIABLE COMBINATIONS & INTERACTIONS FOR ENTEROCOCCUS PREDICTION
# ============================================================================

cat("=== ANALYZING VARIABLE COMBINATIONS & INTERACTIONS ===\n\n")

# 1. TWO-WAY INTERACTION EFFECTS IN LOGISTIC REGRESSION
cat("1. TESTING KEY TWO-WAY INTERACTIONS\n")
cat("(Testing if variable combinations have stronger effects than individual variables)\n\n")

# Test important interactions based on environmental science
interaction_tests <- list(
  "Rain × Temperature" = "rain_3day * maxtemp_f",
  "Rain × Beach" = "rain_3day * beach", 
  "Temperature × Season" = "maxtemp_f * season_f",
  "Water Temp × Air Temp" = "water_temp_avg_f * maxtemp_f",
  "Beach × Season" = "beach * season_f",
  "Rain × Season" = "rain_3day * season_f",
  "Conductivity × Salinity" = "conductivity * salinity",
  "Temperature × Water Quality" = "maxtemp_f * turbidity",
  "Rain × Water Quality" = "rain_3day * turbidity"
)

interaction_results <- data.frame()

for (interaction_name in names(interaction_tests)) {
  interaction_term <- interaction_tests[[interaction_name]]
  
  cat(sprintf("Testing: %s\n", interaction_name))
  
  # Build model with interaction
  base_formula <- "advisory ~ rain_3day + maxtemp_f + water_temp_avg_f + beach + season_f"
  interaction_formula <- paste(base_formula, "+", interaction_term)
  
  tryCatch({
    # Base model without interaction
    base_model <- glm(as.formula(base_formula), data = train_data, family = binomial)
    
    # Model with interaction
    int_model <- glm(as.formula(interaction_formula), data = train_data, family = binomial)
    
    # Compare models using likelihood ratio test
    lr_test <- anova(base_model, int_model, test = "Chisq")
    p_value <- lr_test$`Pr(>Chi)`[2]
    
    # Calculate AIC improvement
    aic_improvement <- base_model$aic - int_model$aic
    
    interaction_results <- rbind(interaction_results, data.frame(
      Interaction = interaction_name,
      P_Value = round(p_value, 4),
      AIC_Improvement = round(aic_improvement, 2),
      Significant = ifelse(p_value < 0.05, "YES", "NO"),
      Effect_Strength = case_when(
        p_value < 0.001 ~ "Very Strong",
        p_value < 0.01 ~ "Strong", 
        p_value < 0.05 ~ "Moderate",
        p_value < 0.10 ~ "Weak",
        TRUE ~ "None"
      )
    ))
    
    cat(sprintf("  P-value: %s, AIC improvement: %s %s\n", 
                round(p_value, 4), 
                round(aic_improvement, 2),
                ifelse(p_value < 0.05, "(SIGNIFICANT)", "")))
    
  }, error = function(e) {
    cat(sprintf("  Error testing %s: %s\n", interaction_name, e$message))
  })
}

cat("\n📊 INTERACTION ANALYSIS SUMMARY:\n")
interaction_results <- interaction_results %>%
  arrange(P_Value)
print(interaction_results)

cat("\n")

# 2. CONDITIONAL RELATIONSHIPS ANALYSIS
cat("2. CONDITIONAL RELATIONSHIPS\n")
cat("(How variable effects change under different conditions)\n\n")

# Rain effects by temperature bins
cat("🌧️ RAIN EFFECTS BY TEMPERATURE CONDITIONS:\n")
temp_bins <- train_data %>%
  mutate(
    temp_category = case_when(
      maxtemp_f < 70 ~ "Cool (<70°F)",
      maxtemp_f < 80 ~ "Moderate (70-80°F)", 
      TRUE ~ "Hot (>80°F)"
    )
  ) %>%
  group_by(temp_category) %>%
  summarise(
    n = n(),
    mean_rain = round(mean(rain_3day), 2),
    advisory_rate = round(mean(advisory) * 100, 1),
    rain_effect = round(cor(rain_3day, advisory, use = "complete.obs"), 3),
    .groups = 'drop'
  )

print(temp_bins)

cat("\n🏖️ BEACH EFFECTS BY SEASON:\n")
beach_season <- train_data %>%
  group_by(beach, season_f) %>%
  summarise(
    n = n(),
    advisory_rate = round(mean(advisory) * 100, 1),
    mean_entero = round(mean(entero), 1),
    .groups = 'drop'
  ) %>%
  filter(n >= 10) %>%  # Only show combinations with sufficient data
  arrange(desc(advisory_rate))

print(head(beach_season, 10))

cat("\n")

# 3. VARIABLE CLUSTERING ANALYSIS
cat("3. VARIABLE CLUSTERING ANALYSIS\n")
cat("(Finding groups of variables that work together)\n\n")

# Create correlation matrix for numeric variables
numeric_data <- train_data %>%
  select(rain_3day, maxtemp_f, water_temp_avg_f, air_water_diff, 
         month, conductivity, do, ph, salinity, turbidity, entero) %>%
  na.omit()

correlation_matrix <- cor(numeric_data)

# Find highly correlated variable pairs
high_correlations <- data.frame()
for (i in 1:(ncol(correlation_matrix)-1)) {
  for (j in (i+1):ncol(correlation_matrix)) {
    correlation <- correlation_matrix[i, j]
    if (abs(correlation) > 0.3) {  # Moderate correlation threshold
      high_correlations <- rbind(high_correlations, data.frame(
        Variable1 = rownames(correlation_matrix)[i],
        Variable2 = colnames(correlation_matrix)[j], 
        Correlation = round(correlation, 3),
        Strength = case_when(
          abs(correlation) > 0.7 ~ "Very Strong",
          abs(correlation) > 0.5 ~ "Strong",
          abs(correlation) > 0.3 ~ "Moderate"
        )
      ))
    }
  }
}

if (nrow(high_correlations) > 0) {
  high_correlations <- high_correlations %>%
    arrange(desc(abs(Correlation)))
  print(high_correlations)
  
  cat("\n🔗 SUGGESTED VARIABLE COMBINATIONS:\n")
  cat("These variables often move together and might have combined effects:\n")
  for (i in 1:min(5, nrow(high_correlations))) {
    cat(sprintf("• %s + %s (r = %s)\n", 
                high_correlations$Variable1[i],
                high_correlations$Variable2[i], 
                high_correlations$Correlation[i]))
  }
} else {
  cat("No strong correlations found between variables (all |r| < 0.3)\n")
}

cat("\n")

# 4. DECISION TREE ANALYSIS FOR COMPLEX INTERACTIONS
cat("4. DECISION TREE ANALYSIS\n")
cat("(Finding complex variable combinations and thresholds)\n\n")

library(rpart)
library(rpart.plot)

# Build decision tree to find important splits
tree_model <- rpart(
  advisory ~ rain_3day + maxtemp_f + water_temp_avg_f + beach + season_f + 
    conductivity + do + ph + salinity + turbidity,
  data = train_data,
  method = "class",
  control = rpart.control(cp = 0.01, minsplit = 50)
)

cat("🌳 DECISION TREE RULES FOR HIGH ENTEROCOCCUS:\n")
cat("(Key variable combinations that predict advisory conditions)\n\n")

# Extract decision rules
tree_rules <- rpart.rules(tree_model, style = "wide", cover = TRUE)
print(tree_rules)

# Get variable importance from tree
tree_importance <- tree_model$variable.importance
if (length(tree_importance) > 0) {
  tree_imp_df <- data.frame(
    Variable = names(tree_importance),
    Tree_Importance = round(tree_importance, 2)
  ) %>%
    arrange(desc(Tree_Importance)) %>%
    head(6)
  
  cat("\n📊 DECISION TREE VARIABLE IMPORTANCE:\n")
  print(tree_imp_df)
}

cat("\n")

# 5. COMPOSITE VARIABLE CREATION & TESTING
cat("5. TESTING COMPOSITE VARIABLES\n")
cat("(Creating meaningful combinations based on environmental science)\n\n")

# Create composite variables
train_data_composite <- train_data %>%
  mutate(
    # Weather stress index
    weather_stress = scale(rain_3day)[,1] + scale(maxtemp_f)[,1],
    
    # Water quality index
    water_quality_index = scale(conductivity)[,1] + scale(turbidity)[,1] - scale(do)[,1],
    
    # Temperature differential
    temp_differential = abs(maxtemp_f - water_temp_avg_f),
    
    # Beach risk category
    beach_risk = case_when(
      beach %in% c("Polk", "Strand") ~ "High",
      TRUE ~ "Low"
    ),
    
    # Season-rain interaction
    summer_rain = ifelse(season_f == "Summer", rain_3day, 0),
    
    # Hot weather indicator
    hot_weather = ifelse(maxtemp_f > 85, 1, 0)
  )

# Test composite variables
composite_tests <- list(
  "Weather Stress Index" = "weather_stress",
  "Water Quality Index" = "water_quality_index", 
  "Temperature Differential" = "temp_differential",
  "Beach Risk Category" = "beach_risk",
  "Summer Rain Effect" = "summer_rain",
  "Hot Weather Days" = "hot_weather"
)

cat("🧪 COMPOSITE VARIABLE EFFECTS:\n")
composite_results <- data.frame()

for (comp_name in names(composite_tests)) {
  comp_var <- composite_tests[[comp_name]]
  
  tryCatch({
    # Test correlation with enterococcus
    if (is.numeric(train_data_composite[[comp_var]])) {
      correlation <- cor(train_data_composite[[comp_var]], 
                         train_data_composite$entero, 
                         use = "complete.obs")
      
      # Test in logistic regression
      formula_test <- as.formula(paste("advisory ~", comp_var))
      comp_model <- glm(formula_test, data = train_data_composite, family = binomial)
      p_value <- summary(comp_model)$coefficients[2, "Pr(>|z|)"]
      coefficient <- summary(comp_model)$coefficients[2, "Estimate"]
      
      composite_results <- rbind(composite_results, data.frame(
        Composite_Variable = comp_name,
        Correlation = round(correlation, 3),
        Coefficient = round(coefficient, 3),
        P_Value = round(p_value, 4),
        Significant = ifelse(p_value < 0.05, "YES", "NO")
      ))
      
    } else {
      # For categorical composites
      aov_test <- aov(entero ~ get(comp_var), data = train_data_composite)
      p_value <- summary(aov_test)[[1]]$`Pr(>F)`[1]
      
      composite_results <- rbind(composite_results, data.frame(
        Composite_Variable = comp_name,
        Correlation = NA,
        Coefficient = NA,
        P_Value = round(p_value, 4),
        Significant = ifelse(p_value < 0.05, "YES", "NO")
      ))
    }
    
  }, error = function(e) {
    cat(sprintf("Error testing %s: %s\n", comp_name, e$message))
  })
}

print(composite_results)

cat("\n")

# 6. SUMMARY OF SIGNIFICANT COMBINATIONS
cat("=== SIGNIFICANT VARIABLE COMBINATIONS SUMMARY ===\n\n")

# Significant interactions
sig_interactions <- interaction_results %>%
  filter(P_Value < 0.05) %>%
  arrange(P_Value)

if (nrow(sig_interactions) > 0) {
  cat("🔥 SIGNIFICANT INTERACTIONS (p < 0.05):\n")
  for (i in 1:nrow(sig_interactions)) {
    cat(sprintf("• %s (p = %s, %s effect)\n", 
                sig_interactions$Interaction[i],
                sig_interactions$P_Value[i],
                sig_interactions$Effect_Strength[i]))
  }
} else {
  cat("❌ NO SIGNIFICANT TWO-WAY INTERACTIONS FOUND\n")
}

cat("\n")

# Significant composites
sig_composites <- composite_results %>%
  filter(Significant == "YES") %>%
  arrange(P_Value)

if (nrow(sig_composites) > 0) {
  cat("🧪 SIGNIFICANT COMPOSITE VARIABLES (p < 0.05):\n")
  for (i in 1:nrow(sig_composites)) {
    cat(sprintf("• %s (p = %s)\n", 
                sig_composites$Composite_Variable[i],
                sig_composites$P_Value[i]))
  }
} else {
  cat("❌ NO SIGNIFICANT COMPOSITE VARIABLES FOUND\n")
}

cat("\n")

# Overall conclusion
cat("🎯 CONCLUSIONS ABOUT VARIABLE COMBINATIONS:\n")

total_significant <- nrow(sig_interactions) + nrow(sig_composites)

if (total_significant > 0) {
  cat(sprintf("✓ Found %d significant variable combinations\n", total_significant))
  cat("• Enterococcus levels are influenced by variable interactions\n")
  cat("• Consider these combinations for improved prediction models\n")
  cat("• Beach management should account for multiple factors simultaneously\n")
} else {
  cat("• Enterococcus levels show complex, non-linear relationships\n")
  cat("• Effects may be driven by unmeasured variables or random variation\n") 
  cat("• Individual variables may be sufficient for current prediction needs\n")
  cat("• Consider three-way interactions or machine learning approaches\n")
}

cat("\n🔬 NEXT STEPS FOR DEEPER ANALYSIS:\n")
cat("1. Test three-way interactions (A × B × C)\n")
cat("2. Use machine learning for non-linear combinations\n")
cat("3. Collect additional environmental variables\n")
cat("4. Consider temporal lag effects (yesterday's weather → today's entero)\n")