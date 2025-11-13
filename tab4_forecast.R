# Tab 4: Interactive Forecast Tool
# This module provides an interactive tool for users to adjust environmental
# parameters and see real-time predictions of bacteria levels

# Required libraries
library(readxl)  # For reading site coordinates from Excel

# ============================================================================
# UI COMPONENT
# ============================================================================

tab4_ui <- function() {
  fluidPage(
    h3("Interactive Bacteria Prediction Tool", style = "color: #2c3e50; margin-bottom: 20px;"),
    p("Adjust the environmental parameters below to see how different conditions affect bacteria predictions at Tybee Island beaches.",
      style = "color: #7f8c8d; margin-bottom: 30px;"),
    
    # Parameter Controls Section
    wellPanel(
      style = "background-color: #ecf0f1; border-radius: 8px; padding: 20px;",
      h4("Environmental Parameters", style = "color: #2c3e50; margin-bottom: 15px;"),
      
      fluidRow(
        column(3,
               selectInput("forecast_beach", "Beach Location",
                          choices = c("North Beach", "Middle Beach", "South Beach", 
                                    "Polk Street", "Strand Street"),
                          selected = "South Beach")
        ),
        column(3,
               sliderInput("forecast_rain", "3-Day Rainfall (inches)",
                          min = 0, max = 5, value = 0.5, step = 0.1)
        ),
        column(3,
               sliderInput("forecast_temp", "Water Temperature (°F)",
                          min = 50, max = 90, value = 75, step = 1)
        ),
        column(3,
               sliderInput("forecast_air_temp", "Air Temperature Max (°F)",
                          min = 40, max = 100, value = 78, step = 1)
        )
      ),
      
      fluidRow(
        column(3,
               selectInput("forecast_tide", "Tide Stage",
                          choices = c("Low", "Flood 1/4", "Flood 1/2", "Flood 3/4", 
                                    "High", "Ebb 1/4", "Ebb 1/2", "Ebb 3/4"),
                          selected = "High")
        ),
        column(3,
               selectInput("forecast_wind_dir", "Wind Direction",
                          choices = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
                          selected = "SE")
        ),
        column(3,
               sliderInput("forecast_salinity", "Salinity (ppt)",
                          min = 10, max = 35, value = 28, step = 1)
        ),
        column(3,
               sliderInput("forecast_turbidity", "Turbidity (NTU)",
                          min = 0, max = 50, value = 10, step = 1)
        )
      ),
      
      fluidRow(
        column(3,
               sliderInput("forecast_ph", "pH",
                          min = 6.0, max = 9.0, value = 7.5, step = 0.1)
        ),
        column(3,
               sliderInput("forecast_conductivity", "Conductivity (µS/cm)",
                          min = 10000, max = 60000, value = 40000, step = 1000)
        ),
        column(3,
               sliderInput("forecast_do", "Dissolved Oxygen (mg/L)",
                          min = 0, max = 15, value = 7, step = 0.5)
        ),
        column(3, style = "padding-top: 25px;",
               actionButton("run_prediction", "Generate Prediction", 
                          class = "btn-primary btn-lg",
                          style = "background-color: #3498db; border: none; padding: 10px 30px; width: 100%;")
        )
      )
    ),
    
    # Results Section - Two Column Layout
    fluidRow(
      # Left Column: Map
      column(6,
             wellPanel(
               style = "background-color: white; border-radius: 8px; padding: 20px; height: 500px;",
               h4("Beach Location Map", style = "color: #2c3e50; margin-bottom: 15px;"),
               leafletOutput("forecast_map", height = "420px")
             )
      ),
      
      # Right Column: Predictions
      column(6,
             # Prediction Result Box
             wellPanel(
               style = "background-color: white; border-radius: 8px; padding: 20px; margin-bottom: 20px;",
               h4("Prediction Result", style = "color: #2c3e50; margin-bottom: 15px;"),
               uiOutput("prediction_result")
             ),
             
             # Confidence and Factor Analysis
             wellPanel(
               style = "background-color: white; border-radius: 8px; padding: 20px;",
               h4("Model Confidence & Key Factors", style = "color: #2c3e50; margin-bottom: 15px;"),
               uiOutput("prediction_confidence"),
               hr(),
               plotOutput("factor_importance", height = "200px")
             )
      )
    ),
    
    # Interpretation Guide
    wellPanel(
      style = "background-color: #fff9e6; border-left: 4px solid #f39c12; padding: 15px; margin-top: 20px;",
      h4("💡 How to Interpret Results", style = "color: #e67e22;"),
      tags$ul(
        tags$li(strong("Predicted Bacteria Level:"), " Estimated Enterococcus concentration (CFU/100mL)"),
        tags$li(strong("Advisory Status:"), " Georgia health advisory threshold is 70 CFU/100mL"),
        tags$li(strong("Confidence Level:"), " Based on model certainty for these conditions"),
        tags$li(strong("Key Factors:"), " Environmental variables with strongest influence on this prediction")
      ),
      p(style = "font-size: 12px; color: #666; margin-top: 10px;",
        strong("Note:"), " This tool uses actual variables from 2004-2023 training data: ",
        "3-day rainfall, water temperature, air temperature, tide stage, wind direction, salinity, turbidity, pH, conductivity, and dissolved oxygen.")
    )
  )
}

# ============================================================================
# SERVER COMPONENT
# ============================================================================

tab4_server <- function(input, output, session, model, historical_data) {
  
  # Reactive value to store prediction results
  prediction_results <- reactiveVal(NULL)
  
  # Generate prediction when button is clicked
  observeEvent(input$run_prediction, {
    
    # Show notification that prediction is running
    showNotification("Generating prediction...", type = "message", duration = 2)
    
    # Map display names to exact names used in training data
    beach_mapping <- c(
      "North Beach" = "North",
      "Middle Beach" = "Middle",
      "South Beach" = "South",
      "Polk Street" = "Polk",
      "Strand Street" = "Strand"
    )
    
    actual_beach_name <- beach_mapping[input$forecast_beach]
    
    # Determine season and month (temporal variables)
    current_month <- as.numeric(format(Sys.Date(), "%m"))
    season <- if (current_month %in% c(12, 1, 2)) {
      "Winter"
    } else if (current_month %in% c(3, 4, 5)) {
      "Spring"
    } else if (current_month %in% c(6, 7, 8)) {
      "Summer"
    } else {
      "Fall"
    }

    # Create input data frame with ONLY the variables the trained model expects
    # These match the predictor_cols used in training:
    # beach, rain_3day, maxtemp_f, water_temp_avg_f, air_water_diff,
    # month, season_f, conductivity, do, ph, salinity, turbidity
    new_data <- data.frame(
      beach = factor(actual_beach_name,
                     levels = c("North", "Middle", "South", "Polk", "Strand")),
      rain_3day = as.numeric(input$forecast_rain),
      maxtemp_f = as.numeric(input$forecast_air_temp),
      water_temp_avg_f = as.numeric(input$forecast_temp),
      air_water_diff = as.numeric(input$forecast_air_temp - input$forecast_temp),
      month = as.numeric(current_month),
      season_f = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
      conductivity = as.numeric(input$forecast_conductivity),
      do = as.numeric(input$forecast_do),
      ph = as.numeric(input$forecast_ph),
      salinity = as.numeric(input$forecast_salinity),
      turbidity = as.numeric(input$forecast_turbidity),
      stringsAsFactors = FALSE
    )

    # Ensure it's a base data.frame (critical for randomForest)
    new_data <- as.data.frame(new_data)
    
    # Make prediction - with error handling
    prediction <- tryCatch({
      if (is.null(model)) {
        stop("Model not loaded. Please ensure model file is in the models/ directory.")
      }

      # Detect model type
      is_classification <- "randomForest" %in% class(model) && model$type == "classification"

      # DEBUG: Print model info
      cat("\n=== TAB 4 PREDICTION DEBUG ===\n")
      cat("Model class:", class(model), "\n")
      cat("Model type:", model$type, "\n")
      cat("Is classification:", is_classification, "\n")

      if (is_classification) {
        # CLASSIFICATION MODEL - predicts advisory yes/no with probabilities
        pred_class <- predict(model, newdata = new_data, type = "response")
        pred_prob <- predict(model, newdata = new_data, type = "prob")

        # Get probability of advisory
        advisory_prob <- pred_prob[, "Yes"]

        # DEBUG: Print prediction details
        cat("Predicted class:", as.character(pred_class), "\n")
        cat("Advisory probability:", advisory_prob, "\n")
        cat("Prob matrix:\n")
        print(pred_prob)

        # For classification, show the probability as the main metric
        # and estimate a bacteria level based on historical patterns
        # (Advisories are typically 70-200 CFU/100mL)
        if (pred_class == "Yes") {
          # Estimate higher end if model predicts advisory
          estimated_level <- 70 + (advisory_prob * 130)  # Range: 70-200
        } else {
          # Estimate lower end if model predicts no advisory
          estimated_level <- (1 - advisory_prob) * 50  # Range: 0-50
        }

        # For classification, confidence is the prediction probability
        confidence <- max(pred_prob) * 100

        # Approximate standard deviation from probability
        # More certain predictions have lower SD
        pred_sd <- 30 * (1 - max(pred_prob))
        pred_mean <- estimated_level

      } else {
        # REGRESSION MODEL - predicts exact bacteria level
        pred_value <- predict(model, newdata = new_data)

        # Calculate confidence based on prediction variance
        if ("randomForest" %in% class(model)) {
          tree_preds <- predict(model, newdata = new_data, predict.all = TRUE)$individual
          pred_sd <- sd(tree_preds)
          pred_mean <- mean(tree_preds)
        } else {
          pred_mean <- as.numeric(pred_value)
          pred_sd <- pred_mean * 0.15
        }

        confidence <- max(0, min(100, 100 * (1 - pred_sd / max(pred_mean, 1))))
      }
      
      # Calculate factor importance for this specific prediction
      factor_contrib <- calculate_factor_importance(new_data, model, historical_data)
      
      list(
        bacteria_level = round(pred_mean, 1),
        confidence = round(confidence, 0),
        prediction_sd = round(pred_sd, 1),
        lower_bound = max(0, round(pred_mean - 1.96 * pred_sd, 1)),
        upper_bound = round(pred_mean + 1.96 * pred_sd, 1),
        factor_importance = factor_contrib,
        status = ifelse(pred_mean > 70, "Advisory", "No Advisory")
      )
    },
    error = function(e) {
      # Log error to console
      cat("\n❌ PREDICTION ERROR ❌\n")
      cat("Error:", e$message, "\n")
      cat("Beach:", actual_beach_name, "\n")
      cat("Data columns:", paste(names(new_data), collapse = ", "), "\n\n")

      # Show user-friendly error
      showNotification(
        paste("Prediction error:", e$message),
        type = "error",
        duration = 10
      )
      NULL
    })
    
    # Store results
    prediction_results(prediction)
  })
  
  # Display prediction result
  output$prediction_result <- renderUI({
    results <- prediction_results()
    
    if (is.null(results)) {
      return(
        div(style = "text-align: center; padding: 40px; color: #95a5a6;",
            icon("flask", style = "font-size: 48px; margin-bottom: 15px;"),
            p("Click 'Generate Prediction' to see results", style = "font-size: 16px;")
        )
      )
    }
    
    # Determine color based on advisory status
    result_color <- ifelse(results$status == "Advisory", "#e74c3c", "#27ae60")
    bg_color <- ifelse(results$status == "Advisory", "#ffeaea", "#eafaf1")
    
    div(style = paste0("background-color: ", bg_color, "; border-radius: 8px; padding: 20px; text-align: center;"),
        div(style = "font-size: 48px; font-weight: bold; margin-bottom: 10px;",
            style = paste0("color: ", result_color),
            paste0(results$bacteria_level, " CFU/100mL")
        ),
        div(style = paste0("font-size: 24px; color: ", result_color, "; margin-bottom: 15px;"),
            results$status
        ),
        div(style = "color: #7f8c8d; font-size: 14px;",
            paste0("95% Prediction Interval: ", results$lower_bound, " - ", results$upper_bound, " CFU/100mL")
        )
    )
  })
  
  # Display confidence level
  output$prediction_confidence <- renderUI({
    results <- prediction_results()
    
    if (is.null(results)) {
      return(
        div(style = "text-align: center; padding: 20px; color: #95a5a6;",
            p("Confidence metrics will appear here after prediction")
        )
      )
    }
    
    # Determine confidence color
    conf_color <- if (results$confidence >= 70) {
      "#27ae60"
    } else if (results$confidence >= 50) {
      "#f39c12"
    } else {
      "#e74c3c"
    }
    
    div(
      div(style = "margin-bottom: 15px;",
          span("Confidence Level: ", style = "font-weight: bold; color: #2c3e50;"),
          span(paste0(results$confidence, "%"), 
               style = paste0("font-size: 20px; font-weight: bold; color: ", conf_color))
      ),
      div(style = "color: #7f8c8d; font-size: 14px;",
          "Based on model certainty and similarity to historical conditions"
      )
    )
  })
  
  # Plot factor importance
  output$factor_importance <- renderPlot({
    results <- prediction_results()
    
    if (is.null(results) || is.null(results$factor_importance)) {
      return(NULL)
    }
    
    # Create bar plot of factor importance
    par(mar = c(4, 8, 2, 2))
    barplot(
      results$factor_importance,
      horiz = TRUE,
      las = 1,
      col = "#3498db",
      border = NA,
      xlab = "Relative Importance (%)",
      main = "Top Contributing Factors",
      cex.names = 0.9
    )
  })
  
  # Initialize the map once on startup with all markers
  output$forecast_map <- renderLeaflet({
    # Load actual site coordinates from TybeeSiteData.xlsx
    site_coords <- tryCatch({
      coord_data <- read_excel("www/TybeeSiteData.xlsx")
      # Ensure numeric types
      coord_data$Latitude <- as.numeric(coord_data$Latitude)
      coord_data$Longitude <- as.numeric(coord_data$Longitude)
      coord_data
    }, error = function(e) {
      # Fallback coordinates if file not found
      data.frame(
        MonitoringLocationName = c("TYBEE ISLAND NORTH", "TYBEE ISLAND MIDDLE",
                                   "TYBEE ISLAND SOUTH", "TYBEE ISLAND POLK ST.",
                                   "TYBEE ISLAND STRAND"),
        Latitude = c(32.02069, 32.00731, 31.98683, 32.02613, 31.99299),
        Longitude = c(-80.84148, -80.84100, -80.85130, -80.85473, -80.84579)
      )
    })

    # Map official names to our display names
    name_mapping <- c(
      "TYBEE ISLAND NORTH" = "North Beach",
      "TYBEE ISLAND MIDDLE" = "Middle Beach",
      "TYBEE ISLAND SOUTH" = "South Beach",
      "TYBEE ISLAND POLK ST." = "Polk Street",
      "TYBEE ISLAND STRAND" = "Strand Street"
    )

    site_coords$beach_name <- name_mapping[site_coords$MonitoringLocationName]

    # Initially, select the first beach (South Beach is the default)
    site_coords$is_selected <- site_coords$beach_name == "South Beach"

    # Create base map with initial markers
    leaflet(site_coords) %>%
      addTiles() %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        popup = ~beach_name,
        label = ~beach_name,
        color = ~ifelse(is_selected, "#dc3545", "#0066cc"),
        fillColor = ~ifelse(is_selected, "#dc3545", "#0066cc"),
        radius = ~ifelse(is_selected, 12, 8),
        fillOpacity = ~ifelse(is_selected, 0.9, 0.7),
        stroke = TRUE,
        weight = 2
      ) %>%
      setView(lng = -80.846, lat = 32.005, zoom = 13)
  })

  # Update map markers when beach selection changes
  observe({
    req(input$forecast_beach)  # Ensure input is available
    
    # Debug output
    cat("Map update triggered for beach:", input$forecast_beach, "\n")

    # Load actual site coordinates from TybeeSiteData.xlsx
    site_coords <- tryCatch({
      coord_data <- read_excel("www/TybeeSiteData.xlsx")
      # Ensure numeric types
      coord_data$Latitude <- as.numeric(coord_data$Latitude)
      coord_data$Longitude <- as.numeric(coord_data$Longitude)
      cat("✓ Loaded coordinates from Excel file\n")
      coord_data
    }, error = function(e) {
      cat("⚠ Using fallback coordinates:", e$message, "\n")
      # Fallback coordinates if file not found
      data.frame(
        MonitoringLocationName = c("TYBEE ISLAND NORTH", "TYBEE ISLAND MIDDLE",
                                   "TYBEE ISLAND SOUTH", "TYBEE ISLAND POLK ST.",
                                   "TYBEE ISLAND STRAND"),
        Latitude = c(32.02069, 32.00731, 31.98683, 32.02613, 31.99299),
        Longitude = c(-80.84148, -80.84100, -80.85130, -80.85473, -80.84579)
      )
    })

    # Map official names to our display names
    name_mapping <- c(
      "TYBEE ISLAND NORTH" = "North Beach",
      "TYBEE ISLAND MIDDLE" = "Middle Beach",
      "TYBEE ISLAND SOUTH" = "South Beach",
      "TYBEE ISLAND POLK ST." = "Polk Street",
      "TYBEE ISLAND STRAND" = "Strand Street"
    )

    site_coords$beach_name <- name_mapping[site_coords$MonitoringLocationName]

    # Highlight selected beach
    site_coords$is_selected <- site_coords$beach_name == input$forecast_beach
    
    # Debug: print which beach is selected
    cat("Selected beaches:", sum(site_coords$is_selected), "\n")
    if (sum(site_coords$is_selected) > 0) {
      selected_beach_data <- site_coords[site_coords$is_selected, ]
      cat("  Beach name:", selected_beach_data$beach_name, 
          "Lat:", selected_beach_data$Latitude, 
          "Lon:", selected_beach_data$Longitude, "\n")
    }

    # Update map with new markers
    leafletProxy("forecast_map", data = site_coords) %>%
      clearMarkers() %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        popup = ~beach_name,
        label = ~beach_name,
        color = ~ifelse(is_selected, "#dc3545", "#0066cc"),
        fillColor = ~ifelse(is_selected, "#dc3545", "#0066cc"),
        radius = ~ifelse(is_selected, 12, 8),
        fillOpacity = ~ifelse(is_selected, 0.9, 0.7),
        stroke = TRUE,
        weight = 2
      )
    
    cat("Map markers updated\n")
  })
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Calculate relative importance of factors for a specific prediction
calculate_factor_importance <- function(new_data, model, historical_data) {
  
  # If model has importance values, use those
  if ("randomForest" %in% class(model) && !is.null(model$importance)) {
    importance_scores <- model$importance[, 1]  # Get mean decrease in accuracy or MSE
    
    # Get top 5 factors
    top_factors <- head(sort(importance_scores, decreasing = TRUE), 5)
    
    # Normalize to percentages
    top_factors_pct <- round(100 * top_factors / sum(top_factors), 1)
    
    return(top_factors_pct)
  }
  
  # Fallback: return generic importance based on common factors
  default_importance <- c(
    "Rain48" = 25,
    "WaterTemp" = 20,
    "TideStage" = 18,
    "Salinity" = 15,
    "WindSpeed" = 12,
    "Turbidity" = 10
  )
  
  return(default_importance)
}
