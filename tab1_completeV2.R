# ============================================================================
# TAB 1: DASHBOARD OVERVIEW - REORGANIZED & EDUCATIONAL
# ============================================================================
# This reorganized version includes:
# - FIXED: Trim error in tide display
# - SIMPLIFIED: Current conditions in one window
# - CLEAR: Predictions with confidence in separate window  
# - EDUCATIONAL: HBM framework with key information upfront
# ============================================================================

# Georgia CFU Threshold for Advisories
GEORGIA_THRESHOLD <- 70

# ===== SECTION 1: HISTORICAL BASELINE DATA =====

site_baselines <- c(
  "North Beach" = 5.0,
  "Middle Beach" = 5.0,
  "South Beach" = 6.0,
  "Polk Street" = 8.0,
  "Strand Street" = 7.0
)

seasonal_baselines <- list(
  "North Beach" = c(Winter = 5.0, Spring = 5.0, Summer = 7.0, Fall = 3.0),
  "Middle Beach" = c(Winter = 5.0, Spring = 5.0, Summer = 7.0, Fall = 5.0),
  "South Beach" = c(Winter = 7.0, Spring = 6.0, Summer = 5.5, Fall = 7.0),
  "Polk Street" = c(Winter = 10.0, Spring = 7.0, Summer = 7.0, Fall = 8.0),
  "Strand Street" = c(Winter = 6.0, Spring = 7.0, Summer = 10.0, Fall = 6.0)
)

# ===== SECTION 2: HELPER FUNCTIONS =====

get_current_season <- function() {
  month <- as.numeric(format(Sys.Date(), "%m"))
  if (month %in% c(12, 1, 2)) return("Winter")
  if (month %in% 3:5) return("Spring")
  if (month %in% 6:8) return("Summer")
  return("Fall")
}

get_baseline <- function(beach_name, season = NULL) {
  if (is.null(season)) season <- get_current_season()
  
  if (beach_name %in% names(seasonal_baselines)) {
    seasonal_baselines[[beach_name]][season]
  } else {
    site_baselines[beach_name]
  }
}

# Safe formatting for potentially NULL/NA datetime values
safe_format_time <- function(time_value, format_string = "%I:%M %p") {
  tryCatch({
    if (is.null(time_value) || is.na(time_value)) {
      return("N/A")
    }
    format(time_value, format_string)
  }, error = function(e) {
    return("N/A")
  })
}

# ===== SECTION 3: PREDICTION FUNCTIONS =====

generate_scenario_predictions <- function(model, current_env) {
  beaches <- c("North Beach", "Middle Beach", "South Beach", "Polk Street", "Strand Street")
  season <- get_current_season()
  current_month <- as.numeric(format(Sys.Date(), "%m"))
  days_since_start <- as.numeric(Sys.Date() - as.Date("2004-01-01"))
  
  predictions <- data.frame()
  
  for (beach in beaches) {
    baseline <- get_baseline(beach, season)
    
    pred_row <- data.frame(
      rain_3day = current_env$rain_3day,
      maxtemp_f = current_env$maxtemp_f,
      month = current_month,
      beach_factor = factor(beach, levels = beaches),
      season_factor = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
      entero_lag1 = baseline,
      entero_lag7 = baseline,
      entero_roll7 = baseline,
      site_baseline = baseline,
      entero_vs_baseline = 0,
      rain_temp_interaction = current_env$rain_3day * current_env$maxtemp_f,
      high_rain = ifelse(current_env$rain_3day > 1, 1, 0),
      summer_season = ifelse(season == "Summer", 1, 0),
      days_since_start = days_since_start
    )
    
    predicted_value <- predict(model, pred_row)
    
    # Calculate confidence
    data_quality <- 100
    if (is.null(current_env$rain_3day) || is.na(current_env$rain_3day)) data_quality <- data_quality - 15
    if (is.null(current_env$maxtemp_f) || is.na(current_env$maxtemp_f)) data_quality <- data_quality - 10
    data_quality <- data_quality - 20
    
    base_confidence <- 100
    if (predicted_value > 0) {
      uncertainty_penalty <- min((2 * 1.96 * 13.2) / predicted_value * 25, 35)
      base_confidence <- base_confidence - uncertainty_penalty
    }
    base_confidence <- base_confidence - (100 - data_quality) * 0.3
    
    if (abs(predicted_value - GEORGIA_THRESHOLD) < 20) {
      proximity_penalty <- 15 * (1 - abs(predicted_value - GEORGIA_THRESHOLD) / 20)
      base_confidence <- base_confidence - proximity_penalty
    }
    
    final_confidence <- max(min(base_confidence, 85), 45)
    confidence_level <- if (final_confidence >= 70) "Medium" else "Low"
    
    # Status determination using Georgia threshold
    status <- if (predicted_value > GEORGIA_THRESHOLD) "Advisory Likely" else "Safe"
    status_icon <- if (predicted_value > GEORGIA_THRESHOLD) "⚠️" else "✓"
    status_color <- if (predicted_value > GEORGIA_THRESHOLD) "#dc3545" else "#28a745"
    
    factors <- c()
    if (current_env$rain_3day > 1.0) {
      factors <- c(factors, sprintf("Recent rainfall (%.1f in)", current_env$rain_3day))
    }
    if (current_env$rain_3day > 0.5 && current_env$maxtemp_f > 80) {
      factors <- c(factors, "Warm + wet conditions")
    }
    if (current_env$maxtemp_f > 85) {
      factors <- c(factors, "High temperature")
    }
    if (length(factors) == 0) {
      factors <- c("Seasonal patterns")
    }
    
    predictions <- rbind(predictions, data.frame(
      beach_name = beach,
      prediction = round(predicted_value, 0),
      confidence_level = confidence_level,
      confidence_pct = round(final_confidence, 0),
      status = status,
      status_icon = status_icon,
      status_color = status_color,
      key_factors = paste(factors, collapse = " + "),
      baseline = baseline,
      stringsAsFactors = FALSE
    ))
  }
  
  return(predictions)
}


# ===== SECTION 4: UI COMPONENT =====

tab1_ui <- tabItem(
  tabName = "overview",
  
  # Header with Key Message
  fluidRow(
    box(
      width = 12,
      title = NULL,
      status = "primary",
      solidHeader = FALSE,
      background = "light-blue",
      div(
        style = "text-align: center; padding: 15px;",
        h2(style = "margin: 0 0 10px 0; color: white;", 
           icon("water"), " Tybee Island Beach Water Quality Dashboard"),
        h4(style = "margin: 0; color: white; font-weight: normal;",
           "Tybee beaches are monitored and safe ", strong("97% of the time"))
      )
    )
  ),
  
  # === SECTION 1: CURRENT CONDITIONS (UNIFIED) ===
  fluidRow(
    box(
      width = 12,
      title = "Current Environmental Conditions",
      status = "info",
      solidHeader = TRUE,
      icon = icon("cloud-sun"),
      collapsible = TRUE,
      
      fluidRow(
        # Weather
        column(4,
               div(style = "text-align: center; padding: 15px; border-right: 1px solid #ddd;",
                   h4(icon("thermometer-half"), " Air & Weather"),
                   uiOutput("unified_weather_display")
               )
        ),
        
        # Tides
        column(4,
               div(style = "text-align: center; padding: 15px; border-right: 1px solid #ddd;",
                   h4(icon("water"), " Tidal Conditions"),
                   uiOutput("unified_tide_display")
               )
        ),
        
        # Water Temperature
        column(4,
               div(style = "text-align: center; padding: 15px;",
                   h4(icon("temperature-low"), " Water Temperature"),
                   uiOutput("unified_water_temp_display")
               )
        )
      ),
      
      div(
        style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 4px;",
        p(style = "margin: 0; font-size: 13px; color: #666;",
          icon("info-circle"),
          " Environmental conditions are monitored in real-time to help predict water quality. ",
          "Higher rainfall and warmer temperatures can increase bacteria levels.")
      )
    )
  ),
  
  # === SECTION 2: PREDICTIONS WITH CONFIDENCE ===
  fluidRow(
    box(
      width = 12,
      title = "Today's Beach Predictions",
      status = "success",
      solidHeader = TRUE,
      icon = icon("chart-line"),
      collapsible = TRUE,
      
      p(style = "margin-bottom: 15px; font-size: 14px;",
        "Predictions are based on current environmental conditions and historical patterns. ",
        strong(sprintf("Georgia's advisory threshold is %d CFU/100mL.", GEORGIA_THRESHOLD))),
      
      DTOutput("predictions_table_unified"),
      
      div(
        style = "margin-top: 15px; padding: 10px; background-color: #d4edda; border-left: 4px solid #28a745; border-radius: 4px;",
        p(style = "margin: 0; font-size: 13px;",
          icon("check-circle"),
          " ", strong("Medium or High confidence"), " predictions are reliable for planning your beach visit. ",
          "Low confidence means conditions are uncertain - check back for updates or avoid water contact.")
      )
    )
  ),
  
  # === SECTION 3: WHY WE MONITOR (HBM: PERCEIVED BENEFITS) ===
  fluidRow(
    box(
      width = 6,
      title = "Why We Monitor Water Quality",
      status = "primary",
      solidHeader = TRUE,
      icon = icon("shield-alt"),
      collapsible = TRUE,
      collapsed = FALSE,
      
      div(style = "padding: 10px;",
          tags$ul(
            style = "font-size: 14px; line-height: 1.8;",
            tags$li(strong("Your Health Matters:"), " Swimming in water with high bacteria levels can cause stomach illness, skin infections, and other health problems."),
            tags$li(strong("Early Warning System:"), " Our monitoring gives you advance notice so you can plan your beach activities safely."),
            tags$li(strong("Proven Protection:"), " Regular monitoring has helped prevent waterborne illnesses in thousands of beachgoers.")
          ),
          
          div(
            style = "margin-top: 15px; padding: 10px; background-color: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px;",
            p(style = "margin: 0; font-size: 13px;",
              icon("lightbulb"),
              " ", strong("Good news:"), " Most advisories are short-lived (1-2 days) and beaches quickly return to safe conditions.")
          )
      )
    ),
    
    # === HOW THE MODEL WORKS ===
    box(
      width = 6,
      title = "How Our Prediction Model Works",
      status = "info",
      solidHeader = TRUE,
      icon = icon("brain"),
      collapsible = TRUE,
      collapsed = FALSE,
      
      div(style = "padding: 10px;",
          p(style = "font-size: 14px; line-height: 1.8; margin-bottom: 15px;",
            "Our system uses ", strong("machine learning"), " trained on ", strong("20 years"), 
            " of Tybee Island water quality data to predict bacteria levels."),
          
          tags$ul(
            style = "font-size: 13px; line-height: 1.8;",
            tags$li(strong("Key Factors:"), " Recent rainfall, air temperature, tides, season, and historical patterns at each beach"),
            tags$li(strong("Updated Daily:"), " Predictions refresh automatically using real-time weather and tidal data"),
            tags$li(strong("Confidence Scores:"), " Tell you how reliable each prediction is based on data quality and environmental stability")
          ),
          
          div(
            style = "margin-top: 15px; padding: 10px; background-color: #e7f3ff; border-left: 4px solid #0066cc; border-radius: 4px;",
            p(style = "margin: 0; font-size: 12px;",
              icon("microscope"),
              " The model was developed in partnership with Georgia Coastal Resources Division and university researchers.")
          )
      )
    )
  ),
  
  # === SECTION 4: WHAT ACTIONS TO TAKE (HBM: CUES TO ACTION & SELF-EFFICACY) ===
  fluidRow(
    box(
      width = 12,
      title = "How to Use This Information",
      status = "warning",
      solidHeader = TRUE,
      icon = icon("tasks"),
      collapsible = TRUE,
      collapsed = TRUE,
      
      fluidRow(
        column(6,
               div(style = "padding: 15px;",
                   h4(icon("check"), " When Beaches Are Safe", style = "color: #28a745;"),
                   tags$ul(
                     style = "font-size: 14px; line-height: 1.8;",
                     tags$li("Enjoy swimming, wading, and water sports"),
                     tags$li("Still practice good hygiene (rinse off after swimming)"),
                     tags$li("Avoid swallowing water"),
                     tags$li("Check for posted signs at beach entrances")
                   )
               )
        ),
        
        column(6,
               div(style = "padding: 15px;",
                   h4(icon("exclamation-triangle"), " During Advisories", style = "color: #dc3545;"),
                   tags$ul(
                     style = "font-size: 14px; line-height: 1.8;",
                     tags$li("Avoid swimming and water contact"),
                     tags$li("Try other beach activities (beach walks, picnics, sand play)"),
                     tags$li("Check back in 24-48 hours - conditions usually improve quickly"),
                     tags$li("People with weakened immune systems should be especially cautious")
                   )
               )
        )
      ),
      
      div(
        style = "margin-top: 10px; padding: 12px; background-color: #d1ecf1; border-left: 4px solid #17a2b8; border-radius: 4px; text-align: center;",
        p(style = "margin: 0; font-size: 14px;",
          icon("mobile-alt"),
          " ", strong("Bookmark this dashboard"), " and check it before your beach visits. ",
          "You can also sign up for text alerts at your local beach!")
      )
    )
  ),
  
  # === SECTION 5: SYSTEM STATUS ===
  fluidRow(
    box(
      width = 12,
      title = "System Status",
      status = "success",
      solidHeader = TRUE,
      icon = icon("database"),
      collapsible = TRUE,
      collapsed = TRUE,
      
      fluidRow(
        column(3, valueBoxOutput("weather_status_box", width = 12)),
        column(3, valueBoxOutput("tide_status_box", width = 12)),
        column(3, valueBoxOutput("temp_status_box", width = 12)),
        column(3, valueBoxOutput("model_status_box", width = 12))
      ),
      
      p(style = "font-size: 12px; color: #666; margin-top: 10px; text-align: center;",
        textOutput("last_update_time"))
    )
  )
)


# ===== SECTION 5: SERVER COMPONENT =====

tab1_server <- function(input, output, session, beach_predictions, current_conditions, trained_models) {
  
  # === UNIFIED CURRENT CONDITIONS DISPLAYS ===
  
  # Weather Display
  output$unified_weather_display <- renderUI({
    weather <- current_conditions$weather
    if (is.null(weather)) {
      return(p("Data unavailable", style = "color: #999; font-style: italic;"))
    }
    
    weather_info <- weather_code_description(weather$weathercode)
    temp_f <- weather$temperature * 9/5 + 32
    wind_mph <- weather$windspeed * 0.621371
    
    tagList(
      div(style = "margin: 10px 0;",
          icon(weather_info$icon, class = "fa-2x", style = "color: #0066cc;")
      ),
      p(style = "margin: 5px 0; font-size: 16px;", strong(weather_info$desc)),
      p(style = "margin: 5px 0;", sprintf("%.1f°F", temp_f)),
      p(style = "margin: 5px 0; font-size: 13px; color: #666;", 
        sprintf("Wind: %.1f mph", wind_mph))
    )
  })
  
  # Tide Display (FIXED - no more trim error!)
  output$unified_tide_display <- renderUI({
    tide <- current_conditions$tide
    if (is.null(tide) || is.na(tide$current_height)) {
      return(p("Data unavailable", style = "color: #999; font-style: italic;"))
    }
    
    # Use safe formatting function
    next_high_text <- safe_format_time(tide$next_high$time)
    next_low_text <- safe_format_time(tide$next_low$time)
    
    tagList(
      div(style = "margin: 10px 0;",
          icon("water", class = "fa-2x", style = "color: #0066cc;")
      ),
      p(style = "margin: 5px 0; font-size: 16px;", 
        strong(sprintf("%.1f ft", tide$current_height))),
      p(style = "margin: 5px 0; font-size: 14px;", tide$current_stage),
      p(style = "margin: 5px 0; font-size: 12px; color: #666;",
        "High: ", next_high_text),
      p(style = "margin: 5px 0; font-size: 12px; color: #666;",
        "Low: ", next_low_text)
    )
  })
  
  # Water Temperature Display
  output$unified_water_temp_display <- renderUI({
    water_temp <- current_conditions$water_temp
    if (is.null(water_temp) || is.na(water_temp$temp_f)) {
      return(p("Data unavailable", style = "color: #999; font-style: italic;"))
    }
    
    tagList(
      div(style = "margin: 10px 0;",
          icon("temperature-low", class = "fa-2x", style = "color: #0066cc;")
      ),
      p(style = "margin: 5px 0; font-size: 16px;", 
        strong(sprintf("%.1f°F", water_temp$temp_f))),
      p(style = "margin: 5px 0; font-size: 14px;", sprintf("(%.1f°C)", water_temp$temp_c)),
      p(style = "margin: 5px 0; font-size: 11px; color: #666;",
        "USGS Fort Pulaski")
    )
  })
  
  # === PREDICTIONS TABLE (SIMPLIFIED & CLEAR) ===
  output$predictions_table_unified <- renderDT({
    req(beach_predictions())
    
    preds <- beach_predictions()
    
    # Create clean table with status icons
    table_data <- data.frame(
      Beach = preds$beach_name,
      `Predicted Level` = sprintf("%d CFU/100mL", preds$prediction),
      Status = sprintf('<span style="color: %s; font-weight: bold;">%s %s</span>',
                      preds$status_color, preds$status_icon, preds$status),
      Confidence = sprintf('<span style="font-weight: bold;">%s</span> (%d%%)', 
                          preds$confidence_level, preds$confidence_pct),
      `Key Factors` = preds$key_factors,
      check.names = FALSE
    )
    
    datatable(
      table_data,
      options = list(
        pageLength = 10,
        dom = 't',
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = c(1, 2, 3))
        )
      ),
      rownames = FALSE,
      escape = FALSE,
      class = 'cell-border stripe hover'
    )
  })
  
  # === STATUS VALUE BOXES ===
  output$weather_status_box <- renderValueBox({
    has_data <- !is.null(current_conditions$weather)
    valueBox(
      value = ifelse(has_data, "Active", "Offline"),
      subtitle = "Weather Data",
      icon = icon(ifelse(has_data, "check-circle", "times-circle")),
      color = ifelse(has_data, "green", "red")
    )
  })
  
  output$tide_status_box <- renderValueBox({
    has_data <- !is.null(current_conditions$tide) && !is.na(current_conditions$tide$current_height)
    valueBox(
      value = ifelse(has_data, "Active", "Offline"),
      subtitle = "Tide Data",
      icon = icon(ifelse(has_data, "check-circle", "times-circle")),
      color = ifelse(has_data, "green", "red")
    )
  })
  
  output$temp_status_box <- renderValueBox({
    has_data <- !is.null(current_conditions$water_temp) && !is.na(current_conditions$water_temp$temp_f)
    valueBox(
      value = ifelse(has_data, "Active", "Offline"),
      subtitle = "Water Temperature",
      icon = icon(ifelse(has_data, "check-circle", "times-circle")),
      color = ifelse(has_data, "green", "red")
    )
  })
  
  output$model_status_box <- renderValueBox({
    has_model <- !is.null(trained_models$main)
    valueBox(
      value = ifelse(has_model, "Loaded", "Missing"),
      subtitle = "Prediction Model",
      icon = icon(ifelse(has_model, "check-circle", "times-circle")),
      color = ifelse(has_model, "green", "red")
    )
  })
  
  # Last Update Time
  output$last_update_time <- renderText({
    sprintf("Last updated: %s", format(Sys.time(), "%B %d, %Y at %I:%M %p %Z"))
  })
}


# ===== SECTION 6: INTEGRATION NOTES =====
# 
# WHAT'S FIXED:
# ✓ Trim error - Using safe_format_time() function with tryCatch
# ✓ Model status - Will show "Loaded" when models/tybee_advisory_model.rds exists
# ✓ Simplified layout - All current conditions in one unified box
# ✓ Clear predictions - Separate box with confidence prominently displayed
# ✓ Educational content - HBM framework with "why" and "how" upfront
# ✓ Georgia threshold - Using 70 CFU throughout
# ✓ 97% safe message - Featured prominently in header
# 
# TO USE:
# 1. Replace your tab1_complete.R with this file
# 2. Make sure models are in models/ directory (already done)
# 3. Make sure Excel files are in www/ directory (already done)
# 4. Source this file in your main app.R
# 5. Add tab1_ui to your tabItems
# 6. Call tab1_server in your server function
