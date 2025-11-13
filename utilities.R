# ============================================================================
# UTILITIES.R - Helper Functions for Tybee Water Quality App
# ============================================================================
# This file contains utility functions for API calls, data processing,
# and other helper functions to keep the main app file cleaner
# ============================================================================

# ===== API FUNCTIONS =====

# Get USGS water temperature data
get_usgs_water_temp <- function(verbose = FALSE) {
  site <- "02198980"  # Fort Pulaski
  param <- "00010"    # Water temperature in Celsius
  
  usgs_url <- paste0("https://waterservices.usgs.gov/nwis/iv/?format=json&sites=", 
                     site, "&parameterCd=", param, "&period=P1D")
  
  tryCatch({
    response <- GET(usgs_url, timeout(15))
    
    if (response$status_code == 200) {
      content <- fromJSON(rawToChar(response$content))
      
      if ("value" %in% names(content) && "timeSeries" %in% names(content$value)) {
        ts_list <- content$value$timeSeries
        
        # Go directly to Time Series 3 where we know the data is
        if (length(ts_list) >= 3) {
          ts3 <- ts_list[[3]]
          
          if (is.list(ts3) && length(ts3) > 0) {
            element1 <- ts3[[1]]
            
            if (is.data.frame(element1) && "value" %in% names(element1)) {
              value_list <- element1$value
              
              if (is.list(value_list) && length(value_list) > 0) {
                temp_data <- value_list[[1]]
                
                if (is.data.frame(temp_data) && nrow(temp_data) > 0) {
                  if ("value" %in% names(temp_data)) {
                    valid_data <- temp_data[!is.na(temp_data$value) & temp_data$value != "", ]
                    
                    if (nrow(valid_data) > 0) {
                      latest_row <- tail(valid_data, 1)
                      temp_celsius <- as.numeric(latest_row$value)
                      
                      if (!is.na(temp_celsius) && temp_celsius > -5 && temp_celsius < 45) {
                        temp_fahrenheit <- round((temp_celsius * 9/5) + 32, 1)
                        
                        # Get timestamp
                        if ("dateTime" %in% names(latest_row)) {
                          timestamp <- as.POSIXct(latest_row$dateTime)
                        } else {
                          timestamp <- Sys.time()
                        }
                        
                        cat("âœ“ Water temperature:", temp_fahrenheit, "Â°F\n")
                        
                        return(list(
                          temp_f = temp_fahrenheit,
                          temp_c = temp_celsius,
                          timestamp = timestamp,
                          source = "USGS Fort Pulaski, GA"
                        ))
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
    if (verbose) cat("No water temperature data extracted\n")
    return(NULL)
    
  }, error = function(e) {
    if (verbose) cat("Error fetching water temperature:", e$message, "\n")
    return(NULL)
  })
}

# Get current weather from Tybee Island area
get_tybee_weather <- function() {
  url <- "https://api.open-meteo.com/v1/forecast?latitude=32.0&longitude=-80.85&current_weather=true"
  response <- GET(url)
  if (response$status_code == 200) {
    content <- fromJSON(rawToChar(response$content))
    return(content$current_weather)
  } else {
    return(NULL)
  }
}

# Convert weather code to description and icon
weather_code_description <- function(code) {
  codes <- list(
    "0" = list(icon = "sun", desc = "Clear sky"),
    "1" = list(icon = "cloud-sun", desc = "Mainly clear"),
    "2" = list(icon = "cloud", desc = "Partly cloudy"),
    "3" = list(icon = "cloud", desc = "Overcast"),
    "45" = list(icon = "smog", desc = "Fog"),
    "48" = list(icon = "snowflake", desc = "Depositing rime fog"),
    "51" = list(icon = "cloud-drizzle", desc = "Drizzle: Light"),
    "53" = list(icon = "cloud-showers-heavy", desc = "Drizzle: Moderate"),
    "55" = list(icon = "cloud-showers-heavy", desc = "Drizzle: Dense intensity"),
    "61" = list(icon = "cloud-rain", desc = "Rain: Slight"),
    "63" = list(icon = "cloud-rain", desc = "Rain: Moderate"),
    "65" = list(icon = "cloud-showers-heavy", desc = "Rain: Heavy intensity"),
    "71" = list(icon = "snowflake", desc = "Snow fall: Slight"),
    "73" = list(icon = "snowflake", desc = "Snow fall: Moderate"),
    "75" = list(icon = "snowflake", desc = "Snow fall: Heavy intensity"),
    "95" = list(icon = "bolt", desc = "Thunderstorm")
  )
  
  code_str <- as.character(code)
  if (code_str %in% names(codes)) {
    return(codes[[code_str]])
  } else {
    return(list(icon = "question", desc = "Unknown weather condition"))
  }
}

# Get NOAA tidal data for Tybee Light station
get_noaa_tidal_data <- function(verbose = FALSE) {
  tryCatch({
    # Format current date for the API
    current_date <- format(Sys.Date(), "%Y%m%d")
    tomorrow_date <- format(Sys.Date() + 1, "%Y%m%d")
    
    result <- list(
      current_height = NA,
      current_stage = "Unknown",
      next_high = list(time = NA, height = NA),
      next_low = list(time = NA, height = NA),
      data_source = "NOAA CO-OPS",
      station_id = "8670892",
      station_name = "Tybee Light, GA"
    )
    
    # Try real-time water levels first
    water_level_url <- paste0("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                              "begin_date=", current_date, "&end_date=", current_date,
                              "&station=8670892&product=water_level&datum=MLLW&",
                              "time_zone=lst_ldt&units=english&application=TybeeNowCast&format=json")
    
    # Try the primary water level API
    wl_response <- GET(water_level_url, timeout(10))
    
    if (wl_response$status_code == 200) {
      wl_data <- fromJSON(rawToChar(wl_response$content))
      
      if (!is.null(wl_data$data) && nrow(wl_data$data) > 0) {
        latest_data <- tail(wl_data$data, 1)
        result$current_height <- round(as.numeric(latest_data$v), 1)
        
        if (result$current_height < 2.5) {
          result$current_stage <- "Low"
        } else if (result$current_height > 6.5) {
          result$current_stage <- "High"
        } else {
          result$current_stage <- "Mid-tide"
        }
      }
    }
    
    # If primary API failed, try fallback approaches
    if (wl_response$status_code != 200 || is.na(result$current_height)) {
      if (verbose) cat("Primary API unavailable, trying Fort Pulaski station...\n")
      
      # Try Fort Pulaski station as fallback
      fort_url <- paste0("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                         "begin_date=", current_date, "&end_date=", current_date,
                         "&station=8670870&product=water_level&datum=MLLW&",
                         "time_zone=lst_ldt&units=english&application=TybeeNowCast&format=json")
      
      fort_response <- GET(fort_url, timeout(10))
      
      if (fort_response$status_code == 200) {
        fort_data <- fromJSON(rawToChar(fort_response$content))
        if (!is.null(fort_data$data) && nrow(fort_data$data) > 0) {
          latest_data <- tail(fort_data$data, 1)
          result$current_height <- round(as.numeric(latest_data$v), 1)
          
          if (result$current_height < 2.5) {
            result$current_stage <- "Low"
          } else if (result$current_height > 6.5) {
            result$current_stage <- "High"
          } else {
            result$current_stage <- "Mid-tide"
          }
          result$station_name <- "Fort Pulaski, GA (nearby)"
          result$station_id <- "8670870"
          cat("âœ“ Tide data from Fort Pulaski:", result$current_stage, "\n")
        }
      }
    }
    
    # Get tide predictions for high/low times
    pred_url <- paste0("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                       "begin_date=", current_date, "&end_date=", tomorrow_date,
                       "&station=8670892&product=predictions&datum=MLLW&",
                       "time_zone=lst_ldt&units=english&interval=hilo&",
                       "application=TybeeNowCast&format=json")
    
    pred_response <- GET(pred_url, timeout(10))
    
    if (pred_response$status_code == 200) {
      pred_data <- fromJSON(rawToChar(pred_response$content))
      
      if (!is.null(pred_data$predictions) && nrow(pred_data$predictions) > 0) {
        predictions <- pred_data$predictions
        predictions$datetime <- as.POSIXct(predictions$t, format = "%Y-%m-%d %H:%M")
        predictions$height <- as.numeric(predictions$v)
        
        # Find future high and low tides
        current_time <- Sys.time()
        future_preds <- predictions[predictions$datetime > current_time, ]
        
        if (nrow(future_preds) > 0) {
          # Next high tide
          next_high <- future_preds[future_preds$type == "H", ][1, ]
          if (!is.na(next_high$datetime)) {
            result$next_high$time <- format(next_high$datetime, "%I:%M %p")
            result$next_high$height <- round(next_high$height, 1)
            cat("âœ“ Next high tide:", result$next_high$time, "at", result$next_high$height, "ft\n")
          }
          
          # Next low tide
          next_low <- future_preds[future_preds$type == "L", ][1, ]
          if (!is.na(next_low$datetime)) {
            result$next_low$time <- format(next_low$datetime, "%I:%M %p")
            result$next_low$height <- round(next_low$height, 1)
            cat("âœ“ Next low tide:", result$next_low$time, "at", result$next_low$height, "ft\n")
          }
        }
      }
    }
    
    return(result)
    
  }, error = function(e) {
    if (verbose) cat("Error fetching tidal data:", e$message, "\n")
    return(list(
      current_height = NA,
      current_stage = "Unknown",
      next_high = list(time = NA, height = NA),
      next_low = list(time = NA, height = NA),
      data_source = "NOAA CO-OPS",
      station_id = "8670892",
      station_name = "Tybee Light, GA"
    ))
  })
}

# ===== DATA PROCESSING FUNCTIONS =====

# Determine water advisory status based on bacterial levels
determine_advisory_status <- function(bacteria_level, threshold = 70) {
  if (is.na(bacteria_level)) {
    return(list(
      status = "Unknown",
      color = "gray",
      severity = "unknown"
    ))
  }
  
  if (bacteria_level <= threshold) {
    return(list(
      status = "No Advisory",
      color = "green",
      severity = "safe"
    ))
  } else {
    return(list(
      status = "Advisory Recommended",
      color = "red",
      severity = "warning"
    ))
  }
}

# Calculate risk level based on bacterial count
calculate_risk_level <- function(bacteria_count) {
  if (is.na(bacteria_count)) return("Unknown")
  
  if (bacteria_count < 35) {
    return("Very Low")
  } else if (bacteria_count < 70) {
    return("Low")
  } else if (bacteria_count < 140) {
    return("Moderate")
  } else if (bacteria_count < 280) {
    return("High")
  } else {
    return("Very High")
  }
}

# Make bacterial prediction using models or enhanced estimation
make_bacterial_prediction <- function(beach_name, rainfall_3day, max_temp, 
                                     water_temp = NULL, month, season, models = NULL) {
  
  # Try using trained model first
  if (!is.null(models) && !is.null(models$main)) {
    tryCatch({
      prediction_data <- data.frame(
        Rainfall_3day = rainfall_3day,
        MaxTemp = max_temp,
        Month = month,
        Season = season,
        Beach = beach_name
      )
      
      predicted_value <- predict(models$main, newdata = prediction_data)
      predicted_bacteria <- max(1, round(predicted_value))
      model_used <- "Random Forest"
      
    }, error = function(e) {
      # Fall through to enhanced estimation
      predicted_bacteria <- NULL
      model_used <- NULL
    })
  } else {
    predicted_bacteria <- NULL
    model_used <- NULL
  }
  
  # If model prediction failed or unavailable, use enhanced estimation
  if (is.null(predicted_bacteria)) {
    base_level <- 35
    
    # Rainfall factor
    rainfall_factor <- 1 + (rainfall_3day * 0.5)
    
    # Temperature factor
    if (!is.null(water_temp)) {
      # Use actual water temperature if available
      temp_factor <- ifelse(water_temp > 75, 1.3, 
                           ifelse(water_temp > 65, 1.1, 0.9))
    } else {
      # Fallback to air temperature
      temp_factor <- ifelse(max_temp > 85, 1.3, 
                           ifelse(max_temp > 75, 1.1, 0.9))
    }
    
    # Seasonal factor
    seasonal_factor <- switch(season,
                             "Summer" = 1.3,
                             "Fall" = 1.1,
                             "Spring" = 1.0,
                             "Winter" = 0.8,
                             1.0)
    
    # Beach-specific baseline
    beach_factor <- switch(beach_name,
                          "South Beach" = 1.15,
                          "Polk Street" = 1.1,
                          "Middle Beach" = 1.0,
                          "North Beach" = 0.95,
                          "Strand Street" = 0.9,
                          1.0)
    
    predicted_bacteria <- round(base_level * rainfall_factor * temp_factor * 
                               seasonal_factor * beach_factor)
    predicted_bacteria <- max(1, predicted_bacteria)
    model_used <- ifelse(!is.null(water_temp), 
                        "Enhanced (with water temp)", 
                        "Enhanced")
  }
  
  # Calculate confidence
  confidence_score <- ifelse(!is.null(models$main), 85, 70)
  if (!is.null(water_temp)) confidence_score <- confidence_score + 10
  confidence_score <- min(95, confidence_score)
  
  confidence <- ifelse(confidence_score >= 80, "High",
                      ifelse(confidence_score >= 65, "Medium", "Low"))
  
  # Determine status
  status <- ifelse(predicted_bacteria <= 70, "No Advisory", "Advisory Recommended")
  
  # Calculate probability of exceeding advisory
  if (predicted_bacteria < 50) {
    prob_exceed <- round(5 + (predicted_bacteria / 50) * 15)
  } else if (predicted_bacteria < 70) {
    prob_exceed <- round(20 + ((predicted_bacteria - 50) / 20) * 30)
  } else {
    prob_exceed <- min(95, round(50 + ((predicted_bacteria - 70) / 70) * 45))
  }
  
  return(list(
    predicted_bacteria = predicted_bacteria,
    status = status,
    confidence = confidence,
    confidence_score = confidence_score,
    risk_level = calculate_risk_level(predicted_bacteria),
    prob_exceed_advisory = prob_exceed,
    model_used = model_used
  ))
}

# ===== ADVANCED PREDICTION FUNCTIONS =====

# Enhanced prediction function with all environmental factors
predict_water_quality <- function(rainfall_3day, air_temp, water_temp = NULL, tide_stage, 
                                  wind_speed = 10, uv_index = 5, days_since_rain = 1, 
                                  season = "Summer", weekend = FALSE,
                                  # User-controllable parameters
                                  base_bacteria = 45, rainfall_sensitivity = 1.5, 
                                  uv_disinfection_power = 0.7) {
  
  # Start with user-defined base bacterial level
  predicted_bacteria <- base_bacteria
  
  # Rainfall impact (major factor) - now user controllable
  rainfall_multiplier <- 1 + (rainfall_3day * rainfall_sensitivity)
  if (rainfall_3day > 1.5) rainfall_multiplier <- rainfall_multiplier * 1.3
  
  # Water temperature impact (ENHANCED with actual measurements)
  temp_multiplier <- 1
  if (!is.null(water_temp) && !is.na(water_temp)) {
    # Use actual water temperature for more accurate predictions
    if (water_temp > 78) temp_multiplier <- 1 + ((water_temp - 78) * 0.18)
    if (water_temp < 65) temp_multiplier <- 0.8
    if (water_temp > 85) temp_multiplier <- temp_multiplier * 1.2
  } else {
    # Fall back to air temperature if water temp not available
    if (air_temp > 78) temp_multiplier <- 1 + ((air_temp - 78) * 0.15)
    if (air_temp < 65) temp_multiplier <- 0.85
  }
  
  # Tide stage impact
  tide_multipliers <- c("Low" = 1.3, "Rising" = 1.1, "High" = 0.9, "Falling" = 1.05)
  tide_multiplier <- tide_multipliers[tide_stage]
  if (is.na(tide_multiplier)) tide_multiplier <- 1.0
  
  # Wind speed impact
  wind_multiplier <- 1
  if (wind_speed > 15) wind_multiplier <- 1.25
  if (wind_speed < 5) wind_multiplier <- 1.05
  
  # UV index impact
  uv_multiplier <- 1
  if (uv_index > 7) uv_multiplier <- uv_disinfection_power
  if (uv_index < 3) uv_multiplier <- 1.15
  
  # Days since last rain
  decay_multiplier <- 1 / (1 + days_since_rain * 0.2)
  
  # Seasonal factors
  season_multipliers <- c("Winter" = 0.8, "Spring" = 0.95, "Summer" = 1.2, "Fall" = 1.0)
  season_multiplier <- season_multipliers[season]
  if (is.na(season_multiplier)) season_multiplier <- 1.0
  
  # Weekend effect
  weekend_multiplier <- ifelse(weekend, 1.15, 1.0)
  
  # Calculate predicted bacterial level
  predicted_bacteria <- predicted_bacteria * rainfall_multiplier * temp_multiplier * 
    tide_multiplier * wind_multiplier * uv_multiplier * 
    decay_multiplier * season_multiplier * weekend_multiplier
  
  # Add some natural variability
  log_sd <- 0.3
  predicted_bacteria <- predicted_bacteria * exp(rnorm(1, 0, log_sd))
  
  # Ensure minimum value
  predicted_bacteria <- max(predicted_bacteria, 5)
  
  # Determine advisory status based on 70 CFU threshold
  if (predicted_bacteria <= 70) {
    status <- "No Advisory"
    color <- "green"
    risk_level <- "Low"
  } else {
    status <- "Advisory Recommended"
    color <- "red"
    risk_level <- "High"
  }
  
  # Calculate confidence based on how well-defined the inputs are
  confidence_factors <- c()
  
  if (rainfall_3day > 0.1) confidence_factors <- c(confidence_factors, 0.9)
  else confidence_factors <- c(confidence_factors, 0.75)
  
  # Higher confidence when we have actual water temperature
  if (!is.null(water_temp) && !is.na(water_temp)) {
    confidence_factors <- c(confidence_factors, 0.95)
  } else {
    confidence_factors <- c(confidence_factors, 0.75)
  }
  
  confidence_factors <- c(confidence_factors, 0.85)
  
  overall_confidence <- mean(confidence_factors)
  confidence_level <- ifelse(overall_confidence > 0.85, "High", 
                             ifelse(overall_confidence > 0.75, "Medium", "Low"))
  
  # Probability of exceeding advisory threshold
  prob_exceed_70 <- pnorm(log(70), log(predicted_bacteria), log_sd, lower.tail = FALSE)
  
  return(list(
    predicted_bacteria = round(predicted_bacteria, 1),
    status = status,
    color = color,
    risk_level = risk_level,
    confidence = confidence_level,
    confidence_score = round(overall_confidence * 100),
    prob_exceed_advisory = round(prob_exceed_70 * 100),
    factors = list(
      rainfall = round((rainfall_multiplier - 1) * 100),
      temperature = round((temp_multiplier - 1) * 100),
      tide = round((tide_multiplier - 1) * 100),
      wind = round((wind_multiplier - 1) * 100),
      uv = round((uv_multiplier - 1) * 100),
      decay = round((decay_multiplier - 1) * 100),
      season = round((season_multiplier - 1) * 100)
    )
  ))
}
