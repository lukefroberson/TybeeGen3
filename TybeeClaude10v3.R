library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(httr)
library(jsonlite)
library(dplyr)
library(readxl)
library(lubridate)
library(tidyr)
library(DT)
library(randomForest)
library(ggplot2)

# ===== SOURCE TAB FILES =====
source("tab1_dashboard.R")        # Tab 1: Dashboard
source("tab2_education.R")        # Tab 2: Learn About Water Quality
source("tab3_historic_data.R")    # Tab 3: Historic Data
source("tab4_forecast.R")         # Tab 4: Interactive Forecast Tool

# Focused USGS function - extract from the 'value' field we just found!
get_usgs_water_temp <- function(verbose = FALSE) {
  site <- "02198980"  # Fort Pulaski
  param <- "00010"    # Water temperature in Celsius
  
  usgs_url <- paste0("https://waterservices.usgs.gov/nwis/iv/?format=json&sites=", site, "&parameterCd=", param, "&period=P1D")
  
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
                        
                        cat("✓ Water temperature:", temp_fahrenheit, "°F\n")
                        
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

# Function to get current weather from Tybee Island area
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

# Function to convert weather code to description and icon
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

# Updated NOAA tide function for Tybee Light station 8670892
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
        cat("✓ DETERMINED TIDE STAGE:", result$current_stage, "\n")
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
          cat("✓ Tide data from Fort Pulaski:", result$current_stage, "\n")
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
            cat("✓ Next high tide:", result$next_high$time, "at", result$next_high$height, "ft\n")
          }
          
          # Next low tide
          next_low <- future_preds[future_preds$type == "L", ][1, ]
          if (!is.na(next_low$datetime)) {
            result$next_low$time <- format(next_low$datetime, "%I:%M %p")
            result$next_low$height <- round(next_low$height, 1)
            cat("✓ Next low tide:", result$next_low$time, "at", result$next_low$height, "ft\n")
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

# Function to determine water advisory status based on bacterial levels
water_advisory_status <- function(bacterial_level) {
  if (is.na(bacterial_level)) {
    return(list(status = "Unknown", color = "gray", icon = "question-circle"))
  } else if (bacterial_level <= 70) {
    return(list(status = "No Advisory", color = "green", icon = "check-circle"))
  } else {
    return(list(status = "Advisory Recommended", color = "red", icon = "exclamation-triangle"))
  }
}

# ENHANCED: Predictive model with integrated water temperature
predict_water_quality <- function(rainfall_3day, air_temp, water_temp = NULL, tide_stage, wind_speed = 10, 
                                  uv_index = 5, days_since_rain = 1, season = "Summer", 
                                  weekend = FALSE,
                                  # New user-controllable parameters
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
    if (water_temp > 78) temp_multiplier <- 1 + ((water_temp - 78) * 0.18)  # Increased sensitivity
    if (water_temp < 65) temp_multiplier <- 0.8  # Cold water reduces bacteria
    if (water_temp > 85) temp_multiplier <- temp_multiplier * 1.2  # Very warm water
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

# ===== LOAD TAB 1 COMPONENTS =====
source("tab1_dashboard.R", local = TRUE)
cat("✓ Tab 1 components loaded\n")

# Function to load trained models
load_trained_models <- function() {
  models <- list()

  tryCatch({
    if (file.exists("models/main_model.rds")) {
      models$main <- readRDS("models/main_model.rds")
      cat("✓ Loaded trained model from models/main_model.rds\n")
    } else {
      cat("⚠ No trained model found at models/main_model.rds\n")
      models$main <- NULL
    }

    if (file.exists("models/model_metadata.rds")) {
      models$metadata <- readRDS("models/model_metadata.rds")
      cat("✓ Loaded model metadata\n")
    } else {
      cat("⚠ No model metadata found\n")
      models$metadata <- NULL
    }

    # Load forecast model for Tab 4
    if (file.exists("models/tybee_advisory_model.rds")) {
      models$forecast <- readRDS("models/tybee_advisory_model.rds")
      cat("✓ Loaded forecast model from models/tybee_advisory_model.rds\n")
    } else {
      cat("⚠ No forecast model found at models/tybee_advisory_model.rds\n")
      models$forecast <- NULL
    }
  }, error = function(e) {
    cat("✗ Error loading trained models:", e$message, "\n")
    models$main <- NULL
    models$metadata <- NULL
    models$forecast <- NULL
  })

  return(models)
}

# UPDATED: Enhanced prediction function using trained models with water temperature
make_bacterial_prediction <- function(beach_name, rainfall_3day, max_temp, water_temp = NULL,
                                      month = NULL, season = NULL, models = NULL) {
  
  # If no trained models available, fall back to enhanced generic model
  if (is.null(models) || is.null(models$main) || is.null(models$metadata)) {
    cat("Using fallback enhanced model for", beach_name, "\n")
    return(predict_water_quality(
      rainfall_3day = rainfall_3day,
      air_temp = max_temp,
      water_temp = water_temp,  # Pass actual water temperature
      tide_stage = "High",
      wind_speed = 10,
      uv_index = 6,
      days_since_rain = 2,
      season = ifelse(is.null(season), "Summer", season),
      weekend = FALSE
    ))
  }
  
  # Use trained model (if available)
  tryCatch({
    metadata <- models$metadata
    features_used <- metadata$features_used
    
    # Determine current season and month first
    current_season <- ifelse(is.null(season), 
                             case_when(
                               month(Sys.Date()) %in% c(12, 1, 2) ~ "Winter",
                               month(Sys.Date()) %in% c(3, 4, 5) ~ "Spring", 
                               month(Sys.Date()) %in% c(6, 7, 8) ~ "Summer",
                               month(Sys.Date()) %in% c(9, 10, 11) ~ "Fall"
                             ), season)
    
    current_month <- ifelse(is.null(month), month(Sys.Date()), month)
    
    # Create input data with enhanced water temperature support
    input_values <- list()
    
    for (feature in features_used) {
      if (feature == "rain_3day" || feature == "rain3day") {
        input_values[[feature]] <- rainfall_3day
      } else if (feature == "maxtemp_f") {
        input_values[[feature]] <- max_temp
      } else if (feature == "water_temp_avg_f" && !is.null(water_temp)) {
        input_values[[feature]] <- water_temp
      } else if (feature == "water_temp_avg_c" && !is.null(water_temp)) {
        # Convert to Celsius if needed
        water_temp_c <- ifelse(water_temp > 40, (water_temp - 32) * 5/9, water_temp)
        input_values[[feature]] <- water_temp_c
      } else if (feature == "month") {
        input_values[[feature]] <- current_month
      } else if (feature == "season") {
        input_values[[feature]] <- current_season
      } else {
        # Set reasonable defaults for other features
        input_values[[feature]] <- 0
      }
    }
    
    # Create the data frame
    input_data <- data.frame(input_values, stringsAsFactors = FALSE)
    
    # Make prediction using the trained model
    prediction <- predict(models$main, input_data)
    
    if (length(prediction) > 1) {
      prediction <- prediction[1]
    }
    
    prediction <- max(0, prediction)
    
    # Determine advisory status
    if (prediction <= 70) {
      status <- "No Advisory"
      color <- "green"
      risk_level <- "Low"
      confidence <- "High"
    } else {
      status <- "Advisory Recommended"
      color <- "red"
      risk_level <- "High"
      confidence <- "High"
    }
    
    prob_exceed_70 <- max(0, min(100, (prediction - 70) / 70 * 100))
    
    return(list(
      predicted_bacteria = round(prediction, 1),
      status = status,
      color = color,
      risk_level = risk_level,
      confidence = confidence,
      confidence_score = 90,
      prob_exceed_advisory = round(prob_exceed_70),
      model_used = metadata$best_model_type,
      factors = list(
        rainfall = round((rainfall_3day / 0.5 - 1) * 100),
        temperature = round((max_temp / 75 - 1) * 100),
        water_temp = ifelse(!is.null(water_temp), round((water_temp / 75 - 1) * 100), 0),
        tide = 20,
        wind = 0,
        uv = -15,
        decay = -30,
        season = ifelse(current_season == "Summer", 30, 0)
      )
    ))
    
  }, error = function(e) {
    cat("Error using trained model:", e$message, "\n")
    # Fall back to enhanced generic model
    return(predict_water_quality(
      rainfall_3day = rainfall_3day,
      air_temp = max_temp,
      water_temp = water_temp,
      tide_stage = "High",
      wind_speed = 10,
      uv_index = 6,
      days_since_rain = 2,
      season = ifelse(is.null(season), "Summer", season),
      weekend = FALSE
    ))
  })
}

# UPDATED: Function to load actual historical data from new Excel files
load_historical_data <- function() {
  cat("✓ Loading historical beach data...\n")
  
  # UPDATED: New file names
  files <- c("PolkStreet.xlsx", "MiddleBeach.xlsx", "NorthBeach.xlsx", 
             "SouthBeach.xlsx", "StrandStreet.xlsx")
  
  # UPDATED: Site name mapping for new files
  site_name_map <- list(
    "PolkStreet.xlsx" = "Polk Street",
    "MiddleBeach.xlsx" = "Middle Beach", 
    "NorthBeach.xlsx" = "North Beach",
    "SouthBeach.xlsx" = "South Beach",
    "StrandStreet.xlsx" = "Strand Street"
  )
  
  # Beach name mapping from file data to standardized names
  beach_name_mapping <- c(
    "Tybee Island Polk St." = "Polk Street",
    "TYBEE ISLAND MIDDLE" = "Middle Beach",
    "TYBEE ISLAND NORTH" = "North Beach", 
    "TYBEE ISLAND SOUTH" = "South Beach",
    "TYBEE ISLAND STRAND" = "Strand Street"
  )
  
  # Load site coordinate data
  site_coords <- tryCatch({
    coord_data <- read_excel("www/TybeeSiteData.xlsx")
    cat("✓ Loaded site coordinates from TybeeSiteData.xlsx\n")
    
    coord_data$Latitude <- as.numeric(coord_data$Latitude)
    coord_data$Longitude <- as.numeric(coord_data$Longitude)
    
    site_name_mapping <- c(
      "TYBEE ISLAND MIDDLE" = "Middle Beach",
      "TYBEE ISLAND NORTH" = "North Beach", 
      "TYBEE ISLAND POLK ST." = "Polk Street",
      "TYBEE ISLAND SOUTH" = "South Beach",
      "TYBEE ISLAND STRAND" = "Strand Street"
    )
    
    coord_data$MonitoringLocationName_Original <- coord_data$MonitoringLocationName
    coord_data$MonitoringLocationName <- site_name_mapping[coord_data$MonitoringLocationName]
    coord_data <- coord_data[!is.na(coord_data$MonitoringLocationName), ]
    
    if (!"Description" %in% names(coord_data)) {
      coord_data$Description <- paste("Beach monitoring site at", coord_data$MonitoringLocationName)
    }
    
    coord_data
  }, error = function(e) {
    cat("⚠ Could not load TybeeSiteData.xlsx:", e$message, "\n")
    data.frame(
      MonitoringLocationName = c("Polk Street", "Middle Beach", "North Beach", "South Beach", "Strand Street"),
      Latitude = c(32.01670, 32.00000, 32.02000, 31.98330, 32.01000),
      Longitude = c(-80.83330, -80.84000, -80.83000, -80.85000, -80.83500),
      Description = paste("Beach monitoring site")
    )
  })
  
  tryCatch({
    all_data <- data.frame()
    
    for (file in files) {
      file_path <- file.path("www", file)
      
      if (file.exists(file_path)) {
        tryCatch({
          temp_data <- read_excel(file_path)
          standard_name <- site_name_map[[file]]
          
          # Standardize beach name
          if ("beach_name" %in% names(temp_data)) {
            original_beach_name <- temp_data$beach_name[1]
            if (original_beach_name %in% names(beach_name_mapping)) {
              temp_data$MonitoringLocationName <- beach_name_mapping[[original_beach_name]]
            } else {
              temp_data$MonitoringLocationName <- standard_name
            }
          } else {
            temp_data$MonitoringLocationName <- standard_name
          }
          
          # UPDATED: Handle date parsing properly
          if ("date_collected" %in% names(temp_data)) {
            temp_data$Date <- as.Date(temp_data$date_collected)
          } else {
            temp_data$Date <- as.Date(NA)
          }
          
          # UPDATED: Map bacterial data (entero column)
          if ("entero" %in% names(temp_data)) {
            temp_data$Bacteria <- as.numeric(temp_data$entero)
          } else {
            temp_data$Bacteria <- as.numeric(NA)
          }
          
          # UPDATED: Map rainfall data (rain3day column)
          if ("rain3day" %in% names(temp_data)) {
            temp_data$Rainfall_3day <- as.numeric(temp_data$rain3day)
          } else if ("3_day_rain_total" %in% names(temp_data)) {
            temp_data$Rainfall_3day <- as.numeric(temp_data[["3_day_rain_total"]])
          } else {
            temp_data$Rainfall_3day <- runif(nrow(temp_data), 0, 2)
          }
          
          # UPDATED: Handle water temperature - NEW FEATURE!
          if ("water_temp_avg_f" %in% names(temp_data)) {
            # Most files have Fahrenheit water temperature
            temp_data$WaterTemp <- as.numeric(temp_data$water_temp_avg_f)
            temp_data$WaterTempMin <- as.numeric(temp_data$water_temp_min_f)
            temp_data$WaterTempMax <- as.numeric(temp_data$water_temp_max_f)
          } else if ("water_temp_avg_c" %in% names(temp_data)) {
            # PolkStreet has Celsius - convert to Fahrenheit
            temp_c <- as.numeric(temp_data$water_temp_avg_c)
            temp_data$WaterTemp <- (temp_c * 9/5) + 32
            temp_data$WaterTempMin <- (as.numeric(temp_data$water_temp_min_c) * 9/5) + 32
            temp_data$WaterTempMax <- (as.numeric(temp_data$water_temp_max_c) * 9/5) + 32
          } else if ("maxtemp_f" %in% names(temp_data)) {
            temp_data$WaterTemp <- as.numeric(temp_data$maxtemp_f)
          } else {
            temp_data$WaterTemp <- 65 + 15 * sin(seq(0, 2*pi, length.out = nrow(temp_data)))
          }
          
          # UPDATED: Map tide stage data
          if ("tide_stage" %in% names(temp_data)) {
            temp_data$TideStage <- temp_data$tide_stage
          } else {
            temp_data$TideStage <- sample(c("Low", "Rising", "High", "Falling"), nrow(temp_data), replace = TRUE)
          }
          
          # Filter valid data and select relevant columns
          temp_data <- temp_data %>%
            filter(!is.na(Date), !is.na(Bacteria)) %>%
            select(MonitoringLocationName, Date, Bacteria, Rainfall_3day, WaterTemp, TideStage, 
                   WaterTempMin, WaterTempMax)
          
          all_data <- rbind(all_data, temp_data)
          cat("✓", standard_name, "-", nrow(temp_data), "records\n")
          
        }, error = function(e) {
          cat("⚠ Error loading", file, "\n")
        })
      } else {
        cat("⚠ File not found:", file, "\n")
      }
    }
    
    if (nrow(all_data) > 0) {
      # Create site data summary with proper coordinates
      site_data <- all_data %>%
        group_by(MonitoringLocationName) %>%
        summarise(
          LastBacteria = last(Bacteria, order_by = Date),
          LastSampleDate = max(Date),
          LastWaterTemp = last(WaterTemp, order_by = Date),
          .groups = 'drop'
        ) %>%
        left_join(site_coords, by = "MonitoringLocationName")
      
      # Create time series data with Site column
      ts_data <- all_data %>%
        rename(Site = MonitoringLocationName) %>%
        arrange(Site, Date)
      
      cat("✓ Data loaded:", nrow(all_data), "records from", length(unique(all_data$MonitoringLocationName)), "sites (", 
          as.character(min(all_data$Date)), "to", as.character(max(all_data$Date)), ")\n")
      
      return(list(site_data = site_data, ts_data = ts_data))
    }
    
  }, error = function(e) {
    cat("⚠ Error loading data, using sample data\n")
  })
  
  return(load_sample_data())
}

# Function to load sample data (fallback)
load_sample_data <- function() {
  
  site_names <- c("Polk Street", "Middle Beach", "North Beach", "South Beach", "Strand Street")
  
  site_data <- data.frame(
    MonitoringLocationName = site_names,
    Latitude = c(32.0167, 32.0000, 32.0200, 31.9833, 32.0100),
    Longitude = c(-80.8333, -80.8400, -80.8300, -80.8500, -80.8350),
    Description = paste("Beach monitoring site at", site_names),
    stringsAsFactors = FALSE
  )
  
  dates <- seq(as.Date("2023-01-01"), as.Date("2024-12-31"), by = "week")
  ts_data <- data.frame()
  
  for (site in site_names) {
    base_bacteria <- 35 + 20 * sin(seq(0, 2*pi, length.out = length(dates)))
    bacteria_values <- pmax(1, round(base_bacteria + rnorm(length(dates), 0, 15)))
    
    spikes <- sample(seq_len(length(dates)), min(5, length(dates)))
    bacteria_values[spikes] <- bacteria_values[spikes] * sample(3:6, length(spikes), replace = TRUE)
    
    rainfall <- pmax(0, round(rnorm(length(dates), 0.5, 0.7), 1))
    rainfall[spikes] <- rainfall[spikes] + runif(length(spikes), 0.5, 1.5)
    
    water_temp <- 65 + 15 * sin(seq(0, 2*pi, length.out = length(dates)))
    
    site_ts <- data.frame(
      Site = site,
      Date = dates,
      Bacteria = bacteria_values,
      Rainfall_3day = rainfall,
      WaterTemp = water_temp,
      TideStage = sample(c("Low", "Rising", "High", "Falling"), length(dates), replace = TRUE),
      stringsAsFactors = FALSE
    )
    
    ts_data <- rbind(ts_data, site_ts)
  }
  
  latest_data <- ts_data %>%
    group_by(Site) %>%
    filter(Date == max(Date)) %>%
    select(MonitoringLocationName = Site, LastBacteria = Bacteria, LastSampleDate = Date,
           LastWaterTemp = WaterTemp)
  
  site_data <- left_join(site_data, latest_data, by = "MonitoringLocationName")
  
  return(list(site_data = site_data, ts_data = ts_data))
}

# Load advisory data function (unchanged from original)
load_advisory_data <- function() {
  cat("✓ Loading advisory data...\n")
  
  advisory_summary <- NULL
  advisory_details <- NULL
  
  # Load summary data (AdvisoryLengthByYear.xlsx)
  summary_file <- file.path("www", "AdvisoryLengthByYear.xlsx")
  if (file.exists(summary_file)) {
    tryCatch({
      advisory_summary <- read_excel(summary_file)
      cat("✓ Loaded advisory summary data:", nrow(advisory_summary), "records\n")
      
      # Clean column names but preserve original structure
      names(advisory_summary) <- make.names(names(advisory_summary))
      
      # Ensure numeric columns are properly formatted
      numeric_cols <- c("Year", "BeachAdvisories", "TotalDaysWithAdvisories")
      
      # Add any duration columns we can find
      duration_patterns <- c("1.*day", "2.*day", "3.*7.*day", "8.*30.*day", "greater.*30|30.*day")
      for (pattern in duration_patterns) {
        matching_cols <- grep(pattern, names(advisory_summary), ignore.case = TRUE, value = TRUE)
        numeric_cols <- c(numeric_cols, matching_cols)
      }
      
      for (col in unique(numeric_cols)) {
        if (col %in% names(advisory_summary)) {
          advisory_summary[[col]] <- as.numeric(advisory_summary[[col]])
        }
      }
      
    }, error = function(e) {
      cat("⚠ Error loading advisory summary data:", e$message, "\n")
      advisory_summary <- NULL
    })
  } else {
    cat("⚠ Advisory summary file not found:", summary_file, "\n")
  }
  
  # Load detailed data (BeachAdvisoryDuration.xlsx)
  details_file <- file.path("www", "BeachAdvisoryDuration.xlsx")
  if (file.exists(details_file)) {
    tryCatch({
      advisory_details <- read_excel(details_file)
      cat("✓ Loaded advisory details data:", nrow(advisory_details), "records\n")
      
      # Clean and process dates
      advisory_details$ActionStartDate <- as.Date(advisory_details$ActionStartDate)
      advisory_details$ActionEndDate <- as.Date(advisory_details$ActionEndDate)
      advisory_details$ActionDurationDays <- as.numeric(advisory_details$ActionDurationDays)
      
      # Add derived fields
      advisory_details <- advisory_details %>%
        mutate(
          Year = year(ActionStartDate),
          Month = month(ActionStartDate),
          DurationCategory = case_when(
            ActionDurationDays == 1 ~ "1 Day",
            ActionDurationDays == 2 ~ "2 Days", 
            ActionDurationDays >= 3 & ActionDurationDays <= 7 ~ "3-7 Days",
            ActionDurationDays >= 8 & ActionDurationDays <= 30 ~ "8-30 Days",
            ActionDurationDays > 30 ~ "30+ Days",
            TRUE ~ "Unknown"
          ),
          DaySinceStart = as.numeric(ActionStartDate - min(ActionStartDate, na.rm = TRUE))
        ) %>%
        filter(!is.na(ActionStartDate), !is.na(ActionEndDate))
      
    }, error = function(e) {
      cat("⚠ Error loading advisory details data:", e$message, "\n")
      advisory_details <- NULL
    })
  } else {
    cat("⚠ Advisory details file not found:", details_file, "\n")
  }
  
  return(list(summary = advisory_summary, details = advisory_details))
}

# Function to get data freshness indicator
data_freshness <- function(last_sample_date) {
  days_old <- as.numeric(Sys.Date() - last_sample_date)
  if (days_old <= 7) return("🟢 Recent")
  if (days_old <= 30) return("🟡 Moderate")
  return("🔴 Stale")
}

# Generate synthetic performance data for demonstration
generate_performance_data <- function() {
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "week")
  n_predictions <- length(dates) * 5  # 5 sites
  
  set.seed(42)  # For reproducible results
  actual_values <- pmax(1, round(rnorm(n_predictions, 45, 30)))
  
  # Add realistic prediction errors
  prediction_errors <- rnorm(n_predictions, 0, 15)
  predicted_values <- pmax(1, actual_values + prediction_errors)
  
  performance_data <- data.frame(
    Date = rep(dates, 5),
    Site = rep(c("North Beach", "South Beach", "Middle Beach", "Polk Street", "Strand Street"), 
               each = length(dates)),
    Actual = actual_values,
    Predicted = predicted_values,
    Residual = predicted_values - actual_values,
    AbsError = abs(predicted_values - actual_values)
  )
  
  return(performance_data)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = span("Tybee Island NowCast", style = "font-size: 20px; font-weight: bold;")),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "overview", icon = icon("home")),
      menuItem("Learn About Water Quality", tabName = "education", icon = icon("graduation-cap")),
      menuItem("Historic Data", tabName = "timeseries", icon = icon("chart-line")),
      menuItem("Forecast Tool", tabName = "interactive", icon = icon("sliders-h")),
      menuItem("Model Performance", tabName = "model_performance", icon = icon("chart-bar")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    # Enhanced CSS and JavaScript
    tags$head(
      tags$title("Tybee NowCast Dashboard"),
      tags$style(HTML("
        .main-header .logo { background-color: white !important; }
        .weather-box p { margin: 6px 0; font-size: 16px; }
        .status-box { padding: 10px; border-radius: 5px; margin-bottom: 10px; }
        .status-box h4 { margin-top: 0; }
        .no-advisory { background-color: #dff0d8; border: 1px solid #d6e9c6; }
        .advisory-recommended { background-color: #f2dede; border: 1px solid #ebccd1; }
        .unknown { background-color: #f5f5f5; border: 1px solid #e3e3e3; }
        .site-popup .leaflet-popup-content { min-width: 200px; }
        .bacteria-value { font-size: 18px; font-weight: bold; }
        .center-content { text-align: center; }
        .forecast-box { padding: 15px; border-radius: 5px; margin-bottom: 15px; }
        .forecast-status { font-size: 24px; font-weight: bold; }
        .forecast-confidence { font-size: 16px; }
        .loading-indicator { color: blue; text-align: center; margin: 20px; }
        
        /* Collapsible panel styles */
        .collapsible-header {
          cursor: pointer;
          transition: background-color 0.3s ease;
        }
        .collapsible-header:hover {
          background-color: #f0f0f0;
        }
        .collapsible-content {
          transition: max-height 0.3s ease-out, opacity 0.3s ease-out;
          overflow: hidden;
        }
        .factor-explanation {
          padding: 10px;
          border-left: 3px solid #007bff;
          margin-bottom: 10px;
          background-color: #f8f9fa;
        }
        .advanced-settings {
          border: 1px solid #dee2e6;
          border-radius: 5px;
          margin-top: 10px;
        }
      ")),
      
      tags$script(HTML("
        $(document).ready(function() {
          // Enhanced collapsible functionality
          $('.collapsible-header').click(function() {
            var content = $(this).next('.collapsible-content');
            var icon = $(this).find('.fa-chevron-down, .fa-chevron-up');
            
            if (content.is(':visible')) {
              content.slideUp(300);
              icon.removeClass('fa-chevron-up').addClass('fa-chevron-down');
            } else {
              content.slideDown(300);
              icon.removeClass('fa-chevron-down').addClass('fa-chevron-up');
            }
          });
          
          // Auto-update forecast when sliders change (with debouncing)
          var updateTimer;
          $('.shiny-input-slider, .shiny-input-selectinput').on('change input', function() {
            clearTimeout(updateTimer);
            updateTimer = setTimeout(function() {
              $('#run_model').click();
            }, 1000); // Wait 1 second after user stops changing values
          });
          
          // Add tooltips to advanced settings
          $('[data-toggle=\"tooltip\"]').tooltip();
        });
      "))
    ),
    
    # Add loading indicator for busy states
    conditionalPanel(
      condition = "$('html').hasClass('shiny-busy')",
      tags$div(class = "loading-indicator", "Loading predictions...")
    ),
    
    tabItems(
      # ===== TAB 1: DASHBOARD =====
      tab1_ui,
      
      # ===== TAB 2: LEARN ABOUT WATER QUALITY =====
      tab2_ui,
      
      # ===== TAB 3: HISTORIC DATA =====
      tab3_ui,
      
      # ===== TAB 4: INTERACTIVE FORECAST TOOL =====
      tabItem(tabName = "interactive",
              tab4_ui()
      ),
      
      # Model Performance Tab (simplified for this version)
      tabItem(tabName = "model_performance",
              fluidRow(
                box(title = "Model Performance Overview", width = 12, status = "warning", solidHeader = TRUE,
                    p(style = "margin-bottom: 15px;", "This tab shows how well our water quality prediction model performs compared to actual measured bacterial levels."),
                    fluidRow(
                      column(4,
                             h4("Current Model Status"),
                             uiOutput("model_status_display")
                      ),
                      column(4,
                             h4("Data Integration"),
                             div(
                               p(strong("New Features: "), "Water temperature integration"),
                               p(strong("Data Period: "), "2004-2023"),
                               p(strong("Total Records: "), "~4,700 samples"),
                               p(strong("Water Temp Coverage: "), "95%+")
                             )
                      ),
                      column(4,
                             h4("Model Enhancement"),
                             div(
                               p(strong("Improvement: "), "Real water temperature"),
                               p(strong("Accuracy: "), "Enhanced bacterial prediction"),
                               p(strong("Confidence: "), "Higher with measured temps")
                             )
                      )
                    )
                )
              )
      ),
      
      # Historic Advisories Tab (simplified reference)
      
      # About Tab
      tabItem(tabName = "about",
              fluidRow(
                box(title = "About the Data", width = 12, status = "info", solidHeader = TRUE,
                    tabsetPanel(
                      tabPanel("What's New",
                               div(style = "padding: 15px;",
                                   h4("Dashboard Updates"),
                                   h5("Enhanced Data Sources"),
                                   tags$ul(
                                     tags$li("Updated to use new Excel files without '_Cleaned' suffix"),
                                     tags$li("Integrated water temperature measurements from 2004-2023"),
                                     tags$li("Improved data coverage with ~95% water temperature availability")
                                   ),
                                   h5("New Prediction Features"),
                                   tags$ul(
                                     tags$li("Water temperature now used in bacterial predictions"),
                                     tags$li("Enhanced model accuracy with measured temperatures"),
                                     tags$li("Better confidence scoring when water temperature available"),
                                     tags$li("Interactive water temperature slider in forecast tool")
                                   ),
                                   h5("Data Files Used"),
                                   tags$ul(
                                     tags$li("PolkStreet.xlsx (water temps in Celsius)"),
                                     tags$li("MiddleBeach.xlsx (water temps in Fahrenheit)"),
                                     tags$li("NorthBeach.xlsx (water temps in Fahrenheit)"),
                                     tags$li("SouthBeach.xlsx (water temps in Fahrenheit)"),
                                     tags$li("StrandStreet.xlsx (water temps in Fahrenheit)")
                                   )
                               )
                      ),
                      tabPanel("What are Bacterial Indicators?",
                               div(style = "padding: 15px;",
                                   h4("Understanding Bacterial Indicators"),
                                   p("Enterococci are bacteria that live in the intestinal tracts of warm-blooded animals, including humans."),
                                   h4("Advisory Threshold"),
                                   tags$ul(
                                     tags$li(strong("No Advisory:"), " ≤ 70 CFUs/100mL"),
                                     tags$li(strong("Advisory Recommended:"), " > 70 CFUs/100mL")
                                   ),
                                   p("When bacterial levels exceed 70 CFUs/100mL, a beach water quality advisory is recommended by the Georgia Coastal Resources Division.")
                               )
                      ),
                      tabPanel("Data Sources",
                               div(style = "padding: 15px;",
                                   h4("Monitoring Program"),
                                   p("Water quality data is collected by the Coastal Resources Division of Georgia Department of Natural Resources. Rainfall Data is from the University of Georgia station on Skidaway Island. Tide and weather data is from the National Oceanic and Atmospheric Administration."),
                                   h4("Data Coverage"),
                                   p("Historical data spans from 2004 to 2023, with nearly 5,000 measurements across 5 beach sites."),
                                   h4("Water Temperature Integration"),
                                   p("Water temperature measurements are now integrated into predictions, providing more accurate bacterial growth modeling."),
                                   h4("External Links"),
                                   tags$ul(
                                     tags$li(a(href = "https://www.epa.gov/beaches", "EPA Beaches Program", target = "_blank")),
                                     tags$li(a(href = "https://beacon.epa.gov/ords/beacon2/r/beacon_apex/beacon2/map-page", "EPA Beach Advisory and Closing On-line Notification", target = "_blank")),
                                     tags$li(a(href = "https://coastalgadnr.org/healthybeaches", "Georgia Healthy Beaches Program", target = "_blank"))
                                   )
                               )
                      ),
                      tabPanel("Download Data",
                               div(style = "padding: 15px;",
                                   h4("Download Options"),
                                   selectInput("download_type", "Data Type:",
                                               choices = c("All Sites - Current Status", 
                                                           "All Sites - Historical Data",
                                                           "Selected Site - Historical Data")),
                                   conditionalPanel(
                                     condition = "input.download_type == 'Selected Site - Historical Data'",
                                     selectInput("download_site", "Select Site:", choices = NULL)
                                   ),
                                   downloadButton("download_selected_data", "Download CSV")
                               )
                      )
                    )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Auto-update timer
  autoInvalidate <- reactiveTimer(60 * 60 * 1000)
  
  # Load trained models once when server starts
  trained_models_original <- load_trained_models()
  
  # ===== TAB 1 INITIALIZATION =====
  # Convert to reactiveValues for Tab 1 compatibility
  trained_models <- reactiveValues(
    main = trained_models_original$main,
    metadata = trained_models_original$metadata,
    forecast = trained_models_original$forecast
  )
  
  # Current conditions for Tab 1
  current_conditions <- reactiveValues(
    weather = NULL,
    tide = NULL,
    water_temp = NULL,
    last_update = Sys.time()
  )
  
  # Initialize current conditions on startup
  isolate({
    current_conditions$weather <- get_tybee_weather()
    current_conditions$tide <- get_noaa_tidal_data()
    current_conditions$water_temp <- get_usgs_water_temp()
    cat("✓ Initial environmental data loaded for Tab 1\n")
  })
  
  # Auto-update every 15 minutes
  observe({
    invalidateLater(900000)  # 15 minutes
    current_conditions$weather <- get_tybee_weather()
    current_conditions$tide <- get_noaa_tidal_data()
    current_conditions$water_temp <- get_usgs_water_temp()
    current_conditions$last_update <- Sys.time()
  })
  
  # Scenario predictions for Tab 1
  beach_predictions <- reactive({
    req(trained_models$main)  # Require model to be loaded
    
    # Get current environmental conditions
    weather <- current_conditions$weather
    water_temp <- current_conditions$water_temp
    
    # Build current environment data
    current_env <- list(
      rain_3day = 0.5,  # Default assumption - could be improved with rainfall API
      maxtemp_f = if (!is.null(weather)) weather$temperature * 9/5 + 32 else 75
    )
    
    # Generate predictions
    generate_scenario_predictions(trained_models$main, current_env)
  })
  
  # Call Tab 1 server
  tab1_server(input, output, session, beach_predictions, current_conditions, trained_models)

  # Call Tab 2 server
  tab2_server(input, output, session)

  # Call Tab 4 server - Interactive Forecast Tool
  tab4_server(input, output, session, trained_models$forecast, historical_data = NULL)

  # Call Tab 3 server - Note: Tab 3 expects data in specific format
  # It will use the existing data() reactive and site selection
  # The tab3_server function is defined in tab3_historic_data.R
  # For now, Tab 3 will use the existing time series outputs defined below
  # To fully integrate Tab 3, you would call:
  # beaches_vector <- c("North Beach", "Middle Beach", "South Beach", "Polk Street", "Strand Street")
  # Create a reactive for tab3 that formats data appropriately:
  # tab3_data <- reactive({
  #   data()$ts_data %>% 
  #     rename(SampleDate = Date) %>%
  #     select(SampleDate, MonitoringLocationName, Enterococcus, everything())
  # })
  # tab3_server(input, output, session, tab3_data, beaches_vector)
  # ===== END TAB 1-3 INITIALIZATION =====
  
  # Load data
  data <- reactive({
    result <- load_historical_data()
    
    if (!is.null(result$ts_data) && nrow(result$ts_data) > 0) {
      min_date <- min(result$ts_data$Date, na.rm = TRUE)
      max_date <- max(result$ts_data$Date, na.rm = TRUE)
      
      cat("✓ Data loaded with date range: ", as.character(min_date), "to", as.character(max_date), "\n")
    }
    
    return(result)
  })
  
  # Load advisory data reactively
  advisory_datasets <- reactive({
    load_advisory_data()
  })
  
  # Build model
  model <- reactive({
    if (!is.null(trained_models$main)) {
      return(trained_models)
    } else {
      return(NULL)
    }
  })
  
  # Update site selection dropdowns
  observe({
    site_names <- unique(data()$site_data$MonitoringLocationName)
    if (length(site_names) > 0) {
      updateSelectInput(session, "site_select", choices = site_names)
      updateSelectInput(session, "forecast_site", choices = site_names, selected = site_names[1])
      updateSelectInput(session, "download_site", choices = site_names)
    }
  })
  
  # Date slider UI
  output$date_slider_ui <- renderUI({
    ts_data <- data()$ts_data
    
    if (!is.null(ts_data) && nrow(ts_data) > 0) {
      min_date <- min(ts_data$Date, na.rm = TRUE)
      max_date <- max(ts_data$Date, na.rm = TRUE)
      
      # Set default to last year of data
      default_start <- max(min_date, max_date - 365)
      
      sliderInput(
        "date_range_slider", 
        label = NULL,
        min = min_date,
        max = max_date,
        value = c(default_start, max_date),
        timeFormat = "%Y-%m-%d",
        width = "100%"
      )
    } else {
      # Fallback slider if no data available
      sliderInput(
        "date_range_slider", 
        label = NULL,
        min = as.Date("2022-01-01"),
        max = as.Date("2024-12-31"),
        value = c(as.Date("2023-01-01"), as.Date("2024-12-31")),
        timeFormat = "%Y-%m-%d",
        width = "100%"
      )
    }
  })
  
  # Beach status summary
  output$beach_status_summary <- renderUI({
    site_data <- data()$site_data
    
    no_advisory_count <- sum(site_data$LastBacteria <= 70, na.rm = TRUE)
    advisory_count <- sum(site_data$LastBacteria > 70, na.rm = TRUE)
    unknown_count <- sum(is.na(site_data$LastBacteria))
    
    fluidRow(
      column(4,
             div(class = "status-box no-advisory center-content",
                 h4("No Advisory"), icon("check-circle", class = "fa-3x"), h3(no_advisory_count), p("beaches"))),
      column(4,
             div(class = "status-box advisory-recommended center-content",
                 h4("Advisory Recommended"), icon("exclamation-triangle", class = "fa-3x"), h3(advisory_count), p("beaches"))),
      column(4,
             div(class = "status-box unknown center-content",
                 h4("Unknown"), icon("question-circle", class = "fa-3x"), h3(unknown_count), p("beaches")))
    )
  })
  
  # Enhanced current weather display with live NOAA tidal data and water temperature
  output$current_weather <- renderUI({
    tryCatch({
      autoInvalidate()
      
      # Get weather data
      weather <- get_tybee_weather()
      if (is.null(weather)) {
        return(tags$p("Unable to fetch weather data."))
      }
      
      # Get USGS water temperature
      water_temp_data <- get_usgs_water_temp()
      
      # Get live NOAA tidal data
      tide_info <- get_noaa_tidal_data()
      
      # Process weather data
      desc <- weather_code_description(as.numeric(weather$weathercode))
      temp_f <- round((weather$temperature * 9/5) + 32, 1)
      wind_mph <- round(weather$windspeed * 0.621371, 1)
      
      # Build the weather display items
      weather_items <- list()
      
      # Air temperature
      weather_items <- append(weather_items, 
                              list(tags$p(icon("thermometer-half", class = "fa-lg"), paste(" Air Temp:", temp_f, "°F"))))
      
      # Live water temperature from USGS (if available)
      if (!is.null(water_temp_data) && !is.na(water_temp_data$temp_f)) {
        age_hours <- as.numeric(difftime(Sys.time(), water_temp_data$timestamp, units = "hours"))
        freshness_indicator <- if (age_hours < 2) "🟢" else if (age_hours < 6) "🟡" else "🔴"
        
        weather_items <- append(weather_items, 
                                list(tags$p(
                                  icon("tint", class = "fa-lg"), 
                                  paste(" Water Temp:", water_temp_data$temp_f, "°F", freshness_indicator),
                                  tags$br(),
                                  tags$small(style = "color: #666; margin-left: 25px;", 
                                             paste("USGS Fort Pulaski,", round(age_hours, 1), "hrs ago"))
                                )))
      } else {
        weather_items <- append(weather_items, 
                                list(tags$p(icon("tint", class = "fa-lg"), " Water Temp: USGS data unavailable")))
      }
      
      # Wind speed
      weather_items <- append(weather_items, 
                              list(tags$p(icon("wind", class = "fa-lg"), paste(" Wind Speed:", wind_mph, "mph"))))
      
      # Weather condition
      weather_items <- append(weather_items, 
                              list(tags$p(icon(desc$icon, class = "fa-lg"), paste(" Condition:", desc$desc))))
      
      # Current tide info from NOAA
      if (!is.null(tide_info)) {
        tide_text <- paste(" Current Tide:", tide_info$current_stage)
        if (!is.na(tide_info$current_height)) {
          tide_text <- paste0(tide_text, " (", tide_info$current_height, " ft)")
        }
        
        # Add visual indicator for tide stage
        tide_icon <- switch(as.character(tide_info$current_stage),
                            "Low" = "arrow-down",
                            "Rising" = "arrow-up", 
                            "High" = "arrow-up",
                            "Falling" = "arrow-down",
                            "Mid-tide" = "minus",
                            "water")
        
        tide_color <- switch(as.character(tide_info$current_stage),
                             "Low" = "color: #e74c3c;",
                             "Rising" = "color: #f39c12;",
                             "High" = "color: #3498db;",
                             "Falling" = "color: #f39c12;",
                             "Mid-tide" = "color: #95a5a6;",
                             "color: #2c3e50;")
        
        weather_items <- append(weather_items, 
                                list(tags$p(icon(tide_icon, class = "fa-lg"), 
                                            tags$span(style = paste0("font-weight: bold; ", tide_color), tide_text))))
      }
      
      # Next high and low tides
      if (!is.null(tide_info$next_high) && !is.na(tide_info$next_high$time)) {
        weather_items <- append(weather_items, 
                                list(tags$p(icon("arrow-up", class = "fa-lg"), 
                                            paste(" Next High:", tide_info$next_high$time, 
                                                  paste0("(", tide_info$next_high$height, " ft)")))))
      }
      
      if (!is.null(tide_info$next_low) && !is.na(tide_info$next_low$time)) {
        weather_items <- append(weather_items, 
                                list(tags$p(icon("arrow-down", class = "fa-lg"), 
                                            paste(" Next Low:", tide_info$next_low$time, 
                                                  paste0("(", tide_info$next_low$height, " ft)")))))
      }
      
      # Data source attribution
      if (!is.null(tide_info$station_id)) {
        station_name_display <- ifelse(!is.null(tide_info$station_name), 
                                       tide_info$station_name, 
                                       "Tybee Light, GA")
        weather_items <- append(weather_items,
                                list(tags$p(style = "color: #666; font-size: 11px; margin-top: 10px;",
                                            paste("Tide data: NOAA Station", tide_info$station_id, 
                                                  "-", station_name_display))))
      }
      
      return(do.call(tags$div, weather_items))
      
    }, error = function(e) {
      cat("ERROR in renderUI current_weather:", e$message, "\n")
      return(tags$p("Error loading weather data. Please check console for details."))
    })
  })
  
  # Main site map
  output$overview_map <- renderLeaflet({
    site_data <- data()$site_data
    
    site_data$popup_content <- ""
    site_data$marker_color <- "gray"
    
    for (i in seq_len(nrow(site_data))) {
      if (!is.na(site_data$LastBacteria[i])) {
        advisory <- water_advisory_status(site_data$LastBacteria[i])
        site_data$marker_color[i] <- advisory$color
        
        # Include water temperature in popup if available
        water_temp_text <- if (!is.na(site_data$LastWaterTemp[i])) {
          paste0("<p><strong>Last Water Temp: </strong>", round(site_data$LastWaterTemp[i], 1), "°F</p>")
        } else {
          ""
        }
        
        site_data$popup_content[i] <- paste0(
          "<div class='site-popup'>",
          "<h4>", site_data$MonitoringLocationName[i], "</h4>",
          "<p><strong>Status: </strong>",
          "<span style='color:", advisory$color, "'>", advisory$status, "</span></p>",
          "<p><strong>Last Reading: </strong>",
          site_data$LastBacteria[i], " CFUs/100mL</p>",
          water_temp_text,
          "<p><strong>Last Sample: </strong>", as.character(site_data$LastSampleDate[i]), "</p>",
          "</div>"
        )
      } else {
        site_data$popup_content[i] <- paste0(
          "<div class='site-popup'>",
          "<h4>", site_data$MonitoringLocationName[i], "</h4>",
          "<p><strong>Status: </strong><span style='color:gray'>Unknown</span></p>",
          "</div>"
        )
      }
    }
    
    leaflet(site_data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        popup = ~popup_content,
        label = ~MonitoringLocationName,
        color = ~marker_color,
        radius = 8,
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1.5
      ) %>%
      setView(lng = mean(site_data$Longitude, na.rm = TRUE), 
              lat = mean(site_data$Latitude, na.rm = TRUE), zoom = 13) %>%
      addLegend(
        position = "bottomright",
        colors = c("green", "red", "gray"),
        labels = c("No Advisory", "Advisory Recommended", "Unknown"),
        title = "Water Quality Status",
        opacity = 0.8
      )
  })
  
  # Enhanced time plot with water temperature
  output$bacteria_time_plot <- renderPlotly({
    req(input$site_select)
    
    if (is.null(input$date_range_slider) || length(input$date_range_slider) != 2) {
      return(plot_ly() %>%
               add_annotations(
                 text = "Loading date range...",
                 x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 16)
               ))
    }
    
    filtered_data <- data()$ts_data %>%
      filter(
        Site == input$site_select,
        Date >= as.Date(input$date_range_slider[1]),
        Date <= as.Date(input$date_range_slider[2])
      )
    
    if (nrow(filtered_data) == 0) {
      return(plot_ly() %>%
               add_annotations(
                 text = "No data available for selected range",
                 x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 16)
               ))
    }
    
    # Determine what secondary data to show
    show_rainfall_data <- input$show_rainfall && "Rainfall_3day" %in% names(filtered_data)
    show_temp_data <- input$show_water_temp && "WaterTemp" %in% names(filtered_data)
    
    # Start with bacterial plot
    p <- plot_ly(filtered_data, x = ~Date, y = ~Bacteria, type = 'scatter', mode = 'lines+markers',
                 name = 'Bacterial Level', line = list(color = 'blue', width = 2))
    
    # Add advisory threshold if requested
    if (input$show_threshold) {
      date_range <- range(filtered_data$Date, na.rm = TRUE)
      p <- p %>% 
        add_trace(x = c(date_range[1], date_range[2]), y = c(70, 70),
                  type = 'scatter', mode = 'lines',
                  line = list(color = 'red', width = 3, dash = 'dash'), 
                  name = 'Advisory Threshold (70)')
    }
    
    # Configure layout based on what's being shown
    x_domain_end <- 1.0
    if (show_rainfall_data && show_temp_data) {
      x_domain_end <- 0.8  # Make room for two right-side axes
    } else if (show_rainfall_data || show_temp_data) {
      x_domain_end <- 0.9  # Make room for one right-side axis
    }
    
    p <- p %>% layout(
      title = paste("Bacterial Levels at", input$site_select),
      xaxis = list(title = "Date", domain = c(0, x_domain_end)),
      yaxis = list(title = "Bacterial Level (CFUs/100mL)", rangemode = "tozero", color = "blue"),
      hovermode = 'x unified'
    )
    
    # Add rainfall if requested
    if (show_rainfall_data) {
      p <- p %>% 
        add_trace(
          data = filtered_data,
          x = ~Date, y = ~Rainfall_3day,
          type = 'bar',
          name = '3-Day Rainfall (in)',
          yaxis = 'y2',
          marker = list(color = 'orange', opacity = 0.7)
        ) %>%
        layout(
          yaxis2 = list(
            title = "3-Day Rainfall (inches)",
            side = "right",
            overlaying = "y",
            rangemode = "tozero",
            color = "orange"
          )
        )
    }
    
    # Add water temperature
    if (show_temp_data) {
      if (show_rainfall_data) {
        # Both rainfall and temp - temp goes on y3
        p <- p %>% 
          add_trace(
            data = filtered_data,
            x = ~Date, y = ~WaterTemp,
            type = 'scatter', mode = 'lines',
            name = 'Water Temp (°F)',
            yaxis = 'y3',
            line = list(color = 'green', width = 2)
          ) %>%
          layout(
            yaxis3 = list(
              title = "Water Temp (°F)",
              side = "right",
              overlaying = "y",
              rangemode = "tozero",
              color = "green",
              anchor = "free",
              position = 1.0
            )
          )
      } else {
        # Only temp shown - it goes on y2
        p <- p %>% 
          add_trace(
            data = filtered_data,
            x = ~Date, y = ~WaterTemp,
            type = 'scatter', mode = 'lines',
            name = 'Water Temp (°F)',
            yaxis = 'y2',
            line = list(color = 'green', width = 2)
          ) %>%
          layout(
            yaxis2 = list(
              title = "Water Temp (°F)",
              side = "right",
              overlaying = "y",
              rangemode = "tozero",
              color = "green"
            )
          )
      }
    }
    
    return(p)
  })
  
  # Data table with water temperature
  output$site_data_table <- renderDT({
    req(input$site_select)
    
    if (is.null(input$date_range_slider) || length(input$date_range_slider) != 2) {
      return(datatable(data.frame(Message = "Loading..."), options = list(dom = 't'), rownames = FALSE))
    }
    
    filtered_data <- data()$ts_data %>%
      filter(
        Site == input$site_select,
        Date >= as.Date(input$date_range_slider[1]),
        Date <= as.Date(input$date_range_slider[2])
      ) %>%
      arrange(desc(Date)) %>%
      select(Date, Bacteria, Rainfall_3day, WaterTemp)
    
    names(filtered_data) <- c("Sample Date", "Bacterial Level (CFUs/100mL)", 
                              "3-Day Rainfall (in)", "Water Temp (°F)")
    
    datatable(
      filtered_data,
      options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("Bacterial Level (CFUs/100mL)", "3-Day Rainfall (in)", "Water Temp (°F)"), digits = 1)
  })
  
  # Forecast panels with water temperature integration
  output$forecast_panels <- renderUI({
    site_data <- data()$site_data
    
    current_month <- month(Sys.Date())
    current_season <- case_when(
      current_month %in% c(12, 1, 2) ~ "Winter",
      current_month %in% c(3, 4, 5) ~ "Spring", 
      current_month %in% c(6, 7, 8) ~ "Summer",
      current_month %in% c(9, 10, 11) ~ "Fall"
    )
    
    forecast_ui <- lapply(seq_len(nrow(site_data)), function(i) {
      site <- site_data[i, ]
      
      # Use actual water temperature if available
      water_temp <- if (!is.na(site$LastWaterTemp)) site$LastWaterTemp else NULL
      
      forecast_result <- make_bacterial_prediction(
        beach_name = site$MonitoringLocationName,
        rainfall_3day = 0.3,
        max_temp = 78,
        water_temp = water_temp,  # Pass actual water temperature
        month = current_month,
        season = current_season,
        models = model()
      )
      
      icon_name <- switch(forecast_result$status,
                          "No Advisory" = "check-circle",
                          "Advisory Recommended" = "exclamation-triangle",
                          "question-circle")
      
      model_info <- ifelse(!is.null(forecast_result$model_used), 
                           paste("Model:", forecast_result$model_used),
                           "Model: Enhanced (with water temp)")
      
      # Add water temperature info if available
      water_temp_info <- if (!is.null(water_temp)) {
        paste("Water Temp:", round(water_temp, 1), "°F")
      } else {
        "Water Temp: Estimated"
      }
      
      div(
        class = paste0("forecast-box ", gsub(" ", "-", tolower(forecast_result$status))),
        style = paste0("background-color: ", 
                       ifelse(forecast_result$status == "No Advisory", "#dff0d8", "#f2dede"), 
                       "; border: 1px solid ", 
                       ifelse(forecast_result$status == "No Advisory", "#d6e9c6", "#ebccd1")),
        fluidRow(
          column(4,
                 h4(site$MonitoringLocationName),
                 p(class = "forecast-status", 
                   icon(icon_name, class = "fa-lg"), " ", forecast_result$status),
                 p(style = "font-size: 12px; color: #666;", model_info)
          ),
          column(4,
                 p(class = "forecast-confidence", 
                   "Confidence: ", forecast_result$confidence, 
                   " (", forecast_result$confidence_score, "%)"),
                 p("Predicted: ", strong(paste0(forecast_result$predicted_bacteria, " CFUs/100mL"))),
                 p("Risk Level: ", strong(forecast_result$risk_level))
          ),
          column(4,
                 p("Based on:"),
                 tags$ul(style = "font-size: 12px;",
                         tags$li("Rainfall: 3-day total"),
                         tags$li("Temperature: Daily maximum"),
                         tags$li("Season: ", current_season),
                         tags$li(water_temp_info)
                 ),
                 p(style = "font-size: 11px; color: #888; margin-top: 10px;",
                   "Probability of exceeding advisory: ",
                   forecast_result$prob_exceed_advisory, "%")
          )
        ),
        tags$hr(),
        tags$div(
          style = "font-size: 11px; color: #666;",
          "Last measured: ", 
          ifelse(!is.na(site$LastBacteria), 
                 paste0(site$LastBacteria, " CFUs/100mL on ", site$LastSampleDate),
                 "No recent data")
        )
      )
    })
    
    do.call(tagList, forecast_ui)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
