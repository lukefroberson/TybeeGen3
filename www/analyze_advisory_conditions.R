#!/usr/bin/env Rscript

# Analyze what conditions lead to advisories in training data

library(readxl)
library(dplyr)
library(lubridate)

cat("=== ANALYZING ADVISORY CONDITIONS ===\n\n")

# Load data
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
  }
}

# Prepare data
model_data <- all_data %>%
  mutate(
    beach = case_when(
      grepl("NORTH", beach_name, ignore.case = TRUE) ~ "North",
      grepl("MIDDLE", beach_name, ignore.case = TRUE) ~ "Middle",
      grepl("SOUTH", beach_name, ignore.case = TRUE) ~ "South",
      grepl("POLK", beach_name, ignore.case = TRUE) ~ "Polk",
      grepl("STRAND", beach_name, ignore.case = TRUE) ~ "Strand",
      TRUE ~ as.character(beach_name)
    ),
    date = as.Date(date_collected),
    month = as.numeric(month(date)),
    rain_3day = as.numeric(rain3day),
    maxtemp_f = as.numeric(maxtemp_f),
    water_temp_avg_f = as.numeric(water_temp_avg_f),
    conductivity = as.numeric(conductivity),
    do = as.numeric(do),
    ph = as.numeric(ph),
    salinity = as.numeric(salinity),
    turbidity = as.numeric(turbidity),
    entero = as.numeric(entero),
    air_water_diff = maxtemp_f - water_temp_avg_f,
    advisory = ifelse(entero > 70, "Yes", "No")
  ) %>%
  filter(
    !is.na(beach), !is.na(entero),
    !is.na(rain_3day), !is.na(maxtemp_f), !is.na(water_temp_avg_f),
    !is.na(air_water_diff), !is.na(month),
    !is.na(conductivity), !is.na(do), !is.na(ph),
    !is.na(salinity), !is.na(turbidity)
  )

cat("Total records:", nrow(model_data), "\n")

# Split by advisory status
advisories <- model_data %>% filter(advisory == "Yes")
non_advisories <- model_data %>% filter(advisory == "No")

cat("Advisories:", nrow(advisories), "\n")
cat("Non-advisories:", nrow(non_advisories), "\n\n")

# Compare conditions
cat("=== MEDIAN VALUES BY ADVISORY STATUS ===\n\n")

vars <- c("rain_3day", "maxtemp_f", "water_temp_avg_f", "air_water_diff",
          "conductivity", "do", "ph", "salinity", "turbidity")

comparison <- data.frame(
  Variable = vars,
  Advisory = sapply(vars, function(v) median(advisories[[v]], na.rm = TRUE)),
  No_Advisory = sapply(vars, function(v) median(non_advisories[[v]], na.rm = TRUE)),
  Difference = sapply(vars, function(v)
    median(advisories[[v]], na.rm = TRUE) - median(non_advisories[[v]], na.rm = TRUE))
)

print(comparison, row.names = FALSE)

cat("\n=== TOP 10 HIGHEST ADVISORY READINGS ===\n\n")

top_advisories <- advisories %>%
  arrange(desc(entero)) %>%
  select(beach, entero, rain_3day, water_temp_avg_f, air_water_diff,
         salinity, turbidity, do, ph) %>%
  head(10)

print(top_advisories, row.names = FALSE)

cat("\n=== RAIN DISTRIBUTION ===\n\n")
cat("Advisories with 0 rain:", sum(advisories$rain_3day == 0),
    sprintf("(%.1f%%)\n", 100 * sum(advisories$rain_3day == 0) / nrow(advisories)))
cat("Advisories with >2 inches rain:", sum(advisories$rain_3day > 2),
    sprintf("(%.1f%%)\n", 100 * sum(advisories$rain_3day > 2) / nrow(advisories)))
cat("Advisories with >4 inches rain:", sum(advisories$rain_3day > 4),
    sprintf("(%.1f%%)\n", 100 * sum(advisories$rain_3day > 4) / nrow(advisories)))
