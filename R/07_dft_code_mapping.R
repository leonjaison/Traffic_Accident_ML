# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 7: DfT STATS19 Code Mapping
# ============================================================

library(tidyverse)

uk_accidents <- read_csv(
  "data/UK/dft-road-casualty-statistics-collision-last-5-years.csv",
  show_col_types = FALSE
)

# ------------------------------------------------------------
# Collision severity
# ------------------------------------------------------------

severity_mapping <- tibble(
  code = c(1, 2, 3),
  description = c(
    "Fatal",
    "Serious",
    "Slight"
  )
)

# ------------------------------------------------------------
# Speed limit
# ------------------------------------------------------------

speed_mapping <- tibble(
  code = c(-1, 20, 30, 40, 50, 60, 70),
  description = c(
    "No data",
    "20 mph",
    "30 mph",
    "40 mph",
    "50 mph",
    "60 mph",
    "70 mph"
  )
)

# ------------------------------------------------------------
# Display mappings
# ------------------------------------------------------------

cat("\n=== COLLISION SEVERITY ===\n")
print(severity_mapping)

cat("\n=== SPEED LIMIT ===\n")
print(speed_mapping)

# ------------------------------------------------------------
# Verify severity codes against dataset
# ------------------------------------------------------------

cat("\n=== SEVERITY CODES IN DATASET ===\n")
print(sort(unique(uk_accidents$collision_severity)))

cat("\n=== SPEED LIMIT CODES IN DATASET ===\n")
print(sort(unique(uk_accidents$speed_limit)))

# ------------------------------------------------------------
# Road type
# ------------------------------------------------------------

road_type_mapping <- tibble(
  code = c(1, 2, 3, 6, 7, 9),
  description = c(
    "Roundabout",
    "One way street",
    "Dual carriageway",
    "Single carriageway",
    "Slip road",
    "Unknown"
  )
)

cat("\n=== ROAD TYPE ===\n")
print(road_type_mapping)


# ------------------------------------------------------------
# Urban / rural area
# ------------------------------------------------------------

urban_rural_mapping <- tibble(
  code = c(-1, 1, 2, 3),
  description = c(
    "No data",
    "Urban",
    "Rural",
    "Unallocated"
  )
)

cat("\n=== URBAN / RURAL AREA ===\n")
print(urban_rural_mapping)


# ------------------------------------------------------------
# Light conditions
# ------------------------------------------------------------

light_mapping <- tibble(
  code = c(-1, 1, 4, 5, 6, 7),
  description = c(
    "No data",
    "Daylight",
    "Darkness - lights lit",
    "Darkness - lights unlit",
    "Darkness - no lighting",
    "Darkness - lighting unknown"
  )
)

cat("\n=== LIGHT CONDITIONS ===\n")
print(light_mapping)


# ------------------------------------------------------------
# Weather conditions
# ------------------------------------------------------------

weather_mapping <- tibble(
  code = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
  description = c(
    "No data",
    "Fine no high winds",
    "Raining no high winds",
    "Snowing no high winds",
    "Fine + high winds",
    "Raining + high winds",
    "Snowing + high winds",
    "Fog or mist",
    "Other",
    "Unknown"
  )
)

cat("\n=== WEATHER CONDITIONS ===\n")
print(weather_mapping)


# ------------------------------------------------------------
# Road surface conditions
# ------------------------------------------------------------

surface_mapping <- tibble(
  code = c(-1, 1, 2, 3, 4, 5, 9),
  description = c(
    "No data",
    "Dry",
    "Wet or damp",
    "Snow",
    "Frost or ice",
    "Flood over 3cm deep",
    "Unknown"
  )
)

cat("\n=== ROAD SURFACE CONDITIONS ===\n")
print(surface_mapping)