# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 3: Inspect DfT Coding Values
# ============================================================

library(tidyverse)

# Load dataset
uk_accidents <- read_csv(
  "data/UK/dft-road-casualty-statistics-collision-last-5-years.csv",
  show_col_types = FALSE
)

# ------------------------------------------------------------
# Severity
# ------------------------------------------------------------

cat("\n=== COLLISION SEVERITY ===\n")
print(table(uk_accidents$collision_severity, useNA = "ifany"))

# ------------------------------------------------------------
# Road type
# ------------------------------------------------------------

cat("\n=== ROAD TYPE ===\n")
print(table(uk_accidents$road_type, useNA = "ifany"))

# ------------------------------------------------------------
# Speed limit
# ------------------------------------------------------------

cat("\n=== SPEED LIMIT ===\n")
print(table(uk_accidents$speed_limit, useNA = "ifany"))

# ------------------------------------------------------------
# Junction detail
# ------------------------------------------------------------

cat("\n=== JUNCTION DETAIL ===\n")
print(table(uk_accidents$junction_detail, useNA = "ifany"))

# ------------------------------------------------------------
# Junction control
# ------------------------------------------------------------

cat("\n=== JUNCTION CONTROL ===\n")
print(table(uk_accidents$junction_control, useNA = "ifany"))

# ------------------------------------------------------------
# Light conditions
# ------------------------------------------------------------

cat("\n=== LIGHT CONDITIONS ===\n")
print(table(uk_accidents$light_conditions, useNA = "ifany"))

# ------------------------------------------------------------
# Weather conditions
# ------------------------------------------------------------

cat("\n=== WEATHER CONDITIONS ===\n")
print(table(uk_accidents$weather_conditions, useNA = "ifany"))

# ------------------------------------------------------------
# Road surface conditions
# ------------------------------------------------------------

cat("\n=== ROAD SURFACE CONDITIONS ===\n")
print(table(uk_accidents$road_surface_conditions, useNA = "ifany"))

# ------------------------------------------------------------
# Urban / rural
# ------------------------------------------------------------

cat("\n=== URBAN / RURAL ===\n")
print(table(uk_accidents$urban_or_rural_area, useNA = "ifany"))