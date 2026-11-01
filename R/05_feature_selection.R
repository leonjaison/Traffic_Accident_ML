# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 5: Feature Selection
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# 1. Load the preprocessed dataset
# ------------------------------------------------------------

uk_accidents <- read_csv(
  "data/UK/dft-road-casualty-statistics-collision-last-5-years.csv",
  show_col_types = FALSE
)

# ------------------------------------------------------------
# 2. Define the target variable
# ------------------------------------------------------------

target_variable <- "collision_severity"

cat("Target variable:", target_variable, "\n")


# ------------------------------------------------------------
# 3. Identify possible identifier variables
# ------------------------------------------------------------

identifier_variables <- c(
  "collision_index",
  "collision_ref_no",
  "lsoa_of_accident_location"
)

cat("\nIdentifier / location-code variables:\n")
print(identifier_variables)


# ------------------------------------------------------------
# 4. Identify severity-derived variables
# ------------------------------------------------------------

severity_derived_variables <- c(
  "enhanced_severity_collision",
  "collision_injury_based",
  "collision_adjusted_severity_serious",
  "collision_adjusted_severity_slight"
)

cat("\nSeverity-derived variables:\n")
print(severity_derived_variables)


# ------------------------------------------------------------
# 5. Variables to exclude from prediction
# ------------------------------------------------------------

excluded_variables <- c(
  identifier_variables,
  severity_derived_variables
)

cat("\nVariables excluded from severity modelling:\n")
print(excluded_variables)


# ------------------------------------------------------------
# 6. Candidate predictor variables
# ------------------------------------------------------------

candidate_predictors <- setdiff(
  names(uk_accidents),
  c(target_variable, excluded_variables)
)

cat("\nCandidate predictor variables:\n")
print(candidate_predictors)

cat("\nNumber of candidate predictors:",
    length(candidate_predictors), "\n")

    # ------------------------------------------------------------
# Final initial predictor set
# ------------------------------------------------------------

final_predictors <- c(
  "number_of_vehicles",
  "number_of_casualties",
  "collision_year",
  "day_of_week",
  "speed_limit",
  "road_type",
  "light_conditions",
  "weather_conditions",
  "road_surface_conditions",
  "urban_or_rural_area",
  "first_road_class",
  "second_road_class",
  "junction_control",
  "police_force",
  "trunk_road_flag",
  "accident_month",
  "accident_hour",
  "accident_day"
)

cat("\nFinal initial predictor set:\n")
print(final_predictors)

cat(
  "\nNumber of predictors:",
  length(final_predictors),
  "\n"
)