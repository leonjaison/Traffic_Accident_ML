# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 8: Final Modelling Dataset
# ============================================================

library(tidyverse)
library(lubridate)

# ------------------------------------------------------------
# 1. Load raw dataset
# ------------------------------------------------------------

uk_accidents <- read_csv(
  "data/UK/dft-road-casualty-statistics-collision-last-5-years.csv",
  show_col_types = FALSE
)

cat("Raw records:", nrow(uk_accidents), "\n")


# ------------------------------------------------------------
# 2. Create temporal features
# ------------------------------------------------------------

uk_accidents <- uk_accidents %>%
  mutate(
    date = dmy(date),
    accident_year = year(date),
    accident_month = month(date),
    accident_hour = hour(time),
    accident_day = wday(date, label = TRUE)
  )


# ------------------------------------------------------------
# 3. Convert target variable
# ------------------------------------------------------------

uk_accidents <- uk_accidents %>%
  mutate(
    collision_severity = factor(
      collision_severity,
      levels = c(1, 2, 3),
      labels = c("Fatal", "Serious", "Slight")
    )
  )


# ------------------------------------------------------------
# 4. Define severity-model predictors
# ------------------------------------------------------------

model_predictors <- c(
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
  "junction_detail",
  "junction_control",
  "police_force",
  "trunk_road_flag",
  "special_conditions_at_site",
  "carriageway_hazards",
  "pedestrian_crossing",
  "accident_month",
  "accident_hour",
  "accident_day"
)


# ------------------------------------------------------------
# 5. Create severity modelling dataset
# ------------------------------------------------------------

model_data <- uk_accidents %>%
  select(
    collision_severity,
    all_of(model_predictors)
  )


# ------------------------------------------------------------
# 6. Create spatial dataset
# ------------------------------------------------------------

spatial_data <- uk_accidents %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(location_easting_osgr),
    !is.na(location_northing_osgr)
  ) %>%
  select(
    collision_index,
    collision_severity,
    latitude,
    longitude,
    location_easting_osgr,
    location_northing_osgr,
    collision_year
  )


# ------------------------------------------------------------
# 7. Check modelling dataset
# ------------------------------------------------------------

cat("\n=== MODELLING DATASET ===\n")
cat("Rows:", nrow(model_data), "\n")
cat("Columns:", ncol(model_data), "\n")

cat("\nSeverity distribution:\n")
print(table(model_data$collision_severity))


# ------------------------------------------------------------
# 8. Check missing values
# ------------------------------------------------------------

cat("\nMissing values in modelling dataset:\n")

missing_model <- colSums(is.na(model_data))

print(
  sort(
    missing_model,
    decreasing = TRUE
  )
)


# ------------------------------------------------------------
# 9. Check spatial dataset
# ------------------------------------------------------------

cat("\n=== SPATIAL DATASET ===\n")
cat("Rows:", nrow(spatial_data), "\n")
cat("Columns:", ncol(spatial_data), "\n")

cat("\nMissing spatial coordinates:\n")

print(
  colSums(
    is.na(
      spatial_data %>%
        select(
          latitude,
          longitude,
          location_easting_osgr,
          location_northing_osgr
        )
    )
  )
)


# ------------------------------------------------------------
# 10. Save datasets
# ------------------------------------------------------------

dir.create(
  "results/data",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  model_data,
  "results/data/model_data.rds"
)

saveRDS(
  spatial_data,
  "results/data/spatial_data.rds"
)

cat("\nDatasets saved successfully.\n")