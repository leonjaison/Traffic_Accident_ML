
# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 17: UK DBSCAN Hotspot Detection
# ============================================================

library(tidyverse)
library(dbscan)
library(sf)

# ------------------------------------------------------------
# 1. Configuration
# ------------------------------------------------------------

input_file <- "data/UK/dft-road-casualty-statistics-collision-last-5-years.csv"
output_dir <- "results/hotspots"

# Selected candidate based on the parameter comparison.
# Clusters are density-connected regions, not necessarily
# compact or independently verified accident hotspots.
EPS_KM <- 1
MIN_PTS <- 10

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 2. Load required columns
# ------------------------------------------------------------

uk_raw <- read_csv(
  input_file,
  col_select = c(
    collision_index,
    collision_year,
    collision_severity,
    latitude,
    longitude
  ),
  show_col_types = FALSE
)

input_records <- nrow(uk_raw)

cat("Records loaded:", input_records, "\n")

# ------------------------------------------------------------
# 3. Validate coordinates and collision identifiers
# ------------------------------------------------------------

uk_spatial <- uk_raw %>%
  filter(
    !is.na(collision_index),
    !is.na(latitude),
    !is.na(longitude),
    is.finite(latitude),
    is.finite(longitude),
    between(latitude, -90, 90),
    between(longitude, -180, 180)
  )

records_after_coordinate_validation <- nrow(uk_spatial)

cat(
  "Records with valid identifiers and coordinates:",
  records_after_coordinate_validation,
  "\n"
)

cat(
  "Records excluded during initial validation:",
  input_records - records_after_coordinate_validation,
  "\n"
)

# Remove duplicate collision identifiers.
records_before_deduplication <- nrow(uk_spatial)

uk_spatial <- uk_spatial %>%
  distinct(collision_index, .keep_all = TRUE)

cat(
  "Duplicate records removed:",
  records_before_deduplication - nrow(uk_spatial),
  "\n"
)

if (nrow(uk_spatial) == 0) {
  stop("No records remain after coordinate validation.")
}

# ------------------------------------------------------------
# 4. Map collision severity
# ------------------------------------------------------------

# Source codes: 1 = Fatal, 2 = Serious, 3 = Slight.

uk_spatial <- uk_spatial %>%
  mutate(
    severity_label = recode(
      as.character(collision_severity),
      "1" = "Fatal",
      "2" = "Serious",
      "3" = "Slight",
      .default = "Unknown"
    )
  )

# ------------------------------------------------------------
# 5. Project coordinates to British National Grid
# ------------------------------------------------------------

# EPSG:27700 is intended for Great Britain.
# Check projected bounds and report excluded records.
# This script does not independently establish that every
# source record is geographically within the intended study area.

uk_sf <- st_as_sf(
  uk_spatial,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

uk_grid <- st_transform(uk_sf, 27700)

xy <- st_coordinates(uk_grid)

projected_coordinates <- tibble(
  easting_m = xy[, 1],
  northing_m = xy[, 2]
)

valid_grid <- with(
  projected_coordinates,
  is.finite(easting_m) &
    is.finite(northing_m) &
    easting_m >= 0 &
    easting_m <= 800000 &
    northing_m >= 0 &
    northing_m <= 1400000
)

cat(
  "Records excluded by grid bounds:",
  sum(!valid_grid),
  "\n"
)

# Keep source rows and projected coordinates aligned.
uk_spatial <- uk_spatial[valid_grid, , drop = FALSE]

uk_spatial <- uk_spatial %>%
  mutate(
    easting_m = projected_coordinates$easting_m[valid_grid],
    northing_m = projected_coordinates$northing_m[valid_grid]
  )

cat(
  "Records retained for British National Grid analysis:",
  nrow(uk_spatial),
  "\n"
)

if (nrow(uk_spatial) == 0) {
  stop("No records remain after projected-coordinate validation.")
}

# ------------------------------------------------------------
# 6. Run DBSCAN
# ------------------------------------------------------------

eps_m <- EPS_KM * 1000

cat("\nDBSCAN configuration:\n")
cat("eps:", EPS_KM, "km (", eps_m, "metres )\n")
cat("minPts:", MIN_PTS, "\n")
cat("Clustering", nrow(uk_spatial), "records...\n")

set.seed(42)

dbscan_fit <- dbscan::dbscan(
  x = as.matrix(
    uk_spatial[, c("easting_m", "northing_m")]
  ),
  eps = eps_m,
  minPts = MIN_PTS,
  borderPoints = TRUE
)

uk_spatial$cluster_id <- dbscan_fit$cluster
uk_spatial$is_noise <- uk_spatial$cluster_id == 0

# ------------------------------------------------------------
# 7. Save collision-level assignments
# ------------------------------------------------------------

assignments <- uk_spatial %>%
  select(
    collision_index,
    collision_year,
    collision_severity,
    severity_label,
    latitude,
    longitude,
    cluster_id,
    is_noise
  )

write_csv(
  assignments,
  file.path(output_dir, "uk_dbscan_assignments.csv")
)

saveRDS(
  assignments,
  file.path(output_dir, "uk_dbscan_assignments.rds")
)

# ------------------------------------------------------------
# 8. Summarize density-connected clusters
# ------------------------------------------------------------

hotspot_summary <- assignments %>%
  filter(cluster_id != 0) %>%
  group_by(cluster_id) %>%
  summarise(
    collision_count = n(),
    centroid_latitude = mean(latitude),
    centroid_longitude = mean(longitude),
    fatal_count = sum(severity_label == "Fatal"),
    serious_count = sum(severity_label == "Serious"),
    slight_count = sum(severity_label == "Slight"),
    unknown_severity_count = sum(severity_label == "Unknown"),
    first_year = if (all(is.na(collision_year))) {
      NA_real_
    } else {
      min(collision_year, na.rm = TRUE)
    },
    last_year = if (all(is.na(collision_year))) {
      NA_real_
    } else {
      max(collision_year, na.rm = TRUE)
    },
    .groups = "drop"
  ) %>%
  arrange(desc(collision_count))

write_csv(
  hotspot_summary,
  file.path(output_dir, "uk_dbscan_hotspot_summary.csv")
)

# ------------------------------------------------------------
# 9. Save run diagnostics
# ------------------------------------------------------------

noise_count <- sum(assignments$is_noise)

cluster_count <- n_distinct(
  assignments$cluster_id[assignments$cluster_id != 0]
)

assignment_count <- nrow(assignments)
clustered_count <- sum(!assignments$is_noise)

diagnostics <- tibble(
  input_records = input_records,
  records_after_coordinate_validation =
    records_after_coordinate_validation,
  records_after_deduplication = records_before_deduplication -
    (records_before_deduplication - nrow(uk_raw %>%
      filter(
        !is.na(collision_index),
        !is.na(latitude),
        !is.na(longitude),
        is.finite(latitude),
        is.finite(longitude),
        between(latitude, -90, 90),
        between(longitude, -180, 180)
      ) %>%
      distinct(collision_index))),
  records_retained = assignment_count,
  eps_km = EPS_KM,
  minPts = MIN_PTS,
  cluster_count = cluster_count,
  clustered_records = clustered_count,
  noise_records = noise_count,
  noise_percentage = round(
    100 * noise_count / assignment_count,
    2
  )
)

write_csv(
  diagnostics,
  file.path(output_dir, "uk_dbscan_diagnostics.csv")
)

cat("\nDBSCAN diagnostics:\n")
print(diagnostics)

cat("\nLargest 10 clusters:\n")
print(head(hotspot_summary, 10))

cat("\nOutputs saved in:", output_dir, "\n")
cat(
  "Reminder: clusters are density-connected regions. ",
  "Validate their geographic extent before calling them hotspots.\n",
  sep = ""
)

