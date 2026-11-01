
# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 24: Final DBSCAN Cluster Extent Check
# ============================================================

library(tidyverse)
library(sf)

# ------------------------------------------------------------
# 1. Load saved DBSCAN assignments
# ------------------------------------------------------------

assignments_file <- "results/hotspots/uk_dbscan_assignments.csv"

assignments <- read_csv(
  assignments_file,
  show_col_types = FALSE
)

cat("Total assignment records:", nrow(assignments), "\n")

# ------------------------------------------------------------
# 2. Identify the largest non-noise cluster
# ------------------------------------------------------------

largest_cluster <- assignments %>%
  filter(cluster_id != 0) %>%
  count(cluster_id, name = "collision_count", sort = TRUE) %>%
  slice_head(n = 1)

if (nrow(largest_cluster) == 0) {
  stop("No DBSCAN clusters found in the saved assignments.")
}

largest_id <- largest_cluster$cluster_id[[1]]

cat("Largest cluster ID:", largest_id, "\n")
cat("Records in largest cluster:", largest_cluster$collision_count[[1]], "\n")

# ------------------------------------------------------------
# 3. Extract coordinates for that cluster
# ------------------------------------------------------------

largest <- assignments %>%
  filter(
    cluster_id == largest_id,
    !is.na(latitude),
    !is.na(longitude),
    is.finite(latitude),
    is.finite(longitude)
  )

if (nrow(largest) < 2) {
  stop("Not enough valid coordinates to measure cluster extent.")
}

# Convert longitude/latitude to British National Grid metres.
points_sf <- st_as_sf(
  largest,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

points_grid <- st_transform(points_sf, 27700)

# Explicit namespace avoids the previous function lookup error.
xy <- sf::st_coordinates(points_grid)

# ------------------------------------------------------------
# 4. Calculate geographic extent
# ------------------------------------------------------------

east_west_km <- diff(range(xy[, 1])) / 1000
north_south_km <- diff(range(xy[, 2])) / 1000

cat("\nLargest cluster geographic extent:\n")
cat("East-west bounding span (km):", round(east_west_km, 2), "\n")
cat("North-south bounding span (km):", round(north_south_km, 2), "\n")

# ------------------------------------------------------------
# 5. Estimate pairwise spread using a reproducible sample
# ------------------------------------------------------------

set.seed(42)

sample_n <- min(2000L, nrow(xy))

sample_indices <- sample(
  seq_len(nrow(xy)),
  sample_n
)

sample_xy <- xy[sample_indices, , drop = FALSE]

distance_matrix <- as.matrix(dist(sample_xy))
max_sampled_distance_km <- max(distance_matrix) / 1000

cat(
  "Maximum pairwise distance among sampled points (km):",
  round(max_sampled_distance_km, 2),
  "\n"
)

# ------------------------------------------------------------
# 6. Save diagnostic summary
# ------------------------------------------------------------

extent_summary <- tibble(
  cluster_id = largest_id,
  collision_count = nrow(xy),
  east_west_span_km = round(east_west_km, 2),
  north_south_span_km = round(north_south_km, 2),
  sampled_point_count = sample_n,
  max_sampled_pairwise_distance_km =
    round(max_sampled_distance_km, 2)
)

output_file <- "results/hotspots/dbscan_largest_cluster_extent.csv"

write_csv(extent_summary, output_file)

cat("\nExtent summary:\n")
print(extent_summary)

cat("\nSaved diagnostic to:", output_file, "\n")
cat("No DBSCAN model or existing assignments were modified.\n")
