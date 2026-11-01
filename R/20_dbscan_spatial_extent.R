library(tidyverse)
library(dbscan)
library(sf)

input_file <- "data/UK/dft-road-casualty-statistics-collision-last-5-years.csv"

data <- read_csv(
  input_file,
  col_select = c(collision_index, latitude, longitude),
  show_col_types = FALSE
) %>%
  filter(
    !is.na(latitude), !is.na(longitude),
    is.finite(latitude), is.finite(longitude),
    between(latitude, -90, 90),
    between(longitude, -180, 180)
  ) %>%
  distinct(collision_index, .keep_all = TRUE)

set.seed(42)
sample_size <- min(20000L, nrow(data))
pilot <- data[sample(seq_len(nrow(data)), sample_size), ]

projected <- st_transform(
  st_as_sf(pilot, coords = c("longitude", "latitude"), crs = 4326),
  27700
)

xy <- st_coordinates(projected)

fit <- dbscan(xy, eps = 1000, minPts = 10)

cluster_id <- as.integer(names(
  sort(table(fit$cluster[fit$cluster != 0]), decreasing = TRUE)[1]
))

largest_xy <- xy[fit$cluster == cluster_id, , drop = FALSE]

cat("Largest cluster ID:", cluster_id, "\n")
cat("Records:", nrow(largest_xy), "\n")
cat("Easting range (km):",
    round(diff(range(largest_xy[, 1])) / 1000, 2), "\n")
cat("Northing range (km):",
    round(diff(range(largest_xy[, 2])) / 1000, 2), "\n")

set.seed(42)
check_n <- min(1000L, nrow(largest_xy))
check_points <- largest_xy[
  sample(seq_len(nrow(largest_xy)), check_n), ,
  drop = FALSE
]

distance_matrix <- as.matrix(dist(check_points))
cat("Maximum pairwise distance in sampled cluster points (km):",
    round(max(distance_matrix) / 1000, 2), "\n")