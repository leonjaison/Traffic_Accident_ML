
library(tidyverse)
library(dbscan)
library(sf)

input_file <- "data/UK/dft-road-casualty-statistics-collision-last-5-years.csv"
output_dir <- "results/hotspots"

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

pilot_sf <- st_as_sf(
  pilot,
  coords = c("longitude", "latitude"),
  crs = 4326
)
pilot_projected <- st_transform(pilot_sf, 27700)
xy <- st_coordinates(pilot_projected)

fit <- dbscan(
  xy,
  eps = 1000,
  minPts = 10
)

cluster_sizes <- tibble(cluster_id = fit$cluster) %>%
  filter(cluster_id != 0) %>%
  count(cluster_id, name = "collision_count") %>%
  arrange(desc(collision_count))

cat("Sample size:", sample_size, "\n")
cat("Number of clusters:", nrow(cluster_sizes), "\n")
cat("Noise records:", sum(fit$cluster == 0), "\n")
cat("Largest cluster size:\n")
print(head(cluster_sizes, 10))

write_csv(
  cluster_sizes,
  file.path(output_dir, "dbscan_1km_sample_cluster_sizes.csv")
)
