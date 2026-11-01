
library(tidyverse)
library(dbscan)
library(sf)

input_file <- "data/UK/dft-road-casualty-statistics-collision-last-5-years.csv"
output_dir <- "results/hotspots"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

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
sample_rows <- sample(seq_len(nrow(data)), sample_size)
pilot <- data[sample_rows, ]

pilot_sf <- st_as_sf(
  pilot,
  coords = c("longitude", "latitude"),
  crs = 4326
)

pilot_projected <- st_transform(pilot_sf, 27700)
xy <- st_coordinates(pilot_projected)

# Use a reproducible sample to compare radius settings.
radii_km <- c(0.25, 0.5, 1, 2)
min_pts <- 10

results <- purrr::map_dfr(radii_km, function(radius_km) {
  fit <- dbscan(
    xy,
    eps = radius_km * 1000,
    minPts = min_pts
  )

  cluster_sizes <- table(fit$cluster[fit$cluster > 0])

  tibble(
    sample_size = nrow(xy),
    eps_km = radius_km,
    minPts = min_pts,
    cluster_count = length(cluster_sizes),
    clustered_records = sum(fit$cluster > 0),
    noise_records = sum(fit$cluster == 0),
    noise_percentage = round(mean(fit$cluster == 0) * 100, 2),
    largest_cluster = if (length(cluster_sizes)) {
      max(cluster_sizes)
    } else {
      0L
    }
  )
})

write_csv(
  results,
  file.path(output_dir, "dbscan_parameter_comparison.csv")
)

cat("\nDBSCAN parameter comparison (fixed sample):\n")
print(results)
