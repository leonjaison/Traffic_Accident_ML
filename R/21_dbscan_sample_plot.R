library(tidyverse)
library(dbscan)
library(sf)
library(ggplot2)

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
pilot <- data[sample(seq_len(nrow(data)), min(20000L, nrow(data))), ]

projected <- st_transform(
  st_as_sf(pilot, coords = c("longitude", "latitude"), crs = 4326),
  27700
)

xy <- st_coordinates(projected)
fit <- dbscan(xy, eps = 1000, minPts = 10)

plot_data <- as_tibble(xy, .name_repair = "minimal")
names(plot_data) <- c("easting", "northing")
plot_data$cluster <- factor(ifelse(fit$cluster == 0, "Noise", as.character(fit$cluster)))

p <- ggplot(plot_data, aes(easting / 1000, northing / 1000, color = cluster)) +
  geom_point(size = 0.35, alpha = 0.65) +
  labs(
    title = "DBSCAN Collision Clusters — UK Sample",
    subtitle = "20,000 records | eps = 1 km | minPts = 10",
    x = "Easting (km)",
    y = "Northing (km)",
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  file.path(output_dir, "dbscan_1km_sample_plot.png"),
  p, width = 10, height = 7, dpi = 200
)

print(p)
cat("Saved plot to results/hotspots/dbscan_1km_sample_plot.png\n")