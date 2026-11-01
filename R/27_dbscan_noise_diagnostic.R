
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
    !is.na(collision_index),
    !is.na(latitude),
    !is.na(longitude),
    is.finite(latitude),
    is.finite(longitude),
    between(latitude, -90, 90),
    between(longitude, -180, 180)
  ) %>%
  distinct(collision_index, .keep_all = TRUE)

set.seed(42)

pilot <- data[
  sample(seq_len(nrow(data)), min(20000L, nrow(data))),
]

projected <- st_transform(
  st_as_sf(
    pilot,
    coords = c("longitude", "latitude"),
    crs = 4326
  ),
  27700
)

xy <- st_coordinates(projected)

fit <- dbscan::dbscan(
  xy,
  eps = 1000,
  minPts = 10
)

plot_data <- tibble(
  easting = xy[, 1] / 1000,
  northing = xy[, 2] / 1000,
  group = ifelse(fit$cluster == 0, "Noise", "Clustered")
)

cat("Sample records:", nrow(plot_data), "\n")
cat("\nPoint classification:\n")
print(table(plot_data$group))

p <- ggplot(
  plot_data,
  aes(x = easting, y = northing, color = group)
) +
  geom_point(size = 0.45, alpha = 0.65) +
  scale_color_manual(
    values = c(
      "Noise" = "grey70",
      "Clustered" = "darkorange"
    )
  ) +
  labs(
    title = "DBSCAN Noise and Clustered Records — UK Sample",
    subtitle = "20,000 records | eps = 1 km | minPts = 10",
    x = "Easting (km)",
    y = "Northing (km)",
    color = "Classification"
  ) +
  theme_minimal()

ggsave(
  file.path(output_dir, "dbscan_noise_diagnostic.png"),
  p,
  width = 10,
  height = 7,
  dpi = 200
)

print(p)

cat(
  "\nSaved diagnostic plot to:",
  file.path(output_dir, "dbscan_noise_diagnostic.png"),
  "\n"
)
