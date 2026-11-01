
library(tidyverse)
library(sf)
library(ggplot2)

project_dir <- "D:/Traffic_Accident_ML"
setwd(project_dir)

input_file <- "results/hotspots/uk_dbscan_assignments.csv"
output_dir <- "results/hotspots"

if (!file.exists(input_file)) {
  stop("DBSCAN assignments not found: ", input_file)
}

assignments <- read_csv(input_file, show_col_types = FALSE)

required <- c("latitude", "longitude", "cluster_id", "is_noise")
missing <- setdiff(required, names(assignments))

if (length(missing) > 0) {
  stop("Missing columns: ", paste(missing, collapse = ", "))
}

# Retain valid coordinates and classify each point.
plot_data <- assignments %>%
  filter(
    is.finite(latitude),
    is.finite(longitude),
    between(latitude, -90, 90),
    between(longitude, -180, 180)
  ) %>%
  mutate(
    point_type = if_else(
      cluster_id == 0,
      "Noise",
      "Clustered"
    )
  )

# Use a reproducible sample for a readable plot.
# Full assignments remain unchanged.
set.seed(42)
plot_sample <- plot_data %>%
  slice_sample(n = min(50000L, nrow(plot_data)))

# Convert sampled points to British National Grid.
points_sf <- st_as_sf(
  plot_sample,
  coords = c("longitude", "latitude"),
  crs = 4326
) %>%
  st_transform(27700)

xy <- st_coordinates(points_sf)

plot_sample$easting_km <- xy[, 1] / 1000
plot_sample$northing_km <- xy[, 2] / 1000

cat("Full assignment records:", nrow(plot_data), "\n")
cat("Points displayed:", nrow(plot_sample), "\n")
cat("\nDisplayed point types:\n")
print(table(plot_sample$point_type))

p <- ggplot(
  plot_sample,
  aes(easting_km, northing_km, color = point_type)
) +
  geom_point(size = 0.35, alpha = 0.6) +
  scale_color_manual(
    values = c(
      "Noise" = "grey65",
      "Clustered" = "orangered2"
    )
  ) +
  coord_equal() +
  labs(
    title = "UK Road Collision Density: DBSCAN Results",
    subtitle = paste(
      "Displayed sample:", nrow(plot_sample),
      "| eps = 1 km | minPts = 10"
    ),
    x = "Easting (km)",
    y = "Northing (km)",
    color = "DBSCAN classification",
    caption = paste(
      "Grey = noise; orange/red = clustered records.",
      "Projected coordinates show spatial distribution,",
      "not administrative boundaries."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

output_file <- file.path(
  output_dir,
  "dbscan_uk_noise_and_clusters.png"
)

ggsave(
  output_file,
  p,
  width = 10,
  height = 7,
  dpi = 200
)

print(p)
cat("\nSaved map:", output_file, "\n")
