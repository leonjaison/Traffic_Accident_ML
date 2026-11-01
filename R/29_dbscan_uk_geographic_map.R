
# ============================================================
# Step 29: Geographic UK map of DBSCAN results
# Uses saved assignments; does NOT rerun DBSCAN.
# ============================================================

library(tidyverse)
library(sf)
library(ggplot2)

# 1. Project setup
project_dir <- "D:/Traffic_Accident_ML"
setwd(project_dir)

input_file <- "results/hotspots/uk_dbscan_assignments.csv"
output_dir <- "results/hotspots"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

output_file <- file.path(
  output_dir,
  "dbscan_uk_geographic_map.png"
)

# 2. Load existing DBSCAN assignments
if (!file.exists(input_file)) {
  stop("DBSCAN assignments not found: ", input_file)
}

assignments <- read_csv(
  input_file,
  show_col_types = FALSE
)

required_columns <- c(
  "collision_index",
  "latitude",
  "longitude",
  "cluster_id",
  "is_noise"
)

missing_columns <- setdiff(
  required_columns,
  names(assignments)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# 3. Validate coordinates and classifications
plot_data <- assignments %>%
  filter(
    is.finite(latitude),
    is.finite(longitude),
    between(latitude, -90, 90),
    between(longitude, -180, 180),
    !is.na(cluster_id)
  ) %>%
  mutate(
    point_type = if_else(
      cluster_id == 0,
      "Noise",
      "Clustered"
    )
  )

cat("Assignment records:", nrow(assignments), "\n")
cat("Valid records for mapping:", nrow(plot_data), "\n")

cat("\nClassification counts:\n")
print(table(plot_data$point_type))

# Verify saved noise flags
noise_flag <- tolower(as.character(plot_data$is_noise)) %in%
  c("true", "t", "1", "yes")

flag_mismatches <- sum(
  noise_flag != (plot_data$cluster_id == 0),
  na.rm = TRUE
)

cat("Noise-flag mismatches:", flag_mismatches, "\n")

# 4. Load UK geographic boundary
if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
  stop(
    "Package 'rnaturalearth' is missing. ",
    "Install it in PowerShell/R before rerunning this script."
  )
}

world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

uk_boundary <- world %>%
  filter(admin == "United Kingdom") %>%
  st_make_valid() %>%
  st_transform(27700)

if (nrow(uk_boundary) == 0) {
  stop("Could not load the United Kingdom boundary.")
}

# 5. Transform collision points into British National Grid
# EPSG:27700 coordinates are in metres.
points_sf <- st_as_sf(
  plot_data,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(27700)

xy_m <- st_coordinates(points_sf)

plot_data$easting_m <- xy_m[, 1]
plot_data$northing_m <- xy_m[, 2]

# 6. Diagnose points against the mapped UK boundary
inside_uk <- lengths(
  st_intersects(points_sf, uk_boundary)
) > 0

cat(
  "Points inside mapped UK boundary:",
  sum(inside_uk),
  "\n"
)

cat(
  "Points outside mapped UK boundary:",
  sum(!inside_uk),
  "\n"
)

# 7. Sample points for display
set.seed(42)

sample_n <- min(50000L, nrow(plot_data))

plot_sample <- plot_data %>%
  slice_sample(n = sample_n)

cat(
  "Points displayed on map:",
  nrow(plot_sample),
  "\n"
)

# 8. Plot boundary and points using the SAME CRS and units.
# Boundary: EPSG:27700, metres.
# Points:   EPSG:27700, metres.
# Axis labels are converted to kilometres for readability.

p <- ggplot() +
  geom_sf(
    data = uk_boundary,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.35
  ) +
  geom_point(
    data = plot_sample,
    aes(
      x = easting_m,
      y = northing_m,
      color = point_type
    ),
    size = 0.45,
    alpha = 0.65
  ) +
  scale_color_manual(
    values = c(
      "Noise" = "grey65",
      "Clustered" = "orangered2"
    ),
    breaks = c("Clustered", "Noise")
  ) +
  scale_x_continuous(
    labels = function(x) {
      format(
        x / 1000,
        trim = TRUE,
        scientific = FALSE
      )
    }
  ) +
  scale_y_continuous(
    labels = function(y) {
      format(
        y / 1000,
        trim = TRUE,
        scientific = FALSE
      )
    }
  ) +
  coord_sf(
    crs = st_crs(27700),
    default_crs = st_crs(27700),
    datum = NA,
    expand = TRUE
  ) +
  labs(
    title = "UK Road Collision DBSCAN Analysis",
    subtitle = paste0(
      "Sample of ",
      format(sample_n, big.mark = ","),
      " records | eps = 1 km | minPts = 10"
    ),
    x = "Easting (km)",
    y = "Northing (km)",
    color = "Classification",
    caption = paste(
      "Grey: DBSCAN noise.",
      "Orange: assigned to a cluster.",
      "Density-connected clusters are not independently",
      "validated safety hotspots.",
      "Boundary source: Natural Earth."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 8)
  )

# 9. Save the map to a NEW output file
ggsave(
  filename = output_file,
  plot = p,
  width = 10,
  height = 10,
  dpi = 200,
  bg = "white"
)

print(p)

cat("\nMap saved to:\n", output_file, "\n")
cat("Existing DBSCAN assignments were not modified.\n")

