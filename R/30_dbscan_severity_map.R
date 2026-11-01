
# ============================================================
# Step 30: DBSCAN map coloured by recorded accident severity
# Uses existing DBSCAN assignments. Does NOT rerun DBSCAN.
# Does NOT modify the locked Random Forest model.
# ============================================================

library(tidyverse)
library(sf)
library(ggplot2)

# 1. Project paths
project_dir <- "D:/Traffic_Accident_ML"
setwd(project_dir)

assignments_file <-
  "results/hotspots/uk_dbscan_assignments.csv"

dataset_file <-
  "data/UK/dft-road-casualty-statistics-collision-last-5-years.csv"

output_dir <- "results/hotspots"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# 2. Verify input files
if (!file.exists(assignments_file)) {
  stop("DBSCAN assignments file not found: ", assignments_file)
}

if (!file.exists(dataset_file)) {
  stop("Original UK dataset not found: ", dataset_file)
}

# 3. Load saved DBSCAN assignments
assignments <- read_csv(
  assignments_file,
  show_col_types = FALSE
)

required_assignment_columns <- c(
  "collision_index",
  "latitude",
  "longitude",
  "cluster_id"
)

missing_assignment_columns <- setdiff(
  required_assignment_columns,
  names(assignments)
)

if (length(missing_assignment_columns) > 0) {
  stop(
    "Missing assignment columns: ",
    paste(missing_assignment_columns, collapse = ", ")
  )
}

# 4. Load the required columns from the original dataset
severity_data <- read_csv(
  dataset_file,
  col_select = any_of(c(
    "collision_index",
    "collision_severity"
  )),
  show_col_types = FALSE
)

if (!all(c(
  "collision_index",
  "collision_severity"
) %in% names(severity_data))) {
  stop(
    "Original dataset must contain collision_index ",
    "and collision_severity."
  )
}

# 5. Convert severity codes to readable labels
severity_data <- severity_data %>%
  mutate(
    collision_severity = case_when(
      as.character(collision_severity) == "1" ~ "Fatal",
      as.character(collision_severity) == "2" ~ "Serious",
      as.character(collision_severity) == "3" ~ "Slight",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(collision_index)) %>%
  distinct(collision_index, .keep_all = TRUE)

# 6. Join severity to existing DBSCAN assignments
assignments <- assignments %>%
  mutate(collision_index = as.character(collision_index))

severity_data <- severity_data %>%
  mutate(collision_index = as.character(collision_index))

severity_map_data <- assignments %>%
  select(
    collision_index,
    latitude,
    longitude,
    cluster_id
  ) %>%
  left_join(
    severity_data,
    by = "collision_index"
  )

if (nrow(severity_map_data) != nrow(assignments)) {
  stop("Unexpected row-count change after severity join.")
}

cat("DBSCAN assignment rows:", nrow(assignments), "\n")
cat("Rows after severity join:", nrow(severity_map_data), "\n")

cat("\nSeverity counts after join:\n")
print(table(
  severity_map_data$collision_severity,
  useNA = "ifany"
))

missing_severity <- sum(
  is.na(severity_map_data$collision_severity)
)

cat("Records without matched severity:", missing_severity, "\n")

# 7. Validate records for mapping
severity_map_data <- severity_map_data %>%
  filter(
    !is.na(collision_severity),
    !is.na(cluster_id),
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

severity_map_data$collision_severity <- factor(
  severity_map_data$collision_severity,
  levels = c("Fatal", "Serious", "Slight")
)

# 8. Transform points to British National Grid
points_sf <- st_as_sf(
  severity_map_data,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(27700)

coordinates_m <- st_coordinates(points_sf)

severity_map_data$easting_m <- coordinates_m[, 1]
severity_map_data$northing_m <- coordinates_m[, 2]

# 9. Load the UK boundary
if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
  stop(
    "Package 'rnaturalearth' is required. ",
    "Install it before running this script."
  )
}

uk_boundary <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  filter(admin == "United Kingdom") %>%
  st_make_valid() %>%
  st_transform(27700)

if (nrow(uk_boundary) == 0) {
  stop("Could not load the United Kingdom boundary.")
}

# 10. Report mapped records and DBSCAN classes
cat("\nValid records for map:", nrow(severity_map_data), "\n")

cat("\nSeverity counts:\n")
print(table(severity_map_data$collision_severity))

cat("\nDBSCAN classification counts:\n")
print(table(severity_map_data$point_type))

inside_uk <- lengths(
  st_intersects(points_sf, uk_boundary)
) > 0

cat("Points inside mapped UK boundary:", sum(inside_uk), "\n")
cat("Points outside mapped UK boundary:", sum(!inside_uk), "\n")

# 11. Sample records for visualization only
set.seed(42)

sample_n <- min(50000L, nrow(severity_map_data))

plot_sample <- severity_map_data %>%
  slice_sample(n = sample_n)

cat("Points displayed:", nrow(plot_sample), "\n")

cat("\nSeverity counts in displayed sample:\n")
print(table(plot_sample$collision_severity))

# 12. Create severity-coloured map
# Boundary and point coordinates are both in metres.
severity_colors <- c(
  "Fatal" = "#c62828",
  "Serious" = "#ef8c24",
  "Slight" = "#2878b5"
)

p <- ggplot() +
  geom_sf(
    data = uk_boundary,
    fill = "grey97",
    color = "grey40",
    linewidth = 0.35
  ) +
  geom_point(
    data = plot_sample,
    aes(
      x = easting_m,
      y = northing_m,
      color = collision_severity
    ),
    size = 0.7,
    alpha = 0.75
  ) +
  scale_color_manual(
    values = severity_colors,
    breaks = c("Fatal", "Serious", "Slight"),
    drop = FALSE
  ) +
  scale_x_continuous(
    labels = function(x) {
      format(x / 1000, trim = TRUE, scientific = FALSE)
    }
  ) +
  scale_y_continuous(
    labels = function(y) {
      format(y / 1000, trim = TRUE, scientific = FALSE)
    }
  ) +
  coord_sf(
    crs = st_crs(27700),
    default_crs = st_crs(27700),
    datum = NA,
    expand = TRUE
  ) +
  labs(
    title = "UK Road Collision Severity Map",
    subtitle = paste0(
      "Sample of ",
      format(sample_n, big.mark = ","),
      " records | Colours show recorded collision severity"
    ),
    x = "Easting (km)",
    y = "Northing (km)",
    color = "Recorded severity",
    caption = paste(
      "Source: UK Department for Transport Road Safety Data.",
      "DBSCAN assignments are unchanged.",
      "Recorded severity is not a model prediction."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9)
  )

# 13. Save the map to a separate output file
output_file <- file.path(
  output_dir,
  "dbscan_uk_severity_map.png"
)

ggsave(
  filename = output_file,
  plot = p,
  width = 12,
  height = 12,
  dpi = 250,
  bg = "white"
)

print(p)

cat("\nSeverity map saved to:\n", output_file, "\n")
cat("Existing DBSCAN assignments were not modified.\n")
