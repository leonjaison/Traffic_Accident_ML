# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 2: Data Quality Assessment
# ============================================================

library(tidyverse)

# Load dataset
uk_accidents <- read_csv(
  "data/UK/dft-road-casualty-statistics-collision-last-5-years.csv",
  show_col_types = FALSE
)

# ------------------------------------------------------------
# 1. Dataset dimensions
# ------------------------------------------------------------

cat("Rows:", nrow(uk_accidents), "\n")
cat("Columns:", ncol(uk_accidents), "\n")

# ------------------------------------------------------------
# 2. Year range
# ------------------------------------------------------------

cat("\nCollision years:\n")
print(table(uk_accidents$collision_year))

# ------------------------------------------------------------
# 3. Severity distribution
# ------------------------------------------------------------

cat("\nCollision severity:\n")
print(table(uk_accidents$collision_severity, useNA = "ifany"))

# ------------------------------------------------------------
# 4. Missing values
# ------------------------------------------------------------

missing_values <- colSums(is.na(uk_accidents))

cat("\nMissing values by column:\n")
print(sort(missing_values, decreasing = TRUE))

# ------------------------------------------------------------
# 5. Duplicate collision IDs
# ------------------------------------------------------------

duplicate_ids <- sum(duplicated(uk_accidents$collision_index))

cat("\nDuplicate collision IDs:", duplicate_ids, "\n")

# ------------------------------------------------------------
# 6. Coordinate availability
# ------------------------------------------------------------

missing_coordinates <- sum(
  is.na(uk_accidents$latitude) |
  is.na(uk_accidents$longitude)
)

cat("\nRecords with missing coordinates:",
    missing_coordinates, "\n")

# ------------------------------------------------------------
# 7. Coordinate ranges
# ------------------------------------------------------------

cat("\nLatitude range:\n")
print(range(uk_accidents$latitude, na.rm = TRUE))

cat("\nLongitude range:\n")
print(range(uk_accidents$longitude, na.rm = TRUE))