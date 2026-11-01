# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 1: Data Loading
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# 1. Load UK collision dataset
# ------------------------------------------------------------

uk_accidents <- read_csv(
  "data/UK/dft-road-casualty-statistics-collision-last-5-years.csv",
  show_col_types = FALSE
)

# ------------------------------------------------------------
# 2. Basic dataset information
# ------------------------------------------------------------

cat("Number of rows:", nrow(uk_accidents), "\n")
cat("Number of columns:", ncol(uk_accidents), "\n\n")

cat("Column names:\n")
print(names(uk_accidents))

# ------------------------------------------------------------
# 3. Dataset structure
# ------------------------------------------------------------

cat("\nDataset structure:\n")
glimpse(uk_accidents)

# ------------------------------------------------------------
# 4. First records
# ------------------------------------------------------------

cat("\nFirst 6 records:\n")
print(head(uk_accidents))

cat("\nAvailable speed limits:\n")
print(sort(unique(uk_accidents$speed_limit)))

cat("\nMaximum speed limit:\n")
print(max(uk_accidents$speed_limit, na.rm = TRUE))