# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 4: Data Preprocessing
# ============================================================

library(tidyverse)
library(lubridate)

# ------------------------------------------------------------
# 1. Load raw dataset
# ------------------------------------------------------------

uk_accidents <- read_csv(
  "data/UK/dft-road-casualty-statistics-collision-last-5-years.csv",
  show_col_types = FALSE
)

cat("Raw records:", nrow(uk_accidents), "\n")
cat("Raw variables:", ncol(uk_accidents), "\n")


# ------------------------------------------------------------
# 2. Create working copy
# ------------------------------------------------------------

uk_clean <- uk_accidents


# ------------------------------------------------------------
# 3. Convert date
# ------------------------------------------------------------

uk_clean <- uk_clean %>%
  mutate(
    date = dmy(date)
  )


# ------------------------------------------------------------
# 4. Create temporal features
# ------------------------------------------------------------

uk_clean <- uk_clean %>%
  mutate(
    accident_year = year(date),
    accident_month = month(date),
    accident_hour = hour(time),
    accident_day = wday(date, label = TRUE)
  )


# ------------------------------------------------------------
# 5. Convert collision severity to factor
# ------------------------------------------------------------

uk_clean <- uk_clean %>%
  mutate(
    collision_severity = factor(
      collision_severity,
      levels = c(1, 2, 3)
    )
  )


# ------------------------------------------------------------
# 6. Create spatial dataset for DBSCAN
# ------------------------------------------------------------

uk_spatial <- uk_clean %>%
  filter(
    !is.na(latitude),
    !is.na(longitude)
  )


# ------------------------------------------------------------
# 7. Basic checks
# ------------------------------------------------------------

cat("\nClean dataset records:", nrow(uk_clean), "\n")
cat("Spatial dataset records:", nrow(uk_spatial), "\n")

cat("\nSeverity distribution:\n")
print(table(uk_clean$collision_severity))

cat("\nMissing coordinates:\n")
print(
  sum(
    is.na(uk_clean$latitude) |
    is.na(uk_clean$longitude)
  )
)

cat("\nDate range:\n")
print(range(uk_clean$date, na.rm = TRUE))

cat("\nNew temporal features:\n")
print(
  uk_clean %>%
    select(
      date,
      time,
      accident_year,
      accident_month,
      accident_hour,
      accident_day
    ) %>%
    head()
)