library(readr)
library(dplyr)

d <- read_csv(
  "data/UK/dft-road-casualty-statistics-collision-last-5-years.csv",
  col_select = c(collision_index, collision_year, latitude, longitude),
  show_col_types = FALSE
)

western <- d %>%
  filter(
    !is.na(longitude),
    !is.na(latitude),
    longitude < -6
  ) %>%
  arrange(longitude)

cat("Records west of -6 degrees:", nrow(western), "\n")
cat("Longitude range:", range(western$longitude), "\n")
cat("Latitude range:", range(western$latitude), "\n")
cat("\nTen westernmost records:\n")

print(
  western %>%
    select(collision_index, collision_year, latitude, longitude) %>%
    slice_head(n = 10)
)

cat("\nRecords by longitude band:\n")
print(
  western %>%
    mutate(longitude_band = cut(
      longitude,
      breaks = c(-Inf, -7, -6.5, -6, Inf),
      right = FALSE
    )) %>%
    count(longitude_band)
)