library(readr)

d <- read_csv(
  "data/UK/dft-road-casualty-statistics-collision-last-5-years.csv",
  col_select = c(collision_index, latitude, longitude),
  show_col_types = FALSE
)

cat("Total rows:", nrow(d), "\n")
cat(
  "Missing coordinates:",
  sum(is.na(d[["latitude"]]) | is.na(d[["longitude"]])),
  "\n"
)
cat("Longitude range:", range(d[["longitude"]], na.rm = TRUE), "\n")
cat("Latitude range:", range(d[["latitude"]], na.rm = TRUE), "\n")
cat(
  "Records west of -6 degrees longitude:",
  sum(d[["longitude"]] < -6, na.rm = TRUE),
  "\n"
)