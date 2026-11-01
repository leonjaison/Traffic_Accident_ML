
setwd("D:/Traffic_Accident_ML")

library(ranger)

model <- readRDS("results/random_forest/model.rds")
features <- model$forest$independent.variable.names
levels_list <- model$forest$covariate.levels

cat("\n=== SAVED MODEL ===\n")
print(class(model))
print(features)

data <- read.csv(
  "data/UK/dft-road-casualty-statistics-collision-last-5-years.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Convert numeric predictors safely.
numeric_features <- c(
  "number_of_vehicles",
  "number_of_casualties",
  "accident_hour"
)

data$number_of_vehicles <- suppressWarnings(
  as.numeric(data$number_of_vehicles)
)
data$number_of_casualties <- suppressWarnings(
  as.numeric(data$number_of_casualties)
)
data$accident_hour <- suppressWarnings(
  as.integer(substr(trimws(as.character(data$time)), 1, 2))
)

# Keep only rows with valid numeric predictors.
valid <- complete.cases(data[, numeric_features])

# Keep categorical values that exist in the saved model.
for (feature in names(levels_list)) {
  if (is.null(levels_list[[feature]])) next

  valid <- valid &
    !is.na(data[[feature]]) &
    as.character(data[[feature]]) %in% levels_list[[feature]]
}

valid <- valid &
  data$number_of_vehicles >= 1 &
  data$number_of_casualties >= 1 &
  data$accident_hour >= 0 &
  data$accident_hour <= 23

data_valid <- data[valid, , drop = FALSE]

cat("\nValid rows available:", nrow(data_valid), "\n")

if (nrow(data_valid) == 0) {
  stop("No valid rows found. We need to inspect the data preprocessing.")
}

# Test several genuine rows without retraining the model.
set.seed(42)
sample_indices <- sample(
  seq_len(nrow(data_valid)),
  min(10, nrow(data_valid))
)

test <- data_valid[sample_indices, features, drop = FALSE]

# Recreate the factor types expected by the saved model.
for (feature in names(levels_list)) {
  if (is.null(levels_list[[feature]])) next

  test[[feature]] <- factor(
    as.character(test[[feature]]),
    levels = levels_list[[feature]]
  )
}

cat("\n=== INPUT ROWS ===\n")
print(test)

cat("\n=== PREDICTIONS ===\n")
predictions <- predict(model, data = test)$predictions
print(predictions)

cat("\n=== PREDICTED CLASS COUNTS ===\n")
print(table(predictions))

cat("\nDiagnostic complete. The saved model was not changed.\n")

