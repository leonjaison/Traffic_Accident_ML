
# Phase 2: Baseline Random Forest comparison against SMOTE
# Uses 2021-2023 for training and 2024 for validation.
# This experiment does NOT modify the locked project model.

set.seed(42)

project_dir <- "D:/Traffic_Accident_ML"
setwd(project_dir)

library(ranger)

cat("\n=== SMOTE Baseline Comparison ===\n")

# Load the chronological training data
data_path <- "results/data/chronological_train_data.rds"

if (!file.exists(data_path)) {
  stop("Training data not found: ", data_path)
}

dat <- readRDS(data_path)

target <- "collision_severity"

features <- c(
  "number_of_vehicles",
  "number_of_casualties",
  "speed_limit",
  "road_type",
  "light_conditions",
  "weather_conditions",
  "road_surface_conditions",
  "urban_or_rural_area",
  "junction_control",
  "accident_hour"
)

required_columns <- c(target, "collision_year", features)
missing_columns <- setdiff(required_columns, names(dat))

if (length(missing_columns) > 0) {
  stop(
    "Missing columns in training data: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Keep only the columns needed for this experiment
dat <- dat[, required_columns, drop = FALSE]

dat[[target]] <- factor(
  as.character(dat[[target]]),
  levels = c("Fatal", "Serious", "Slight")
)

# Same chronological split as the SMOTE experiment
train_data <- dat[dat$collision_year <= 2023, , drop = FALSE]
valid_data <- dat[dat$collision_year == 2024, , drop = FALSE]

if (nrow(train_data) == 0 || nrow(valid_data) == 0) {
  stop("Training or validation split is empty. Check collision_year.")
}

cat("Training rows before sampling:", nrow(train_data), "\n")
cat("2024 validation rows:", nrow(valid_data), "\n")

# Apply the same class caps used in the SMOTE experiment
set.seed(42)

fatal_rows <- train_data[
  train_data[[target]] == "Fatal", , drop = FALSE
]

serious_rows <- train_data[
  train_data[[target]] == "Serious", , drop = FALSE
]

slight_rows <- train_data[
  train_data[[target]] == "Slight", , drop = FALSE
]

if (nrow(fatal_rows) == 0 ||
    nrow(serious_rows) == 0 ||
    nrow(slight_rows) == 0) {
  stop("One or more severity classes are missing from training data.")
}

serious_rows <- serious_rows[
  sample(seq_len(nrow(serious_rows)),
         min(60000, nrow(serious_rows))),
  , drop = FALSE
]

slight_rows <- slight_rows[
  sample(seq_len(nrow(slight_rows)),
         min(60000, nrow(slight_rows))),
  , drop = FALSE
]

baseline_train <- rbind(
  fatal_rows,
  serious_rows,
  slight_rows
)

baseline_train <- baseline_train[
  sample(seq_len(nrow(baseline_train))),
  , drop = FALSE
]

# Treat categorical predictors as factors and handle missing values
categorical_features <- c(
  "road_type",
  "light_conditions",
  "weather_conditions",
  "road_surface_conditions",
  "urban_or_rural_area",
  "junction_control"
)

for (column in categorical_features) {
  baseline_train[[column]] <- as.character(baseline_train[[column]])
  valid_data[[column]] <- as.character(valid_data[[column]])

  baseline_train[[column]][is.na(baseline_train[[column]])] <- "Missing"
  valid_data[[column]][is.na(valid_data[[column]])] <- "Missing"

  known_levels <- unique(baseline_train[[column]])
  valid_data[[column]][
    !valid_data[[column]] %in% known_levels
  ] <- "Missing"

  baseline_train[[column]] <- factor(
    baseline_train[[column]],
    levels = unique(c(known_levels, "Missing"))
  )

  valid_data[[column]] <- factor(
    valid_data[[column]],
    levels = levels(baseline_train[[column]])
  )
}

numeric_features <- setdiff(features, categorical_features)

for (column in numeric_features) {
  med <- median(baseline_train[[column]], na.rm = TRUE)

  if (!is.finite(med)) {
    med <- 0
  }

  baseline_train[[column]][
    is.na(baseline_train[[column]])
  ] <- med

  valid_data[[column]][
    is.na(valid_data[[column]])
  ] <- med
}

# Remove validation rows whose target is missing
valid_data <- valid_data[
  !is.na(valid_data[[target]]),
  , drop = FALSE
]

cat("\nBaseline training class counts:\n")
print(table(baseline_train[[target]]))

cat("\n2024 validation class counts:\n")
print(table(valid_data[[target]]))

# Train baseline RF: no SMOTE and no class weights
set.seed(42)

baseline_model <- ranger(
  formula = collision_severity ~ .,
  data = baseline_train[, c(target, features), drop = FALSE],
  num.trees = 300,
  mtry = 5,
  min.node.size = 20,
  probability = TRUE,
  importance = "none",
  seed = 42
)

cat("\nBaseline model training completed.\n")

# Predict on the untouched 2024 validation set
probabilities <- predict(
  baseline_model,
  data = valid_data[, features, drop = FALSE]
)$predictions

predicted_class <- colnames(probabilities)[
  max.col(probabilities, ties.method = "first")
]

actual <- factor(
  valid_data[[target]],
  levels = c("Fatal", "Serious", "Slight")
)

predicted <- factor(
  predicted_class,
  levels = levels(actual)
)

confusion <- table(
  Actual = actual,
  Predicted = predicted
)

cat("\n2024 validation confusion matrix:\n")
print(confusion)

# Calculate per-class precision, recall and F1
class_metrics <- data.frame(
  class = levels(actual),
  support = as.integer(table(actual)),
  precision = NA_real_,
  recall = NA_real_,
  f1 = NA_real_
)

for (i in seq_along(levels(actual))) {
  class_name <- levels(actual)[i]

  tp <- sum(actual == class_name & predicted == class_name)
  fp <- sum(actual != class_name & predicted == class_name)
  fn <- sum(actual == class_name & predicted != class_name)

  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  recall <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  f1 <- if ((precision + recall) > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    0
  }

  class_metrics$precision[i] <- precision
  class_metrics$recall[i] <- recall
  class_metrics$f1[i] <- f1
}

accuracy <- mean(actual == predicted)
macro_f1 <- mean(class_metrics$f1)
balanced_accuracy <- mean(class_metrics$recall)

summary_metrics <- data.frame(
  model = "Baseline Random Forest (no SMOTE)",
  validation_year = 2024,
  accuracy = accuracy,
  macro_f1 = macro_f1,
  balanced_accuracy = balanced_accuracy,
  fatal_recall = class_metrics$recall[
    class_metrics$class == "Fatal"
  ],
  serious_recall = class_metrics$recall[
    class_metrics$class == "Serious"
  ]
)

# Save experiment outputs separately from the locked model
output_dir <- "results/smote_experiment"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

write.csv(
  summary_metrics,
  file.path(output_dir, "baseline_validation_summary.csv"),
  row.names = FALSE
)

write.csv(
  class_metrics,
  file.path(output_dir, "baseline_validation_class_metrics.csv"),
  row.names = FALSE
)

write.csv(
  as.data.frame.matrix(confusion),
  file.path(output_dir, "baseline_validation_confusion_matrix.csv")
)

saveRDS(
  data.frame(
    actual = actual,
    predicted = predicted
  ),
  file.path(output_dir, "baseline_validation_predictions.rds")
)

saveRDS(
  baseline_model,
  file.path(output_dir, "baseline_random_forest_2024.rds")
)

cat("\n=== BASELINE VALIDATION SUMMARY ===\n")
print(summary_metrics)

cat("\n=== PER-CLASS METRICS ===\n")
print(class_metrics)

cat("\nResults saved to:", normalizePath(output_dir), "\n")
cat("\nExperiment completed successfully.\n")
