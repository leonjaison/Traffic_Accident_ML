
# ============================================================
# SMOTE Experiment — Traffic Accident Severity Prediction
# Separate experiment; does NOT modify the locked Random Forest
# ============================================================

setwd("D:/Traffic_Accident_ML")

suppressPackageStartupMessages({
  library(recipes)
  library(themis)
  library(ranger)
})

set.seed(42)

# Output directory for this experiment only
output_dir <- "results/smote_experiment"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Load the existing chronological training data
train_all <- readRDS("results/data/chronological_train_data.rds")

# Locked Feature Set A
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

target <- "collision_severity"
required <- c(target, features)

missing_cols <- setdiff(required, names(train_all))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# Keep only the target and locked feature set
dat <- as.data.frame(train_all[, required, drop = FALSE])

# Standardize target labels as factors
dat[[target]] <- factor(
  dat[[target]],
  levels = c("Fatal", "Serious", "Slight")
)

if (anyNA(dat[[target]])) {
  stop("Target contains missing or unexpected labels.")
}

# Chronological validation: use 2021–2023 for training
# and 2024 for validation, using the existing collision_year.
if (!"collision_year" %in% names(train_all)) {
  stop("collision_year is required for chronological validation.")
}

year <- train_all$collision_year
if (is.factor(year)) year <- as.character(year)
year <- suppressWarnings(as.integer(year))

fit_idx <- !is.na(year) & year <= 2023
val_idx <- !is.na(year) & year == 2024

if (sum(fit_idx) == 0 || sum(val_idx) == 0) {
  stop("Could not create 2021–2023 training and 2024 validation split.")
}

fit_data <- dat[fit_idx, , drop = FALSE]
val_data <- dat[val_idx, , drop = FALSE]

cat("Training rows before SMOTE:", nrow(fit_data), "\n")
cat("Validation rows:", nrow(val_data), "\n")
cat("\nTraining class distribution:\n")
print(table(fit_data[[target]]))
cat("\nValidation class distribution:\n")
print(table(val_data[[target]]))

# Convert character predictors to factors and retain factor predictors
for (nm in features) {
  if (is.character(fit_data[[nm]])) {
    fit_data[[nm]] <- factor(fit_data[[nm]])
  }
  if (is.character(val_data[[nm]])) {
    val_data[[nm]] <- factor(
      val_data[[nm]],
      levels = levels(fit_data[[nm]])
    )
  }
}

# Handle unseen/missing categorical values safely
for (nm in features) {
  if (is.factor(fit_data[[nm]])) {
    fit_data[[nm]] <- addNA(fit_data[[nm]])
    val_chr <- as.character(val_data[[nm]])
    val_chr[is.na(val_chr) | !val_chr %in% levels(fit_data[[nm]])] <- NA
    val_data[[nm]] <- factor(
      val_chr,
      levels = levels(fit_data[[nm]])
    )
  }
}

# Use a capped minority-class sample for this first feasibility run.
# The original validation set remains untouched.
majority_n <- sum(fit_data[[target]] == "Slight")
fatal_n <- sum(fit_data[[target]] == "Fatal")
serious_n <- sum(fit_data[[target]] == "Serious")

# Keep all Fatal records and cap each other class to a manageable
# maximum before SMOTE; sampling is performed only on training data.
cap_n <- min(60000L, majority_n)
set.seed(42)

keep_fatal <- which(fit_data[[target]] == "Fatal")
keep_serious <- which(fit_data[[target]] == "Serious")
keep_slight <- which(fit_data[[target]] == "Slight")

keep_serious <- if (length(keep_serious) > cap_n) {
  sample(keep_serious, cap_n)
} else {
  keep_serious
}

keep_slight <- if (length(keep_slight) > cap_n) {
  sample(keep_slight, cap_n)
} else {
  keep_slight
}

fit_sample <- fit_data[c(keep_fatal, keep_serious, keep_slight), , drop = FALSE]
fit_sample <- fit_sample[sample(seq_len(nrow(fit_sample))), , drop = FALSE]

cat("\nTraining sample before SMOTE:\n")
print(table(fit_sample[[target]]))

# Recipe: SMOTE only during training prep/bake.
# Use all available numeric predictors and preserve factors.
rec <- recipe(collision_severity ~ ., data = fit_sample) |>
  step_unknown(all_nominal_predictors()) |>
  step_novel(all_nominal_predictors()) |>
  step_smote(collision_severity, over_ratio = 0.25, neighbors = 5)

cat("\nPreparing SMOTE recipe. This may take some time...\n")
rec_prep <- prep(rec, training = fit_sample, retain = TRUE)
smote_train <- bake(rec_prep, new_data = NULL)

cat("\nTraining distribution after SMOTE:\n")
print(table(smote_train[[target]]))

# Align validation factor levels with training data
for (nm in features) {
  if (is.factor(smote_train[[nm]])) {
    val_data[[nm]] <- factor(
      as.character(val_data[[nm]]),
      levels = levels(smote_train[[nm]])
    )
  }
}

# Train a separate experimental Random Forest
cat("\nTraining experimental SMOTE Random Forest...\n")

smote_model <- ranger(
  formula = collision_severity ~ .,
  data = smote_train,
  num.trees = 300,
  mtry = 5,
  min.node.size = 20,
  importance = "impurity",
  seed = 42
)

saveRDS(
  smote_model,
  file.path(output_dir, "smote_random_forest.rds")
)

# Predict on the untouched chronological validation set
pred <- predict(smote_model, data = val_data)$predictions
pred <- factor(pred, levels = levels(val_data[[target]]))
actual <- val_data[[target]]

conf <- table(
  Actual = actual,
  Predicted = pred,
  useNA = "no"
)

# Compute class-wise precision, recall and F1
class_metrics <- data.frame(
  class = levels(actual),
  support = as.integer(table(actual)),
  precision = NA_real_,
  recall = NA_real_,
  f1_score = NA_real_
)

for (i in seq_along(class_metrics$class)) {
  cls <- class_metrics$class[i]
  tp <- sum(actual == cls & pred == cls, na.rm = TRUE)
  fp <- sum(actual != cls & pred == cls, na.rm = TRUE)
  fn <- sum(actual == cls & pred != cls, na.rm = TRUE)

  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  recall <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  f1 <- if ((precision + recall) > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    0
  }

  class_metrics$precision[i] <- precision
  class_metrics$recall[i] <- recall
  class_metrics$f1_score[i] <- f1
}

accuracy <- mean(pred == actual, na.rm = TRUE)
macro_f1 <- mean(class_metrics$f1_score)
balanced_accuracy <- mean(class_metrics$recall)

summary_metrics <- data.frame(
  model = "SMOTE Random Forest (exploratory validation)",
  accuracy = accuracy,
  macro_f1 = macro_f1,
  balanced_accuracy = balanced_accuracy,
  fatal_recall = class_metrics$recall[class_metrics$class == "Fatal"],
  serious_recall = class_metrics$recall[class_metrics$class == "Serious"]
)

write.csv(
  summary_metrics,
  file.path(output_dir, "smote_validation_summary.csv"),
  row.names = FALSE
)

write.csv(
  class_metrics,
  file.path(output_dir, "smote_validation_class_metrics.csv"),
  row.names = FALSE
)

write.csv(
  as.data.frame.matrix(conf),
  file.path(output_dir, "smote_validation_confusion_matrix.csv")
)

saveRDS(
  list(
    actual = actual,
    predicted = pred,
    validation_year = 2024,
    features = features
  ),
  file.path(output_dir, "smote_validation_predictions.rds")
)

cat("\nSMOTE experiment completed.\n")
cat("Summary metrics:\n")
print(summary_metrics)
cat("\nClass-wise metrics:\n")
print(class_metrics)
cat("\nOutputs saved under:", output_dir, "\n")
