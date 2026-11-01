# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 13: Decision Tree Tuning
# ============================================================

library(tidyverse)
library(rpart)

# ------------------------------------------------------------
# 1. Load training data only
# ------------------------------------------------------------

train_data <- readRDS(
  "results/data/chronological_train_data.rds"
)

tree_predictors <- c(
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

categorical_variables <- c(
  "speed_limit",
  "road_type",
  "light_conditions",
  "weather_conditions",
  "road_surface_conditions",
  "urban_or_rural_area",
  "junction_control"
)

tree_data <- train_data %>%
  select(collision_severity, all_of(tree_predictors)) %>%
  mutate(
    across(all_of(categorical_variables), as.factor)
  )

class_levels <- c("Fatal", "Serious", "Slight")

# ------------------------------------------------------------
# 2. Create reproducible validation folds within training data
# ------------------------------------------------------------

set.seed(42)

fold_id <- integer(nrow(tree_data))

for (severity in class_levels) {
  severity_rows <- which(tree_data$collision_severity == severity)
  fold_id[severity_rows] <- sample(rep(1:3, length.out = length(severity_rows)))
}

# ------------------------------------------------------------
# 3. Define evaluation metrics for validation predictions
# ------------------------------------------------------------

calculate_metrics <- function(actual, predicted) {
  actual <- factor(actual, levels = class_levels)
  predicted <- factor(predicted, levels = class_levels)
  confusion_matrix <- table(actual, predicted)
  support <- rowSums(confusion_matrix)
  predicted_count <- colSums(confusion_matrix)
  true_positive <- diag(confusion_matrix)

  precision <- ifelse(predicted_count > 0, true_positive / predicted_count, 0)
  recall <- ifelse(support > 0, true_positive / support, 0)
  f1_score <- ifelse(
    precision + recall > 0,
    2 * precision * recall / (precision + recall),
    0
  )

  tibble(
    accuracy = sum(true_positive) / sum(confusion_matrix),
    macro_f1 = mean(f1_score),
    weighted_f1 = sum(f1_score * support) / sum(support),
    balanced_accuracy = mean(recall),
    fatal_recall = recall[class_levels == "Fatal"],
    serious_recall = recall[class_levels == "Serious"]
  )
}

# ------------------------------------------------------------
# 4. Define a justified tuning grid
# ------------------------------------------------------------

tuning_grid <- crossing(
  cp = c(0.0001, 0.001),
  minsplit = c(50, 100),
  maxdepth = c(6, 8),
  weight_set = c("cost_5_2_1", "cost_10_3_1")
) %>%
  mutate(
    weights = map(
      weight_set,
      ~ c(Fatal = ifelse(.x == "cost_5_2_1", 5, 10),
            Serious = ifelse(.x == "cost_5_2_1", 2, 3),
            Slight = 1)
    )
  )

cat("Configurations:", nrow(tuning_grid), "\n")
cat("Validation folds: 3 stratified folds within training data\n")

# ------------------------------------------------------------
# 5. Tune using training folds only
# ------------------------------------------------------------

validation_results <- vector("list", nrow(tuning_grid) * 3)
result_index <- 1

for (configuration in seq_len(nrow(tuning_grid))) {
  parameters <- tuning_grid[configuration, ]

  for (fold in 1:3) {
    fold_train <- tree_data[fold_id != fold, ]
    fold_validation <- tree_data[fold_id == fold, ]

    model <- rpart(
      collision_severity ~ .,
      data = fold_train,
      method = "class",
      weights = parameters$weights[[1]][
        as.character(fold_train$collision_severity)
      ],
      control = rpart.control(
        cp = parameters$cp,
        minsplit = parameters$minsplit,
        maxdepth = parameters$maxdepth
      )
    )

    predictions <- predict(
      model,
      newdata = fold_validation,
      type = "class"
    )

    validation_results[[result_index]] <- calculate_metrics(
      fold_validation$collision_severity,
      predictions
    ) %>%
      mutate(
        cp = parameters$cp,
        minsplit = parameters$minsplit,
        maxdepth = parameters$maxdepth,
        weight_set = parameters$weight_set,
        fold = fold
      )

    result_index <- result_index + 1
  }
}

validation_results <- bind_rows(validation_results)

mean_validation_results <- validation_results %>%
  group_by(cp, minsplit, maxdepth, weight_set) %>%
  summarise(
    across(
      c(accuracy, macro_f1, weighted_f1, balanced_accuracy,
        fatal_recall, serious_recall),
      mean
    ),
    .groups = "drop"
  ) %>%
  arrange(
    desc(macro_f1),
    desc(balanced_accuracy),
    desc(fatal_recall),
    desc(accuracy)
  )

cat("\n=== VALIDATION RESULTS RANKED BY MACRO F1 ===\n")
print(mean_validation_results)

best_parameters <- mean_validation_results %>%
  slice(1)

cat("\n=== SELECTED PARAMETERS ===\n")
print(best_parameters)

# ------------------------------------------------------------
# 6. Fit selected tree on all training data
# ------------------------------------------------------------

selected_weights <- if (best_parameters$weight_set == "cost_5_2_1") {
  c(Fatal = 5, Serious = 2, Slight = 1)
} else {
  c(Fatal = 10, Serious = 3, Slight = 1)
}

set.seed(42)

tuned_decision_tree <- rpart(
  collision_severity ~ .,
  data = tree_data,
  method = "class",
  weights = selected_weights[
    as.character(tree_data$collision_severity)
  ],
  control = rpart.control(
    cp = best_parameters$cp,
    minsplit = best_parameters$minsplit,
    maxdepth = best_parameters$maxdepth
  )
)

saveRDS(
  tuned_decision_tree,
  "results/data/tuned_decision_tree.rds"
)

write_csv(
  validation_results,
  "results/tables/decision_tree_tuning_fold_results.csv"
)

write_csv(
  mean_validation_results,
  "results/tables/decision_tree_tuning_results.csv"
)

cat("\nTuned Decision Tree saved successfully.\n")