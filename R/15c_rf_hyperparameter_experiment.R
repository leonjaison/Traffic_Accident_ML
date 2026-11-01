# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 15c: Controlled Random Forest Hyperparameter Experiment
# ============================================================

library(tidyverse)
library(ranger)

train_data <- readRDS("results/data/chronological_train_data.rds")
test_data <- readRDS("results/data/chronological_test_data.rds")

predictors <- c(
  "number_of_vehicles", "number_of_casualties", "speed_limit",
  "road_type", "light_conditions", "weather_conditions",
  "road_surface_conditions", "urban_or_rural_area", "junction_control",
  "accident_hour"
)

categorical_variables <- c(
  "speed_limit", "road_type", "light_conditions", "weather_conditions",
  "road_surface_conditions", "urban_or_rural_area", "junction_control"
)

class_levels <- c("Fatal", "Serious", "Slight")
class_weights <- c(Fatal = 10, Serious = 3, Slight = 1)

train_data <- train_data %>%
  select(collision_severity, all_of(predictors)) %>%
  mutate(
    collision_severity = factor(collision_severity, levels = class_levels),
    across(all_of(categorical_variables), as.factor)
  )

test_data <- test_data %>%
  select(collision_severity, all_of(predictors)) %>%
  mutate(collision_severity = factor(collision_severity, levels = class_levels))

for (variable in categorical_variables) {
  test_data[[variable]] <- factor(
    test_data[[variable]],
    levels = levels(train_data[[variable]])
  )
}

if (anyNA(test_data[categorical_variables])) {
  stop("Chronological test data contains unseen categorical levels.")
}

set.seed(42)
fold_id <- integer(nrow(train_data))
for (severity in class_levels) {
  severity_rows <- which(train_data$collision_severity == severity)
  fold_id[severity_rows] <- sample(rep(1:3, length.out = length(severity_rows)))
}

calculate_metrics <- function(actual, predicted) {
  confusion <- table(factor(actual, levels = class_levels),
                     factor(predicted, levels = class_levels))
  support <- rowSums(confusion)
  predicted_count <- colSums(confusion)
  true_positive <- diag(confusion)
  precision <- ifelse(predicted_count > 0, true_positive / predicted_count, 0)
  recall <- ifelse(support > 0, true_positive / support, 0)
  f1 <- ifelse(precision + recall > 0,
               2 * precision * recall / (precision + recall), 0)
  tibble(
    accuracy = sum(true_positive) / sum(confusion),
    macro_f1 = mean(f1),
    weighted_f1 = sum(f1 * support) / sum(support),
    balanced_accuracy = mean(recall),
    fatal_precision = precision[1],
    fatal_recall = recall[1],
    fatal_f1 = f1[1],
    serious_precision = precision[2],
    serious_recall = recall[2],
    serious_f1 = f1[2],
    slight_precision = precision[3],
    slight_recall = recall[3],
    slight_f1 = f1[3]
  )
}

tuning_grid <- crossing(
  mtry = c(5, 8, 10),
  min_node_size = c(5, 10, 20, 40)
)

validation_results <- vector("list", nrow(tuning_grid) * 3)
result_index <- 1

for (configuration in seq_len(nrow(tuning_grid))) {
  parameters <- tuning_grid[configuration, ]
  for (fold in 1:3) {
    fold_train <- train_data[fold_id != fold, ]
    fold_validation <- train_data[fold_id == fold, ]
    model <- ranger(
      collision_severity ~ .,
      data = fold_train,
      num.trees = 150,
      mtry = parameters$mtry,
      min.node.size = parameters$min_node_size,
      class.weights = class_weights,
      probability = FALSE,
      seed = 42,
      num.threads = 2
    )
    predictions <- predict(model, data = fold_validation)$predictions
    validation_results[[result_index]] <- calculate_metrics(
      fold_validation$collision_severity, predictions
    ) %>% mutate(
      mtry = parameters$mtry,
      min_node_size = parameters$min_node_size,
      fold = fold
    )
    result_index <- result_index + 1
  }
}

validation_results <- bind_rows(validation_results)
final_tuning_results <- validation_results %>%
  group_by(mtry, min_node_size) %>%
  summarise(across(c(accuracy, macro_f1, weighted_f1, balanced_accuracy,
                    fatal_precision, fatal_recall, fatal_f1,
                    serious_precision, serious_recall, serious_f1,
                    slight_precision, slight_recall, slight_f1), mean),
            .groups = "drop") %>%
  arrange(desc(macro_f1), desc(balanced_accuracy), desc(fatal_recall),
          desc(serious_recall), desc(accuracy)) %>%
  mutate(rank = row_number(), .before = 1)

cat("\n=== FINAL RF HYPERPARAMETER CV RESULTS ===\n")
print(final_tuning_results)
best_parameters <- slice(final_tuning_results, 1)
cat("\n=== SELECTED PARAMETERS ===\n")
print(best_parameters)

current_cv <- final_tuning_results %>%
  filter(mtry == 5, min_node_size == 20) %>%
  mutate(model = "Current RF", .before = 1)
optimized_cv <- best_parameters %>%
  mutate(model = "Optimized RF", .before = 1)
current_vs_optimized <- bind_rows(current_cv, optimized_cv)

set.seed(42)
final_model <- ranger(
  collision_severity ~ .,
  data = train_data,
  num.trees = 500,
  mtry = best_parameters$mtry,
  min.node.size = best_parameters$min_node_size,
  class.weights = class_weights,
  probability = FALSE,
  importance = "impurity",
  seed = 42,
  num.threads = 2
)

final_predictions <- predict(final_model, data = test_data)$predictions
final_confusion_matrix <- table(
  Actual = factor(test_data$collision_severity, levels = class_levels),
  Predicted = factor(final_predictions, levels = class_levels)
)
final_metrics <- calculate_metrics(test_data$collision_severity, final_predictions) %>%
  mutate(model = "Optimized Random Forest", .before = 1)

final_class_metrics <- {
  support <- rowSums(final_confusion_matrix)
  predicted_count <- colSums(final_confusion_matrix)
  true_positive <- diag(final_confusion_matrix)
  precision <- ifelse(predicted_count > 0, true_positive / predicted_count, 0)
  recall <- ifelse(support > 0, true_positive / support, 0)
  f1 <- ifelse(precision + recall > 0,
               2 * precision * recall / (precision + recall), 0)
  tibble(
    model = "Optimized Random Forest",
    class = class_levels,
    support = as.integer(support),
    precision = as.numeric(precision),
    recall = as.numeric(recall),
    f1_score = as.numeric(f1)
  )
}

current_metrics <- read_csv(
  "results/random_forest/test_metrics.csv",
  show_col_types = FALSE
) %>%
  mutate(model = "Current Random Forest", .before = 1)

current_vs_optimized_test <- bind_rows(
  current_metrics,
  final_metrics
)

random_forest_dir <- "results/random_forest"
dir.create(random_forest_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(final_tuning_results,
          file.path(random_forest_dir, "final_tuning_results.csv"))
write_csv(current_vs_optimized,
          file.path(random_forest_dir, "current_vs_optimized_cv.csv"))
write_csv(current_vs_optimized_test,
          file.path(random_forest_dir, "current_vs_optimized.csv"))
write_csv(final_metrics,
          file.path(random_forest_dir, "final_metrics.csv"))
write_csv(final_class_metrics,
          file.path(random_forest_dir, "final_class_metrics.csv"))
write_csv(as.data.frame(final_confusion_matrix),
          file.path(random_forest_dir, "final_confusion_matrix.csv"))

final_feature_importance <- tibble(
  feature = names(final_model$variable.importance),
  importance = as.numeric(final_model$variable.importance)
) %>% arrange(desc(importance))
write_csv(final_feature_importance,
          file.path(random_forest_dir, "final_feature_importance.csv"))

saveRDS(final_model, file.path(random_forest_dir, "final_model.rds"))
saveRDS(final_predictions, file.path(random_forest_dir, "final_predictions.rds"))
saveRDS(list(
  overall_metrics = final_metrics,
  class_metrics = final_class_metrics,
  confusion_matrix = final_confusion_matrix,
  correctly_predicted_fatal = unname(final_confusion_matrix["Fatal", "Fatal"]),
  selected_parameters = best_parameters,
  class_weights = class_weights,
  training_years = 2021:2024,
  test_year = 2025
), file.path(random_forest_dir, "final_evaluation.rds"))

cat("\n=== OPTIMIZED RF 2025 TEST METRICS ===\n")
print(final_metrics)
cat("\n=== OPTIMIZED RF CONFUSION MATRIX ===\n")
print(final_confusion_matrix)
cat("Correctly predicted Fatal accidents:",
    unname(final_confusion_matrix["Fatal", "Fatal"]), "\n")
cat("\nFinal RF experiment outputs saved successfully.\n")
