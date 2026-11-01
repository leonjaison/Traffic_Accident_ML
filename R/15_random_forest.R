# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 15: Random Forest with Training-Only Tuning
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
  matrix <- table(factor(actual, levels = class_levels),
                  factor(predicted, levels = class_levels))
  support <- rowSums(matrix)
  predicted_count <- colSums(matrix)
  true_positive <- diag(matrix)
  precision <- ifelse(predicted_count > 0, true_positive / predicted_count, 0)
  recall <- ifelse(support > 0, true_positive / support, 0)
  f1_score <- ifelse(precision + recall > 0,
                     2 * precision * recall / (precision + recall), 0)
  tibble(
    accuracy = sum(true_positive) / sum(matrix),
    macro_f1 = mean(f1_score),
    weighted_f1 = sum(f1_score * support) / sum(support),
    balanced_accuracy = mean(recall),
    fatal_recall = recall[1],
    serious_recall = recall[2]
  )
}

# Compare only the requested class-weight configurations while holding the
# previously selected forest settings constant.
tuning_grid <- tibble(
  weight_set = c("A", "B", "C", "D"),
  fatal_weight = c(5, 10, 15, 20),
  serious_weight = c(2, 3, 4, 5),
  slight_weight = 1,
  mtry = 5,
  min_node_size = 20
)

weight_lookup <- list(
  A = c(Fatal = 5, Serious = 2, Slight = 1),
  B = c(Fatal = 10, Serious = 3, Slight = 1),
  C = c(Fatal = 15, Serious = 4, Slight = 1),
  D = c(Fatal = 20, Serious = 5, Slight = 1)
)

validation_results <- vector("list", nrow(tuning_grid) * 3)
result_index <- 1

for (configuration in seq_len(nrow(tuning_grid))) {
  parameters <- tuning_grid[configuration, ]
  for (fold in 1:3) {
    fold_train <- train_data[fold_id != fold, ]
    fold_validation <- train_data[fold_id == fold, ]
    class_weights <- weight_lookup[[parameters$weight_set]]
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
    ) %>% mutate(mtry = parameters$mtry,
                 min_node_size = parameters$min_node_size,
                 weight_set = parameters$weight_set,
                 fatal_weight = parameters$fatal_weight,
                 serious_weight = parameters$serious_weight,
                 slight_weight = parameters$slight_weight,
                 fold = fold)
    result_index <- result_index + 1
  }
}

validation_results <- bind_rows(validation_results)
mean_validation_results <- validation_results %>%
  group_by(weight_set, fatal_weight, serious_weight, slight_weight,
           mtry, min_node_size) %>%
  summarise(across(c(accuracy, macro_f1, weighted_f1, balanced_accuracy,
                     fatal_recall, serious_recall), mean), .groups = "drop") %>%
          arrange(desc(macro_f1), desc(balanced_accuracy), desc(fatal_recall),
            desc(accuracy))

cat("\n=== RANDOM FOREST VALIDATION RESULTS ===\n")
print(mean_validation_results)
best_parameters <- slice(mean_validation_results, 1)
print(best_parameters)
class_weights <- weight_lookup[[best_parameters$weight_set]]

class_weight_comparison <- mean_validation_results %>%
  select(weight_set, fatal_weight, serious_weight, slight_weight,
         macro_f1, balanced_accuracy, fatal_recall, serious_recall,
         accuracy, weighted_f1)

set.seed(42)
random_forest <- ranger(
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

predictions <- predict(random_forest, data = test_data)$predictions
test_metrics <- calculate_metrics(test_data$collision_severity, predictions)
test_metrics <- test_metrics %>% mutate(model = "Random Forest", .before = 1)

class_metrics <- {
  evaluation_matrix <- table(
    factor(test_data$collision_severity, levels = class_levels),
    factor(predictions, levels = class_levels)
  )
  support <- rowSums(evaluation_matrix)
  predicted_count <- colSums(evaluation_matrix)
  true_positive <- diag(evaluation_matrix)
  precision <- ifelse(predicted_count > 0, true_positive / predicted_count, 0)
  recall <- ifelse(support > 0, true_positive / support, 0)
  f1_score <- ifelse(precision + recall > 0,
                     2 * precision * recall / (precision + recall), 0)
  tibble(
    model = "Random Forest",
    class = class_levels,
    support = as.integer(support),
    precision = as.numeric(precision),
    recall = as.numeric(recall),
    f1_score = as.numeric(f1_score)
  )
}

cat("\n=== RANDOM FOREST TEST METRICS ===\n")
print(test_metrics)

confusion_matrix <- table(
  Actual = factor(test_data$collision_severity, levels = class_levels),
  Predicted = factor(predictions, levels = class_levels)
)
print(confusion_matrix)
fatal_correct <- unname(confusion_matrix["Fatal", "Fatal"])
cat("Correctly predicted Fatal accidents:", fatal_correct, "\n")

random_forest_dir <- "results/random_forest"
dir.create(random_forest_dir, recursive = TRUE, showWarnings = FALSE)

legacy_random_forest_files <- c(
  "results/data/random_forest.rds",
  "results/data/random_forest_predictions.rds",
  "results/data/random_forest_evaluation.rds",
  "results/tables/random_forest_tuning_fold_results.csv",
  "results/tables/random_forest_tuning_results.csv",
  "results/tables/random_forest_test_metrics.csv",
  "results/tables/random_forest_feature_importance.csv"
)
unlink(legacy_random_forest_files[file.exists(legacy_random_forest_files)])

saveRDS(random_forest, file.path(random_forest_dir, "model.rds"))
saveRDS(predictions, file.path(random_forest_dir, "predictions.rds"))
saveRDS(list(overall_metrics = test_metrics,
             class_metrics = class_metrics,
             confusion_matrix = confusion_matrix,
             fatal_correct = fatal_correct,
             selected_parameters = best_parameters,
             class_weights = class_weights),
  file.path(random_forest_dir, "evaluation.rds"))
write_csv(validation_results,
    file.path(random_forest_dir, "tuning_fold_results.csv"))
write_csv(mean_validation_results,
    file.path(random_forest_dir, "tuning_results.csv"))
write_csv(class_weight_comparison,
  file.path(random_forest_dir, "class_weight_comparison.csv"))
write_csv(test_metrics,
    file.path(random_forest_dir, "test_metrics.csv"))
write_csv(class_metrics,
  file.path(random_forest_dir, "class_metrics.csv"))

importance_data <- tibble(
  feature = names(random_forest$variable.importance),
  importance = as.numeric(random_forest$variable.importance)
) %>% arrange(desc(importance))
write_csv(importance_data,
          file.path(random_forest_dir, "feature_importance.csv"))

cat("\nRandom Forest outputs saved successfully.\n")