# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 14: Evaluate Tuned Decision Tree
# ============================================================

library(tidyverse)

train_data <- readRDS("results/data/chronological_train_data.rds")
test_data <- readRDS("results/data/chronological_test_data.rds")

tree_predictors <- c(
  "number_of_vehicles", "number_of_casualties", "speed_limit",
  "road_type", "light_conditions", "weather_conditions",
  "road_surface_conditions", "urban_or_rural_area", "junction_control",
  "accident_hour"
)

categorical_variables <- c(
  "speed_limit", "road_type", "light_conditions", "weather_conditions",
  "road_surface_conditions", "urban_or_rural_area", "junction_control"
)

tree_train <- train_data %>%
  select(collision_severity, all_of(tree_predictors)) %>%
  mutate(across(all_of(categorical_variables), as.factor))

tree_test <- test_data %>%
  select(collision_severity, all_of(tree_predictors)) %>%
  mutate(across(all_of(categorical_variables), as.factor))

for (variable in categorical_variables) {
  tree_test[[variable]] <- factor(
    tree_test[[variable]],
    levels = levels(tree_train[[variable]])
  )
}

class_levels <- c("Fatal", "Serious", "Slight")

tuned_decision_tree <- readRDS("results/data/tuned_decision_tree.rds")

predictions <- predict(tuned_decision_tree, tree_test, type = "class")
confusion_matrix <- table(
  Actual = factor(tree_test$collision_severity, levels = class_levels),
  Predicted = factor(predictions, levels = class_levels)
)

support <- rowSums(confusion_matrix)
predicted_count <- colSums(confusion_matrix)
true_positive <- diag(confusion_matrix)
precision <- ifelse(predicted_count > 0, true_positive / predicted_count, 0)
recall <- ifelse(support > 0, true_positive / support, 0)
f1_score <- ifelse(precision + recall > 0,
                   2 * precision * recall / (precision + recall), 0)

overall_metrics <- tibble(
  model = "Tuned Decision Tree",
  accuracy = sum(true_positive) / sum(confusion_matrix),
  macro_f1 = mean(f1_score),
  weighted_f1 = sum(f1_score * support) / sum(support),
  balanced_accuracy = mean(recall),
  fatal_recall = recall[class_levels == "Fatal"],
  serious_recall = recall[class_levels == "Serious"]
)

class_metrics <- tibble(
  model = "Tuned Decision Tree",
  class = class_levels,
  support = as.integer(support),
  precision = as.numeric(precision),
  recall = as.numeric(recall),
  f1_score = as.numeric(f1_score)
)

cat("\n=== TUNED TREE TEST METRICS ===\n")
print(overall_metrics)
print(class_metrics)
cat("\n=== TUNED TREE CONFUSION MATRIX ===\n")
print(confusion_matrix)

saveRDS(tuned_decision_tree, "results/data/tuned_decision_tree.rds")
saveRDS(predictions, "results/data/tuned_tree_predictions.rds")
saveRDS(list(overall_metrics = overall_metrics,
             class_metrics = class_metrics,
             confusion_matrix = confusion_matrix),
        "results/data/tuned_tree_evaluation.rds")

write_csv(overall_metrics, "results/tables/tuned_tree_overall_metrics.csv")
write_csv(class_metrics, "results/tables/tuned_tree_class_metrics.csv")
cat("\nTuned tree evaluation outputs saved successfully.\n")