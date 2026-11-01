# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 16: Final Decision Tree / Random Forest Comparison
# ============================================================

library(tidyverse)

test_data <- readRDS("results/data/chronological_test_data.rds")
predictions <- list(
  "Baseline Decision Tree" = readRDS("results/data/tree_predictions.rds"),
  "Cost-Sensitive Decision Tree" = readRDS("results/data/cost_tree_predictions.rds"),
  "Tuned Decision Tree" = readRDS("results/data/tuned_tree_predictions.rds"),
  "Random Forest" = readRDS("results/random_forest/predictions.rds")
)
class_levels <- c("Fatal", "Serious", "Slight")
actual <- factor(test_data$collision_severity, levels = class_levels)

evaluate <- function(actual, predicted, model) {
  matrix <- table(actual, factor(predicted, levels = class_levels))
  support <- rowSums(matrix)
  predicted_count <- colSums(matrix)
  true_positive <- diag(matrix)
  precision <- ifelse(predicted_count > 0, true_positive / predicted_count, 0)
  recall <- ifelse(support > 0, true_positive / support, 0)
  f1 <- ifelse(precision + recall > 0, 2 * precision * recall / (precision + recall), 0)
  list(
    overall = tibble(model = model,
                     accuracy = sum(true_positive) / sum(matrix),
                     macro_f1 = mean(f1),
                     weighted_f1 = sum(f1 * support) / sum(support),
                     balanced_accuracy = mean(recall),
                     fatal_recall = recall[1],
                     serious_recall = recall[2]),
    classes = tibble(model = model, class = class_levels,
                     support = as.integer(support),
                     precision = as.numeric(precision),
                     recall = as.numeric(recall),
                     f1_score = as.numeric(f1)),
    confusion_matrix = matrix
  )
}

evaluations <- imap(predictions, ~ evaluate(actual, .x, .y))
overall_metrics <- map_dfr(evaluations, "overall")
class_metrics <- map_dfr(evaluations, "classes")

cat("\n=== FINAL MODEL COMPARISON ===\n")
print(overall_metrics %>% arrange(desc(macro_f1)))
cat("\n=== CLASS METRICS ===\n")
print(class_metrics)

dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write_csv(overall_metrics, "results/tables/final_model_comparison.csv")
write_csv(class_metrics, "results/tables/final_model_class_metrics.csv")
saveRDS(list(overall_metrics = overall_metrics,
             class_metrics = class_metrics,
             confusion_matrices = map(evaluations, "confusion_matrix")),
        "results/data/final_model_comparison.rds")

comparison_plot_data <- overall_metrics %>%
  select(model, accuracy, macro_f1, weighted_f1, balanced_accuracy) %>%
  pivot_longer(-model, names_to = "metric", values_to = "value")

comparison_plot <- ggplot(comparison_plot_data,
                          aes(x = reorder(model, value), y = value, fill = metric)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Model Performance Comparison", x = NULL, y = "Score", fill = "Metric") +
  theme_minimal()

ggsave("results/plots/final_model_comparison.png", comparison_plot,
       width = 10, height = 6, dpi = 300)

cat("\nFinal model comparison saved successfully.\n")