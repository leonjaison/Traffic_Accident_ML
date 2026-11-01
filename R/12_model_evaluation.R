# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 12: Decision Tree Model Evaluation
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# 1. Load test labels and saved predictions
# ------------------------------------------------------------

test_data <- readRDS(
  "results/data/test_data.rds"
)

baseline_predictions <- readRDS(
  "results/data/tree_predictions.rds"
)

cost_sensitive_predictions <- readRDS(
  "results/data/cost_tree_predictions.rds"
)

class_levels <- c("Fatal", "Serious", "Slight")

actual <- factor(
  test_data$collision_severity,
  levels = class_levels
)

baseline_predictions <- factor(
  baseline_predictions,
  levels = class_levels
)

cost_sensitive_predictions <- factor(
  cost_sensitive_predictions,
  levels = class_levels
)

if (length(actual) != length(baseline_predictions) ||
    length(actual) != length(cost_sensitive_predictions)) {
  stop("Test labels and prediction lengths do not match.")
}

# ------------------------------------------------------------
# 2. Define evaluation function
# ------------------------------------------------------------

evaluate_model <- function(actual, predicted, model_name) {
  confusion_matrix <- table(
    Actual = actual,
    Predicted = predicted
  )

  support <- rowSums(confusion_matrix)
  predicted_count <- colSums(confusion_matrix)
  true_positive <- diag(confusion_matrix)

  precision <- ifelse(
    predicted_count > 0,
    true_positive / predicted_count,
    0
  )

  recall <- ifelse(
    support > 0,
    true_positive / support,
    0
  )

  f1_score <- ifelse(
    precision + recall > 0,
    2 * precision * recall / (precision + recall),
    0
  )

  total <- sum(confusion_matrix)
  accuracy <- sum(true_positive) / total

  class_metrics <- tibble(
    model = model_name,
    class = class_levels,
    support = as.integer(support),
    precision = as.numeric(precision),
    recall = as.numeric(recall),
    f1_score = as.numeric(f1_score)
  )

  overall_metrics <- tibble(
    model = model_name,
    accuracy = accuracy,
    macro_f1 = mean(f1_score),
    weighted_f1 = sum(f1_score * support) / total,
    balanced_accuracy = mean(recall),
    fatal_recall = recall[class_levels == "Fatal"],
    serious_recall = recall[class_levels == "Serious"]
  )

  list(
    confusion_matrix = confusion_matrix,
    class_metrics = class_metrics,
    overall_metrics = overall_metrics
  )
}

# ------------------------------------------------------------
# 3. Evaluate both existing decision trees
# ------------------------------------------------------------

baseline_results <- evaluate_model(
  actual,
  baseline_predictions,
  "Baseline Decision Tree"
)

cost_sensitive_results <- evaluate_model(
  actual,
  cost_sensitive_predictions,
  "Cost-Sensitive Decision Tree"
)

overall_metrics <- bind_rows(
  baseline_results$overall_metrics,
  cost_sensitive_results$overall_metrics
)

class_metrics <- bind_rows(
  baseline_results$class_metrics,
  cost_sensitive_results$class_metrics
)

cat("\n=== OVERALL MODEL METRICS ===\n")
print(
  overall_metrics %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 4)
      )
    )
)

cat("\n=== CLASS-LEVEL METRICS ===\n")
print(
  class_metrics %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 4)
      )
    )
)

cat("\n=== BASELINE CONFUSION MATRIX ===\n")
print(baseline_results$confusion_matrix)

cat("\n=== COST-SENSITIVE CONFUSION MATRIX ===\n")
print(cost_sensitive_results$confusion_matrix)

# ------------------------------------------------------------
# 4. Save evaluation outputs
# ------------------------------------------------------------

dir.create(
  "results/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  overall_metrics,
  "results/tables/decision_tree_overall_metrics.csv"
)

write_csv(
  class_metrics,
  "results/tables/decision_tree_class_metrics.csv"
)

evaluation_results <- list(
  overall_metrics = overall_metrics,
  class_metrics = class_metrics,
  confusion_matrices = list(
    baseline = baseline_results$confusion_matrix,
    cost_sensitive = cost_sensitive_results$confusion_matrix
  )
)

saveRDS(
  evaluation_results,
  "results/data/decision_tree_evaluation.rds"
)

# ------------------------------------------------------------
# 5. Plot confusion matrices
# ------------------------------------------------------------

confusion_plot_data <- bind_rows(
  as.data.frame(baseline_results$confusion_matrix) %>%
    mutate(model = "Baseline Decision Tree"),
  as.data.frame(cost_sensitive_results$confusion_matrix) %>%
    mutate(model = "Cost-Sensitive Decision Tree")
)

dir.create(
  "results/plots",
  recursive = TRUE,
  showWarnings = FALSE
)

confusion_plot <- ggplot(
  confusion_plot_data,
  aes(x = Predicted, y = Actual, fill = Freq)
) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 4) +
  facet_wrap(~ model) +
  scale_fill_viridis_c() +
  labs(
    title = "Decision Tree Confusion Matrices",
    x = "Predicted severity",
    y = "Actual severity",
    fill = "Records"
  ) +
  theme_minimal()

ggsave(
  "results/plots/decision_tree_confusion_matrices.png",
  confusion_plot,
  width = 10,
  height = 5,
  dpi = 300
)

cat("\nDecision tree evaluation outputs saved successfully.\n")