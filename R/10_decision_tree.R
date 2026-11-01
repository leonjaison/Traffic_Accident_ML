# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 10: Decision Tree - Diagnostic Baseline
# ============================================================

library(tidyverse)
library(rpart)
library(rpart.plot)

# ------------------------------------------------------------
# 1. Load train/test datasets
# ------------------------------------------------------------

train_data <- readRDS(
  "results/data/chronological_train_data.rds"
)

test_data <- readRDS(
  "results/data/chronological_test_data.rds"
)

cat("Training records:", nrow(train_data), "\n")
cat("Testing records:", nrow(test_data), "\n")


# ------------------------------------------------------------
# 2. Select core predictors
# ------------------------------------------------------------

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

tree_train <- train_data %>%
  select(
    collision_severity,
    all_of(tree_predictors)
  )

tree_test <- test_data %>%
  select(
    collision_severity,
    all_of(tree_predictors)
  )


# ------------------------------------------------------------
# 3. Convert categorical variables to factors
# ------------------------------------------------------------

categorical_variables <- c(
  "speed_limit",
  "road_type",
  "light_conditions",
  "weather_conditions",
  "road_surface_conditions",
  "urban_or_rural_area",
  "junction_control"
)

tree_train <- tree_train %>%
  mutate(
    across(
      all_of(categorical_variables),
      as.factor
    )
  )

tree_test <- tree_test %>%
  mutate(
    across(all_of(categorical_variables), as.factor)
  )

for (variable in categorical_variables) {
  tree_test[[variable]] <- factor(
    tree_test[[variable]],
    levels = levels(tree_train[[variable]])
  )
}


# ------------------------------------------------------------
# 4. Train Decision Tree
# ------------------------------------------------------------

set.seed(42)

cat("\nTraining Decision Tree...\n")

decision_tree <- rpart(
  collision_severity ~ .,
  data = tree_train,
  method = "class",
  control = rpart.control(
    cp = 0.001,
    minsplit = 100,
    maxdepth = 6
  )
)

cat("Decision Tree training completed.\n")


# ------------------------------------------------------------
# 5. Display model
# ------------------------------------------------------------

cat("\n=== DECISION TREE MODEL ===\n")

print(decision_tree)


# ------------------------------------------------------------
# 6. Complexity parameter table
# ------------------------------------------------------------

cat("\n=== COMPLEXITY PARAMETER TABLE ===\n")

printcp(decision_tree)


# ------------------------------------------------------------
# 7. Variables used
# ------------------------------------------------------------

cat("\n=== VARIABLES USED IN TREE ===\n")

print(
  names(decision_tree$variable.importance)
)


# ------------------------------------------------------------
# 8. Plot tree
# ------------------------------------------------------------

dir.create(
  "results/plots",
  recursive = TRUE,
  showWarnings = FALSE
)

png(
  "results/plots/decision_tree.png",
  width = 1600,
  height = 1200,
  res = 150
)

rpart.plot(
  decision_tree,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  box.palette = "Blues",
  main = "Decision Tree for Collision Severity"
)

dev.off()


# ------------------------------------------------------------
# 9. Predict test set
# ------------------------------------------------------------

cat("\nGenerating test predictions...\n")

tree_predictions <- predict(
  decision_tree,
  newdata = tree_test,
  type = "class"
)


# ------------------------------------------------------------
# 10. Confusion matrix
# ------------------------------------------------------------

confusion_matrix <- table(
  Actual = tree_test$collision_severity,
  Predicted = tree_predictions
)

cat("\n=== CONFUSION MATRIX ===\n")

print(confusion_matrix)


# ------------------------------------------------------------
# 11. Accuracy
# ------------------------------------------------------------

accuracy <- mean(
  tree_predictions == tree_test$collision_severity
)

cat(
  "\nDecision Tree Accuracy:",
  round(accuracy * 100, 2),
  "%\n"
)


# ------------------------------------------------------------
# 12. Save model and predictions
# ------------------------------------------------------------

saveRDS(
  decision_tree,
  "results/data/decision_tree.rds"
)

saveRDS(
  tree_predictions,
  "results/data/tree_predictions.rds"
)

cat("\nDecision Tree model saved successfully.\n")