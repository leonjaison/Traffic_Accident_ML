# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 11: Cost-Sensitive Decision Tree
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
# 2. Select predictors
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
# 3. Convert categorical predictors to factors
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
# 4. Define class weights
# ------------------------------------------------------------

class_weights <- c(
  Fatal = 5,
  Serious = 2,
  Slight = 1
)

cat("\n=== CLASS WEIGHTS ===\n")
print(class_weights)


# ------------------------------------------------------------
# 5. Train cost-sensitive Decision Tree
# ------------------------------------------------------------

set.seed(42)

cat("\nTraining cost-sensitive Decision Tree...\n")

cost_tree <- rpart(
  collision_severity ~ .,
  data = tree_train,
  method = "class",
  weights = class_weights[
    as.character(tree_train$collision_severity)
  ],
  control = rpart.control(
    cp = 0.001,
    minsplit = 100,
    maxdepth = 6
  )
)

cat("Cost-sensitive Decision Tree training completed.\n")


# ------------------------------------------------------------
# 6. Model summary
# ------------------------------------------------------------

cat("\n=== COST-SENSITIVE DECISION TREE ===\n")

print(cost_tree)


# ------------------------------------------------------------
# 7. Complexity parameter table
# ------------------------------------------------------------

cat("\n=== COMPLEXITY PARAMETER TABLE ===\n")

printcp(cost_tree)


# ------------------------------------------------------------
# 8. Variables used in tree
# ------------------------------------------------------------

cat("\n=== VARIABLES USED IN TREE ===\n")

print(
  names(cost_tree$variable.importance)
)


# ------------------------------------------------------------
# 9. Plot tree
# ------------------------------------------------------------

dir.create(
  "results/plots",
  recursive = TRUE,
  showWarnings = FALSE
)

png(
  "results/plots/cost_sensitive_decision_tree.png",
  width = 1600,
  height = 1200,
  res = 150
)

rpart.plot(
  cost_tree,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  box.palette = "Blues",
  main = "Cost-Sensitive Decision Tree for Collision Severity"
)

dev.off()


# ------------------------------------------------------------
# 10. Generate predictions
# ------------------------------------------------------------

cat("\nGenerating test predictions...\n")

cost_tree_predictions <- predict(
  cost_tree,
  newdata = tree_test,
  type = "class"
)


# ------------------------------------------------------------
# 11. Confusion matrix
# ------------------------------------------------------------

confusion_matrix <- table(
  Actual = tree_test$collision_severity,
  Predicted = cost_tree_predictions
)

cat("\n=== CONFUSION MATRIX ===\n")

print(confusion_matrix)


# ------------------------------------------------------------
# 12. Overall accuracy
# ------------------------------------------------------------

accuracy <- mean(
  cost_tree_predictions == tree_test$collision_severity
)

cat(
  "\nCost-Sensitive Decision Tree Accuracy:",
  round(accuracy * 100, 2),
  "%\n"
)


# ------------------------------------------------------------
# 13. Save model and predictions
# ------------------------------------------------------------

saveRDS(
  cost_tree,
  "results/data/cost_sensitive_tree.rds"
)

saveRDS(
  cost_tree_predictions,
  "results/data/cost_tree_predictions.rds"
)

cat("\nCost-sensitive Decision Tree saved successfully.\n")