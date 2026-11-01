library(ranger)
library(caret)

print("Loading model and test data...")
rf_model <- readRDS("results/random_forest/model.rds")
test_data <- readRDS("results/data/chronological_test_data.rds")

print("Generating predictions...")
predictions <- predict(rf_model, data = test_data)$predictions

print("--- CONFUSION MATRIX & METRICS ---")
cm <- confusionMatrix(predictions, test_data$accident_severity)
print(cm)