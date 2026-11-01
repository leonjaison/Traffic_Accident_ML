# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 9: Train / Test Split
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# 1. Load final modelling dataset
# ------------------------------------------------------------

model_data <- readRDS(
  "results/data/model_data.rds"
)

cat("Total records:", nrow(model_data), "\n")


# ------------------------------------------------------------
# 2. Set seed for reproducibility
# ------------------------------------------------------------

set.seed(42)

# ------------------------------------------------------------
# 3. Stratified train/test split
# ------------------------------------------------------------

set.seed(42)

model_data <- model_data %>%
  mutate(row_id = row_number())

train_data <- model_data %>%
  group_by(collision_severity) %>%
  slice_sample(prop = 0.80) %>%
  ungroup()

test_data <- model_data %>%
  filter(!row_id %in% train_data$row_id)

# Remove helper ID
train_data <- train_data %>%
  select(-row_id)

test_data <- test_data %>%
  select(-row_id)
# ------------------------------------------------------------
# 4. Check dataset sizes
# ------------------------------------------------------------

cat("\n=== DATASET SIZES ===\n")

cat("Training records:", nrow(train_data), "\n")
cat("Testing records:", nrow(test_data), "\n")


# ------------------------------------------------------------
# 5. Check severity distribution
# ------------------------------------------------------------

cat("\n=== TRAINING SEVERITY DISTRIBUTION ===\n")

train_distribution <- train_data %>%
  count(collision_severity) %>%
  mutate(
    percentage = n / sum(n) * 100
  )

print(train_distribution)


cat("\n=== TESTING SEVERITY DISTRIBUTION ===\n")

test_distribution <- test_data %>%
  count(collision_severity) %>%
  mutate(
    percentage = n / sum(n) * 100
  )

print(test_distribution)


# ------------------------------------------------------------
# 6. Save train/test datasets
# ------------------------------------------------------------

saveRDS(
  train_data,
  "results/data/train_data.rds"
)

saveRDS(
  test_data,
  "results/data/test_data.rds"
)

cat("\nTrain/test datasets saved successfully.\n")