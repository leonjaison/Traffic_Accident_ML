# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 9b: Chronological Train / Test Split
# ============================================================

library(tidyverse)

model_data <- readRDS("results/data/model_data.rds")
required_years <- 2021:2025
available_years <- sort(unique(model_data$collision_year))

cat("Available collision years:\n")
print(available_years)

if (!all(required_years %in% available_years)) {
  stop("Expected collision years 2021-2025 were not all found.")
}

train_data <- model_data %>%
  filter(collision_year <= 2024)

test_data <- model_data %>%
  filter(collision_year == 2025)

if (nrow(train_data) == 0 || nrow(test_data) == 0) {
  stop("Chronological split produced an empty training or test set.")
}

summarise_split <- function(data, split_name) {
  data %>%
    count(collision_severity, name = "n") %>%
    complete(
      collision_severity = factor(
        c("Fatal", "Serious", "Slight"),
        levels = c("Fatal", "Serious", "Slight")
      ),
      fill = list(n = 0)
    ) %>%
    mutate(
      split = split_name,
      percentage = n / sum(n) * 100,
      .before = 1
    )
}

split_summary <- bind_rows(
  summarise_split(train_data, "train_2021_2024"),
  summarise_split(test_data, "test_2025")
)

cat("\n=== CHRONOLOGICAL SPLIT ===\n")
cat("Total observations:", nrow(model_data), "\n")
cat("Training observations:", nrow(train_data), "\n")
cat("Test observations:", nrow(test_data), "\n")
cat("Training years:", paste(sort(unique(train_data$collision_year)), collapse = ", "), "\n")
cat("Test years:", paste(sort(unique(test_data$collision_year)), collapse = ", "), "\n")
cat("\nSeverity summary:\n")
print(split_summary)

dir.create("results/data", recursive = TRUE, showWarnings = FALSE)
saveRDS(train_data, "results/data/chronological_train_data.rds")
saveRDS(test_data, "results/data/chronological_test_data.rds")
write_csv(split_summary, "results/data/chronological_split_summary.csv")

cat("\nChronological split saved successfully.\n")
