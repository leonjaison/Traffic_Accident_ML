# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 15b: Chronological Random Forest Feature-Set Experiment
# ============================================================

library(tidyverse)
library(ranger)

train_data <- readRDS("results/data/chronological_train_data.rds")

class_levels <- c("Fatal", "Serious", "Slight")
current_predictors <- c(
  "number_of_vehicles", "number_of_casualties", "speed_limit",
  "road_type", "light_conditions", "weather_conditions",
  "road_surface_conditions", "urban_or_rural_area", "junction_control",
  "accident_hour"
)

additional_predictors <- c(
  "collision_year", "day_of_week", "first_road_class", "second_road_class",
  "junction_detail", "police_force", "trunk_road_flag", "accident_month",
  "accident_day", "special_conditions_at_site", "carriageway_hazards",
  "pedestrian_crossing"
)

feature_sets <- list(
  A = current_predictors,
  B = c(current_predictors, additional_predictors)
)

raw_data <- read_csv(
  "data/UK/dft-road-casualty-statistics-collision-last-5-years.csv",
  show_col_types = FALSE
)

known_leakage <- c(
  "enhanced_severity_collision",
  "collision_injury_based",
  "collision_adjusted_severity_serious",
  "collision_adjusted_severity_slight"
)

candidate_variables <- c(
  "road_class", "junction_detail", "junction_location",
  "pedestrian_crossing_human_control", "pedestrian_crossing_physical_facilities",
  "special_conditions_at_site", "carriageway_hazards", "first_road_class",
  "second_road_class", "first_road_number", "second_road_number",
  "accident_year", "collision_year", "accident_month", "accident_day",
  "accident_hour", "day_of_week", "light_conditions", "weather_conditions",
  "road_surface_conditions", "number_of_vehicles", "number_of_casualties",
  known_leakage
)

feature_audit <- tibble(
  variable = candidate_variables,
  exists = variable %in% names(raw_data),
  description = case_when(
    variable %in% c("number_of_casualties", "number_of_vehicles") ~ "Recorded collision counts",
    variable %in% known_leakage ~ "Severity-derived field",
    variable %in% c("collision_year", "accident_month", "accident_day", "accident_hour", "day_of_week") ~ "Temporal collision characteristic",
    TRUE ~ "Road, junction, environment, or location characteristic"
  ),
  available_at_prediction = case_when(
    variable %in% c("number_of_casualties", "number_of_vehicles") ~ "retrospective_only",
    variable %in% known_leakage ~ "no",
    TRUE ~ "yes_for_recorded_collision"
  ),
  leakage_status = if_else(variable %in% known_leakage, "exclude", "not_target_derived"),
  post_outcome_status = if_else(variable %in% c("number_of_casualties", "number_of_vehicles"), "post_collision_count", "not_identified"),
  missing_or_unknown_rate = map_dbl(variable, function(name) {
    if (!name %in% names(raw_data)) return(NA_real_)
    values <- raw_data[[name]]
    mean(is.na(values) | (is.character(values) & trimws(values) == ""))
  }),
  redundant_status = if_else(variable %in% current_predictors, "already_current", "not_current_predictor"),
  decision = case_when(
    variable %in% known_leakage ~ "exclude",
    variable %in% c("road_class", "junction_location", "pedestrian_crossing_human_control", "pedestrian_crossing_physical_facilities", "accident_year") ~ "exclude_missing_from_dataset",
    variable %in% c("first_road_number", "second_road_number") ~ "exclude_high_cardinality_identifier_like",
    variable %in% c("number_of_casualties", "number_of_vehicles") ~ "retain_current_retrospective",
    variable %in% additional_predictors ~ "include_feature_set_B",
    TRUE ~ "retain_current_or_investigate"
  ),
  reason = case_when(
    variable %in% known_leakage ~ "Directly derived from severity or injury outcome.",
    variable %in% c("road_class", "junction_location", "pedestrian_crossing_human_control", "pedestrian_crossing_physical_facilities", "accident_year") ~ "Column is not present in the supplied DfT file.",
    variable %in% c("first_road_number", "second_road_number") ~ "High-cardinality road identifiers are not needed for this minor project.",
    variable %in% c("number_of_casualties", "number_of_vehicles") ~ "Used under the recorded-collision retrospective framing.",
    variable %in% additional_predictors ~ "Available recorded collision characteristic and not target-derived.",
    TRUE ~ "Current feature or candidate requires no additional action."
  )
)

write_csv(feature_audit, "results/random_forest/feature_audit.csv")

categorical_variables <- c(
  "speed_limit", "road_type", "light_conditions", "weather_conditions",
  "road_surface_conditions", "urban_or_rural_area", "junction_control",
  "day_of_week", "first_road_class", "second_road_class", "junction_detail",
  "police_force", "trunk_road_flag", "accident_month", "accident_day",
  "special_conditions_at_site", "carriageway_hazards", "pedestrian_crossing"
)

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
  f1 <- ifelse(precision + recall > 0, 2 * precision * recall / (precision + recall), 0)
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

weight_values <- c(Fatal = 10, Serious = 3, Slight = 1)
validation_results <- vector("list", length(feature_sets) * 3)
result_index <- 1

for (feature_set_name in names(feature_sets)) {
  predictors <- feature_sets[[feature_set_name]]
  missing_predictors <- setdiff(predictors, names(train_data))
  if (length(missing_predictors) > 0) {
    stop("Missing predictors for feature set ", feature_set_name, ": ", paste(missing_predictors, collapse = ", "))
  }

  feature_data <- train_data %>%
    select(collision_severity, all_of(predictors)) %>%
    mutate(
      collision_severity = factor(collision_severity, levels = class_levels),
      across(any_of(intersect(categorical_variables, predictors)), as.factor)
    )

  for (fold in 1:3) {
    fold_train <- feature_data[fold_id != fold, ]
    fold_validation <- feature_data[fold_id == fold, ]
    model <- ranger(
      collision_severity ~ .,
      data = fold_train,
      num.trees = 150,
      mtry = 5,
      min.node.size = 20,
      class.weights = weight_values,
      probability = FALSE,
      seed = 42,
      num.threads = 2
    )
    predictions <- predict(model, data = fold_validation)$predictions
    validation_results[[result_index]] <- calculate_metrics(
      fold_validation$collision_severity, predictions
    ) %>% mutate(feature_set = feature_set_name, fold = fold,
                 predictors = paste(predictors, collapse = ", "))
    result_index <- result_index + 1
  }
}

feature_set_comparison <- bind_rows(validation_results) %>%
  group_by(feature_set, predictors) %>%
  summarise(across(-fold, mean), .groups = "drop") %>%
  arrange(desc(macro_f1), desc(balanced_accuracy), desc(fatal_recall), desc(accuracy))

cat("\n=== FEATURE SET CV RESULTS ===\n")
print(feature_set_comparison)
write_csv(feature_set_comparison, "results/random_forest/feature_set_comparison.csv")
cat("\nFeature-set experiment outputs saved successfully.\n")
