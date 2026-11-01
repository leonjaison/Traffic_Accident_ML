# ============================================================
# Traffic Accident Severity Prediction & Hotspot Detection
# Step 6: Exploratory Data Analysis
# ============================================================

library(tidyverse)
library(lubridate)

# ------------------------------------------------------------
# 1. Load dataset
# ------------------------------------------------------------

uk_accidents <- read_csv(
  "data/UK/dft-road-casualty-statistics-collision-last-5-years.csv",
  show_col_types = FALSE
)

# ------------------------------------------------------------
# 2. Prepare basic variables for EDA
# ------------------------------------------------------------

uk_accidents <- uk_accidents %>%
  mutate(
    collision_severity = factor(
      collision_severity,
      levels = c(1, 2, 3),
      labels = c("Fatal", "Serious", "Slight")
    ),
    date = dmy(date),
    accident_year = year(date),
    accident_month = month(date),
    accident_hour = hour(time)
  )

# ------------------------------------------------------------
# 3. Overall severity distribution
# ------------------------------------------------------------

severity_counts <- uk_accidents %>%
  count(collision_severity)

print(severity_counts)

ggplot(severity_counts,
       aes(x = collision_severity, y = n)) +
  geom_col() +
  labs(
    title = "Distribution of Collision Severity",
    x = "Collision Severity",
    y = "Number of Collisions"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 4. Accidents by year
# ------------------------------------------------------------

year_counts <- uk_accidents %>%
  count(accident_year)

print(year_counts)

ggplot(year_counts,
       aes(x = accident_year, y = n)) +
  geom_col() +
  labs(
    title = "Number of Collisions by Year",
    x = "Year",
    y = "Number of Collisions"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 5. Severity by year
# ------------------------------------------------------------

year_severity <- uk_accidents %>%
  count(accident_year, collision_severity)

print(year_severity)

ggplot(year_severity,
       aes(x = accident_year, y = n, fill = collision_severity)) +
  geom_col() +
  labs(
    title = "Collision Severity by Year",
    x = "Year",
    y = "Number of Collisions",
    fill = "Severity"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 6. Severity by speed limit
# ------------------------------------------------------------

speed_severity <- uk_accidents %>%
  filter(speed_limit != -1) %>%
  count(speed_limit, collision_severity)

ggplot(speed_severity,
       aes(x = factor(speed_limit),
           y = n,
           fill = collision_severity)) +
  geom_col() +
  labs(
    title = "Collision Severity by Speed Limit",
    x = "Speed Limit (mph)",
    y = "Number of Collisions",
    fill = "Severity"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 7. Severity by road type
# ------------------------------------------------------------

road_severity <- uk_accidents %>%
  count(road_type, collision_severity)

ggplot(road_severity,
       aes(x = factor(road_type),
           y = n,
           fill = collision_severity)) +
  geom_col() +
  labs(
    title = "Collision Severity by Road Type",
    x = "Road Type Code",
    y = "Number of Collisions",
    fill = "Severity"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 8. Severity by light conditions
# ------------------------------------------------------------

light_severity <- uk_accidents %>%
  count(light_conditions, collision_severity)

ggplot(light_severity,
       aes(x = factor(light_conditions),
           y = n,
           fill = collision_severity)) +
  geom_col() +
  labs(
    title = "Collision Severity by Light Conditions",
    x = "Light Condition Code",
    y = "Number of Collisions",
    fill = "Severity"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 9. Severity by weather conditions
# ------------------------------------------------------------

weather_severity <- uk_accidents %>%
  count(weather_conditions, collision_severity)

ggplot(weather_severity,
       aes(x = factor(weather_conditions),
           y = n,
           fill = collision_severity)) +
  geom_col() +
  labs(
    title = "Collision Severity by Weather Conditions",
    x = "Weather Condition Code",
    y = "Number of Collisions",
    fill = "Severity"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 10. Severity by road surface conditions
# ------------------------------------------------------------

surface_severity <- uk_accidents %>%
  count(road_surface_conditions, collision_severity)

ggplot(surface_severity,
       aes(x = factor(road_surface_conditions),
           y = n,
           fill = collision_severity)) +
  geom_col() +
  labs(
    title = "Collision Severity by Road Surface Conditions",
    x = "Road Surface Condition Code",
    y = "Number of Collisions",
    fill = "Severity"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 11. Accidents by hour
# ------------------------------------------------------------

hour_counts <- uk_accidents %>%
  count(accident_hour)

ggplot(hour_counts,
       aes(x = accident_hour, y = n)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Collisions by Hour of Day",
    x = "Hour",
    y = "Number of Collisions"
  ) +
  theme_minimal()
  # ============================================================
# 12. PROPORTION-BASED EDA
# ============================================================

# ------------------------------------------------------------
# Severity percentage by speed limit
# ------------------------------------------------------------

speed_proportion <- uk_accidents %>%
  filter(speed_limit != -1) %>%
  count(speed_limit, collision_severity) %>%
  group_by(speed_limit) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup()

print(speed_proportion)

ggplot(
  speed_proportion,
  aes(
    x = factor(speed_limit),
    y = percentage,
    fill = collision_severity
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Collision Severity by Speed Limit",
    x = "Speed Limit (mph)",
    y = "Severity Proportion",
    fill = "Severity"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# Severity percentage by road type
# ------------------------------------------------------------

road_proportion <- uk_accidents %>%
  count(road_type, collision_severity) %>%
  group_by(road_type) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup()

print(road_proportion)

ggplot(
  road_proportion,
  aes(
    x = factor(road_type),
    y = percentage,
    fill = collision_severity
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Collision Severity by Road Type",
    x = "Road Type Code",
    y = "Severity Proportion",
    fill = "Severity"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# Severity percentage by weather conditions
# ------------------------------------------------------------

weather_proportion <- uk_accidents %>%
  count(weather_conditions, collision_severity) %>%
  group_by(weather_conditions) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup()

print(weather_proportion)

ggplot(
  weather_proportion,
  aes(
    x = factor(weather_conditions),
    y = percentage,
    fill = collision_severity
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Collision Severity by Weather",
    x = "Weather Condition Code",
    y = "Severity Proportion",
    fill = "Severity"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# Severity percentage by road surface
# ------------------------------------------------------------

surface_proportion <- uk_accidents %>%
  count(road_surface_conditions, collision_severity) %>%
  group_by(road_surface_conditions) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup()

print(surface_proportion)

ggplot(
  surface_proportion,
  aes(
    x = factor(road_surface_conditions),
    y = percentage,
    fill = collision_severity
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Collision Severity by Road Surface",
    x = "Road Surface Condition Code",
    y = "Severity Proportion",
    fill = "Severity"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# Severity percentage by light conditions
# ------------------------------------------------------------

light_proportion <- uk_accidents %>%
  count(light_conditions, collision_severity) %>%
  group_by(light_conditions) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup()

print(light_proportion)

ggplot(
  light_proportion,
  aes(
    x = factor(light_conditions),
    y = percentage,
    fill = collision_severity
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Collision Severity by Light Conditions",
    x = "Light Condition Code",
    y = "Severity Proportion",
    fill = "Severity"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# Severity percentage by urban/rural area
# ------------------------------------------------------------

urban_proportion <- uk_accidents %>%
  count(urban_or_rural_area, collision_severity) %>%
  group_by(urban_or_rural_area) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup()

print(urban_proportion)

ggplot(
  urban_proportion,
  aes(
    x = factor(urban_or_rural_area),
    y = percentage,
    fill = collision_severity
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Collision Severity by Urban/Rural Area",
    x = "Area Type Code",
    y = "Severity Proportion",
    fill = "Severity"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# Severity percentage by hour
# ------------------------------------------------------------

hour_proportion <- uk_accidents %>%
  count(accident_hour, collision_severity) %>%
  group_by(accident_hour) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup()

print(hour_proportion)

ggplot(
  hour_proportion,
  aes(
    x = accident_hour,
    y = percentage,
    color = collision_severity
  )
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Collision Severity Proportion by Hour",
    x = "Hour of Day",
    y = "Severity Proportion",
    color = "Severity"
  ) +
  theme_minimal()