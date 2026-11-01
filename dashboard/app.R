# ============================================================

# Traffic Accident Severity Prediction Dashboard

# UK Department for Transport Road Safety Data, 2021-2025

# ============================================================

library(shiny)

library(ggplot2)

library(leaflet)
library(ranger)

# ------------------------------------------------------------

# 1. PROJECT PATHS

# ------------------------------------------------------------

current_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

project_root <- if (tolower(basename(current_dir)) == "dashboard") {

   normalizePath(file.path(current_dir, ".."), winslash = "/", mustWork = TRUE)

} else {

   current_dir

}

data_file <- file.path(

   project_root, "data", "UK",

   "dft-road-casualty-statistics-collision-last-5-years.csv"

)

comparison_file <- file.path(

   project_root, "results", "tables", "final_model_comparison.csv"

)

class_metrics_file <- file.path(

   project_root, "results", "tables", "final_model_class_metrics.csv"

)

dbscan_file <- file.path(

   project_root, "results", "hotspots", "uk_dbscan_assignments.csv"

)

model_file <- file.path(

   project_root, "results", "random_forest", "model.rds"

)

required_files <- c(

   data_file, comparison_file, class_metrics_file, dbscan_file, model_file

)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {

   stop(

      "Missing required files:\n",

      paste(missing_files, collapse = "\n")

   )

}

# ------------------------------------------------------------

# 2. LOAD AND VALIDATE DATA

# ------------------------------------------------------------

accident_data <- read.csv(

   data_file,

   stringsAsFactors = FALSE,

   check.names = FALSE

)

comparison <- read.csv(

   comparison_file,

   stringsAsFactors = FALSE,

   check.names = FALSE

)

class_metrics <- read.csv(

   class_metrics_file,

   stringsAsFactors = FALSE,

   check.names = FALSE

)

dbscan_data <- read.csv(

   dbscan_file,

   stringsAsFactors = FALSE,

   check.names = FALSE

)


# Load the locked Random Forest for individual predictions; no retraining occurs.
if (!requireNamespace("ranger", quietly = TRUE)) {
  stop("Package 'ranger' is required for the Severity Prediction tab.")
}
severity_model <- readRDS(model_file)
if (!inherits(severity_model, "ranger")) stop("The saved model is not a ranger model.")
model_features <- severity_model$forest$independent.variable.names
expected_features <- c(
  "number_of_vehicles", "number_of_casualties", "speed_limit", "road_type",
  "light_conditions", "weather_conditions", "road_surface_conditions",
  "urban_or_rural_area", "junction_control", "accident_hour"
)
if (!identical(model_features, expected_features)) {
  stop("The saved Random Forest feature list does not match the locked feature set.")
}
model_levels <- severity_model$forest$covariate.levels

# Prefer common, interpretable default codes rather than unknown (-1) when available.
default_model_level <- function(feature, preferred) {
  available <- model_levels[[feature]]
  if (preferred %in% available) preferred else available[1]
}

# Human-readable dropdown labels. The names remain the exact dataset codes,
# so the saved Random Forest receives the same values it was trained on.
model_choice_labels <- function(feature) {
  codes <- as.character(model_levels[[feature]])
  mappings <- list(
    speed_limit = c("-1" = "Unknown / not recorded (-1)", "20" = "20 mph", "30" = "30 mph", "40" = "40 mph", "50" = "50 mph", "60" = "60 mph", "70" = "70 mph"),
    road_type = c("-1" = "Unknown / not recorded (-1)", "1" = "Roundabout (1)", "2" = "One-way street (2)", "3" = "Dual carriageway (3)", "6" = "Single carriageway (6)", "7" = "Slip road (7)", "9" = "Unknown (9)"),
    light_conditions = c("-1" = "Unknown / not recorded (-1)", "1" = "Daylight (1)", "4" = "Darkness — street lights present and lit (4)", "5" = "Darkness — street lights present but unlit (5)", "6" = "Darkness — no street lighting (6)", "7" = "Darkness — street lighting unknown (7)"),
    weather_conditions = c("-1" = "Unknown / not recorded (-1)", "1" = "Fine, no high winds (1)", "2" = "Raining, no high winds (2)", "3" = "Snowing, no high winds (3)", "4" = "Fine, with high winds (4)", "5" = "Raining, with high winds (5)", "6" = "Snowing, with high winds (6)", "7" = "Fog or mist (7)", "8" = "Other weather (8)", "9" = "Unknown (9)"),
    road_surface_conditions = c("-1" = "Unknown / not recorded (-1)", "1" = "Dry (1)", "2" = "Wet or damp (2)", "3" = "Snow (3)", "4" = "Frost or ice (4)", "5" = "Flood over 3 cm deep (5)", "6" = "Oil or diesel (6)", "7" = "Mud (7)"),
    urban_or_rural_area = c("-1" = "Unknown / not recorded (-1)", "1" = "Urban (1)", "2" = "Rural (2)", "3" = "Unallocated (3)"),
    junction_control = c("-1" = "Unknown / not recorded (-1)", "1" = "Authorised person (1)", "2" = "Automatic traffic signal (2)", "3" = "Stop sign (3)", "4" = "Give way or uncontrolled (4)" )
  )
  map <- mappings[[feature]]
  labels <- vapply(codes, function(code) {
    if (!is.null(map) && code %in% names(map)) unname(map[[code]])
    else paste0("Other / unclassified (code ", code, ")")
  }, character(1))
  stats::setNames(codes, labels)
}
required_data_columns <- c(

   "collision_index", "collision_year",

   "collision_severity", "latitude", "longitude"

)

if (!all(required_data_columns %in% names(accident_data))) {

   stop("The original dataset is missing required columns.")

}

if (!all(c(

   "model", "accuracy", "macro_f1", "weighted_f1",

   "balanced_accuracy", "fatal_recall", "serious_recall"

) %in% names(comparison))) {

   stop("The model comparison CSV is missing required columns.")

}

if (!all(c(

   "model", "class", "support",

   "precision", "recall", "f1_score"

) %in% names(class_metrics))) {

   stop("The class metrics CSV is missing required columns.")

}

if (!all(c("collision_index", "cluster_id") %in% names(dbscan_data))) {

   stop("The DBSCAN assignment CSV is missing required columns.")

}

# ------------------------------------------------------------

# 3. PREPARE SEVERITY AND TRENDS

# ------------------------------------------------------------

accident_data$collision_year <- suppressWarnings(

   as.integer(as.character(accident_data$collision_year))

)

severity_labels <- c("1" = "Fatal", "2" = "Serious", "3" = "Slight")

accident_data$severity <- unname(

   severity_labels[as.character(accident_data$collision_severity)]

)

accident_data <- accident_data[

   !is.na(accident_data$collision_year) &

      !is.na(accident_data$severity),

   ,

   drop = FALSE

]

severity_order <- c("Fatal", "Serious", "Slight")

severity_colors <- c(

   "Fatal" = "#c62828",

   "Serious" = "#ef8c24",

   "Slight" = "#2878b5"

)

cluster_colors <- c(

   "Clustered" = "#ef6c00",

   "Noise" = "#9e9e9e"

)

severity_summary <- as.data.frame(

   table(factor(accident_data$severity, levels = severity_order)),

   stringsAsFactors = FALSE

)

names(severity_summary) <- c("Severity", "Collisions")

severity_summary$Collisions <- as.integer(severity_summary$Collisions)

severity_summary$Percent <- (

   100 * severity_summary$Collisions /

      sum(severity_summary$Collisions)

)

yearly_summary <- aggregate(

   x = list(Collisions = rep(1L, nrow(accident_data))),

   by = list(

      Year = accident_data$collision_year,

      Severity = accident_data$severity

   ),

   FUN = sum

)

format_pct <- function(x, digits = 2) {

   paste0(formatC(100 * x, format = "f", digits = digits), "%")

}

# ------------------------------------------------------------

# 4. PREPARE MAP DATA

# ------------------------------------------------------------

map_source <- accident_data[, c(

   "collision_index", "latitude", "longitude", "severity"

)]

map_source$collision_index <- as.character(map_source$collision_index)

dbscan_data$collision_index <- as.character(dbscan_data$collision_index)

dbscan_data$cluster_id <- suppressWarnings(

   as.integer(as.character(dbscan_data$cluster_id))

)

dbscan_data <- dbscan_data[

   !is.na(dbscan_data$collision_index) &

      !is.na(dbscan_data$cluster_id) &

      !duplicated(dbscan_data$collision_index),

   c("collision_index", "cluster_id"),

   drop = FALSE

]

map_data <- merge(

   map_source,

   dbscan_data,

   by = "collision_index",

   all = FALSE,

   sort = FALSE

)

map_data <- map_data[

   !is.na(map_data$severity) &

      !is.na(map_data$cluster_id) &

      is.finite(map_data$latitude) &

      is.finite(map_data$longitude) &

      map_data$latitude >= -90 &

      map_data$latitude <= 90 &

      map_data$longitude >= -180 &

      map_data$longitude <= 180,

   ,

   drop = FALSE

]

map_data$cluster_type <- ifelse(

   map_data$cluster_id == 0,

   "Noise",

   "Clustered"

)

map_data$severity <- factor(

   map_data$severity,

   levels = severity_order

)

if (nrow(map_data) == 0) {

   stop("No records matched the source dataset to DBSCAN assignments.")

}

cat("Dashboard records:", nrow(accident_data), "\n")

cat("Matched map records:", nrow(map_data), "\n")

cat("Severity counts:\n")

print(table(map_data$severity))

cat("DBSCAN counts:\n")

print(table(map_data$cluster_type))

# Sample only for interactive visualization performance.

set.seed(42)

map_sample_size <- min(30000L, nrow(map_data))

map_sample <- map_data[

   sample.int(nrow(map_data), map_sample_size),

   ,

   drop = FALSE

]

cat("Map markers available:", nrow(map_sample), "\n")

# ------------------------------------------------------------

# 5. MAP INITIAL VIEW

# ------------------------------------------------------------

UK_WEST <- -8.5

UK_SOUTH <- 49.5

UK_EAST <- 2.2

UK_NORTH <- 59.5

# Explicit reset center and zoom

UK_CENTER_LNG <- -2.5

UK_CENTER_LAT <- 54.5

UK_ZOOM <- 5

# ------------------------------------------------------------

# 6. USER INTERFACE

# ------------------------------------------------------------

ui <- fluidPage(

   tags$head(

      tags$title("Traffic Accident Severity Prediction"),

      tags$style(HTML("

         body {

            background: #f4f6f9;

            color: #202938;

         }

         .app-header {

            background: #172554;

            color: white;

            padding: 22px 26px;

            margin: -15px -15px 22px -15px;

         }

         .app-header h2 {

            margin-top: 0;

            font-weight: 700;

         }

         .app-header p {

            margin-bottom: 0;

            color: #dbeafe;

         }

         .metric-card {

            background: white;

            border-radius: 10px;

            padding: 18px;

            margin-bottom: 15px;

            box-shadow: 0 2px 8px rgba(15,23,42,.07);

            min-height: 104px;

         }

         .metric-label {

            color: #64748b;

            font-size: 13px;

            margin-bottom: 8px;

         }

         .metric-value {

            color: #172554;

            font-size: 27px;

            font-weight: 700;

         }

         .panel-card {

            background: white;

            border-radius: 10px;

            padding: 18px;

            margin-bottom: 18px;

            box-shadow: 0 2px 8px rgba(15,23,42,.07);

         }

         .note {

            background: #eff6ff;

            border-left: 4px solid #2563eb;

            padding: 12px 15px;

            border-radius: 4px;

            margin: 12px 0 18px 0;

         }

         .small-note {

            color: #64748b;

            font-size: 12px;

         }

         .map-help {

            color: #475569;

            margin-bottom: 12px;

         }

         .leaflet-container {

            width: 100% !important;

            height: 700px !important;

            border-radius: 8px;

         }

         .nav-tabs {

            margin-bottom: 18px;

         }

      "))

   ),

   div(

      class = "app-header",

      h2("Traffic Accident Severity Prediction"),

      p(

         "UK Department for Transport collision data | 2021–2025 | ",

         "Model evaluation and geographic analysis dashboard"

      )

   ),

   tabsetPanel(

      id = "main_tabs",

      # OVERVIEW

      tabPanel(

         "Overview",

         br(),

         fluidRow(

            column(

               3,

               div(

                  class = "metric-card",

                  div(class = "metric-label", "Records analysed"),

                  div(

                     class = "metric-value",

                     format(nrow(accident_data), big.mark = ",")

                  )

               )

            ),

            column(

               3,

               div(

                  class = "metric-card",

                  div(class = "metric-label", "Years covered"),

                  div(

                     class = "metric-value",

                     paste0(

                        min(accident_data$collision_year),

                        "–",

                        max(accident_data$collision_year)

                     )

                  )

               )

            ),

            column(

               3,

               div(

                  class = "metric-card",

                  div(class = "metric-label", "Most common severity"),

                  div(

                     class = "metric-value",

                     as.character(

                        severity_summary$Severity[

                           which.max(severity_summary$Collisions)

                        ]

                     )

                  )

               )

            ),

            column(

               3,

               div(

                  class = "metric-card",

                  div(class = "metric-label", "Locked Random Forest accuracy"),

                  div(

                     class = "metric-value",

                     format_pct(

                        comparison$accuracy[

                           comparison$model == "Random Forest"

                        ][1]

                     )

                  )

               )

            )

         ),

         fluidRow(

            column(

               6,

               div(

                  class = "panel-card",

                  h4("Recorded collision severity distribution"),

                  plotOutput("severity_plot", height = "350px")

               )

            ),

            column(

               6,

               div(

                  class = "panel-card",

                  h4("Collision records by year"),

                  plotOutput("year_total_plot", height = "350px")

               )

            )

         ),

         div(

            class = "note",

            strong("Interpretation note: "),

            paste(

               "This project classifies recorded collision severity",

               "(Fatal, Serious, Slight). It does not predict whether",

               "an accident will occur. Some features, such as number",

               "of casualties, may only be known after a collision."

            )

         )

      ),

      # ACCIDENT TRENDS

      tabPanel(

         "Accident Trends",

         br(),

         div(

            class = "panel-card",

            h4("Explore recorded collisions by year and severity"),

            selectInput(

               "trend_severity",

               "Severity class",

               choices = c("All severities", "Fatal", "Serious", "Slight"),

               selected = "All severities"

            ),

            plotOutput("year_severity_plot", height = "430px")

         ),

         div(

            class = "panel-card",

            h4("Severity summary"),

            tableOutput("severity_table")

         )

      ),

      # MODEL COMPARISON

      tabPanel(

         "Model Comparison",

         br(),

         div(

            class = "panel-card",

            h4("Saved holdout evaluation results"),

            p("Values are displayed as percentages."),

            plotOutput("comparison_plot", height = "430px"),

            tableOutput("comparison_table")

         ),

         div(

            class = "note",

            strong("How to read this: "),

            paste(

               "Accuracy measures overall correctness. Macro F1 and",

               "balanced accuracy help assess performance across severity",

               "classes, including rare Fatal cases."

            )

         )

      ),

      # CLASS-WISE PERFORMANCE

      tabPanel(

         "Class-wise Performance",

         br(),

         div(

            class = "panel-card",

            h4("Precision, recall and F1-score by severity class"),

            selectInput(

               "class_metric",

               "Metric",

               choices = c(

                  "Recall" = "recall",

                  "Precision" = "precision",

                  "F1-score" = "f1_score"

               ),

               selected = "recall"

            ),

            selectInput(

               "class_models",

               "Models",

               choices = c("All models", sort(unique(class_metrics$model))),

               selected = "All models"

            ),

            plotOutput("class_metrics_plot", height = "430px"),

            tableOutput("class_metrics_table")

         )

      ),

    # INDIVIDUAL SEVERITY PREDICTION
    tabPanel(
      "Severity Prediction",
      br(),
      div(
        class = "note",
        strong("Using the saved model: "),
        "Locked Random Forest (500 trees). This tab uses the saved model and does not retrain it."
      ),
      fluidRow(
        column(
          6,
          div(
            class = "panel-card",
            h3("Collision details"),
            p(class = "small-note", "Choose the description that best matches the collision. The code in parentheses is the original UK dataset value used by the saved model. Unknown or unrecorded values are labelled clearly."),
            numericInput("pred_vehicles", "Number of vehicles", value = 2, min = 1, max = 100, step = 1),
            numericInput("pred_casualties", "Number of casualties", value = 1, min = 1, max = 100, step = 1),
            selectInput("pred_speed_limit", "Speed limit", choices = model_choice_labels("speed_limit"), selected = if ("30" %in% model_levels$speed_limit) "30" else model_levels$speed_limit[1]),
            selectInput("pred_road_type", "Road type", choices = model_choice_labels("road_type"), selected = default_model_level("road_type", "6")),
            selectInput("pred_light", "Light conditions", choices = model_choice_labels("light_conditions"), selected = default_model_level("light_conditions", "1")),
            selectInput("pred_weather", "Weather conditions", choices = model_choice_labels("weather_conditions"), selected = default_model_level("weather_conditions", "1")),
            selectInput("pred_surface", "Road surface conditions", choices = model_choice_labels("road_surface_conditions"), selected = default_model_level("road_surface_conditions", "1")),
            selectInput("pred_urban_rural", "Area type", choices = model_choice_labels("urban_or_rural_area"), selected = default_model_level("urban_or_rural_area", "1")),
            selectInput("pred_junction", "Junction control", choices = model_choice_labels("junction_control"), selected = default_model_level("junction_control", "4")),
            numericInput("pred_hour", "Time of collision — hour (0–23)", value = 17, min = 0, max = 23, step = 1),
            actionButton("run_severity_prediction", "Predict severity", class = "btn-primary"),
            br(), br(),
            actionButton("reset_prediction_form", "Reset form")
          )
        ),
        column(
          6,
          div(class = "panel-card", h3("Prediction result"), uiOutput("severity_prediction_result")),
          div(
            class = "note",
            strong("Important limitation: "),
            "This is retrospective classification. Number of casualties may only be known after a collision, so do not describe this as a verified pre-accident prediction or use it for safety-critical decisions."
          )
        )
      )
    ),

      # ACCIDENT MAPS

      tabPanel(

         "Accident Maps",

         br(),

         div(

            class = "note",

            strong("About these maps: "),

            paste(

               "Both maps use saved DBSCAN assignments and recorded",

               "collision severity. The severity map does not show",

               "Random Forest predictions."

            )

         ),

         div(

            class = "panel-card",

            h3("DBSCAN Hotspot Map"),

            p(

               class = "map-help",

               "Orange: clustered collisions. Grey: DBSCAN noise."

            ),

            checkboxGroupInput(

               "cluster_filter",

               "Show classifications",

               choices = c("Clustered" = "Clustered", "Noise" = "Noise"),

               selected = c("Clustered", "Noise"),

               inline = TRUE

            ),

            actionButton("reset_dbscan_map", "Reset map to UK"),

            leafletOutput("dbscan_map", height = "700px")

         ),

         div(

            class = "panel-card",

            h3("Recorded Accident Severity Map"),

            p(

               class = "map-help",

               "Red: Fatal. Orange: Serious. Blue: Slight."

            ),

            checkboxGroupInput(

               "severity_filter",

               "Show severity classes",

               choices = c(

                  "Fatal" = "Fatal",

                  "Serious" = "Serious",

                  "Slight" = "Slight"

               ),

               selected = c("Fatal", "Serious", "Slight"),

               inline = TRUE

            ),

            actionButton("reset_severity_map", "Reset map to UK"),

            leafletOutput("severity_map", height = "700px")

         ),

         div(

            class = "note",

            strong("Interpretation caution: "),

            paste(

               "Density-connected clusters are not automatically validated",

               "high-risk hotspots. Collision counts alone do not account",

               "for traffic volume or exposure."

            )

         )

      )

   ),

   br(),

   p(

      class = "small-note",

      "Source: UK Department for Transport Road Safety Data. Models are not retrained."

   )

)

# ------------------------------------------------------------

# 7. SERVER

# ------------------------------------------------------------

server <- function(input, output, session) {

   # OVERVIEW: SEVERITY DISTRIBUTION

   output$severity_plot <- renderPlot({

      ggplot(

         severity_summary,

         aes(x = Severity, y = Collisions, fill = Severity)

      ) +

         geom_col(show.legend = FALSE, width = 0.7) +

         geom_text(

            aes(

               label = paste0(

                  format(Collisions, big.mark = ","),

                  "\n",

                  sprintf("%.1f%%", Percent)

               )

            ),

            vjust = -0.35,

            size = 4

         ) +

         scale_fill_manual(values = severity_colors) +

         scale_y_continuous(

            labels = function(x) format(x, big.mark = ","),

            expand = expansion(mult = c(0, 0.18))

         ) +

         labs(x = NULL, y = "Number of collision records") +

         theme_minimal(base_size = 13) +

         theme(panel.grid.minor = element_blank())

   })

   # OVERVIEW: YEARLY TOTALS

   output$year_total_plot <- renderPlot({

      totals <- aggregate(Collisions ~ Year, data = yearly_summary, sum)

      ggplot(totals, aes(x = Year, y = Collisions)) +

         geom_col(fill = "#2563eb", width = 0.7) +

         geom_text(

            aes(label = format(Collisions, big.mark = ",")),

            vjust = -0.35,

            size = 3.8

         ) +

         scale_x_continuous(breaks = sort(unique(totals$Year))) +

         scale_y_continuous(

            labels = function(x) format(x, big.mark = ","),

            expand = expansion(mult = c(0, 0.15))

         ) +

         labs(x = "Collision year", y = "Number of collision records") +

         theme_minimal(base_size = 13) +

         theme(panel.grid.minor = element_blank())

   })

   # ACCIDENT TRENDS

   output$year_severity_plot <- renderPlot({

      plot_data <- yearly_summary

      if (

         !is.null(input$trend_severity) &&

         input$trend_severity != "All severities"

      ) {

         plot_data <- plot_data[

            plot_data$Severity == input$trend_severity,

            ,

            drop = FALSE

         ]

      }

      ggplot(

         plot_data,

         aes(

            x = Year,

            y = Collisions,

            colour = Severity,

            group = Severity

         )

      ) +

         geom_line(linewidth = 1.1) +

         geom_point(size = 2.5) +

         scale_color_manual(values = severity_colors) +

         scale_x_continuous(breaks = sort(unique(yearly_summary$Year))) +

         scale_y_continuous(

            labels = function(x) format(x, big.mark = ",")

         ) +

         labs(

            x = "Collision year",

            y = "Number of collision records",

            colour = "Severity"

         ) +

         theme_minimal(base_size = 13) +

         theme(panel.grid.minor = element_blank())

   })

   output$severity_table <- renderTable({

      out <- severity_summary

      out$Collisions <- format(out$Collisions, big.mark = ",")

      out$Percent <- paste0(sprintf("%.2f", out$Percent), "%")

      out

   }, striped = TRUE, bordered = TRUE, spacing = "s")

   # MODEL COMPARISON

   output$comparison_plot <- renderPlot({

      metric_names <- c(

         "accuracy", "macro_f1", "weighted_f1", "balanced_accuracy"

      )

      metric_labels <- c(

         "Accuracy", "Macro F1", "Weighted F1", "Balanced Accuracy"

      )

      plot_data <- do.call(

         rbind,

         lapply(seq_along(metric_names), function(i) {

            data.frame(

               model = comparison$model,

               Score = comparison[[metric_names[i]]],

               Metric = metric_labels[i],

               stringsAsFactors = FALSE

            )

         })

      )

      plot_data$model <- factor(plot_data$model, levels = rev(comparison$model))

      plot_data$Metric <- factor(plot_data$Metric, levels = metric_labels)

      ggplot(plot_data, aes(x = Score, y = model, colour = Metric)) +

         geom_point(size = 3.3, position = position_dodge(width = 0.5)) +

         scale_x_continuous(

            labels = function(x) paste0(round(100 * x), "%"),

            limits = c(0, 1)

         ) +

         labs(x = "Score", y = NULL, colour = "Metric") +

         theme_minimal(base_size = 12) +

         theme(panel.grid.minor = element_blank())

   })

   output$comparison_table <- renderTable({

      out <- comparison

      metric_cols <- c(

         "accuracy", "macro_f1", "weighted_f1",

         "balanced_accuracy", "fatal_recall", "serious_recall"

      )

      for (nm in intersect(metric_cols, names(out))) {

         out[[nm]] <- format_pct(out[[nm]])

      }

      out

   }, striped = TRUE, bordered = TRUE, spacing = "s")

   # CLASS-WISE PERFORMANCE

   selected_class_data <- reactive({

      out <- class_metrics

      if (

         !is.null(input$class_models) &&

         input$class_models != "All models"

      ) {

         out <- out[out$model == input$class_models, , drop = FALSE]

      }

      out

   })

   output$class_metrics_plot <- renderPlot({

      out <- selected_class_data()

      req(nrow(out) > 0)

      metric <- input$class_metric

      metric_label <- c(

         recall = "Recall",

         precision = "Precision",

         f1_score = "F1-score"

      )[[metric]]

      out$class <- factor(out$class, levels = severity_order)

      ggplot(

         out,

         aes(x = class, y = .data[[metric]], fill = model)

      ) +

         geom_col(

            position = position_dodge(width = 0.8),

            width = 0.7

         ) +

         scale_y_continuous(

            labels = function(x) paste0(round(100 * x), "%"),

            limits = c(0, 1)

         ) +

         labs(

            x = "Recorded severity class",

            y = metric_label,

            fill = "Model"

         ) +

         theme_minimal(base_size = 13) +

         theme(panel.grid.minor = element_blank())

   })

   output$class_metrics_table <- renderTable({

      out <- selected_class_data()

      out$support <- format(out$support, big.mark = ",")

      for (nm in c("precision", "recall", "f1_score")) {

         out[[nm]] <- paste0(sprintf("%.2f", 100 * out[[nm]]), "%")

      }

      out

   }, striped = TRUE, bordered = TRUE, spacing = "s")

  # ----------------------------------------------------------
  # INDIVIDUAL SEVERITY PREDICTION (LOCKED MODEL; NO RETRAINING)
  # ----------------------------------------------------------
  prediction_result <- eventReactive(input$run_severity_prediction, {
    req(input$pred_vehicles, input$pred_casualties, input$pred_speed_limit,
        input$pred_road_type, input$pred_light, input$pred_weather,
        input$pred_surface, input$pred_urban_rural, input$pred_junction,
        input$pred_hour)
    validate(
      need(input$pred_vehicles >= 1, "Number of vehicles must be at least 1."),
      need(input$pred_casualties >= 1, "Number of casualties must be at least 1."),
      need(input$pred_hour >= 0 && input$pred_hour <= 23, "Accident hour must be between 0 and 23.")
    )
    newdata <- data.frame(
      number_of_vehicles = as.numeric(input$pred_vehicles),
      number_of_casualties = as.numeric(input$pred_casualties),
      speed_limit = factor(as.character(input$pred_speed_limit), levels = model_levels$speed_limit),
      road_type = factor(as.character(input$pred_road_type), levels = model_levels$road_type),
      light_conditions = factor(as.character(input$pred_light), levels = model_levels$light_conditions),
      weather_conditions = factor(as.character(input$pred_weather), levels = model_levels$weather_conditions),
      road_surface_conditions = factor(as.character(input$pred_surface), levels = model_levels$road_surface_conditions),
      urban_or_rural_area = factor(as.character(input$pred_urban_rural), levels = model_levels$urban_or_rural_area),
      junction_control = factor(as.character(input$pred_junction), levels = model_levels$junction_control),
      accident_hour = as.numeric(input$pred_hour),
      check.names = FALSE
    )
    newdata <- newdata[, model_features, drop = FALSE]
    if (anyNA(newdata)) stop("An input could not be mapped to a saved model feature level.")
    model_output <- predict(severity_model, data = newdata)
    raw_prediction <- as.character(model_output$predictions[1])
    severity_lookup <- c("1" = "Fatal", "2" = "Serious", "3" = "Slight")
    predicted_label <- unname(severity_lookup[raw_prediction])
    if (length(predicted_label) == 0 || is.na(predicted_label)) predicted_label <- raw_prediction
    list(predicted = predicted_label, raw = raw_prediction, input = newdata, timestamp = Sys.time())
  }, ignoreInit = TRUE)

  output$severity_prediction_result <- renderUI({
    result <- prediction_result()
    if (is.null(result)) {
      return(div(p("Enter collision details and select Predict severity to run the saved model."),
                 p(class = "small-note", "No prediction has been generated yet.")))
    }
    result_color <- switch(result$predicted, "Fatal" = "#c62828", "Serious" = "#ef8c24", "Slight" = "#2878b5", "#172554")
    tagList(
      div(style = paste0("border-left: 6px solid ", result_color, "; padding: 18px; background: #f8fafc; border-radius: 8px; margin-bottom: 16px;"),
          p(class = "metric-label", "Predicted recorded severity"),
          h2(style = paste0("color: ", result_color, "; margin-top: 4px;"), result$predicted),
          p(class = "small-note", paste("Model output class:", result$raw))),
      h4("Inputs used"),
      tableOutput("prediction_input_summary"),
      p(class = "small-note", paste("Generated at:", format(result$timestamp, "%Y-%m-%d %H:%M:%S"))),
      p(class = "small-note", "This is an estimate, not a certainty. Probabilities are not displayed because this saved model has not been verified as a probability forest.")
    )
  })

  output$prediction_input_summary <- renderTable({
    result <- prediction_result()
    req(result)
    data.frame(Feature = names(result$input),
               Value = vapply(result$input, function(x) as.character(x[1]), character(1)),
               check.names = FALSE)
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  observeEvent(input$reset_prediction_form, {
    updateNumericInput(session, "pred_vehicles", value = 2)
    updateNumericInput(session, "pred_casualties", value = 1)
    updateSelectInput(session, "pred_speed_limit", selected = if ("30" %in% model_levels$speed_limit) "30" else model_levels$speed_limit[1])
    updateSelectInput(session, "pred_road_type", selected = default_model_level("road_type", "6"))
    updateSelectInput(session, "pred_light", selected = default_model_level("light_conditions", "1"))
    updateSelectInput(session, "pred_weather", selected = default_model_level("weather_conditions", "1"))
    updateSelectInput(session, "pred_surface", selected = default_model_level("road_surface_conditions", "1"))
    updateSelectInput(session, "pred_urban_rural", selected = default_model_level("urban_or_rural_area", "1"))
    updateSelectInput(session, "pred_junction", selected = default_model_level("junction_control", "4"))
    updateNumericInput(session, "pred_hour", value = 17)
  }, ignoreInit = TRUE)

   # ----------------------------------------------------------

   # DBSCAN MAP

   # ----------------------------------------------------------

   filtered_cluster_map <- reactive({

      req(input$cluster_filter)

      map_sample[

         map_sample$cluster_type %in% input$cluster_filter,

         ,

         drop = FALSE

      ]

   })

   output$dbscan_map <- renderLeaflet({

      plot_data <- filtered_cluster_map()

      leaflet(

         data = plot_data,

         options = leafletOptions(preferCanvas = TRUE)

      ) %>%

         addTiles(

            urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",

            attribution = paste(

               '&copy; <a href="https://www.openstreetmap.org/copyright">',

               'OpenStreetMap contributors</a>'

            )

         ) %>%

         addCircleMarkers(

            lng = ~longitude,

            lat = ~latitude,

            radius = 3,

            stroke = FALSE,

            fillOpacity = 0.75,

            fillColor = ~unname(cluster_colors[cluster_type]),

            label = ~paste0(

               "Collision: ", collision_index,

               " | Cluster: ", cluster_id,

               " | Type: ", cluster_type,

               " | Severity: ", as.character(severity)

            )

         ) %>%

         addLegend(

            position = "bottomright",

            colors = unname(cluster_colors),

            labels = names(cluster_colors),

            title = "DBSCAN classification",

            opacity = 0.9

         ) %>%

         setView(

            lng = UK_CENTER_LNG,

            lat = UK_CENTER_LAT,

            zoom = UK_ZOOM

         )

   })

   # Reset uses setView, which explicitly sets center and zoom.

   observeEvent(input$reset_dbscan_map, {

      leafletProxy("dbscan_map") %>%

         setView(

            lng = UK_CENTER_LNG,

            lat = UK_CENTER_LAT,

            zoom = UK_ZOOM

         )

   }, ignoreInit = TRUE)

   # ----------------------------------------------------------

   # SEVERITY MAP

   # ----------------------------------------------------------

   filtered_severity_map <- reactive({

      req(input$severity_filter)

      map_sample[

         as.character(map_sample$severity) %in% input$severity_filter,

         ,

         drop = FALSE

      ]

   })

   output$severity_map <- renderLeaflet({

      plot_data <- filtered_severity_map()

      leaflet(

         data = plot_data,

         options = leafletOptions(preferCanvas = TRUE)

      ) %>%

         addTiles(

            urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",

            attribution = paste(

               '&copy; <a href="https://www.openstreetmap.org/copyright">',

               'OpenStreetMap contributors</a>'

            )

         ) %>%

         addCircleMarkers(

            lng = ~longitude,

            lat = ~latitude,

            radius = 3,

            stroke = FALSE,

            fillOpacity = 0.75,

            fillColor = ~unname(

               severity_colors[as.character(severity)]

            ),

            label = ~paste0(

               "Collision: ", collision_index,

               " | Severity: ", as.character(severity),

               " | Cluster: ", cluster_id

            )

         ) %>%

         addLegend(

            position = "bottomright",

            colors = unname(severity_colors),

            labels = names(severity_colors),

            title = "Recorded severity",

            opacity = 0.95

         ) %>%

         setView(

            lng = UK_CENTER_LNG,

            lat = UK_CENTER_LAT,

            zoom = UK_ZOOM

         )

   })

   observeEvent(input$reset_severity_map, {

      leafletProxy("severity_map") %>%

         setView(

            lng = UK_CENTER_LNG,

            lat = UK_CENTER_LAT,

            zoom = UK_ZOOM

         )

   }, ignoreInit = TRUE)

}

# ------------------------------------------------------------

# 8. LAUNCH APPLICATION

# ------------------------------------------------------------

shinyApp(ui = ui, server = server)


