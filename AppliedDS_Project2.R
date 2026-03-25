# =============================================================================
# DataPilot: EDA & Preprocessing App
# UI: shinydashboard
# =============================================================================

library(shiny)
library(shinydashboard)
library(shinyBS)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr)
library(scales)
library(jsonlite)

# custom CSS
custom_css <- "
  body { font-family: \'Segoe UI\', Arial, sans-serif; background: #f4f6f9; }
  .main-header .logo { font-weight: 700; font-size: 18px; }
  .info-box { border-radius: 6px; }
  .box { border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,.08); }
  .box-header { border-radius: 8px 8px 0 0; }
  .section-label {
    font-size: 11px; font-weight: 700; text-transform: uppercase;
    letter-spacing: .06em; color: #888; margin: 14px 0 4px 0;
  }
  .tooltip-icon { color: #3c8dbc; cursor: help; margin-left: 4px; font-size: 13px; }
  .data-badge {
    display: inline-block; padding: 2px 8px; border-radius: 12px;
    font-size: 12px; font-weight: 600;
  }
  .badge-blue  { background: #d6eaf8; color: #1a5276; }
  .badge-green { background: #d5f5e3; color: #1e8449; }
  .badge-red   { background: #fadbd8; color: #922b21; }
  .preview-box {
    background: #fff; border: 1px solid #dee2e6; border-radius: 6px;
    padding: 10px 14px; margin-top: 8px; font-size: 13px;
  }
  .filter-badge {
    display:inline-block; background:#d4edda; color:#155724;
    border-radius:10px; padding:2px 9px; font-size:12px; font-weight:600;
  }
  .info-strip {
    background:#1a2a3a; color:#aec6e8;
    padding:6px 15px; font-size:12px;
  }
  .nav-tabs > li > a { color: #555; }
  .nav-tabs > li.active > a { color: #3c8dbc; font-weight: 600; }
  .shiny-notification { border-radius: 6px; font-size: 13px; }
  .guide-step {
    padding: 10px 14px; margin: 6px 0; border-left: 4px solid #3c8dbc;
    background: #f8fbff; border-radius: 0 6px 6px 0; font-size: 14px;
  }
"

# UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$span(icon("database"), " DataPilot")
  ),
  
  # ---- Sidebar ---------------------------------------------------------------
  dashboardSidebar(
    width = 300,
    tags$style(custom_css),
    
    uiOutput("dataset_info_bar"),
    
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("User Guide",               tabName = "guide",      icon = icon("book-open")),
      menuItem("Data Preview",             tabName = "preview",    icon = icon("table")),
      menuItem("Summary Stats",            tabName = "summary",    icon = icon("chart-bar")),
      menuItem("Missing Values",           tabName = "missing",    icon = icon("question-circle")),
      menuItem("Duplicates",               tabName = "dupes",      icon = icon("copy")),
      menuItem("Outliers",                 tabName = "outliers",   icon = icon("exclamation-triangle")),
      menuItem("Transformation Comparison",tabName = "comparison", icon = icon("exchange-alt")),
      menuItem("EDA – Charts",             tabName = "eda",        icon = icon("chart-line")),
      menuItem("Correlation",              tabName = "corr",       icon = icon("project-diagram"))
    ),
    
    hr(),
    
    # 1. Data Source
    tags$div(class = "section-label", style = "padding-left:15px",
             icon("upload"), " 1. Data Source"
    ),
    tags$div(style = "padding: 0 10px",
             fileInput("file", NULL,
                       accept = c(".csv", ".xlsx", ".json", ".rds"),
                       placeholder = "CSV / Excel / JSON / RDS"),
             bsTooltip("file", "Upload your own dataset. Supported: CSV, Excel (.xlsx), JSON, RDS.",
                       placement = "right"),
             selectInput("builtin", "Or use built-in dataset",
                         choices = c("None", "iris", "mtcars"), selected = "iris"),
             bsTooltip("builtin", "Choose a demo dataset if you don\'t have one handy.",
                       placement = "right")
    ),
    
    hr(),
    
    # 2. Missing Values
    tags$div(class = "section-label", style = "padding-left:15px",
             icon("band-aid"), " 2. Missing Values"
    ),
    tags$div(style = "padding: 0 10px",
             checkboxInput("handle_na", "Enable missing-value handling", value = FALSE),
             conditionalPanel("input.handle_na",
                              selectInput("na_cols", "Columns to process", choices = NULL, multiple = TRUE),
                              selectInput("na_action", "Method",
                                          choices = c(
                                            "Remove rows"               = "drop_rows",
                                            "Mean imputation"           = "mean",
                                            "Median imputation"         = "median",
                                            "Mode imputation"           = "mode",
                                            "Custom fill value"         = "custom",
                                            "Identify only (no change)" = "identify_only"
                                          ), selected = "identify_only"),
                              bsTooltip("na_action",
                                        "Mean/Median: numeric only. Mode: any type. Custom: type a fill value below.",
                                        placement = "right"),
                              conditionalPanel("input.na_action == \'custom\'",
                                               textInput("na_custom", "Custom fill value", ""))
             )
    ),
    
    hr(),
    
    # 3. Duplicates
    tags$div(class = "section-label", style = "padding-left:15px",
             icon("clone"), " 3. Duplicates"
    ),
    tags$div(style = "padding: 0 10px",
             checkboxInput("handle_dup", "Enable duplicate handling", value = FALSE),
             conditionalPanel("input.handle_dup",
                              selectInput("dup_cols", "Define duplicates by", choices = NULL, multiple = TRUE),
                              selectInput("dup_action", "Method",
                                          choices = c(
                                            "Keep first"            = "keep_first",
                                            "Keep last"             = "keep_last",
                                            "Remove all duplicates" = "remove_all",
                                            "Identify only"         = "identify_only"
                                          ), selected = "keep_first")
             )
    ),
    
    hr(),
    
    # 4. Outlier Handling
    tags$div(class = "section-label", style = "padding-left:15px",
             icon("filter"), " 4. Outliers (IQR)"
    ),
    tags$div(style = "padding: 0 10px",
             checkboxInput("handle_outliers", "Enable outlier handling", value = FALSE),
             conditionalPanel("input.handle_outliers",
                              selectInput("outlier_cols", "Numeric columns", choices = NULL, multiple = TRUE),
                              selectInput("outlier_action", "Method",
                                          choices = c(
                                            "Identify only"          = "identify_only",
                                            "Remove outlier rows"    = "remove_rows",
                                            "Cap (Winsorize) at IQR" = "winsorize"
                                          ), selected = "identify_only"),
                              bsTooltip("outlier_action",
                                        "Winsorize caps values at Q1-1.5*IQR and Q3+1.5*IQR instead of deleting rows.",
                                        placement = "right")
             )
    ),
    
    hr(),
    
    # 5. Categorical Encoding
    tags$div(class = "section-label", style = "padding-left:15px",
             icon("tag"), " 5. Encoding"
    ),
    tags$div(style = "padding: 0 10px",
             checkboxInput("apply_encoding", "Enable categorical encoding", value = FALSE),
             conditionalPanel("input.apply_encoding",
                              selectInput("cat_var", "Categorical variable", choices = NULL),
                              selectInput("encoding_method", "Method",
                                          choices = c("One-hot encoding" = "onehot", "Label encoding" = "label"),
                                          selected = "onehot"),
                              bsTooltip("encoding_method",
                                        "One-hot: binary dummy columns per category. Label: maps categories to integers.",
                                        placement = "right"),
                              conditionalPanel("input.encoding_method == \'onehot\'",
                                               checkboxInput("drop_first_dummy",
                                                             "Drop first dummy (avoid multicollinearity)", value = FALSE)
                              ),
                              conditionalPanel("input.encoding_method == \'label\'",
                                               textInput("label_var_name", "New column name", "encoded_var")
                              )
             )
    ),
    
    hr(),
    
    # 6. Feature Engineering
    tags$div(class = "section-label", style = "padding-left:15px",
             icon("magic"), " 6. Feature Engineering"
    ),
    tags$div(style = "padding: 0 10px",
             checkboxInput("apply_feature_engineering", "Enable feature engineering", value = FALSE),
             conditionalPanel("input.apply_feature_engineering",
                              selectInput("feature_method", "Transformation",
                                          choices = c(
                                            "Log transform (log1p)"  = "log",
                                            "Square root"            = "sqrt",
                                            "Square (x^2)"           = "square",
                                            "Interaction (x1 x x2)" = "interaction",
                                            "Binning (equal-width)"  = "binning"
                                          ), selected = "log"),
                              selectInput("num_var", "Primary variable", choices = NULL),
                              conditionalPanel("input.feature_method == \'interaction\'",
                                               selectInput("num_var_2", "Second variable", choices = NULL)
                              ),
                              conditionalPanel("input.feature_method == \'binning\'",
                                               numericInput("bin_count", "Number of bins", value = 4, min = 2, max = 20, step = 1)
                              ),
                              textInput("new_var", "New column name", "engineered_var"),
                              uiOutput("fe_preview")
             )
    ),
    
    hr(),
    
    # 7. Scaling
    tags$div(class = "section-label", style = "padding-left:15px",
             icon("arrows-alt-v"), " 7. Scaling"
    ),
    tags$div(style = "padding: 0 10px",
             checkboxInput("apply_scaling", "Enable scaling", value = FALSE),
             conditionalPanel("input.apply_scaling",
                              selectInput("scale_var", "Variable to scale", choices = NULL),
                              selectInput("scale_method", "Method",
                                          choices = c(
                                            "Z-score (standardisation)" = "zscore",
                                            "Min-Max (0 to 1)"          = "minmax"
                                          ), selected = "zscore"),
                              bsTooltip("scale_method",
                                        "Z-score: (x - mean) / sd.   Min-Max: (x - min) / (max - min).",
                                        placement = "right"),
                              textInput("scaled_var_name", "New column name", "scaled_var"),
                              uiOutput("scale_preview")
             )
    ),
    
    hr(),
    
    # 8. Filter
    tags$div(class = "section-label", style = "padding-left:15px",
             icon("search"), " 8. Filter"
    ),
    tags$div(style = "padding: 0 10px",
             checkboxInput("apply_filter", "Enable row filter", value = FALSE),
             conditionalPanel("input.apply_filter",
                              selectInput("filter_var",   "Filter column", choices = NULL),
                              selectInput("filter_value", "Keep value",    choices = NULL),
                              uiOutput("filter_row_count")
             )
    ),
    
    hr(),
    
    # 9. Chart Variables
    tags$div(class = "section-label", style = "padding-left:15px",
             icon("chart-area"), " 9. Chart Variables"
    ),
    tags$div(style = "padding: 0 10px",
             selectInput("xvar",      "X variable",             choices = NULL),
             selectInput("yvar",      "Y variable",             choices = NULL),
             selectInput("color_var", "Color/Group (optional)", choices = c("None" = "none"), selected = "none"),
             numericInput("hist_bins","Histogram bins", value = 30, min = 5, max = 100, step = 5),
             hr(),
             tags$div(class = "section-label", "Boxplot / Violin"),
             selectInput("box_cat_var", "Group (categorical)", choices = NULL),
             selectInput("box_num_var", "Value (numeric)",     choices = NULL),
             hr(),
             tags$div(class = "section-label", "Bar Chart"),
             selectInput("bar_var", "Categorical variable", choices = NULL),
             hr(),
             tags$div(class = "section-label", "Transformation Comparison"),
             selectInput("comparison_target", NULL,
                         choices = c("None" = "none"), selected = "none")
    ),
    
    hr(),
    tags$div(style = "padding: 0 10px 14px",
             downloadButton("download", "Download Processed Data",
                            class = "btn-block btn-success")
    )
  ),
  
  # ---- Body ------------------------------------------------------------------
  dashboardBody(
    tabItems(
      
      # User Guide
      tabItem("guide",
              fluidRow(
                box(width = 12, title = "Welcome to DataPilot", status = "primary", solidHeader = TRUE,
                    fluidRow(
                      column(6,
                             tags$h4("What does this app do?"),
                             tags$p("DataPilot is a no-code interactive data analysis tool. Upload any tabular dataset and use the sidebar controls to clean, transform, and explore your data."),
                             tags$h4("Quick-start steps"),
                             tags$div(class = "guide-step", "\u2460 Upload a CSV/Excel/JSON/RDS file, or choose a built-in demo dataset."),
                             tags$div(class = "guide-step", "\u2461 Use Missing Values, Duplicates, and Outliers panels to clean the data."),
                             tags$div(class = "guide-step", "\u2462 Engineer new features (log, sqrt, interaction, binning) or apply scaling."),
                             tags$div(class = "guide-step", "\u2463 Explore distributions, scatter plots, bar charts, and the correlation heatmap in EDA - Charts."),
                             tags$div(class = "guide-step", "\u2464 Download the fully processed dataset as CSV when ready.")
                      ),
                      column(6,
                             tags$h4("Supported file formats"),
                             tags$table(class = "table table-bordered table-sm",
                                        tags$thead(tags$tr(tags$th("Format"), tags$th("Extension"), tags$th("Notes"))),
                                        tags$tbody(
                                          tags$tr(tags$td("CSV"),   tags$td(".csv"),  tags$td("Comma-separated values")),
                                          tags$tr(tags$td("Excel"), tags$td(".xlsx"), tags$td("First sheet is read")),
                                          tags$tr(tags$td("JSON"),  tags$td(".json"), tags$td("Flat or nested (auto-flattened)")),
                                          tags$tr(tags$td("RDS"),   tags$td(".rds"),  tags$td("Native R serialised object"))
                                        )
                             ),
                             tags$h4("Tips"),
                             tags$ul(
                               tags$li("Sidebar sections collapse: tick the checkbox to expand each section."),
                               tags$li("All transformations are non-destructive."),
                               tags$li("Use Transformation Comparison to see before/after statistics."),
                               tags$li("The real-time preview card shows new column stats instantly.")
                             )
                      )
                    )
                )
              )
      ),
      
      # Data Preview
      tabItem("preview",
              fluidRow(uiOutput("info_boxes")),
              fluidRow(
                box(width = 12, title = "Data Preview", status = "info", solidHeader = TRUE,
                    DTOutput("table"))
              )
      ),
      
      # Summary
      tabItem("summary",
              fluidRow(
                box(width = 12, title = "Statistical Summary", status = "info", solidHeader = TRUE,
                    verbatimTextOutput("summary"))
              )
      ),
      
      # Missing Values
      tabItem("missing",
              fluidRow(
                box(width = 12, title = "Missing Value Report", status = "warning", solidHeader = TRUE,
                    verbatimTextOutput("na_stats"), br(), DTOutput("missing_table"))
              )
      ),
      
      # Duplicates
      tabItem("dupes",
              fluidRow(
                box(width = 12, title = "Duplicate Summary", status = "warning", solidHeader = TRUE,
                    verbatimTextOutput("dup_stats"), br(), DTOutput("dup_table"))
              )
      ),
      
      # Outliers
      tabItem("outliers",
              fluidRow(
                box(width = 12, title = "Outlier Summary (IQR method)", status = "danger", solidHeader = TRUE,
                    verbatimTextOutput("outlier_stats"), br(), DTOutput("outlier_table"))
              )
      ),
      
      # Transformation Comparison
      tabItem("comparison",
              fluidRow(
                box(width = 12, title = "Transformation Comparison", status = "primary", solidHeader = TRUE,
                    verbatimTextOutput("comparison_note"), br(),
                    DTOutput("comparison_table"), br(),
                    plotlyOutput("comparison_hist"))
              )
      ),
      
      # EDA Charts
      tabItem("eda",
              fluidRow(
                box(width = 6, title = "Histogram / Density", status = "info", solidHeader = TRUE,
                    tabsetPanel(
                      tabPanel("Histogram", br(), plotlyOutput("hist")),
                      tabPanel("Density",   br(), plotlyOutput("density"))
                    )
                ),
                box(width = 6, title = "Scatter Plot", status = "info", solidHeader = TRUE,
                    plotlyOutput("scatter"))
              ),
              fluidRow(
                box(width = 6, title = "Grouped Boxplot / Violin", status = "info", solidHeader = TRUE,
                    tabsetPanel(
                      tabPanel("Boxplot", br(), plotlyOutput("grouped_boxplot")),
                      tabPanel("Violin",  br(), plotlyOutput("violin"))
                    )
                ),
                box(width = 6, title = "Bar Chart (Categorical Counts)", status = "info", solidHeader = TRUE,
                    plotlyOutput("bar_chart"))
              )
      ),
      
      # Correlation
      tabItem("corr",
              fluidRow(
                box(width = 12, title = "Correlation Heatmap", status = "info", solidHeader = TRUE,
                    plotlyOutput("corr_heatmap", height = "550px"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # -------- Helper: safely convert object to data frame --------
  to_dataframe <- function(obj) {
    if (is.data.frame(obj)) return(as.data.frame(obj))
    if (is.matrix(obj)) return(as.data.frame(obj))
    
    try_df <- try(as.data.frame(obj), silent = TRUE)
    if (!inherits(try_df, "try-error")) {
      return(as.data.frame(try_df))
    }
    
    stop("Uploaded file could not be converted to a data frame.")
  }
  
  # -------- Helper: mode function --------
  get_mode <- function(x) {
    x_non_na <- x[!is.na(x)]
    if (length(x_non_na) == 0) return(NA)
    ux <- unique(x_non_na)
    ux[which.max(tabulate(match(x_non_na, ux)))]
  }
  
  # -------- Helper: custom fill with correct type --------
  cast_custom_value <- function(fill_value, target_col) {
    if (is.integer(target_col)) {
      return(suppressWarnings(as.integer(fill_value)))
    }
    if (is.numeric(target_col)) {
      return(suppressWarnings(as.numeric(fill_value)))
    }
    if (is.logical(target_col)) {
      val_lower <- tolower(trimws(fill_value))
      if (val_lower %in% c("true", "t", "1")) return(TRUE)
      if (val_lower %in% c("false", "f", "0")) return(FALSE)
      return(NA)
    }
    as.character(fill_value)
  }
  
  # -------- Helper: identify outliers by IQR --------
  iqr_outlier_flag <- function(x) {
    if (!is.numeric(x)) return(rep(FALSE, length(x)))
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    x < lower | x > upper
  }
  
  # -------- Helper: Winsorize (cap at IQR bounds) --------
  iqr_winsorise <- function(x) {
    if (!is.numeric(x)) return(x)
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    pmin(pmax(x, q1 - 1.5 * iqr), q3 + 1.5 * iqr)
  }
  
  # -------- Helper: identify categorical variables --------
  is_categorical_col <- function(x) {
    is.character(x) || is.factor(x) || is.logical(x)
  }
  
  # -------- Helper: safe column names --------
  make_clean_names <- function(x) {
    x <- gsub("[^A-Za-z0-9_]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_|_$", "", x)
    make.unique(x, sep = "_")
  }
  
  # -------- Helper: summary table for one variable --------
  make_variable_summary <- function(x, var_name, role_name) {
    if (is.numeric(x)) {
      data.frame(
        Role = role_name,
        Variable = var_name,
        Type = "Numeric",
        Missing = sum(is.na(x)),
        Unique = dplyr::n_distinct(x, na.rm = TRUE),
        Mean = round(mean(x, na.rm = TRUE), 4),
        Median = round(median(x, na.rm = TRUE), 4),
        SD = round(sd(x, na.rm = TRUE), 4),
        Min = round(min(x, na.rm = TRUE), 4),
        Max = round(max(x, na.rm = TRUE), 4),
        Top_Level = NA,
        Top_Count = NA,
        stringsAsFactors = FALSE
      )
    } else {
      x_char <- as.character(x)
      x_non_na <- x_char[!is.na(x_char)]
      top_level <- if (length(x_non_na) == 0) NA else names(sort(table(x_non_na), decreasing = TRUE))[1]
      top_count <- if (length(x_non_na) == 0) NA else as.integer(sort(table(x_non_na), decreasing = TRUE)[1])
      
      data.frame(
        Role = role_name,
        Variable = var_name,
        Type = "Categorical",
        Missing = sum(is.na(x)),
        Unique = dplyr::n_distinct(x, na.rm = TRUE),
        Mean = NA,
        Median = NA,
        SD = NA,
        Min = NA,
        Max = NA,
        Top_Level = top_level,
        Top_Count = top_count,
        stringsAsFactors = FALSE
      )
    }
  }
  
  # -------- Load Data --------
  # Uploaded file takes priority over built-in dataset selection.
  raw_data <- reactive({
    if (!is.null(input$file)) {
      # User uploaded a file — use it regardless of builtin selection
      ext <- tolower(tools::file_ext(input$file$name))
      path <- input$file$datapath
      
      df <- switch(
        ext,
        "csv"  = read_csv(path, show_col_types = FALSE),
        "xlsx" = read_excel(path),
        "json" = {
          json_obj <- fromJSON(path, flatten = TRUE)
          to_dataframe(json_obj)
        },
        "rds"  = {
          rds_obj <- readRDS(path)
          to_dataframe(rds_obj)
        },
        stop("Unsupported file type. Please upload CSV, XLSX, JSON, or RDS.")
      )
      
      return(as.data.frame(df))
    }
    
    # No file uploaded — fall back to built-in dataset
    if (!is.null(input$builtin) && input$builtin != "None") {
      return(as.data.frame(get(input$builtin)))
    }
    
    # Nothing selected at all
    return(NULL)
  })
  
  # -------- Missing Summary --------
  missing_summary <- reactive({
    df <- raw_data()
    req(df)
    
    data.frame(
      Variable = names(df),
      Missing_Count = sapply(df, function(x) sum(is.na(x))),
      Missing_Percent = sapply(df, function(x) mean(is.na(x))),
      stringsAsFactors = FALSE
    ) %>%
      mutate(Missing_Percent = percent(Missing_Percent, accuracy = 0.1))
  })
  
  # -------- Missing Handling --------
  na_handled_data <- reactive({
    df <- raw_data()
    req(df)
    
    if (!isTRUE(input$handle_na)) return(df)
    
    na_cols <- input$na_cols
    if (is.null(na_cols) || length(na_cols) == 0) {
      na_cols <- names(df)
    }
    
    if (input$na_action == "identify_only") return(df)
    
    if (input$na_action == "drop_rows") {
      df <- df %>% filter(if_all(all_of(na_cols), ~ !is.na(.)))
      return(df)
    }
    
    if (input$na_action == "mean") {
      for (col in na_cols) {
        if (is.numeric(df[[col]])) {
          fill_val <- mean(df[[col]], na.rm = TRUE)
          if (is.nan(fill_val)) fill_val <- NA
          df[[col]][is.na(df[[col]])] <- fill_val
        }
      }
      return(df)
    }
    
    if (input$na_action == "median") {
      for (col in na_cols) {
        if (is.numeric(df[[col]])) {
          fill_val <- median(df[[col]], na.rm = TRUE)
          if (is.nan(fill_val)) fill_val <- NA
          df[[col]][is.na(df[[col]])] <- fill_val
        }
      }
      return(df)
    }
    
    if (input$na_action == "mode") {
      for (col in na_cols) {
        fill_val <- get_mode(df[[col]])
        df[[col]][is.na(df[[col]])] <- fill_val
      }
      return(df)
    }
    
    if (input$na_action == "custom") {
      for (col in na_cols) {
        fill_val <- cast_custom_value(input$na_custom, df[[col]])
        df[[col]][is.na(df[[col]])] <- fill_val
      }
      return(df)
    }
    
    df
  })
  
  # -------- Duplicate Summary --------
  duplicate_summary <- reactive({
    df <- na_handled_data()
    req(df)
    
    dup_cols <- input$dup_cols
    if (is.null(dup_cols) || length(dup_cols) == 0) {
      dup_cols <- names(df)
    }
    
    subset_df <- df[, dup_cols, drop = FALSE]
    
    df %>%
      mutate(
        duplicate_flag = duplicated(subset_df) | duplicated(subset_df, fromLast = TRUE)
      )
  })
  
  # -------- Duplicate Handling --------
  clean_data <- reactive({
    df <- na_handled_data()
    req(df)
    
    if (isTRUE(input$handle_dup)) {
      dup_cols <- input$dup_cols
      
      if (is.null(dup_cols) || length(dup_cols) == 0) {
        dup_cols <- names(df)
      }
      
      if (input$dup_action == "keep_first") {
        df <- df %>% distinct(across(all_of(dup_cols)), .keep_all = TRUE)
      }
      
      if (input$dup_action == "keep_last") {
        df <- df %>%
          mutate(.row_id_temp = row_number()) %>%
          arrange(desc(.row_id_temp)) %>%
          distinct(across(all_of(dup_cols)), .keep_all = TRUE) %>%
          arrange(.row_id_temp) %>%
          select(-.row_id_temp)
      }
      
      if (input$dup_action == "remove_all") {
        df <- df %>%
          add_count(across(all_of(dup_cols)), name = ".dup_count_temp") %>%
          filter(.dup_count_temp == 1) %>%
          select(-.dup_count_temp)
      }
    }
    
    df
  })
  
  # -------- Outlier Summary --------
  outlier_summary <- reactive({
    df <- clean_data()
    req(df)
    
    outlier_cols <- input$outlier_cols
    if (is.null(outlier_cols) || length(outlier_cols) == 0) {
      outlier_cols <- names(df)[sapply(df, is.numeric)]
    }
    
    if (length(outlier_cols) == 0) {
      return(data.frame(Message = "No numeric columns available.", stringsAsFactors = FALSE))
    }
    
    data.frame(
      Variable = outlier_cols,
      Outlier_Count = sapply(
        outlier_cols,
        function(col) sum(iqr_outlier_flag(df[[col]]), na.rm = TRUE)
      ),
      stringsAsFactors = FALSE
    )
  })
  
  # -------- Outlier Handling --------
  outlier_handled_data <- reactive({
    df <- clean_data()
    req(df)
    
    if (!isTRUE(input$handle_outliers)) return(df)
    
    outlier_cols <- input$outlier_cols
    if (is.null(outlier_cols) || length(outlier_cols) == 0) {
      outlier_cols <- names(df)[sapply(df, is.numeric)]
    }
    
    if (input$outlier_action == "identify_only") return(df)
    
    if (input$outlier_action == "remove_rows" && length(outlier_cols) > 0) {
      flag_matrix <- sapply(outlier_cols, function(col) iqr_outlier_flag(df[[col]]))
      if (is.vector(flag_matrix)) {
        flag_matrix <- matrix(flag_matrix, ncol = 1)
      }
      remove_flag <- apply(flag_matrix, 1, any, na.rm = TRUE)
      df <- df[!remove_flag, , drop = FALSE]
    }
    
    if (input$outlier_action == "winsorize" && length(outlier_cols) > 0) {
      for (col in outlier_cols) df[[col]] <- iqr_winsorise(df[[col]])
      showNotification(
        paste("Winsorized", length(outlier_cols), "column(s): values capped at IQR bounds."),
        type = "message", duration = 5
      )
    }
    
    df
  })
  
  # -------- Categorical Encoding --------
  encoded_data <- reactive({
    df <- outlier_handled_data()
    req(df)
    
    if (!isTRUE(input$apply_encoding)) return(df)
    
    var <- input$cat_var
    if (is.null(var) || !(var %in% names(df))) return(df)
    if (!is_categorical_col(df[[var]])) return(df)
    
    x <- as.character(df[[var]])
    
    if (input$encoding_method == "label") {
      new_name <- trimws(input$label_var_name)
      if (is.null(new_name) || new_name == "") {
        new_name <- paste0(var, "_label")
      }
      
      encoded_vals <- as.integer(factor(x, exclude = NULL))
      encoded_vals[is.na(x)] <- NA_integer_
      df[[new_name]] <- encoded_vals
      return(df)
    }
    
    if (input$encoding_method == "onehot") {
      x_factor <- factor(x, exclude = NULL)
      dummy_mat <- model.matrix(~ x_factor - 1)
      dummy_df <- as.data.frame(dummy_mat)
      
      new_names <- sub("^x_factor", paste0(var, "_"), names(dummy_df))
      new_names <- make_clean_names(new_names)
      names(dummy_df) <- new_names
      
      if (isTRUE(input$drop_first_dummy) && ncol(dummy_df) > 1) {
        dummy_df <- dummy_df[, -1, drop = FALSE]
      }
      
      df[[var]] <- NULL
      df <- bind_cols(df, dummy_df)
      return(df)
    }
    
    df
  })
  
  # -------- Feature Engineering + Scaling (before filtering) --------
  pre_filter_data <- reactive({
    df <- encoded_data()
    req(df)
    
    if (isTRUE(input$apply_feature_engineering)) {
      method <- input$feature_method
      var1 <- input$num_var
      var2 <- input$num_var_2
      new_name <- trimws(input$new_var)
      
      if (is.null(new_name) || new_name == "") {
        new_name <- "engineered_var"
      }
      
      if (!is.null(var1) && var1 %in% names(df) && is.numeric(df[[var1]])) {
        
        if (method == "log") {
          df[[new_name]] <- ifelse(df[[var1]] > -1, log(df[[var1]] + 1), NA_real_)
        }
        
        if (method == "sqrt") {
          df[[new_name]] <- ifelse(df[[var1]] >= 0, sqrt(df[[var1]]), NA_real_)
        }
        
        if (method == "square") {
          df[[new_name]] <- df[[var1]] ^ 2
        }
        
        if (method == "interaction" &&
            !is.null(var2) &&
            var2 %in% names(df) &&
            is.numeric(df[[var2]])) {
          df[[new_name]] <- df[[var1]] * df[[var2]]
        }
        
        if (method == "binning") {
          n_bins <- input$bin_count
          if (is.null(n_bins) || is.na(n_bins) || n_bins < 2) {
            n_bins <- 4
          }
          
          x <- df[[var1]]
          if (length(unique(x[!is.na(x)])) >= 2) {
            df[[new_name]] <- cut(
              x,
              breaks = n_bins,
              include.lowest = TRUE,
              ordered_result = TRUE
            )
          }
        }
      }
    }
    
    if (isTRUE(input$apply_scaling) &&
        !is.null(input$scale_var) &&
        input$scale_var %in% names(df) &&
        is.numeric(df[[input$scale_var]])) {
      
      scaled_name <- trimws(input$scaled_var_name)
      if (!is.null(scaled_name) && scaled_name != "") {
        x <- df[[input$scale_var]]
        
        if (input$scale_method == "zscore") {
          s <- sd(x, na.rm = TRUE)
          if (!is.na(s) && s != 0) {
            df[[scaled_name]] <- as.numeric((x - mean(x, na.rm = TRUE)) / s)
          } else {
            showNotification(
              paste0("Scaling skipped: '", input$scale_var, "' has zero or undefined standard deviation (constant column)."),
              type = "warning",
              duration = 6
            )
          }
        }
        
        if (input$scale_method == "minmax") {
          xmin <- min(x, na.rm = TRUE)
          xmax <- max(x, na.rm = TRUE)
          if (!is.na(xmin) && !is.na(xmax) && xmax != xmin) {
            df[[scaled_name]] <- as.numeric((x - xmin) / (xmax - xmin))
          } else {
            showNotification(
              paste0("Scaling skipped: '", input$scale_var, "' is a constant column (min == max)."),
              type = "warning",
              duration = 6
            )
          }
        }
      }
    }
    
    df
  })
  
  # -------- Final Data (after filtering) --------
  feature_data <- reactive({
    df <- pre_filter_data()
    req(df)
    
    if (isTRUE(input$apply_filter) &&
        !is.null(input$filter_var) &&
        !is.null(input$filter_value) &&
        input$filter_var %in% names(df)) {
      
      filter_col <- df[[input$filter_var]]
      df <- df[
        !is.na(filter_col) &
          as.character(filter_col) == as.character(input$filter_value),
        ,
        drop = FALSE
      ]
    }
    
    df
  })
  
  # -------- Comparison choices --------
  comparison_choices <- reactive({
    df <- pre_filter_data()
    req(df)
    
    choices <- c("None" = "none")
    
    new_name <- trimws(input$new_var)
    scaled_name <- trimws(input$scaled_var_name)
    
    if (isTRUE(input$apply_feature_engineering) &&
        !is.null(input$num_var) &&
        input$num_var %in% names(df) &&
        !is.null(new_name) &&
        new_name != "" &&
        new_name %in% names(df)) {
      choices["Feature Engineering"] <- "feature"
    }
    
    if (isTRUE(input$apply_scaling) &&
        !is.null(input$scale_var) &&
        input$scale_var %in% names(df) &&
        !is.null(scaled_name) &&
        scaled_name != "" &&
        scaled_name %in% names(df)) {
      choices["Scaling"] <- "scaling"
    }
    
    choices
  })
  
  # -------- Update UI Choices --------
  observe({
    df_raw <- raw_data()
    req(df_raw)
    
    all_vars <- names(df_raw)
    numeric_raw_vars <- all_vars[sapply(df_raw, is.numeric)]
    categorical_vars <- all_vars[sapply(df_raw, is_categorical_col)]
    
    df_pre_filter <- pre_filter_data()
    feature_names <- names(df_pre_filter)
    numeric_feature_vars <- feature_names[sapply(df_pre_filter, is.numeric)]
    categorical_feature_vars <- feature_names[sapply(df_pre_filter, is_categorical_col)]
    
    updateSelectInput(session, "na_cols", choices = all_vars, selected = all_vars)
    updateSelectInput(session, "dup_cols", choices = all_vars, selected = all_vars)
    
    updateSelectInput(
      session, "outlier_cols",
      choices = numeric_raw_vars,
      selected = numeric_raw_vars
    )
    
    updateSelectInput(
      session, "cat_var",
      choices = categorical_vars,
      selected = if (length(categorical_vars) > 0) categorical_vars[1] else NULL
    )
    
    updateSelectInput(
      session, "num_var",
      choices = numeric_feature_vars,
      selected = if (length(numeric_feature_vars) > 0) numeric_feature_vars[1] else NULL
    )
    
    updateSelectInput(
      session, "num_var_2",
      choices = numeric_feature_vars,
      selected = if (length(numeric_feature_vars) > 1) numeric_feature_vars[2]
      else if (length(numeric_feature_vars) > 0) numeric_feature_vars[1]
      else NULL
    )
    
    updateSelectInput(
      session, "scale_var",
      choices = numeric_feature_vars,
      selected = if (length(numeric_feature_vars) > 0) numeric_feature_vars[1] else NULL
    )
    
    updateSelectInput(
      session, "xvar",
      choices = feature_names,
      selected = if (length(feature_names) > 0) feature_names[1] else NULL
    )
    
    updateSelectInput(
      session, "yvar",
      choices = feature_names,
      selected = if (length(feature_names) > 1) feature_names[2]
      else if (length(feature_names) > 0) feature_names[1]
      else NULL
    )
    
    updateSelectInput(
      session, "color_var",
      choices = c("None" = "none", feature_names),
      selected = "none"
    )
    
    updateSelectInput(
      session, "box_cat_var",
      choices = categorical_feature_vars,
      selected = if (length(categorical_feature_vars) > 0) categorical_feature_vars[1] else NULL
    )
    
    updateSelectInput(
      session, "box_num_var",
      choices = numeric_feature_vars,
      selected = if (length(numeric_feature_vars) > 0) numeric_feature_vars[1] else NULL
    )
    
    updateSelectInput(
      session, "bar_var",
      choices = categorical_feature_vars,
      selected = if (length(categorical_feature_vars) > 0) categorical_feature_vars[1] else NULL
    )
    
    updateSelectInput(
      session, "filter_var",
      choices = feature_names,
      selected = if (length(feature_names) > 0) feature_names[1] else NULL
    )
    
    comp_choices <- comparison_choices()
    selected_comp <- if (!is.null(input$comparison_target) &&
                         input$comparison_target %in% unname(comp_choices)) {
      input$comparison_target
    } else {
      "none"
    }
    
    updateSelectInput(
      session,
      "comparison_target",
      choices = comp_choices,
      selected = selected_comp
    )
  })
  
  # -------- Update Filter Values --------
  observe({
    df <- pre_filter_data()
    req(df, input$filter_var)
    
    if (input$filter_var %in% names(df)) {
      vals <- unique(df[[input$filter_var]])
      vals <- vals[!is.na(vals)]
      vals <- as.character(vals)
      
      default_val <- if (length(vals) > 0) vals[1] else NULL
      
      updateSelectInput(
        session,
        "filter_value",
        choices = vals,
        selected = default_val
      )
    }
  })
  
  # -------- Comparison info --------
  comparison_info <- reactive({
    df <- pre_filter_data()
    req(df)
    
    if (is.null(input$comparison_target) || input$comparison_target == "none") {
      return(NULL)
    }
    
    if (input$comparison_target == "feature") {
      before_var <- input$num_var
      after_var <- trimws(input$new_var)
      
      if (!is.null(before_var) &&
          !is.null(after_var) &&
          before_var %in% names(df) &&
          after_var %in% names(df)) {
        return(list(
          label = "Feature Engineering",
          before_var = before_var,
          after_var = after_var
        ))
      }
    }
    
    if (input$comparison_target == "scaling") {
      before_var <- input$scale_var
      after_var <- trimws(input$scaled_var_name)
      
      if (!is.null(before_var) &&
          !is.null(after_var) &&
          before_var %in% names(df) &&
          after_var %in% names(df)) {
        return(list(
          label = "Scaling",
          before_var = before_var,
          after_var = after_var
        ))
      }
    }
    
    NULL
  })
  
  # -------- Dynamic UI: dataset info bar (sidebar) --------
  output$dataset_info_bar <- renderUI({
    df <- raw_data()
    if (is.null(df)) return(NULL)
    tags$div(class = "info-strip",
             icon("table"), " ",
             tags$strong(nrow(df)), " rows \u00d7 ",
             tags$strong(ncol(df)), " columns  |  ",
             tags$strong(sum(is.na(df))), " missing"
    )
  })
  
  # -------- Dynamic UI: info boxes (Data Preview tab) --------
  output$info_boxes <- renderUI({
    df <- feature_data()
    if (is.null(df)) {
      return(box(width = 12,
                 "No dataset loaded. Upload a file or select a built-in dataset."))
    }
    fluidRow(
      infoBox("Rows",      nrow(df),                         icon = icon("list"),    color = "blue",   fill = TRUE),
      infoBox("Columns",   ncol(df),                         icon = icon("columns"), color = "green",  fill = TRUE),
      infoBox("Missing",   sum(is.na(df)),                   icon = icon("question"),color = "yellow", fill = TRUE),
      infoBox("Numeric",   sum(sapply(df, is.numeric)),       icon = icon("hashtag"), color = "purple", fill = TRUE)
    )
  })
  
  # -------- Dynamic UI: dataset info strip --------
  output$dataset_info_strip <- renderUI({
    df <- raw_data()
    if (is.null(df)) return(NULL)
    tags$div(class = "info-strip",
             icon("table"), " ",
             tags$strong(nrow(df)), " rows × ",
             tags$strong(ncol(df)), " columns  |  ",
             tags$strong(sum(is.na(df))), " missing values"
    )
  })
  
  # -------- Dynamic UI: info strip top of Data Preview --------
  output$info_strip_top <- renderUI({
    df <- feature_data()
    if (is.null(df)) return(NULL)
    tags$div(class = "info-strip", style = "margin-bottom:10px",
             icon("database"),
             tags$strong(nrow(df)), " rows × ",
             tags$strong(ncol(df)), " columns  |  ",
             tags$strong(sum(sapply(df, is.numeric))), " numeric  |  ",
             tags$strong(sum(sapply(df, is_categorical_col))), " categorical  |  ",
             tags$strong(sum(is.na(df))), " missing"
    )
  })
  
  # -------- Dynamic UI: FE real-time preview --------
  output$fe_preview <- renderUI({
    req(isTRUE(input$apply_feature_engineering))
    df  <- pre_filter_data()
    nm  <- trimws(input$new_var)
    if (is.null(df) || nm == "" || !(nm %in% names(df))) return(NULL)
    x <- df[[nm]]
    if (!is.numeric(x)) {
      return(tags$div(class = "preview-box",
                      tags$strong(nm), " created (factor / binned)"
      ))
    }
    tags$div(class = "preview-box",
             tags$strong(nm), " — ",
             sprintf("mean = %.3f  |  sd = %.3f  |  range [%.3f, %.3f]",
                     mean(x, na.rm = TRUE), sd(x, na.rm = TRUE),
                     min(x, na.rm = TRUE), max(x, na.rm = TRUE))
    )
  })
  
  # -------- Dynamic UI: Scaling real-time preview --------
  output$scale_preview <- renderUI({
    req(isTRUE(input$apply_scaling))
    df  <- pre_filter_data()
    sn  <- trimws(input$scaled_var_name)
    if (is.null(df) || sn == "" || !(sn %in% names(df))) return(NULL)
    x <- df[[sn]]
    if (!is.numeric(x)) return(NULL)
    tags$div(class = "preview-box",
             tags$strong(sn), " — ",
             sprintf("mean = %.3f  |  sd = %.3f  |  range [%.3f, %.3f]",
                     mean(x, na.rm = TRUE), sd(x, na.rm = TRUE),
                     min(x, na.rm = TRUE), max(x, na.rm = TRUE))
    )
  })
  
  # -------- Dynamic UI: Filter row count badge --------
  output$filter_row_count <- renderUI({
    req(isTRUE(input$apply_filter))
    df_all  <- pre_filter_data()
    df_filt <- feature_data()
    if (is.null(df_all) || is.null(df_filt)) return(NULL)
    pct <- if (nrow(df_all) > 0) round(100 * nrow(df_filt) / nrow(df_all)) else 0
    tags$div(style = "margin-top:6px",
             tags$span(class = "filter-badge",
                       paste0(nrow(df_filt), " / ", nrow(df_all), " rows kept (", pct, "%)"))
    )
  })
  
  # -------- Outputs --------
  output$table <- renderDT({
    datatable(
      feature_data(),
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  output$summary <- renderPrint({
    summary(feature_data())
  })
  
  output$na_stats <- renderPrint({
    df <- raw_data()
    req(df)
    
    cat("Total missing values:", sum(is.na(df)), "\n")
    cat("Columns with missing values:", sum(sapply(df, function(x) any(is.na(x)))), "\n")
    
    if (isTRUE(input$handle_na)) {
      cat("Current missing-value handling method:", input$na_action, "\n")
    } else {
      cat("Missing-value handling is currently off.\n")
    }
  })
  
  output$missing_table <- renderDT({
    datatable(
      missing_summary(),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$dup_stats <- renderPrint({
    df_dup <- duplicate_summary()
    req(df_dup)
    
    total_rows <- nrow(df_dup)
    dup_rows <- sum(df_dup$duplicate_flag, na.rm = TRUE)
    unique_rows <- total_rows - dup_rows
    
    cat("Total rows:", total_rows, "\n")
    cat("Rows involved in duplicates:", dup_rows, "\n")
    cat("Rows not duplicated:", unique_rows, "\n")
    
    if (isTRUE(input$handle_dup)) {
      cat("Current duplicate handling method:", input$dup_action, "\n")
    } else {
      cat("Duplicate handling is currently off.\n")
    }
  })
  
  output$dup_table <- renderDT({
    df_dup <- duplicate_summary()
    req(df_dup)
    
    dup_only <- df_dup %>% filter(duplicate_flag)
    
    if (nrow(dup_only) == 0) {
      dup_only <- data.frame(
        Message = "No duplicate rows found based on selected columns.",
        stringsAsFactors = FALSE
      )
    }
    
    datatable(
      dup_only,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$outlier_stats <- renderPrint({
    outlier_df <- outlier_summary()
    
    if ("Message" %in% names(outlier_df)) {
      cat(outlier_df$Message[1], "\n")
    } else {
      cat("Columns checked for outliers:", nrow(outlier_df), "\n")
      cat("Total outliers detected:", sum(outlier_df$Outlier_Count, na.rm = TRUE), "\n")
    }
    
    if (isTRUE(input$handle_outliers)) {
      cat("Current outlier handling method:", input$outlier_action, "\n")
    } else {
      cat("Outlier handling is currently off.\n")
    }
  })
  
  output$outlier_table <- renderDT({
    datatable(
      outlier_summary(),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$comparison_note <- renderPrint({
    info <- comparison_info()
    
    if (is.null(info)) {
      cat("No transformation comparison selected.\n")
    } else {
      cat("Current comparison:", info$label, "\n")
      cat("Before variable:", info$before_var, "\n")
      cat("After variable:", info$after_var, "\n")
      
      df <- pre_filter_data()
      if (!(is.numeric(df[[info$before_var]]) && is.numeric(df[[info$after_var]]))) {
        cat("Histogram comparison is only available when both variables are numeric.\n")
      }
    }
  })
  
  output$comparison_table <- renderDT({
    info <- comparison_info()
    
    if (is.null(info)) {
      return(datatable(
        data.frame(Message = "No transformation comparison selected.", stringsAsFactors = FALSE),
        options = list(dom = "t")
      ))
    }
    
    df <- pre_filter_data()
    
    before_summary <- make_variable_summary(df[[info$before_var]], info$before_var, "Before")
    after_summary <- make_variable_summary(df[[info$after_var]], info$after_var, "After")
    
    comparison_df <- bind_rows(before_summary, after_summary)
    
    datatable(
      comparison_df,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$comparison_hist <- renderPlotly({
    info <- comparison_info()
    
    if (is.null(info)) {
      return(plotly_empty() %>% layout(title = "No transformation comparison selected."))
    }
    
    df <- pre_filter_data()
    
    before_x <- df[[info$before_var]]
    after_x <- df[[info$after_var]]
    
    if (!(is.numeric(before_x) && is.numeric(after_x))) {
      return(
        plotly_empty() %>%
          layout(title = "Before vs After histogram is available only for numeric transformations.")
      )
    }
    
    plot_df <- data.frame(
      Value = c(before_x, after_x),
      Stage = c(
        rep(paste0("Before: ", info$before_var), length(before_x)),
        rep(paste0("After: ", info$after_var), length(after_x))
      )
    )
    
    p <- ggplot(plot_df, aes(x = Value, fill = Stage)) +
      geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
      facet_wrap(~ Stage, scales = "free") +
      theme_minimal() +
      labs(
        title = paste("Before vs After Histogram -", info$label),
        x = "Value",
        y = "Count"
      ) +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$hist <- renderPlotly({
    df <- feature_data()
    req(input$xvar)
    
    if (!(input$xvar %in% names(df))) {
      return(plotly_empty() %>% layout(title = "Please select a valid X variable."))
    }
    
    if (!is.numeric(df[[input$xvar]])) {
      return(plotly_empty() %>% layout(title = "Histogram requires a numeric variable."))
    }
    
    bins     <- if (is.null(input$hist_bins) || is.na(input$hist_bins)) 30 else input$hist_bins
    color_v  <- if (!is.null(input$color_var) && input$color_var != "none" &&
                    input$color_var %in% names(df)) input$color_var else NULL
    
    p <- if (!is.null(color_v)) {
      ggplot(df, aes(x = .data[[input$xvar]], fill = .data[[color_v]])) +
        geom_histogram(bins = bins, alpha = 0.7, position = "identity")
    } else {
      ggplot(df, aes(x = .data[[input$xvar]])) +
        geom_histogram(bins = bins, fill = "steelblue", color = "white")
    }
    
    p <- p + theme_minimal() +
      labs(title = paste("Histogram of", input$xvar), x = input$xvar, y = "Count")
    
    ggplotly(p)
  })
  
  output$scatter <- renderPlotly({
    df <- feature_data()
    req(input$xvar, input$yvar)
    
    if (!(input$xvar %in% names(df)) || !(input$yvar %in% names(df))) {
      return(plotly_empty() %>% layout(title = "Please select valid variables."))
    }
    
    if (!is.numeric(df[[input$xvar]]) || !is.numeric(df[[input$yvar]])) {
      return(plotly_empty() %>% layout(title = "Scatter plot requires both X and Y to be numeric."))
    }
    
    color_v <- if (!is.null(input$color_var) && input$color_var != "none" &&
                   input$color_var %in% names(df)) input$color_var else NULL
    
    p <- if (!is.null(color_v)) {
      ggplot(df, aes(x = .data[[input$xvar]], y = .data[[input$yvar]],
                     color = .data[[color_v]])) +
        geom_point(alpha = 0.7)
    } else {
      ggplot(df, aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) +
        geom_point(color = "darkred", alpha = 0.7)
    }
    
    p <- p + theme_minimal() +
      labs(title = paste("Scatter Plot:", input$xvar, "vs", input$yvar),
           x = input$xvar, y = input$yvar)
    
    ggplotly(p)
  })
  
  output$grouped_boxplot <- renderPlotly({
    df <- feature_data()
    req(input$box_cat_var, input$box_num_var)
    
    if (!(input$box_cat_var %in% names(df)) || !(input$box_num_var %in% names(df))) {
      return(plotly_empty() %>% layout(title = "Please select valid variables."))
    }
    
    if (!is_categorical_col(df[[input$box_cat_var]])) {
      return(plotly_empty() %>% layout(title = "Grouped boxplot requires a categorical grouping variable."))
    }
    
    if (!is.numeric(df[[input$box_num_var]])) {
      return(plotly_empty() %>% layout(title = "Grouped boxplot requires a numeric variable to summarize."))
    }
    
    plot_df <- df %>%
      filter(!is.na(.data[[input$box_cat_var]]), !is.na(.data[[input$box_num_var]]))
    
    if (nrow(plot_df) == 0) {
      return(plotly_empty() %>% layout(title = "No non-missing observations available for this grouped boxplot."))
    }
    
    p <- ggplot(
      plot_df,
      aes(
        x = .data[[input$box_cat_var]],
        y = .data[[input$box_num_var]],
        fill = .data[[input$box_cat_var]]
      )
    ) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.6) +
      theme_minimal() +
      labs(
        title = paste("Grouped Boxplot:", input$box_num_var, "by", input$box_cat_var),
        x = input$box_cat_var,
        y = input$box_num_var
      ) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p)
  })
  
  # -------- Violin Plot --------
  output$violin <- renderPlotly({
    df <- feature_data()
    req(input$box_cat_var, input$box_num_var)
    
    if (!(input$box_cat_var %in% names(df)) || !(input$box_num_var %in% names(df)) ||
        !is_categorical_col(df[[input$box_cat_var]]) || !is.numeric(df[[input$box_num_var]])) {
      return(plotly_empty() %>% layout(title = "Select a categorical group and a numeric variable."))
    }
    
    plot_df <- df %>%
      filter(!is.na(.data[[input$box_cat_var]]), !is.na(.data[[input$box_num_var]]))
    
    if (nrow(plot_df) == 0)
      return(plotly_empty() %>% layout(title = "No data available."))
    
    p <- ggplot(plot_df,
                aes(x = .data[[input$box_cat_var]], y = .data[[input$box_num_var]],
                    fill = .data[[input$box_cat_var]])) +
      geom_violin(alpha = 0.6, trim = FALSE) +
      geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
      theme_minimal() +
      labs(title = paste("Violin Plot:", input$box_num_var, "by", input$box_cat_var),
           x = input$box_cat_var, y = input$box_num_var) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # -------- Density Plot --------
  output$density <- renderPlotly({
    df <- feature_data()
    req(input$xvar)
    
    if (!(input$xvar %in% names(df)) || !is.numeric(df[[input$xvar]])) {
      return(plotly_empty() %>% layout(title = "Density plot requires a numeric X variable."))
    }
    
    color_v <- if (!is.null(input$color_var) && input$color_var != "none" &&
                   input$color_var %in% names(df)) input$color_var else NULL
    
    p <- if (!is.null(color_v)) {
      ggplot(df, aes(x = .data[[input$xvar]], fill = .data[[color_v]])) +
        geom_density(alpha = 0.4)
    } else {
      ggplot(df, aes(x = .data[[input$xvar]])) +
        geom_density(fill = "steelblue", alpha = 0.5)
    }
    
    p <- p + theme_minimal() +
      labs(title = paste("Density of", input$xvar), x = input$xvar, y = "Density")
    
    ggplotly(p)
  })
  
  # -------- Bar Chart --------
  output$bar_chart <- renderPlotly({
    df <- feature_data()
    req(input$bar_var)
    
    if (!(input$bar_var %in% names(df)) || !is_categorical_col(df[[input$bar_var]])) {
      return(plotly_empty() %>% layout(title = "Bar chart requires a categorical variable."))
    }
    
    bar_df <- df %>%
      filter(!is.na(.data[[input$bar_var]])) %>%
      count(.data[[input$bar_var]], name = "Count") %>%
      arrange(desc(Count))
    names(bar_df)[1] <- "Category"
    
    p <- ggplot(bar_df, aes(x = reorder(Category, -Count), y = Count,
                            fill = Category,
                            text = paste0(Category, ": ", Count))) +
      geom_col(show.legend = FALSE) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste("Counts of", input$bar_var),
           x = input$bar_var, y = "Count")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$corr_heatmap <- renderPlotly({
    df <- feature_data()
    req(df)
    
    num_df <- df %>% select(where(is.numeric))
    
    if (ncol(num_df) < 2) {
      return(
        plotly_empty() %>%
          layout(title = "Need at least 2 numeric variables to compute correlation.")
      )
    }
    
    corr_mat <- cor(num_df, use = "pairwise.complete.obs")
    corr_long <- as.data.frame(as.table(corr_mat))
    names(corr_long) <- c("Var1", "Var2", "Correlation")
    
    p <- ggplot(
      corr_long,
      aes(
        x = Var1,
        y = Var2,
        fill = Correlation,
        text = paste0(
          "Var1: ", Var1,
          "<br>Var2: ", Var2,
          "<br>Correlation: ", round(Correlation, 3)
        )
      )
    ) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Correlation, 2)), size = 4) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
      theme_minimal() +
      labs(title = "Correlation Heatmap", x = "", y = "") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$download <- downloadHandler(
    filename = function() {
      "cleaned_data.csv"
    },
    content = function(file) {
      write.csv(feature_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)