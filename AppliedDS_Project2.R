library(shiny)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr)
library(scales)
library(jsonlite)

ui <- fluidPage(
  titlePanel("DataPilot: EDA & Preprocessing App"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Upload Data"),
      fileInput(
        "file",
        "Upload CSV, Excel, JSON, or RDS",
        accept = c(".csv", ".xlsx", ".json", ".rds")
      ),
      selectInput(
        "builtin",
        "Or use built-in dataset",
        choices = c("None", "iris", "mtcars"),
        selected = "iris"
      ),
      
      hr(),
      
      h4("2. Missing Values"),
      checkboxInput("handle_na", "Identify / handle missing values", value = FALSE),
      selectInput(
        "na_cols",
        "Columns used to handle missing values",
        choices = NULL,
        multiple = TRUE
      ),
      selectInput(
        "na_action",
        "Missing value handling method",
        choices = c(
          "Remove rows with missing values in selected columns" = "drop_rows",
          "Mean imputation (numeric only)" = "mean",
          "Median imputation (numeric only)" = "median",
          "Mode imputation" = "mode",
          "Fill with custom value" = "custom",
          "Only identify, do not modify" = "identify_only"
        ),
        selected = "identify_only"
      ),
      textInput("na_custom", "Custom fill value", ""),
      
      hr(),
      
      h4("3. Duplicates"),
      checkboxInput("handle_dup", "Identify / handle duplicates", value = FALSE),
      selectInput(
        "dup_cols",
        "Columns used to define duplicates",
        choices = NULL,
        multiple = TRUE
      ),
      selectInput(
        "dup_action",
        "Duplicate handling method",
        choices = c(
          "Keep first" = "keep_first",
          "Keep last" = "keep_last",
          "Remove all duplicates" = "remove_all",
          "Only identify, do not remove" = "identify_only"
        ),
        selected = "keep_first"
      ),
      
      hr(),
      
      h4("4. Outlier Handling"),
      checkboxInput("handle_outliers", "Identify / handle outliers (IQR method)", value = FALSE),
      selectInput(
        "outlier_cols",
        "Numeric columns for outlier handling",
        choices = NULL,
        multiple = TRUE
      ),
      selectInput(
        "outlier_action",
        "Outlier handling method",
        choices = c(
          "Only identify, do not modify" = "identify_only",
          "Remove rows containing outliers" = "remove_rows"
        ),
        selected = "identify_only"
      ),
      
      hr(),
      
      h4("5. Categorical Encoding"),
      checkboxInput("apply_encoding", "Apply categorical encoding", value = FALSE),
      selectInput(
        "cat_var",
        "Select categorical variable",
        choices = NULL
      ),
      selectInput(
        "encoding_method",
        "Encoding method",
        choices = c(
          "One-hot encoding" = "onehot",
          "Label encoding" = "label"
        ),
        selected = "onehot"
      ),
      checkboxInput(
        "drop_first_dummy",
        "For one-hot encoding, drop first dummy column",
        value = FALSE
      ),
      textInput(
        "label_var_name",
        "Label-encoded variable name",
        "encoded_var"
      ),
      
      hr(),
      
      h4("6. Feature Engineering"),
      checkboxInput("apply_feature_engineering", "Apply feature engineering", value = FALSE),
      selectInput(
        "feature_method",
        "Feature engineering method",
        choices = c(
          "Log transform" = "log",
          "Square root transform" = "sqrt",
          "Square transform" = "square",
          "Interaction term" = "interaction",
          "Binning" = "binning"
        ),
        selected = "log"
      ),
      selectInput("num_var", "Primary numeric variable", choices = NULL),
      selectInput("num_var_2", "Second numeric variable (for interaction)", choices = NULL),
      numericInput("bin_count", "Number of bins", value = 4, min = 2, max = 20, step = 1),
      textInput("new_var", "New variable name", "engineered_var"),
      
      hr(),
      
      h4("7. Scaling"),
      checkboxInput("apply_scaling", "Apply scaling to numeric variable", value = FALSE),
      selectInput("scale_var", "Select numeric variable to scale", choices = NULL),
      selectInput(
        "scale_method",
        "Scaling method",
        choices = c(
          "Standardization (z-score)" = "zscore",
          "Min-Max Scaling (0 to 1)" = "minmax"
        ),
        selected = "zscore"
      ),
      textInput("scaled_var_name", "Scaled variable name", "scaled_var"),
      
      hr(),
      
      h4("8. Filter Data"),
      checkboxInput("apply_filter", "Apply filter", value = FALSE),
      selectInput("filter_var", "Filter variable", choices = NULL),
      selectInput("filter_value", "Filter value", choices = NULL),
      
      hr(),
      
      h4("9. Visualization"),
      selectInput("xvar", "X variable", choices = NULL),
      selectInput("yvar", "Y variable", choices = NULL),
      
      tags$hr(),
      h5("Grouped Boxplot"),
      selectInput("box_cat_var", "Categorical variable", choices = NULL),
      selectInput("box_num_var", "Numeric variable", choices = NULL),
      
      hr(),
      
      h4("10. Transformation Comparison"),
      selectInput(
        "comparison_target",
        "Compare transformation",
        choices = c("None" = "none"),
        selected = "none"
      ),
      
      hr(),
      
      downloadButton("download", "Download Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "User Guide",
          br(),
          tags$h4("How to Use the App"),
          tags$ol(
            tags$li("Upload your own dataset or choose a built-in dataset."),
            tags$li("Inspect missing values and duplicates before cleaning."),
            tags$li("Optionally identify or remove outliers using the IQR method."),
            tags$li("Use categorical encoding for character, factor, or logical variables."),
            tags$li("Create new features using log, sqrt, square, interaction, or binning."),
            tags$li("Apply scaling to numeric variables if needed."),
            tags$li("Use transformation comparison to see before-vs-after distributions and summaries."),
            tags$li("Use filtering and visualization tools to explore the data."),
            tags$li("Use grouped boxplots to compare numeric distributions across groups."),
            tags$li("Download the processed dataset when finished.")
          ),
          tags$p("Supported file types: CSV, Excel, JSON, and RDS.")
        ),
        tabPanel("Data Preview", DTOutput("table")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel(
          "Missing Values",
          br(),
          verbatimTextOutput("na_stats"),
          DTOutput("missing_table")
        ),
        tabPanel(
          "Duplicate Summary",
          br(),
          verbatimTextOutput("dup_stats"),
          DTOutput("dup_table")
        ),
        tabPanel(
          "Outlier Summary",
          br(),
          verbatimTextOutput("outlier_stats"),
          DTOutput("outlier_table")
        ),
        tabPanel(
          "Transformation Comparison",
          br(),
          verbatimTextOutput("comparison_note"),
          DTOutput("comparison_table"),
          br(),
          plotlyOutput("comparison_hist")
        ),
        tabPanel("Histogram", plotlyOutput("hist")),
        tabPanel("Scatter", plotlyOutput("scatter")),
        tabPanel("Grouped Boxplot", plotlyOutput("grouped_boxplot")),
        tabPanel("Correlation Heatmap", plotlyOutput("corr_heatmap"))
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
  raw_data <- reactive({
    if (!is.null(input$builtin) && input$builtin != "None") {
      return(as.data.frame(get(input$builtin)))
    }
    
    req(input$file)
    
    ext <- tolower(tools::file_ext(input$file$name))
    path <- input$file$datapath
    
    df <- switch(
      ext,
      "csv" = read_csv(path, show_col_types = FALSE),
      "xlsx" = read_excel(path),
      "json" = {
        json_obj <- fromJSON(path, flatten = TRUE)
        to_dataframe(json_obj)
      },
      "rds" = {
        rds_obj <- readRDS(path)
        to_dataframe(rds_obj)
      },
      stop("Unsupported file type. Please upload CSV, XLSX, JSON, or RDS.")
    )
    
    as.data.frame(df)
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
          }
        }
        
        if (input$scale_method == "minmax") {
          xmin <- min(x, na.rm = TRUE)
          xmax <- max(x, na.rm = TRUE)
          if (!is.na(xmin) && !is.na(xmax) && xmax != xmin) {
            df[[scaled_name]] <- as.numeric((x - xmin) / (xmax - xmin))
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
      session, "filter_var",
      choices = feature_names,
      selected = if ("Species" %in% feature_names) "Species"
      else if (length(feature_names) > 0) feature_names[1]
      else NULL
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
      
      default_val <- if ("setosa" %in% vals) "setosa"
      else if (length(vals) > 0) vals[1]
      else NULL
      
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
    
    p <- ggplot(df, aes_string(x = input$xvar)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      theme_minimal() +
      labs(
        title = paste("Histogram of", input$xvar),
        x = input$xvar,
        y = "Count"
      )
    
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
    
    p <- ggplot(df, aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(color = "darkred", alpha = 0.7) +
      theme_minimal() +
      labs(
        title = paste("Scatter Plot:", input$xvar, "vs", input$yvar),
        x = input$xvar,
        y = input$yvar
      )
    
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
      aes_string(x = input$box_cat_var, y = input$box_num_var, fill = input$box_cat_var)
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