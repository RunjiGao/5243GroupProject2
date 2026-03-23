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
        choices = c("None", "iris", "mtcars")
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
      
      h4("4. Feature Engineering"),
      selectInput("num_var", "Select numeric variable", choices = NULL),
      checkboxInput("log_transform", "Log transform"),
      textInput("new_var", "New variable name", "new_var"),
      
      hr(),
      
      h4("5. Visualization"),
      selectInput("xvar", "X variable", choices = NULL),
      selectInput("yvar", "Y variable", choices = NULL),
      
      hr(),
      
      downloadButton("download", "Download Data")
    ),
    
    mainPanel(
      tabsetPanel(
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
        tabPanel("Histogram", plotlyOutput("hist")),
        tabPanel("Scatter", plotlyOutput("scatter")),
        tabPanel("Correlation Heatmap", plotlyOutput("corr_heatmap"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # -------- Helper: safely convert object to data frame --------
  to_dataframe <- function(obj) {
    if (is.data.frame(obj)) {
      return(as.data.frame(obj))
    }
    
    if (is.matrix(obj)) {
      return(as.data.frame(obj))
    }
    
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
    if (is.numeric(target_col)) {
      val <- suppressWarnings(as.numeric(fill_value))
      return(val)
    }
    if (is.integer(target_col)) {
      val <- suppressWarnings(as.integer(fill_value))
      return(val)
    }
    if (is.logical(target_col)) {
      val_lower <- tolower(trimws(fill_value))
      if (val_lower %in% c("true", "t", "1")) return(TRUE)
      if (val_lower %in% c("false", "f", "0")) return(FALSE)
      return(NA)
    }
    return(as.character(fill_value))
  }
  
  # -------- Load Data --------
  raw_data <- reactive({
    if (input$builtin != "None") {
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
    
    if (!isTRUE(input$handle_na)) {
      return(df)
    }
    
    na_cols <- input$na_cols
    if (is.null(na_cols) || length(na_cols) == 0) {
      na_cols <- names(df)
    }
    
    if (input$na_action == "identify_only") {
      return(df)
    }
    
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
    
    df_check <- df %>%
      mutate(
        duplicate_flag = duplicated(subset_df) | duplicated(subset_df, fromLast = TRUE)
      )
    
    df_check
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
      
      if (input$dup_action == "identify_only") {
        df <- df
      }
    }
    
    df
  })
  
  # -------- Feature Engineering --------
  feature_data <- reactive({
    df <- clean_data()
    req(df)
    
    var <- input$num_var
    new_name <- trimws(input$new_var)
    
    if (!is.null(var) &&
        var %in% colnames(df) &&
        input$log_transform &&
        is.numeric(df[[var]]) &&
        !is.null(new_name) &&
        new_name != "") {
      df[[new_name]] <- log(df[[var]] + 1)
    }
    
    df
  })
  
  # -------- Update UI Choices --------
  observe({
    df <- raw_data()
    req(df)
    
    all_vars <- names(df)
    num_vars <- names(feature_data())[sapply(feature_data(), is.numeric)]
    
    current_num <- isolate(input$num_var)
    current_x <- isolate(input$xvar)
    current_y <- isolate(input$yvar)
    current_dup <- isolate(input$dup_cols)
    current_na <- isolate(input$na_cols)
    
    updateSelectInput(
      session,
      "na_cols",
      choices = all_vars,
      selected = if (!is.null(current_na) && all(current_na %in% all_vars)) {
        current_na
      } else {
        all_vars
      }
    )
    
    updateSelectInput(
      session,
      "dup_cols",
      choices = all_vars,
      selected = if (!is.null(current_dup) && all(current_dup %in% all_vars)) {
        current_dup
      } else {
        all_vars
      }
    )
    
    updateSelectInput(
      session,
      "num_var",
      choices = num_vars,
      selected = if (!is.null(current_num) && current_num %in% num_vars) {
        current_num
      } else if (length(num_vars) > 0) {
        num_vars[1]
      } else {
        NULL
      }
    )
    
    updateSelectInput(
      session,
      "xvar",
      choices = names(feature_data()),
      selected = if (!is.null(current_x) && current_x %in% names(feature_data())) {
        current_x
      } else if (length(names(feature_data())) > 0) {
        names(feature_data())[1]
      } else {
        NULL
      }
    )
    
    updateSelectInput(
      session,
      "yvar",
      choices = names(feature_data()),
      selected = if (!is.null(current_y) && current_y %in% names(feature_data())) {
        current_y
      } else if (length(names(feature_data())) > 1) {
        names(feature_data())[2]
      } else if (length(names(feature_data())) > 0) {
        names(feature_data())[1]
      } else {
        NULL
      }
    )
  })
  
  # -------- Data Preview --------
  output$table <- renderDT({
    datatable(
      feature_data(),
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  # -------- Summary --------
  output$summary <- renderPrint({
    summary(feature_data())
  })
  
  # -------- Missing Stats --------
  output$na_stats <- renderPrint({
    df <- raw_data()
    req(df)
    
    total_missing <- sum(is.na(df))
    cols_with_missing <- sum(sapply(df, function(x) any(is.na(x))))
    
    cat("Total missing values:", total_missing, "\n")
    cat("Columns with missing values:", cols_with_missing, "\n")
    
    if (isTRUE(input$handle_na)) {
      cat("Current missing-value handling method:", input$na_action, "\n")
    } else {
      cat("Missing-value handling is currently off.\n")
    }
  })
  
  # -------- Missing Table --------
  output$missing_table <- renderDT({
    datatable(
      missing_summary(),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # -------- Duplicate Stats --------
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
  
  # -------- Duplicate Table --------
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
  
  # -------- Histogram --------
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
  
  # -------- Scatter --------
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
  
  # -------- Correlation Heatmap --------
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
  
  # -------- Download --------
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