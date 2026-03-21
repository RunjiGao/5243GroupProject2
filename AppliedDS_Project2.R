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
      
      h4("2. Cleaning"),
      checkboxInput("remove_na", "Remove NA rows"),
      checkboxInput("remove_dup", "Remove duplicates"),
      
      hr(),
      
      h4("3. Feature Engineering"),
      selectInput("num_var", "Select numeric variable", choices = NULL),
      checkboxInput("log_transform", "Log transform"),
      textInput("new_var", "New variable name", "new_var"),
      
      hr(),
      
      h4("4. Visualization"),
      selectInput("xvar", "X variable", choices = NULL),
      selectInput("yvar", "Y variable", choices = NULL),
      
      hr(),
      
      downloadButton("download", "Download Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", DTOutput("table")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Missing Values", DTOutput("missing_table")),
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
    
    # matrix -> data frame
    if (is.matrix(obj)) {
      return(as.data.frame(obj))
    }
    
    # list that can be converted
    try_df <- try(as.data.frame(obj), silent = TRUE)
    if (!inherits(try_df, "try-error")) {
      return(as.data.frame(try_df))
    }
    
    stop("Uploaded file could not be converted to a data frame.")
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
  
  # -------- Cleaning --------
  clean_data <- reactive({
    df <- raw_data()
    req(df)
    
    if (input$remove_na) {
      df <- na.omit(df)
    }
    
    if (input$remove_dup) {
      df <- distinct(df)
    }
    
    df
  })
  
  # -------- Feature Engineering --------
  feature_data <- reactive({
    df <- clean_data()
    req(df)
    
    var <- input$num_var
    new_name <- input$new_var
    
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
    df <- feature_data()
    req(df)
    
    all_vars <- names(df)
    num_vars <- names(df)[sapply(df, is.numeric)]
    
    updateSelectInput(
      session,
      "num_var",
      choices = num_vars,
      selected = if (length(num_vars) > 0) num_vars[1] else NULL
    )
    
    updateSelectInput(
      session,
      "xvar",
      choices = all_vars,
      selected = if (length(all_vars) > 0) all_vars[1] else NULL
    )
    
    updateSelectInput(
      session,
      "yvar",
      choices = all_vars,
      selected = if (length(all_vars) > 1) all_vars[2] else if (length(all_vars) > 0) all_vars[1] else NULL
    )
  })
  
  # -------- Data Preview --------
  output$table <- renderDT({
    datatable(feature_data(), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # -------- Summary --------
  output$summary <- renderPrint({
    summary(feature_data())
  })
  
  # -------- Missing Value Summary --------
  missing_summary <- reactive({
    df <- raw_data()
    req(df)
    
    data.frame(
      Variable = names(df),
      Missing_Count = sapply(df, function(x) sum(is.na(x))),
      Missing_Percent = sapply(df, function(x) mean(is.na(x)))
    ) %>%
      mutate(Missing_Percent = percent(Missing_Percent, accuracy = 0.1))
  })
  
  output$missing_table <- renderDT({
    datatable(missing_summary(), options = list(pageLength = 10, scrollX = TRUE))
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