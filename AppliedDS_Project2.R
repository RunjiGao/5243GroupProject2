library(shiny)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr)
library(scales)

ui <- fluidPage(
  titlePanel("DataPilot: EDA & Preprocessing App"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Upload Data"),
      fileInput("file", "Upload CSV or Excel", 
                accept = c(".csv", ".xlsx")),
      selectInput("builtin", "Or use built-in dataset",
                  choices = c("None", "iris", "mtcars")),
      
      hr(),
      
      h4("2. Cleaning"),
      checkboxInput("remove_na", "Remove NA rows"),
      checkboxInput("remove_dup", "Remove duplicates"),
      
      hr(),
      
      h4("3. Feature Engineering"),
      selectInput("num_var", "Select numeric variable",
                  choices = NULL),
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
  
  # -------- Load Data --------
  raw_data <- reactive({
    
    if (input$builtin != "None") {
      return(get(input$builtin))
    }
    
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    
    if (ext == "csv") {
      df <- read_csv(input$file$datapath, show_col_types = FALSE)
    } else if (ext == "xlsx") {
      df <- read_excel(input$file$datapath)
    } else {
      return(NULL)
    }
    
    return(as.data.frame(df))
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
    
    if (!is.null(var) && var %in% colnames(df) && input$log_transform) {
      if (is.numeric(df[[var]]) && !is.null(new_name) && new_name != "") {
        df[[new_name]] <- log(df[[var]] + 1)
      }
    }
    
    df
  })
  
  # -------- Numeric Variables --------
  numeric_vars <- reactive({
    df <- feature_data()
    req(df)
    names(df)[sapply(df, is.numeric)]
  })
  
  # -------- Update UI Choices --------
  observe({
    df <- feature_data()
    req(df)
    
    all_vars <- names(df)
    num_vars <- names(df)[sapply(df, is.numeric)]
    
    updateSelectInput(session, "num_var",
                      choices = num_vars,
                      selected = if (length(num_vars) > 0) num_vars[1] else NULL)
    
    updateSelectInput(session, "xvar",
                      choices = all_vars,
                      selected = if (length(all_vars) > 0) all_vars[1] else NULL)
    
    updateSelectInput(session, "yvar",
                      choices = all_vars,
                      selected = if (length(all_vars) > 1) all_vars[2] else all_vars[1])
  })
  
  # -------- Table --------
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
      return(
        plotly_empty() %>%
          layout(title = "Please select a valid X variable.")
      )
    }
    
    if (!is.numeric(df[[input$xvar]])) {
      return(
        plotly_empty() %>%
          layout(title = "Histogram requires a numeric variable.")
      )
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
      return(
        plotly_empty() %>%
          layout(title = "Please select valid variables.")
      )
    }
    
    if (!is.numeric(df[[input$xvar]]) || !is.numeric(df[[input$yvar]])) {
      return(
        plotly_empty() %>%
          layout(title = "Scatter plot requires both X and Y to be numeric.")
      )
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
  
  # -------- Correlation Heatmap Data --------
  corr_data <- reactive({
    df <- feature_data()
    req(df)
    
    num_df <- df %>% select(where(is.numeric))
    
    validate(
      need(ncol(num_df) >= 2, "Need at least 2 numeric variables to compute correlation.")
    )
    
    corr_mat <- cor(num_df, use = "pairwise.complete.obs")
    
    corr_long <- as.data.frame(as.table(corr_mat))
    names(corr_long) <- c("Var1", "Var2", "Correlation")
    
    corr_long
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