# Load Libraries
library(shiny)
library(tidyverse)
library(DT)
library(caret)  # For model training

# UI
ui <- fluidPage(
  titlePanel("Data Analysis App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      selectInput("xcol", "Select X-axis Variable", choices = NULL),
      selectInput("ycol", "Select Y-axis Variable", choices = NULL),
      selectInput("plot_type", "Select Plot Type", 
                  choices = c("Scatter Plot" = "scatter", 
                              "Bar Plot" = "bar", 
                              "Histogram" = "hist", 
                              "Box Plot" = "box")),
      actionButton("analyze", "Analyze Data"),
      actionButton("train_model", "Train Model")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data View", DTOutput("data_table")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary")),
        tabPanel("Visualization", plotOutput("plot")),
        tabPanel("Model Accuracy", verbatimTextOutput("accuracy"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive Expression to Load Data
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Update Variable Choices for X and Y Axes
  observe({
    req(data())
    updateSelectInput(session, "xcol", choices = names(data()))
    updateSelectInput(session, "ycol", choices = names(data()))
  })
  
  # Display Data Table
  output$data_table <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 10))
  })
  
  # Display Summary Statistics
  output$summary <- renderPrint({
    req(data())
    summary(data())
  })
  
  # Generate Plot Based on User Input
  output$plot <- renderPlot({
    req(data(), input$xcol, input$plot_type)
    plot_data <- data()
    
    if (input$plot_type == "scatter") {
      req(input$ycol)
      ggplot(plot_data, aes_string(x = input$xcol, y = input$ycol)) +
        geom_point(color = "blue") +
        labs(title = "Scatter Plot", x = input$xcol, y = input$ycol)
      
    } else if (input$plot_type == "bar") {
      ggplot(plot_data, aes_string(x = input$xcol)) +
        geom_bar(fill = "lightblue") +
        labs(title = "Bar Plot", x = input$xcol, y = "Count")
      
    } else if (input$plot_type == "hist") {
      ggplot(plot_data, aes_string(x = input$xcol)) +
        geom_histogram(fill = "skyblue", color = "black", bins = 30) +
        labs(title = "Histogram", x = input$xcol, y = "Frequency")
      
    } else if (input$plot_type == "box") {
      ggplot(plot_data, aes_string(x = input$xcol)) +
        geom_boxplot(fill = "lightgreen") +
        labs(title = "Box Plot", x = input$xcol)
    }
  })
  
  # Train Model and Calculate Accuracy
  observeEvent(input$train_model, {
    req(data())
    
    # Check if the data has a valid response column (dependent variable)
    if (input$ycol %in% names(data())) {
      
      # Prepare data for training (removing NA values)
      model_data <- data() %>%
        select(input$xcol, input$ycol) %>%
        na.omit()
      
      # Train a logistic regression model (as an example)
      model <- glm(as.formula(paste(input$ycol, "~", input$xcol)), 
                   data = model_data, 
                   family = "binomial")
      
      # Predict using the trained model
      predictions <- predict(model, model_data, type = "response")
      predicted_class <- ifelse(predictions > 0.5, 1, 0)
      
      # Calculate accuracy
      actual_class <- model_data[[input$ycol]]
      accuracy <- mean(predicted_class == actual_class)
      
      # Display the accuracy in the output
      output$accuracy <- renderPrint({
        cat("Model Accuracy: ", round(accuracy * 100, 2), "%")
      })
    } else {
      output$accuracy <- renderPrint({
        cat("Invalid Y-axis variable selected.")
      })
    }
  })
}

# Run the Application
shinyApp(ui = ui, server = server)
