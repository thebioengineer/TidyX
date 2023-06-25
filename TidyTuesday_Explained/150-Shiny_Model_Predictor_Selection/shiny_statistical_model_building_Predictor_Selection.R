# TidyX Episode 150 - Shiny Model Building Predictor Selection & Toasts

library(tidyverse)
library(shiny)
library(DT)
library(shinyWidgets)

theme_set(theme_classic())

## No changes to the UI this week :)
ui <- fluidPage(
  
  sidebarPanel(
    
    fileInput(inputId = "file", 
              label = "Select a CSV file"),
    
    selectInput(inputId = "y_var",
                label = "Select Continuous Outcome Variable",
                choices = "",
                multiple = FALSE),
    
    selectInput(inputId = "x_vars",
                label = "Select Predictor Variables",
                choices = "",
                multiple = TRUE),
    
    sliderInput(inputId = "train_set",
                label = "What % of data to allocate to the training set",
                value = 0.75,
                min = 0,
                max = 1,
                step = 0.05)
    
  ),
  
  mainPanel(
    tabsetPanel(
      id = "datasets",
      tabPanel(
        title = "Model Results",
        verbatimTextOutput("model_summary"),
        br(),
        br(),
        fluidRow(
          column(plotOutput("resid_plt"), width = 6),
          column(plotOutput("pred_plt"), width = 6)
          )
      ),
      tabPanel(
        title = "Train",
        DTOutput("train_tbl")
      ),
      tabPanel(
        title = "Test",
        DTOutput("test_tbl")
      )
    )
  )
)


server <- function(input, output, session){
  
  input_dataset <- reactive({
    if(is.null(input$file)){
      return("")
    } else {
      read.csv(file = input$file$datapath)
    }
  })
  
  ## update y var selection options simply on file upload
  ## make first column of input_dataset() the selection
  ## same as before
  observeEvent(input_dataset(),{
    updateSelectInput(
        session,
        inputId = 'y_var',
        label = "Select Outcome Variable",
        choices = colnames(input_dataset()),
        selected = colnames(input_dataset())[[1]]
        )
  })


  # ## react based on multiple selections - new data or new input y var
  # ## simplest case, simply reset selection choices to remove new y var
  # observeEvent(c(input_dataset(), input$y_var),{
  #   updateSelectInput(
  #     session,
  #     inputId = 'x_vars',
  #     label = "Select Predictor Variables",
  #     choices = setdiff(colnames(input_dataset()), input$y_var)
  #   )
  # })
  
  ## When x_var selections are made, update this
  x_vars_selected <- reactiveVal()
  observeEvent(input$x_vars,{
    x_vars_selected(input$x_vars)
  })

  observeEvent(c(input_dataset(), input$y_var),{

    if(input$y_var %in% x_vars_selected()){
      show_toast(
        title = "Dropped Predictor",
        text = paste0(
          "Removed predictor ",
          "`",input$y_var,"`",
          " from prediction vars as it is now an Outcome variable"
          ),
        type = "warning",
        position = "top-end"
      )
    }

    updateSelectInput(
      session,
      inputId = 'x_vars',
      label = "Select Predictor Variables",
      choices  = setdiff(colnames(input_dataset()), input$y_var),
      selected = setdiff(x_vars_selected(), input$y_var)
    )
  })

  
  
  ## This is new! what splits do we want?
  train_ids <- reactive({
    sample(
      1:nrow(input_dataset()),
      size = ceiling(input$train_set * nrow(input_dataset())),
      replace = FALSE
      )
  })
  
  # This is new to get the non-training set!
  test_set <- reactive({
    input_dataset() %>%
      select(dependent_var = input$y_var, input$x_vars) %>%
      slice(-train_ids())
  })
  
  
  fit <- reactive({
    
    dat <- input_dataset() %>%
      select(
        dependent_var = input$y_var, 
        input$x_vars) %>%
      ## This is new for selecting training set
      slice(train_ids())
    
    fit <- lm(dependent_var ~ ., data = dat)
    fit
    
  })
  
  ## Tab 1 - Model summary
  
  output$model_summary <- renderPrint({
    req(input_dataset())
    summary(fit())
  })
  

  # Predictions on the hold out set
  preds <- reactive({
    predict(fit(), newdata = test_set())
  })
  
  ## Out of sample plots
  output$resid_plt <- renderPlot({
    
    req(input_dataset())
    
    mae <- round(mean(abs(test_set()$dependent_var - preds())), 1)
    mean_abs_error <- c("Out of Sample Mean Abs Error = ", mae)
    
    hist(
      fit()$resid,
      main = mean_abs_error,
      xlab = "Residuals (Test Set)"
      )
    
    abline(
      v = 0,
      col = "red",
      lwd = 3,
      lty = 2
      )
    
  })
  output$pred_plt <- renderPlot({
    
    req(input_dataset())
    
    test_set() %>%
      mutate(preds = preds()) %>%
      ggplot(aes(x = preds, y = dependent_var)) +
      geom_point(shape = 21,
                 size = 3.5,
                 color = "black",
                 fill = "palegreen"
                 ) +
      geom_abline(intercept = 0,
                  slope = 1,
                  size = 2,
                  color = "red",
                  linetype = "dashed") +
      labs(x = "Predictved Values",
           y = "Actual Values",
           title = "Out Of Sample Predictions\nActual Values vs Predicted Values")
    
  })
  
  
  ## Tab 2 - Data for training
  output$train_tbl <- renderDT({
    
    req(input_dataset())
    
    input_dataset() %>%
      select(dependent_var = input$y_var, input$x_vars) %>%
      slice(train_ids()) %>%
      datatable(caption = "Train Set")
    
  })
  
  ## Tab 3 - Data for testing
  output$test_tbl <- renderDT({
    
    req(input_dataset())
    
    test_set() %>%
      datatable(caption = "Test Set")
    
  })
  
}



shinyApp(ui, server)
