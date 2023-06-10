## TidyX Episode 148 - Shiny Statistical Model Builder

## Load Libraries
library(tidyverse)
library(shiny)
library(DT)

theme_set(theme_classic())


## Create some datasets

# Lahman::Batting %>%
#   filter(
#     yearID >= 2010,
#     AB >= 150
#   ) %>%
#   write.csv("mlb_batting.csv", row.names = FALSE)
# 
# write.csv(mtcars, "mtcars.csv", row.names = FALSE)

## The App

### UI

ui <- fluidPage(
  
  title = "Model Builder",
  
  sidebarPanel(
    
    fileInput(inputId = "file", 
              label = "Select a CSV file",
              accept = "csv"
              ),
    
    selectInput(inputId = "y_var",
                label = "Select Continuous Outcome Variable",
                choices = "",
                multiple = FALSE),
    
    selectInput(inputId = "x_vars",
                label = "Select Predictor Variables",
                choices = "",
                multiple = TRUE)
  ),
  
  mainPanel(
    verbatimTextOutput("model_summary"),
    br(),
    br(),
    fluidRow(
      column(plotOutput("resid_plt"), width = 6),
      column(plotOutput("pred_plt"), width = 6)
    ),
    br(),
    br(),
    DTOutput("tbl")
  )
)


server <- function(input, output, session){
  
  input_dataset <- reactive({
    
    if(is.null(input$file)){
      return(NULL)
    } else {
      
      ## this is a temporary path for the shiny session representing the
      ## uploaded data
      read.csv(file = input$file$datapath)
      
    }
    
  })
  
  #Observe file being selected
  observeEvent(input$file, {
    
    #Update select inputs
    updateSelectInput(session = session, 
                      inputId = 'y_var', 
                      label = "Select Outcome Variable", 
                      choices  = colnames(input_dataset()))
    
    updateSelectInput(session, 
                      inputId = 'x_vars', 
                      label = "Select Predictor Variables", 
                      choices  = colnames(input_dataset()))
    
  })
  
  fit <- reactive({
  
    req(input_dataset())
    
    dat <- input_dataset() %>%
      select(dependent_var = input$y_var, input$x_vars)
    
    fit <- lm(dependent_var ~ ., data = dat)
    fit
    
  })
  
  
  output$model_summary <- renderPrint({
    
    req(input_dataset())
    
    summary(fit())
    
  })
  
  output$resid_plt <- renderPlot({
    
    req(input_dataset())
    
    mae <- round(mean(abs(fit()$resid)), 1)
    mean_abs_error <- c("Mean Abs Error = ", mae)
    
    hist(fit()$resid,
         main = mean_abs_error,
         xlab = "Residuals")
    abline(v = 0,
           col = "red",
           lwd = 3,
           lty = 2)
    
  })
  
  output$pred_plt <- renderPlot({
    
    req(input_dataset())
    
    input_dataset() %>%
      select(dependent_var = input$y_var, input$x_vars) %>%
      mutate(preds = predict(fit(), newdata = .)) %>%
      ggplot(aes(x = preds, y = dependent_var)) +
      geom_point(shape = 21,
                 size = 3.5,
                 color = "black",
                 fill = "palegreen",
                 alpha = 0.4) +
      geom_abline(intercept = 0,
                  slope = 1,
                  size = 2,
                  color = "red",
                  linetype = "dashed") +
      labs(x = "Predictved Values",
           y = "Actual Values",
           title = "Actual Values vs Predicted Values")
    
  })
  
  
  output$tbl <- renderDT({
    
    req(input_dataset())
    
    input_dataset() %>%
      datatable
    
  })
  
}



shinyApp(ui, server)
