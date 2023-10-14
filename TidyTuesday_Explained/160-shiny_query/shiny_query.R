library(shiny)
library(tidyverse)

d <- mtcars %>%
  rownames_to_column(var = "car_types") %>%
  mutate(cyl = as.factor(cyl),
         am = as.factor(am)) %>%
  select(car_types, mpg, cyl, am, disp, wt)

d %>%
  head()

### UI
ui <- fluidPage(
  
  title = "Cars!",
  
  sidebarPanel(
    
    selectInput(inputId = "cyl",
                label = "Cylinders",
                choices = sort(unique(d$cyl)),
                selected = NULL,
                multiple = FALSE),
    
    selectInput(inputId = "am",
                label = "Transmission",
                choices = sort(unique(d$am)),
                selected = NULL,
                multiple = FALSE)
    
    
  ),
  
  mainPanel(
    plotOutput(outputId = 'xy_plt'),
    plotOutput(outputId = "hist_plt"),
    tableOutput(outputId = "tbl")
  )
)


### Server
server <- function(input, output, session){
  
  ## update based on URL parameters
  observe({
    
    query <- parseQueryString(session$clientData$url_search)
    
    if(!is.null(query[['cyl']])){
      updateSelectInput(session, "cyl", selected = query[['cyl']])
    }
    
    if(!is.null(query[['am']])){
      updateSelectInput(session, "am", selected = query[["am"]])
    }
  })
  
  ## get data
  dat <- reactive({
    
    d %>%
      filter(cyl == input$cyl,
             am == input$am)
    
  })
  
  ## xy plot
  output$xy_plt <- renderPlot({
    
    dat() %>%
      ggplot(aes(x = disp, y = mpg)) +
      geom_jitter(size = 5,
                  color = "green") +
      geom_smooth(method = "lm",
                  se = FALSE,
                  color = "black",
                  size = 1.2) +
      theme_light()
    
  })
  
  ## hist plot
  output$hist_plt <- renderPlot({
    
    dat() %>%
      ggplot(aes(x = "1", y = wt)) +
      geom_boxplot(color = "black",
                   fill = "light grey") +
      theme_light()
    
  })
  
  ## table of car types
  output$tbl <- renderTable({
    
    dat() %>% select(car_types)
    
  })
}


shiny_query <- shiny::shinyApp(ui = ui, server = server)

## run on port 8888 of my local computer (127.0.0.1 is the ipv4 address of your local computer)
runApp(shiny_query, port = 8888, host = "127.0.0.1", launch.browser = TRUE)


# http://127.0.0.1:8888/?cyl=4
# http://127.0.0.1:8888/?cyl=6

# http://127.0.0.1:8888/?am=0
# http://127.0.0.1:8888/?am=1&cyl=8



## invalid entry
# http://127.0.0.1:8888/?cyl=24