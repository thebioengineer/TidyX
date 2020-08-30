
### Load Packages ---------------------------------------
library(tidyverse)
library(shiny)

### Simulate Data --------------------------------------
set.seed(8)
Athlete <- rep(LETTERS[1:20], each = 30)
Game <- rep(1:30, times = 20)
Position <- rep(c("center back", "right forward", "left forward", "right mid", "left mid"), each = 120)
Sprint_Dist <- rnorm(n = length(Athlete), mean = 150, sd = 40)
Total_Dist <- rnorm(n = length(Athlete), mean = 12500, sd = 500)

df <- data.frame(Athlete, Game, Position, Sprint_Dist, Total_Dist)
df %>% head()


### Shiny App ------------------------------------------
# User Interface

ui <- fluidPage(
  titlePanel(
    "Athlete Running Demands"
  ),
  sidebarPanel(
    width = 3,
    selectizeInput(inputId = "Position",
                   label = "Pos",
                   choices = unique(df$Position),
                   selected = "center back",
                   multiple = FALSE),
    
    selectizeInput(inputId = "Athlete", 
                   label = "Athlete",
                   choices = unique(df$Athlete),
                   selected = "",
                   multiple = TRUE)
  ),
  
  mainPanel(
    plotOutput(outputId = "dist"),
    plotOutput(outputId = "sprint_dist")
  )
)


## server

server <- function(input, output, session){
  
  dat <- reactive({
    
    d <- df %>%
      filter(Position == input$Position)
    
    updateSelectizeInput(session, "Athlete", choices = unique(d$Athlete))
    
    d
  })
  
  output$sprint_dist <- renderPlot({
    
    dat() %>% 
      filter(Athlete %in% input$Athlete) %>% 
      ggplot(
        aes(
          x = Game,
          y = Sprint_Dist,
          color = Athlete,
          group = Athlete
          )) +
      geom_line()
    
  })
  
  output$dist <- renderPlot({
    
    dat() %>% 
      filter(Athlete %in% input$Athlete) %>% 
      ggplot(
        aes(
          x = Game,
          y = Total_Dist,
          color = Athlete,
          group = Athlete
        )) +
      geom_line()
    
  })
  
}



shinyApp(ui, server)
