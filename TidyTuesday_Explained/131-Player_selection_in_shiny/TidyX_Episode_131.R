# TidyX Episode 131 - The spiderman meme

library(tidyverse)
library(shiny)

player_info <- tibble(
  player = c(
    "Sam Stern",
    "Joe Compton",
    "Tom Smith",
    "Bob Franklin",
    "Tom Smith"
  ),
  team = c("Hawks", "Sharks", "Tigers", "Hawks", "Frogs"),
  position = c("Fwd", "Fwd", "Center", "Back", "Mid"),
  player_id = 1:5
) 

player_info

player_vitals <- tibble(
  player_id = 1:5,
  performance = c(8, 4, 10, 6, 7.5),
  height = runif(5, 59,63),
  weight = runif(5, 100, 130),
  goals = runif(5, 0, 6)
)

player_vitals

#### App 1 -- Create Unique IDs ######

player_info_combined <- player_info %>% 
  unite(
    unique_player_info,
    player,
    team,
    position, 
    sep = "-",
    remove = FALSE
    )

ui <- fluidPage(
  
  sidebarPanel(
    selectInput(
      inputId = "player_id",
      label = "Select a Player:",
      choices = sort(unique(player_info_combined$unique_player_info)),
      multiple = FALSE,
      selected = NULL
      )
  ),
  mainPanel(
    h1("Player Vitals"),
    tableOutput(outputId = "tbl")
  )
)


server <- function(input, output){
  
  d <- reactive({
    
    req(input$player_id)
    
    p_id <- player_info_combined %>%
      filter(unique_player_info %in% input$player_id) %>%
      pull(player_id)
    
    player_vitals %>% 
        filter(player_id == p_id) %>% 
        select(-player_id)

  })
  
  output$tbl <- renderTable({
    req(d())
    d()
  })
  
}


shinyApp(ui, server, options = list(launch.browser = TRUE))


#### App 2 -- Allow Selection of Player ######

# https://rstudio.github.io/DT/shiny.html
library(DT)

ui <- fluidPage(
  sidebarPanel(
    DT::dataTableOutput("player_select_table")
  ),
  mainPanel(
    h1("Player Vitals"),
    tableOutput(outputId = "tbl")
  )
)


server <- function(input, output){
  
  output$player_select_table <- DT::renderDataTable({
    player_info %>% 
      select(-player_id) %>% 
      DT::datatable(
        selection = "single",
        rownames = FALSE,
        options = list(lengthChange = FALSE)
        )
  })
  
  d <- reactive({
    
    req(input$player_select_table_rows_selected)
    
    p_id <- player_info %>% 
      slice(input$player_select_table_rows_selected) %>% 
      pull(player_id)
      
     player_vitals %>% 
        filter(player_id == p_id) %>% 
        select(-player_id)
      
  })
  
  output$tbl <- renderTable({
    req(d())
    d()
  })
  
}


shinyApp(ui, server, options = list(launch.browser = TRUE))

