## Shiny app

library(shiny)
library(tidyverse)
library(ggwaffle)

source("app_helper.R")

rec_targets <- readRDS("rec_targets.RDS")

## create user interface ---------------------------------------------
ui <- fluidPage(
  
  sidebarPanel(
    selectInput(inputId = "tm",
                label = "Team",
                choices = unique(rec_targets$tm),
                selected = NULL,
                multiple = F),
    downloadButton("receiver_report","Download PDF")
    ),
  
  mainPanel(
    plotOutput(outputId = "waffle_plt"),
    tableOutput(outputId = "player_tbl")
  )
)


## create server -----------------------------------------
server <- function(input, output){
  
  waffle_plt <- reactive({
    
    team_rec <- rec_targets %>%
      ungroup() %>% 
      filter(tm == input$tm) %>%
      select(player, tgt ) %>%
      uncount( weights = tgt) %>% 
      mutate(
        player = fct_lump(player, 4)
      )
    
    
    waffle_df <- team_rec %>% 
      mutate(
        player = as.numeric(player)
      ) %>% 
      waffle_iron(
        rows = 25,
        aes_d(group = player)
      ) %>% 
      mutate(
        group = factor(levels(team_rec$player)[group], levels = levels(team_rec$player))
      )
    
    ggplot(waffle_df, aes(x = x, y = y, fill = group)) +
      geom_waffle() +
      theme_waffle()
    
  })
  
  dat_tbl <- reactive({
    rec_targets %>%
      filter(tm == input$tm) %>%
      select(player, pos, tgt, rec, ctch_percent, tgt_shares)
  })
  
  output$waffle_plt <- renderPlot({
    waffle_plt()
  })
  
  output$player_tbl <- renderTable({
    dat_tbl()
  })
  
  output$receiver_report <- downloadHandler(
    filename = "receiver_report.pdf",
    content = function(file) {
      generate_report(
        list(tm = input$tm, waffle = waffle_plt(), table = dat_tbl()),
        file
        )
    },contentType = "application/pdf" )
  
  
}


## run app --------------------------------------------------

shinyApp(ui, server)
