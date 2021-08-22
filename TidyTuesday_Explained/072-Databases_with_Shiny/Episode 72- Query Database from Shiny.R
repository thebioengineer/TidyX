
## TidyX Episode 72: 
# https://dbplyr.tidyverse.org/

### Packages ---------------------------------------------
library(tidyverse)
library(dbplyr) ## this is technically called under the hood by tidyverse
library(RSQLite)
library(DBI)
library(shiny)
library(shinyWidgets)

### Interact with database ---------------------------------------------

db_con <- dbConnect(
  drv = RSQLite::SQLite(),
  here::here("TidyTuesday_Explained/072-Databases_with_Shiny/nba_seasons.db")
)

## what tables exist?
dbListTables(db_con)


## Grouping by
d <- map_dfr(
  paste0("season_",2001:2005),
  function(season){
    tbl(db_con, season) %>% 
    filter(home == "Los Angeles Lakers") %>% 
    select(game_id, home_points, visitor_points) %>%
    as.data.frame()
  })

head(d)
tail(d)

dbDisconnect(db_con)

## Shiny Application

db_con <- dbConnect(
  drv = RSQLite::SQLite(),
  here::here("TidyTuesday_Explained/072-Databases_with_Shiny/nba_seasons.db")
)

## get list of teams 
team <- dbGetQuery(
  db_con,
  paste0("SELECT HOME FROM season_",2001:2020, collapse = " UNION ")
  )

dbDisconnect(db_con)

## SHINY APP 

ui <- fluidPage(
  
  title = "Home Scoring Margin: 2001 - 2020",
  
  sidebarPanel(
    pickerInput(inputId = "home",
                label = "Pick Teams:",
                choices = team,
                multiple = TRUE,
                selected = "Cleveland Cavaliers")
  ),
  
  mainPanel(plotOutput(outputId = "plt"))
  
)


server <- function(input, output){
  
  sql_dat <- reactive({
    
    req(input$home)
    
    ## connect to database
    db_con <- dbConnect(
      drv = RSQLite::SQLite(),
      here::here("TidyTuesday_Explained/072-Databases_with_Shiny/nba_seasons.db")
    )
    
    ##disconnect when reactive finishes
    on.exit(dbDisconnect(db_con))
    
    ## query database
    sql_dat <- map_dfr(paste0("season_",2001:2020),function(season){
      tbl(db_con, season) %>% 
        filter(home %in% !!input$home) %>% 
        select(game_id, home, home_points, visitor_points) %>%
        as.data.frame()
    })
    
    sql_dat
    
  })
  
  dat <- reactive({
    
    d <- sql_dat() %>%
      mutate(season = substring(game_id, first = 1, last = 4)) %>%
      group_by(home, season) %>%
      summarize(home_pts_mov = mean(home_points - visitor_points))
    
    d
    
  })
  
  output$plt <- renderPlot({
    
    dat() %>%
      ggplot(aes(x = season, y = home_pts_mov, color = home, group = home)) +
      labs(
        title = "Average Point Differential at Home Stadium Across Seasons",
        y = "Average Home Stadium Point Differential",
        x = "Season"
      ) +
      geom_hline(yintercept = 0) + 
      geom_line(size = 1.2) +
      geom_point(shape = 21,
                 fill = "white",
                 size = 5) +
      theme_classic()
    
  })
  
}


shinyApp(ui, server)
