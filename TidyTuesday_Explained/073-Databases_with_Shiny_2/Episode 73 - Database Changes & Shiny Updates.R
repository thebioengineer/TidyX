
## TidyX Episode 73: Database Changes & Shiny Updates

### Packages ---------------------------------------------
library(tidyverse)
library(rvest)
library(janitor)

library(dbplyr)

library(RSQLite)
library(DBI)

library(shiny)
library(shinyWidgets)

### Get Seasonal Data ---------------------------------------------
pull_season_data <- function(season, con, verbose = FALSE){
  
  nba_months <- read_html(
    paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_games.html")
  ) %>% 
    html_nodes(".filter") %>% 
    html_text %>% 
    str_split("\n+") %>% 
    purrr::pluck(1) %>% 
    parse_character() %>% 
    {.[!is.na(.)]} %>%
    tolower
  
  nba_raw <- purrr::map_dfr(
    nba_months,
    function(.x){
      
      if(verbose){
        print(paste0(season,": ",.x))
      }
      
      read_html(
        paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_games-",gsub("\\s+","-",.x),".html")
      ) %>% 
        html_table(fill = T) %>%
        as.data.frame() %>% 
        clean_names() %>% 
        filter(date != "Date") %>%  #remove repeated headers
        mutate(across(everything(), as.character)) ## ensure everything is a character
    })
  
  ## Create position groupings
  nba_clean <- nba_raw %>% 
    mutate(notes = case_when(
      notes == "" ~ NA_character_,
      TRUE ~ notes
    )) %>% 
    fill(notes) %>% 
    filter(tolower(date) != "playoffs") %>% 
    mutate(
      game_date_time = as.POSIXct(paste0(date," ",start_et,"m"),format = "%a, %b %d, %Y %I:%M%p",tz = "US/Eastern"),
      game_id = paste0(season,"_",seq_len(n())),
      visitor_points = as.numeric(pts),
      home_points = as.numeric(pts_1),
      attend = parse_number(attend),
      playoffs = case_when(
        notes == "Playoffs" ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% 
    rename(
      visitor = visitor_neutral,
      home = home_neutral,
      overtime = var_8
    ) %>% 
    select(
      game_id,
      game_date_time,
      visitor,
      home, 
      visitor_points, 
      home_points,
      overtime,
      attend,
      playoffs
    )
  
  dbWriteTable(db_con, name = paste0("season_",season), nba_clean, overwrite = TRUE)
}

### Write seasons 2000-2014 to database ---------------------------------------------

# create connection
db_con <- dbConnect(
  drv = RSQLite::SQLite(),
  here::here("TidyTuesday_Explained/073-Databases_with_Shiny_2/nba_seasons.db")
)

## write table
purrr::walk(
  2001:2014, 
  pull_season_data,
  db_con
)

##disconnect
dbDisconnect(db_con)

### Interact with database ---------------------------------------------

## Connect to database
db_con <- dbConnect(
  drv = RSQLite::SQLite(),
  here::here("TidyTuesday_Explained/073-Databases_with_Shiny_2/nba_seasons.db")
)

## Get the highest season in the data base
seasons_tables <- dbListTables(db_con)
seasons_tables_num <- parse_number(seasons_tables)

## Build Shiny App
## get list of teams 
teams <- dbGetQuery(
  db_con,
  paste0("SELECT HOME FROM ", seasons_tables, collapse = " UNION ")
)

dbDisconnect(db_con)

## SHINY APP 

###############################################
###### What if we update our database?? #######

### Shiny app with an auto refresh
ui <- fluidPage(
  
  title = "Home Scoring Margin",
  
  sidebarPanel(
    
    sliderInput(inputId = "season_selection",
                label = "Select Seasons",
                max = max(seasons_tables_num),
                min = min(seasons_tables_num),
                value = c(min(seasons_tables_num), max(seasons_tables_num)),
                post = " season",
                ticks = TRUE,
                sep = "", ## years, so don't want a comma
                dragRange = TRUE
    ),
    
    pickerInput(inputId = "home",
                label = "Pick Teams:",
                choices = teams,
                multiple = TRUE,
                selected = "Cleveland Cavaliers"),

  ),
  
  mainPanel(
    plotOutput(outputId = "plt")
    )
  
)


server <- function(input, output, session){
  
  available_seasons <- reactivePoll(
    
    intervalMillis = 5000, ## 5000ms == 5 seconds
    
    session,
                           
    checkFunc = function(){
      
      ## low cost check function
      
      ## check that n seasons has not changed in database
      db_con <- dbConnect(
        drv = RSQLite::SQLite(),
        here::here("TidyTuesday_Explained/073-Databases_with_Shiny_2/nba_seasons.db")
      )
      on.exit(dbDisconnect(db_con))
      
      dbListTables(db_con)
      
    },
    
    valueFunc = function(){
      
      db_con <- dbConnect(
        drv = RSQLite::SQLite(),
        here::here("TidyTuesday_Explained/073-Databases_with_Shiny_2/nba_seasons.db")
      )
      on.exit(dbDisconnect(db_con))
      
      seasons <- dbListTables(db_con)
      teams <- dbGetQuery(db_con, paste0("SELECT HOME FROM ", seasons, collapse = " UNION "))
      
      list(
        seasons = seasons,
        teams = teams
      )
      
    })
  
  observeEvent(available_seasons(), {

    updatePickerInput(inputId = "home",
                      session = session,
                      choices = available_seasons()$teams,
                      selected = input$home)
    
    seasons_tables_num <- parse_number(available_seasons()$seasons)
    
    updateSliderInput(
      inputId = "season_selection",
      session = session,
      max = max(seasons_tables_num),
      min = min(seasons_tables_num),
      value = c(input$season_selection[1], input$season_selection[2])
    )
    
  })
    
    
  sql_data <- reactive({
    
      req(input$home)
      
      ## connect to database
      db_con <- dbConnect(
        drv = RSQLite::SQLite(),
        here::here("TidyTuesday_Explained/073-Databases_with_Shiny_2/nba_seasons.db")
      )
      
      ##disconnect when reactive finishes
      on.exit(dbDisconnect(db_con))
      
      ## query database
      sql_dat <-
        map_dfr(
          paste0("season_", input$season_selection[1]:input$season_selection[2]),
          function(season) {
          tbl(db_con, season) %>%
            filter(home %in% !!input$home) %>%
            select(game_id, home, home_points, visitor_points) %>%
            as.data.frame()
        })
      
      sql_dat
  })
  
  dat <- reactive({
    d <- sql_data() %>%
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


### Run in separate R session ----

library(tidyverse)
library(rvest)
library(janitor)

library(dbplyr)

library(RSQLite)
library(DBI)

pull_season_data <- function(season, con, verbose = FALSE){
  
  nba_months <- read_html(
    paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_games.html")
  ) %>% 
    html_nodes(".filter") %>% 
    html_text %>% 
    str_split("\n+") %>% 
    purrr::pluck(1) %>% 
    parse_character() %>% 
    {.[!is.na(.)]} %>%
    tolower
  
  nba_raw <- purrr::map_dfr(
    nba_months,
    function(.x){
      
      if(verbose){
        print(paste0(season,": ",.x))
      }
      
      read_html(
        paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_games-",gsub("\\s+","-",.x),".html")
      ) %>% 
        html_table(fill = T) %>%
        as.data.frame() %>% 
        clean_names() %>% 
        filter(date != "Date") %>%  #remove repeated headers
        mutate(across(everything(), as.character)) ## ensure everything is a character
    })
  
  ## Create position groupings
  nba_clean <- nba_raw %>% 
    mutate(notes = case_when(
      notes == "" ~ NA_character_,
      TRUE ~ notes
    )) %>% 
    fill(notes) %>% 
    filter(tolower(date) != "playoffs") %>% 
    mutate(
      game_date_time = as.POSIXct(paste0(date," ",start_et,"m"),format = "%a, %b %d, %Y %I:%M%p",tz = "US/Eastern"),
      game_id = paste0(season,"_",seq_len(n())),
      visitor_points = as.numeric(pts),
      home_points = as.numeric(pts_1),
      attend = parse_number(attend),
      playoffs = case_when(
        notes == "Playoffs" ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% 
    rename(
      visitor = visitor_neutral,
      home = home_neutral,
      overtime = var_8
    ) %>% 
    select(
      game_id,
      game_date_time,
      visitor,
      home, 
      visitor_points, 
      home_points,
      overtime,
      attend,
      playoffs
    )
  
  dbWriteTable(db_con, name = paste0("season_",season), nba_clean, overwrite = TRUE)
}

db_con <- dbConnect(
  drv = RSQLite::SQLite(),
  here::here("TidyTuesday_Explained/073-Databases_with_Shiny_2/nba_seasons.db")
)

## write table
purrr::walk(
  2015:2021, 
  pull_season_data,
  db_con
)

##disconnect
dbDisconnect(db_con)

## Connect to database
db_con <- dbConnect(
  drv = RSQLite::SQLite(),
  here::here("TidyTuesday_Explained/073-Databases_with_Shiny_2/nba_seasons.db")
)

