## TidyX Episode 162 - Connected Shiny Apps Part 1

## Game Level Shiny App
library(tidyverse)
library(shiny)
library(rvest)
library(DT)

## Thanks to hockey-reference.com for the data!

## Get NHL Year Level data

nhl_season <- function(year){
  
  ref_url <- paste0("https://www.hockey-reference.com/leagues/NHL_",year,"_games.html")
  
  nhl_html <- read_html(ref_url)
  
  nhl_data <- nhl_html %>% 
    html_elements("#games") %>% 
    html_elements("tr") %>% 
    .[-1] %>%  ## Drop first element, which is the table header
    map(function(tr){
      
      table_row_cells <- tr %>% 
        html_children()

      row_text <- table_row_cells %>% 
        html_text() %>% 
        as.list() %>%
        setNames(c("date","visitor","visitor_goals","home","home_goals","overtimes","attendance","game_duration","notes"))  
      
      game_url <- table_row_cells %>%
        .[[1]] %>% 
        html_children() %>% 
        html_attr("href") %>% 
        as.list()
      
      if(length(game_url) == 1){
        names(game_url) <- "game_url"
      }
        
      as.data.frame(c(row_text, game_url))
      
    }) %>% 
    bind_rows() %>% 
    filter(!is.na(game_url)) %>% 
    mutate(
      game_id = paste0(
       date," - ", visitor," vs. ",home 
      )
    )
}

nhl_game <- function(game_id){
  
  game_url <- paste0("https://www.hockey-reference.com/", game_id)
  
  game_html <- read_html(game_url)
  
  scoring <- game_html %>% 
    html_element("#scoring") %>% 
    html_table(header = FALSE) %>% 
    setNames(c("time","team","period","player","assist")) %>% 
    mutate(
      period = case_when(
        !grepl("Period",period) ~ NA, 
        TRUE ~ period
      )
    ) %>% 
    fill(period, .direction = "down") %>% 
    filter(
      time != period
    )
  
  penalties <- game_html %>% 
    html_element("#penalty") %>% 
    html_table(header = FALSE) %>% 
    setNames(c("time","team","player","reason","duration")) %>% 
    mutate(
      period = case_when(
        time == team ~ team, 
        .default = NA
      )
    ) %>% 
    fill(period, .direction = "down") %>% 
    filter(
      time != period
    )
  
  ## THIS IS NEW CODE
  ### <<<< FROM HERE
  
  tables <- game_html %>% 
    html_elements("table")
  
  away_roster <- tables[[3]] %>% 
    html_elements("tr") %>% 
    .[-c(1,2)] %>%  ## Drop first two rows
    map(function(tr){
      
      table_row_cells <- tr %>% 
        html_children()
      
      row_text <- table_row_cells %>%
        .[1:5] %>% 
        html_text() %>% 
        as.list() %>%
        setNames(c("rank","skater","goals","assists","points"))  
      
      skater_id <- table_row_cells %>%
        .[[2]] %>% 
        html_children() %>% 
        html_attr("href") %>% 
        basename() %>% 
        tools::file_path_sans_ext() %>% 
        as.list()

      if(length(skater_id) == 1){
        names(skater_id) <- "skater_id"
      }
      
      as.data.frame(c(skater_id, row_text))
      
    }) %>% 
    bind_rows() %>% 
    filter(!is.na(skater_id))
    
  home_roster <- tables[[5]] %>% 
    html_elements("tr") %>% 
    .[-c(1,2)] %>%  ## Drop first two rows
    map(function(tr){
      
      table_row_cells <- tr %>% 
        html_children()
      
      row_text <- table_row_cells %>%
        .[1:5] %>% 
        html_text() %>% 
        as.list() %>%
        setNames(c("rank","skater","goals","assists","points"))  
      
      skater_id <- table_row_cells %>%
        .[[2]] %>% 
        html_children() %>% 
        html_attr("href") %>% 
        basename() %>% 
        tools::file_path_sans_ext() %>% 
        as.list()
      
      if(length(skater_id) == 1){
        names(skater_id) <- "skater_id"
      }
      
      as.data.frame(c(skater_id, row_text))
      
    }) %>% 
    bind_rows() %>% 
    filter(!is.na(skater_id))
  
  
  ### <<<< TO HERE
  
  
  list(
    scoring = scoring,
    penalties = penalties,
    homeroster = home_roster,
    awayroster = away_roster
  )
}


## Shiny Application

ui <- fluidPage(
  
  titlePanel("NHL Season and Games"),
    
  sidebarPanel(
    selectInput(inputId = "year", label = "Select Season", choices = 2024:1950),
    selectInput(inputId = "game", label = "Select Game", choices = NULL)
  ),
  mainPanel(
    uiOutput("main_panel")
  )
)


server <- function(input, output, session){
  
  ## When year is selected, get the games for that year
  available_games <- reactive({
    nhl_season(input$year)
  })
  
  ## Update game options to reflect current games
  observeEvent(available_games(),{
    updateSelectInput(
      session = session,
      inputId = "game",
      ## starting with "Choose Game" prevents values from being selected initially
      choices = c(
        "Choose Game"="",
        available_games()$game_id
        )
    )
  })  
  
  ## Once Selected a game, pull game level data
  game_data <- reactive({
    
    game_info <- available_games() %>% 
      filter(game_id == input$game)
    
    if(nrow(game_info) > 0){
      scraped_data <- nhl_game(game_info$game_url)
      c(
        scraped_data,
        as.list(game_info)
      )
    }else{
      NULL
    }
  })

  ## Generate table outputs
  output$scoring <- renderDT({
    datatable(
      game_data()$scoring,
      options = list(lengthChange = FALSE, dom = 't')
    )
  })
  output$penalties <- renderDT({
    datatable(
      game_data()$penalties,
      options = list(lengthChange = FALSE, dom = 't')
    )
  })
  
  ## THIS IS NEW CODE
  ### <<<< FROM HERE
  
  ## Roster Tables
  output$homeroster <- renderDT({
    
    base_url <- paste0(getOption("shiny.host", "127.0.0.1"),":","3333")
    
    game_data()$homeroster %>% 
      mutate(
        ref_url = paste0("http://", base_url,"/?year=",isolate(input$year),"&skater=",utils::URLencode(paste0(skater," - ",skater_id))),
        skater = paste0("<a href='",ref_url,"' target = '_blank'>",skater,"</a>")
      ) %>% 
      select(-ref_url) %>% 
      datatable(
        escape = FALSE,
        options = list(lengthChange = FALSE, dom = 't')
      )
  })
  
  output$awayroster <- renderDT({
    
    base_url <- paste0(getOption("shiny.host", "127.0.0.1"),":","3333")
    
    game_data()$awayroster %>% 
      mutate(
        ref_url = paste0("http://", base_url,"/?year=",isolate(input$year),"&skater=",utils::URLencode(paste0(skater," - ",skater_id))),
        skater = paste0(
          paste0("<a href='",ref_url,"' target = '_blank'>",skater,"</a>")
        )
      ) %>% 
      select(-ref_url) %>% 
      datatable(
        escape = FALSE,
        options = list(lengthChange = FALSE, dom = 't')
      )
  })
  
  ### <<<< TO HERE
  
  
  output$main_panel <- renderUI({
    
    if(identical(input$game,"") | is.null(game_data())){
      ## If no game is selected, show nothing
      p("Select Game to Display")
    }else{
      tagList(
        
        h2(game_data()$game_id),
        a("Hockey-Reference.com Game Site", href = paste0("https://www.hockey-reference.com/",game_data()$game_url)),
        
        tags$hr(),
        
        h3("Final"),
        p(paste0("Home Team: ", game_data()$home," - ", game_data()$home_goals)),
        p(paste0("Visiting Team: ", game_data()$visitor," - ", game_data()$visitor_goals)),
        p(paste0("Duration: ", game_data()$game_duration)),
        p(paste0("Attendance: ", game_data()$attendance)),
        
        # tags$hr(),
        # 
        # h3("Game Stats"),
        # h4("Scoring"),
        # DTOutput("scoring"),
        # h4("Penalties"),
        # DTOutput("penalties"),
        # 
        tags$hr(),
        
        h3("Rosters"),
        h4(game_data()$home),
        DTOutput("homeroster"),
        h4(game_data()$visitor),
        DTOutput("awayroster")
        
      )
    }
    
  })
  
}


shinyApp(
  ui = ui,
  server = server
)