
## TidyX Episode 76: Joins with updating Databases - Shiny

### Packages ---------------------------------------------
library(tidyverse)
library(rvest)
library(janitor)

library(RSQLite)
library(DBI)

library(shiny)
library(shinyWidgets)
library(DT)


db_path <- "TidyTuesday_Explained/076-Databases_with_Shiny_4/nba_playoffs.db"

### Get Game PbP Data ---------------------------------------------

pull_game_pbp_data <- function(game_id, con, verbose = FALSE){
  
  if(verbose){
    print(game_id)
  }
  
  ## html ----
  espn_pbp <- read_html(paste0("https://www.espn.com/nba/playbyplay/_/gameId/",game_id))
  espn_game_summary <- read_html(paste0("https://www.espn.com/nba/game/_/gameId/",game_id))
  
  ## game info ----
  
  teams <- espn_pbp %>% 
    html_nodes(".competitors")
  
  home <- teams %>% 
    html_nodes(".home") %>% 
    html_nodes("span") %>% 
    `[`(1:3) %>% 
    html_text
  
  away <- teams %>% 
    html_nodes(".away") %>% 
    html_nodes("span") %>% 
    `[`(1:3) %>% 
    html_text  
  
  game_info <- espn_game_summary %>% 
    html_nodes(".game-information") %>% 
    html_nodes(".game-field") 
  
  game_time <- game_info %>% 
    html_nodes(".game-date-time") %>%
    html_node("span") %>% 
    html_attr("data-date")
  
  game_odds <- espn_game_summary %>% 
    html_nodes(".game-information") %>% 
    html_nodes(".odds") %>% 
    html_nodes("li") %>% 
    html_text() %>% 
    str_split(":") %>% 
    data.frame() %>% 
    janitor::row_to_names(1)
  
  game_capacity <- espn_game_summary %>% 
    html_nodes(".game-information") %>% 
    html_nodes(".game-info-note") %>% 
    html_text() %>% 
    str_split(":") %>% 
    data.frame() %>% 
    janitor::row_to_names(1)
  
  game_summary <- espn_game_summary %>% 
    html_nodes(".header") %>% 
    html_text() %>% 
    str_split(",") %>% 
    pluck(1) %>% 
    pluck(1)
  
  game_df <- data.frame(
    game_id = game_id, 
    game_time = game_time,
    game_info = game_summary[[1]],
    home_team = paste(home[1:2],collapse = " "),
    home_team_abbrev = home[3],
    away_team = paste(away[1:2],collapse = " "),
    away_team_abbrev = away[3],
    game_capacity,
    game_odds
  ) %>% 
    janitor::clean_names()
  
  ## pbp info ----
  
  quarter_tabs <- espn_pbp %>% 
    html_nodes("#gamepackage-qtrs-wrap") %>% 
    html_nodes(".webview-internal") %>% 
    html_attr("href")
  
  full_game_pbp <- map_dfr(quarter_tabs, function(qtab){
    ## scrape elements for time stamps, play details, and score
    time_stamps <- espn_pbp %>%
      html_nodes("div") %>%
      html_nodes(qtab) %>%
      html_nodes(".time-stamp") %>%
      html_text() %>%
      as_tibble() %>%
      rename(time = value)
    
    possession_details <- espn_pbp %>%
      html_nodes("div") %>%
      html_nodes(qtab) %>%
      html_nodes(".logo") %>%
      html_nodes("img") %>% 
      html_attr("src") %>%
      as_tibble() %>%
      rename(possession = value) %>% 
      mutate(
        possession = basename(possession)
      ) %>% 
      mutate(
        possession =  str_replace(possession, "(.+)([.]png.+)","\\1")
      )
    
    play_details <- espn_pbp %>%
      html_nodes("div") %>%
      html_nodes(qtab) %>%
      html_nodes(".game-details") %>%
      html_text() %>%
      as_tibble() %>%
      rename(play_details = value)
    
    score <- espn_pbp %>%
      html_nodes("div") %>%
      html_nodes(qtab) %>%
      html_nodes(".combined-score") %>%
      html_text() %>%
      as_tibble() %>%
      rename(score = value)
    
    ## bind data together
    bind_cols(time_stamps, possession_details, play_details, score) %>% 
      mutate(
        quarter = gsub("#","",qtab)
      )
  }) %>% 
    mutate(play_id_num = seq_len(nrow(.)))
  
  dbWriteTable(con, name = paste0("game_pbp_",game_id), full_game_pbp, overwrite = TRUE)
  
  
  ## NEW
  comments_table <- data.frame(
    play_id_num = numeric(),
    comment = character()
  )
  
  dbWriteTable(
    con, 
    name = paste0("game_comments_",game_id),
    comments_table,
    overwrite = TRUE
  )
  
  ## NEW
  dbWriteTable(
    con,
    name = paste0("game_comments_update_time_", game_id),
    data.frame(time = Sys.time()),
    overwrite = TRUE
  )
  
  if("game_ids" %in% dbListTables(con)){
    hist_game_table <- dbReadTable(con, "game_ids")
    game_df <- unique(rbind(hist_game_table, game_df))
  }
  
  dbWriteTable(con, name = "game_ids", game_df, overwrite = TRUE)
  
}

# Write several games to database ---------------------------------------------

## create connection
db_con <- dbConnect(
  drv = RSQLite::SQLite(),
  here::here(db_path)
)

## write table
walk(
  c(
    "401327715", ## Miami Heat @ Milwaukee Bucks (1st Rd | Game 1 Eastern Conference Playoffs, 2021)
    "401327878", ## Miami Heat @ Milwaukee Bucks (1st Rd | Game 2 Eastern Conference Playoffs, 2021)
    "401327879", ## Miami Heat @ Milwaukee Bucks (1st Rd | Game 3 Eastern Conference Playoffs, 2021)
    "401327870" ## Denver Nuggets @ Portland Trail Blazers (1st Rd | Game 4 Western Conference Playoffs, 2021)
  ), 
  pull_game_pbp_data,
  db_con
)

dbListTables(db_con)

##disconnect
dbDisconnect(db_con)


# SHINY APP for coaches to leave comments ----

db_con <- dbConnect(
  drv = RSQLite::SQLite(),
  here::here(db_path)
)

games_available <- tbl(db_con, "game_ids") %>% 
  pull(game_info)


##disconnect
dbDisconnect(db_con)

### Shiny app with an auto refresh
ui <- fluidPage(
  
  title = "2021 Playoffs",
  
  sidebarPanel(
    
    pickerInput(
      inputId = "game_selection",
      label = "Select Game:",
      choices = games_available
    ),
  ),
  
  mainPanel(
    DTOutput(outputId = "pbp_table"),
    uiOutput(outputId = "commit_button_display")
  )
  
)


server <- function(input, output, session){
  
  displayedData <- reactiveValues()
  
  observeEvent(input$game_selection,{
    
    req(input$game_selection)
    
    ## connect to database
    db_con <- dbConnect(
      drv = RSQLite::SQLite(),
      here::here(db_path)
    )
    
    ##disconnect when reactive finishes
    on.exit(dbDisconnect(db_con))
    
    game_id <- tbl(db_con, "game_ids") %>% 
      filter( game_info == !!input$game_selection) %>% 
      pull(game_id)
    
    comments <- tbl(db_con, paste0("game_comments_",game_id))
    
    pbp <- tbl(db_con, paste0("game_pbp_",game_id)) %>% 
      left_join(comments, by = "play_id_num") %>% 
      arrange(play_id_num)
    
    displayedData$game_id <- game_id
    displayedData$comments <- comments %>% collect() 
    displayedData$pbp <- pbp %>% collect() 
    
    displayedData$comment_commit_up_to_date <- TRUE
    
  })

  ## Data rendering 
  output$pbp_table <- renderDT({
    displayedData$pbp
  },
  selection = 'none',
  rownames = FALSE,
  editable = TRUE,
  escape = FALSE)
  
  ## when updated, 
  proxy = dataTableProxy('pbp_table')
  observeEvent(input$pbp_table_cell_edit,{
    
    info = input$pbp_table_cell_edit
    i = info$row
    j = info$col + 1  # column index offset by 1
    v = info$value
    
    ## only comment column can be edited
    if(colnames(displayedData$pbp)[j] == "comment"){
      
      play_num <- displayedData$pbp$play_id_num[i]
      
      displayedData$comments <- bind_rows(
        displayedData$comments[!displayedData$comments$play_id_num == play_num,],
        data.frame(
          play_id_num = play_num, 
          comment = v
        )
      )
      
      displayedData$pbp <- displayedData$pbp %>% 
        select(-comment) %>% 
        left_join(displayedData$comments, by = "play_id_num") %>% 
        arrange(play_id_num)
      
      displayedData$comment_commit_up_to_date <- FALSE
      
    }
    
    replaceData(proxy, displayedData$pbp, resetPaging = FALSE, rownames = FALSE)
    
  })
  
  output$commit_button_display <- renderUI({
    if(!displayedData$comment_commit_up_to_date){
        actionButton("commit","click to save comments")
    }
  })
  
  ## commit updates and share comments
  observeEvent(input$commit,{
    
    db_con <- dbConnect(
      drv = RSQLite::SQLite(),
      here::here(db_path)
    )
    
    ##disconnect when reactive finishes
    on.exit(dbDisconnect(db_con))
    
    dbWriteTable(
      db_con,
      name = paste0("game_comments_", displayedData$game_id),
      as.data.frame(displayedData$comments),
      overwrite = TRUE
    )
    
    dbWriteTable(
      db_con,
      name = paste0("game_comments_update_time_", displayedData$game_id),
      data.frame(time = Sys.time()),
      overwrite = TRUE
    )
    
    displayedData$comment_commit_up_to_date <- TRUE
    
    
  })
  
  ## Add polling of comments table to check if there are committed updated
  comments_updated <- reactivePoll(
    
    intervalMillis = 5000,
    
    session,
                           
    checkFunc = function(){
      
      ## connect to database
      db_con <- dbConnect(
        drv = RSQLite::SQLite(),
        here::here(db_path)
      )
      
      ##disconnect when reactive finishes
      on.exit(dbDisconnect(db_con))
      
      if(!is.null(displayedData$game_id)){
        dbGetQuery(db_con, paste0("SELECT time FROM game_comments_update_time_", displayedData$game_id))
      }

    },
                           
    valueFunc = function() {
      
      ## connect to database
      db_con <- dbConnect(
        drv = RSQLite::SQLite(),
        here::here(db_path)
      )
      
      ##disconnect when reactive finishes
      on.exit(dbDisconnect(db_con))
      
      ## query database
      tbl(db_con, paste0("game_comments_",displayedData$game_id)) %>% collect()
    }
  )
  
  observeEvent(comments_updated(),{
    
    ## check if any differences between committed comments and current comments
    diff_comments <- anti_join(
      comments_updated(),
      displayedData$comments, 
      by= colnames(displayedData$comments)
    )
    
    if(nrow(diff_comments) > 0){
      
      new_comments <- anti_join(
        comments_updated(),
        displayedData$comments, 
        by= "play_id_num"
      )
      
      conflicting_comments <- inner_join(
        diff_comments,
        displayedData$comments, 
        by= "play_id_num"
      )
      
      new_comments_2 <- do.call('rbind',lapply(seq_len(nrow(conflicting_comments)),function(i){
        db_comment <- conflicting_comments[i,"comment.x"]
        local_comment <- conflicting_comments[i,"comment.y"]
        
        data.frame(
          play_id_num = conflicting_comments[i,"play_id_num"],
          comment = paste0(db_comment,"<br>",local_comment)
        )
      }))
      
      updated_comments <- bind_rows(
        new_comments, 
        new_comments_2
      )
      
      displayedData$comments <- bind_rows(
        anti_join(
          displayedData$comments,
          updated_comments,
          by = "play_id_num"
        ),
        updated_comments
      )
      
    displayedData$pbp <- displayedData$pbp %>% 
        select(-comment) %>% 
        left_join(displayedData$comments, by = "play_id_num") %>% 
        arrange(play_id_num)
      
    displayedData$comment_commit_up_to_date <- FALSE
    
    replaceData(proxy, displayedData$pbp, resetPaging = FALSE, rownames = FALSE)
    
    }
    
  })
  
}


shinyApp(ui, server)
