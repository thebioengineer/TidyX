
## TidyX Episode 74: Joins with Databases

### Packages ---------------------------------------------
library(tidyverse)
library(rvest)
library(janitor)

library(RSQLite)
library(DBI)

library(microbenchmark)

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
  
  dbWriteTable(con, name = paste0("game_",game_id), full_game_pbp, overwrite = TRUE)
  
  if("game_ids" %in% dbListTables(con)){
    hist_game_table <- dbReadTable(con, "game_ids")
    if(!game_id %in% hist_game_table$game_id){
      game_df <- rbind(hist_game_table, game_df)
    }
  }
  
  dbWriteTable(con, name = "game_ids", game_df, overwrite = TRUE)
}

# Write several games to database ---------------------------------------------

## create connection
db_con <- dbConnect(
  drv = RSQLite::SQLite(),
  here::here("TidyTuesday_Explained/074-Databases_with_R_3/nba_playoffs.db")
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

##disconnect
dbDisconnect(db_con)

# Interact with database ---------------------------------------------

# create connection
db_con <- dbConnect(
  drv = RSQLite::SQLite(),
  here::here("TidyTuesday_Explained/074-Databases_with_R_3/nba_playoffs.db")
)

## list tables
dbListTables(db_con)


game_info <- tbl(db_con, "game_ids") %>% 
  filter( game_info == "East 1st Round - Game 3")

play_of_interest<- tbl(db_con, "game_401327879") %>%
  filter(play_id_num == "12")

## Join information ----

### Locally 

#### This only works if tables can fit into memory!~

game_pbp <- tbl(db_con, "game_401327879") %>% collect()
game_info <- tbl(db_con, "game_ids") %>% collect()

full_game_info <- game_pbp %>% 
  mutate(
    game_id_number = "401327879"
  ) %>% 
  left_join(
    game_info,
    by = c("game_id_number" = "game_id")
  )

## Using SQL Joins

## this will work, and only the result needs to be able to fit into memory

full_game_info_sql <- tbl(db_con, "game_401327879") %>% 
  mutate(
    game_id_number = "401327879"
  ) %>% 
  left_join(
    tbl(db_con, "game_ids"),
    by = c("game_id_number" = "game_id")
  )

show_query(full_game_info_sql)




## Benchmark joins ----

mb_sql_comparison <- microbenchmark(
  
  sql_join = {
    
    tbl(db_con, "game_401327879") %>% 
      mutate(
        game_id_number = "401327879"
      ) %>% 
      left_join(
        tbl(db_con, "game_ids"),
        by = c("game_id_number" = "game_id")
      )
    
  },
  
  load_to_memory = {
    
    game_pbp <- tbl(db_con, "game_401327879") %>% collect()
    game_info <- tbl(db_con, "game_ids") %>% collect()
    
    game_pbp %>% 
      mutate(
        game_id_number = "401327879"
      ) %>% 
      left_join(
        game_info,
        by = c("game_id_number" = "game_id")
      )
  }
)

autoplot(mb_sql_comparison)

dbDisconnect(db_con)



