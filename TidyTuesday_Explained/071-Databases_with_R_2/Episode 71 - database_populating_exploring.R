
## TidyX Episode 71: 
# https://dbplyr.tidyverse.org/

### Packages ---------------------------------------------
library(tidyverse)
library(rvest)
library(janitor)

library(dbplyr)

library(RSQLite)
library(DBI)

### Get Data ---------------------------------------------

nba_2021_months <- read_html(
  "https://www.basketball-reference.com/leagues/NBA_2021_games.html"
  ) %>% 
  html_nodes(".filter") %>% 
  html_text %>% 
  strsplit("\n+") %>% 
  pluck(1) %>% 
  parse_character() %>% 
  {.[!is.na(.)]} %>%
  tolower

nba_2021_raw <- purrr::map_dfr(
  nba_2021_months,
  function(.x){
    read_html(
    paste0("https://www.basketball-reference.com/leagues/NBA_2021_games-",.x,".html")
    ) %>% 
    html_table(fill = T) %>%
    as.data.frame() %>% 
    filter(Date != "Date") %>%  #remove repeated headers
    clean_names()
  })

## Create position groupings
nba_2021_clean <- nba_2021_raw %>% 
  mutate(
    game_date_time = as.POSIXct(paste0(date," ",start_et,"m"),format = "%a, %b %d, %Y %I:%M%p",tz = "US/Eastern"),
    game_id = paste0("2021_",seq_len(n())),
    visitor_points = as.numeric(pts),
    home_points = as.numeric(pts_1),
    attend = parse_number(attend)
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
    attend
  )

pull_season_data <- function(season, con, verbose = FALSE){
  
  nba_months <- read_html(
    paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_games.html")
  ) %>% 
    html_nodes(".filter") %>% 
    html_text %>% 
    str_split("\n+") %>% 
    pluck(1) %>% 
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

### Write seasons 2001-2021 to database ---------------------------------------------

# create connection
db_con <- dbConnect(
  drv = RSQLite::SQLite(),
  here::here("TidyTuesday_Explained/071-Databases_with_R_2/nba_seasons.db")
  )

## write table
purrr::walk(
  2001:2021, 
  pull_season_data,
  db_con
)

##disconnect
dbDisconnect(db_con)

### Interact with database ---------------------------------------------

db_con <- dbConnect(
  drv = RSQLite::SQLite(),
  here::here("TidyTuesday_Explained/071-Databases_with_R_2/nba_seasons.db")
)

## what tables exist?
dbListTables(db_con)


## Colnames of tables
#### super hacky
colnames(dbGetQuery(db_con, "SELECT * FROM `season_2001` where 1==0"))

#### nicer
colnames(tbl(db_con, "season_2001"))


## table headers
#### SQL
dbGetQuery(db_con, "SELECT * FROM `season_2001` LIMIT 6")

#### nicer
head_vals <- tbl(db_con, "season_2001") %>% 
  head()

head_vals %>% show_query()

## Grouping by
cc_home_points <- tbl(db_con, "season_2001") %>% 
  filter(home == "Cleveland Cavaliers") %>% 
  group_by( visitor ) %>% 
  summarize(
    home_points = mean(home_points)
  ) %>% 
  arrange(desc(home_points))

cc_home_points %>% 
  show_query()

dbGetQuery(db_con,
           "SELECT visitor, AVG(home_points) AS 'AVG Home Score'
           FROM season_2001
           WHERE Home = 'Cleveland Cavaliers'
           GROUP BY visitor
           ORDER BY `AVG Home Score` desc") 
