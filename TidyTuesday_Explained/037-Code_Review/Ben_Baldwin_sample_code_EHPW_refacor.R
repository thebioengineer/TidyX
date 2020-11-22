library(tidyverse)

# json file of player participation from week 1 SEA-ATL
# courtesy of sport radar
pbp <- readRDS(url("https://github.com/guga31bb/sport_radar/blob/master/data/participation/c2b24b1a-98c5-465c-8f83-b82e746b4fcf.rds?raw=true"))

print(paste(pbp$summary$home$alias, pbp$summary$away$alias, "week", pbp$summary$week$sequence))


# the goal: make a flat df with each play one row
# and the players on the field as columns
# (or do it in a better way)
# ben's bad code is below

player_names <- function(player_dat){
      if(is.null(player_dat)){
        NULL
      }else{
        player_dat$name %>% 
          t() %>% 
          data.frame() %>% 
          set_names(1:nrow(player_dat))
      }
  }

bigdat <- NULL
# for (i in 2:length(pbp$plays$description)) {
  
  # the home players on the field for play i
  home_players <- pbp$plays$home.players %>% 
    map_dfr(player_names) %>% 
    set_names(
      paste0("h",1:ncol(.))
    )
    
  
  # the away players on the field for play 
  away_players <- pbp$plays$away.players %>% 
    map_dfr(player_names) %>% 
    set_names(
      paste0("a",1:ncol(.))
    )
  
  supplementary <- pbp$plays[c("sequence","clock","description","type","deleted")] %>%
    do.call('cbind',.) %>% 
    data.frame() %>% 
    filter(is.na(deleted))%>% 
    select(sequence, clock, description, type) %>% 
    rename(
      play_id = sequence,
      time = clock,
      desc = description,
      play_type = type
    )
  
  bigdat <- tibble(
    supplementary,
    home_players,
    away_players
  )
  
testthat::expect_equivalent(
  bb_bigdat[1:20,intersect(colnames(bb_bigdat),colnames(bigdat))],
  bigdat[2:21,intersect(colnames(bb_bigdat),colnames(bigdat))]
)

data <- bigdat %>% 
  mutate(
    home_team = pbp$summary$home$alias,
    away_team = pbp$summary$away$alias,
    week      = pbp$summary$week$sequence,
    season    = pbp$summary$season$year,
  ) %>% 
  mutate(
    # create quarter to join with external data
    quarter_seconds_remaining = lubridate::period_to_seconds(lubridate::ms(time)),
    increment = ifelse(lag(quarter_seconds_remaining) < quarter_seconds_remaining,1,0),
    increment = ifelse(is.na(increment), 0, increment),
    qtr = 1 + cumsum(increment)
  )


sports_radar_pbp <- function(game_url){
  
  #reads RDS URL
  pbp <- readRDS(url(game_url))
  
  # sanity check
  print(paste(pbp$summary$home$alias, pbp$summary$away$alias, "week", pbp$summary$week$sequence))
  
  
  # function to extract player names
  player_names <- function(player_dat){
    if(is.null(player_dat)){
      NULL
    }else{
      player_dat$name %>% 
        t() %>% 
        data.frame() %>% 
        set_names(1:nrow(player_dat))
    }
  }
  
  # home team players
  home_players <- pbp$plays$home.players %>% 
    map_dfr(player_names) %>% 
    set_names(
      paste0("h",1:ncol(.))
    )
  
  # the away players on the field for play 
  away_players <- pbp$plays$away.players %>% 
    map_dfr(player_names) %>% 
    set_names(
      paste0("a",1:ncol(.))
    )
  
  # supplementary play info
  supplementary <- pbp$plays[c("sequence","clock","description","type","deleted")] %>%
    do.call('cbind',.) %>% 
    data.frame() %>% 
    filter(is.na(deleted))%>% 
    select(sequence, clock, description, type) %>% 
    rename(
      play_id = sequence,
      time = clock,
      desc = description,
      play_type = type
    )
  
  # join all data together, assuming rows match
  bigdat <- tibble(
    supplementary,
    home_players,
    away_players
  )
  
  # final data clean up, addition of quarter time
  final_dat <- bigdat %>% 
    mutate(
      home_team = pbp$summary$home$alias,
      away_team = pbp$summary$away$alias,
      week      = pbp$summary$week$sequence,
      season    = pbp$summary$season$year,
    ) %>% 
    mutate(
      # create quarter to join with external data
      quarter_seconds_remaining = lubridate::period_to_seconds(lubridate::ms(time)),
      increment = ifelse(lag(quarter_seconds_remaining) < quarter_seconds_remaining,1,0),
      increment = ifelse(is.na(increment), 0, increment),
      qtr = 1 + cumsum(increment)
    )
  
  return(final_dat)
}


tidyx_pbp <- sports_radar_pbp("https://github.com/guga31bb/sport_radar/blob/master/data/participation/c2b24b1a-98c5-465c-8f83-b82e746b4fcf.rds?raw=true")

multi_games <- c("https://github.com/guga31bb/sport_radar/blob/master/data/participation/c2b24b1a-98c5-465c-8f83-b82e746b4fcf.rds?raw=true",
                 "https://github.com/guga31bb/sport_radar/blob/master/data/participation/c2b24b1a-98c5-465c-8f83-b82e746b4fcf.rds?raw=true",
                 "https://github.com/guga31bb/sport_radar/blob/master/data/participation/c2b24b1a-98c5-465c-8f83-b82e746b4fcf.rds?raw=true",
                 "https://github.com/guga31bb/sport_radar/blob/master/data/participation/c2b24b1a-98c5-465c-8f83-b82e746b4fcf.rds?raw=true") 

multi_games_output <- map(multi_games, sports_radar_pbp)



