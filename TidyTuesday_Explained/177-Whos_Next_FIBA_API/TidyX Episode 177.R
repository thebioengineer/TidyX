### TidyX Episode 177 - Who's Next? FIBA API Viewer Question

## Cohen MacDonald Submitted a question: 
## I found a non-documented URL that provides the game data for FIBA basketball 
## games, provided you have the correct game-Id. (example here:
## https://fibalivestats.dcd.shared.geniussports.com/data/2290252/data.json) 

## The game JSON provides a bunch of datasets including, box-stats, play-by-play
## and shot locations. The play by play data has information on substitutions, 
## and I would like to be able to do some lineup analysis, but I am having 
## trouble figuring out how to establish who is on the court at all times of the
## game.

## I have attached an R script that should let you see where I get stuck. 

library(tidyverse)
library(httr)
library(purrr)
library(jsonlite)
library(glue)

## GAME ID Provided by Cohen in example
### Get game info from fibalivestats

gameid <- 2290252 #2290275 #games_to_scrape$id

FIBA_API_JSON <- readLines(
  glue("https://fibalivestats.dcd.shared.geniussports.com/data/{gameid}/data.json")
  )

FIBA_game <- jsonlite::parse_json(
  FIBA_API_JSON,
  ## Where possible, converts to recognizable R objects (vectors, data.frames)
  simplifyVector = TRUE 
  )

names(FIBA_game)

head(FIBA_game$pbp)

pbp_df <- FIBA_game$pbp %>% 
  arrange(actionNumber)

table(pbp_df$actionType)

### Code from Cohen, modified by TidyX
starters <- pbp_df %>%
  filter(actionType == 'substitution' & periodType == "REGULAR") %>% 
  arrange(tno, actionNumber) %>%
  group_by(tno, player) %>% 
  ## get first "SUBTYPE" value for each player
  ## If the first time a player appears is as "out', must have been a starter
  filter(row_number() == 1 & subType == "out") %>%  
  ungroup() %>% 
  mutate(
    player_name = paste(internationalFirstName, internationalFamilyName)
  ) %>% 
  arrange(tno, player_name) %>% 
  select(tno, player_name)
  

sub_pbp_df <- pbp_df %>%
  mutate(
    player_name = paste(internationalFirstName, internationalFamilyName)
  ) %>% 
  filter(actionType == "substitution") %>% 
  select(gt, clock, period, periodType, tno, actionNumber, player_name, subType) %>% 
  group_by(gt, clock, period, periodType, tno) %>% 
  group_split() %>%
  map(
    \(x){
      tibble(
        gt = x$gt[[1]],
        clock = x$clock[[1]],
        period = x$period[[1]],
        periodtype = x$periodType[[1]], 
        tno = x$tno[[1]],
        action_number_1 = x$actionNumber[x$subType == "in"],
        action_number_2 = x$actionNumber[x$subType == "out"],
        sub_in = x$player_name[x$subType == "in"],
        sub_out = x$player_name[x$subType == "out"]
      )
    }
  ) %>% 
  bind_rows() %>% 
  arrange(tno, action_number_1)

### Now lets construct the line up over time

lineup_over_time <- list(
  tibble(
    action = 0,
    gt = "10:00",
    clock = "10:00:00",
    period = 1,
    periodtype = "REGULAR",
    t1_lineup = list(starters$player_name[starters$tno == 1]),
    t2_lineup = list(starters$player_name[starters$tno == 2])
  )
)

## for loop because the next iteration relies on the previous

current_lineup_row <- lineup_over_time[[1]]
for(i in seq_len(nrow(sub_pbp_df))){
  
  substitution <- sub_pbp_df[i, ]
  
  sub_team <- substitution$tno
  
  current_lineup_team <- current_lineup_row[[paste0("t",sub_team,"_lineup")]][[1]]
  new_lineup_team <- c(setdiff(
    current_lineup_team,
    substitution$sub_out),
    substitution$sub_in
    )
  
  current_lineup_row[["action"]] <- substitution$action_number_1
  current_lineup_row[["gt"]] <- substitution$gt
  current_lineup_row[["clock"]] <- substitution$clock
  current_lineup_row[["period"]] <- substitution$period
  current_lineup_row[["periodtype"]] <- substitution$periodtype
  current_lineup_row[[paste0("t",sub_team,"_lineup")]][[1]] <- new_lineup_team
  
  lineup_over_time[[i+1]] <- current_lineup_row
  
}

lineup_over_time_df <- lineup_over_time %>% 
  bind_rows()
  
t1_players_over_time <- lineup_over_time_df %>% 
  select(gt:periodtype, t1_lineup) %>% 
  unnest(t1_lineup) %>% 
  mutate(is_in = 1) %>% 
  pivot_wider(
    names_from = t1_lineup,
    values_from = is_in,
    values_fn = unique,
    values_fill = 0
  ) %>% 
  mutate(
    # Make quarter time
    elapsed_quarter_time = 10-as.numeric(format(as.POSIXct(gt, format = "%M:%OS"), "%M.%S")),
    elapsed_game_time = case_when(
      periodtype == "REGULAR" ~ elapsed_quarter_time +  ((period-1) * 10),
      periodtype == "OVERTIME" ~ elapsed_quarter_time + ((period-1) * 10) + 40
    )
  ) %>% 
  pivot_longer(
    5:(ncol(.) -2),
    names_to = "player"
    ) %>% 
  group_by(player) %>% 
  filter(c(TRUE,diff(value)!=0)) %>% 
  mutate(
    t1 = elapsed_game_time, # get current records time
    t2 = lead(elapsed_game_time) ## get next time
    ) %>% 
  ungroup()

## get max game time to drop into "NA" values for t2
max_time <- 40

t1_players_over_time_maxtime <- t1_players_over_time %>% 
  mutate(
    t2 = case_when(
      is.na(t2) ~ max_time, 
      TRUE ~ t2
    )
  )
         

t1_players_over_time_maxtime %>% 
  filter(
    value == 1,
  ) %>% 
  mutate(
    is_starter = player %in% starters$player_name[starters$tno == 1],
    player = factor(player, levels = unique(player))
  ) %>% 
  select(
    player, t1, t2, is_starter
  ) %>% 
  ggplot() + 
  geom_segment(
    aes(
      x = t1,
      xend = t2,
      y = player, 
      yend = player,
      color = is_starter
    ),
    linewidth = 4
  ) + 
  ggtitle(
    paste0("Player Roster for Team 1 (",FIBA_game$tm$`1`$code,")")
  ) + 
  scale_y_discrete(limits=rev) + ## list starters first!
  scale_x_continuous(
    expand = c(0,0), 
    breaks = c(10, 20, 30, 40), 
    limits = c(0, 41),
    labels = paste0("Q", c("1", "2", "3", "4"), " end")
    ) + 
  theme(
    axis.line.x =  element_line(colour = "black"),
    axis.line.y =  element_line(colour = "black"),
    axis.text.x = element_text(size = 10, face = "bold", colour = "black"),
    axis.ticks.y =  element_line(colour = "black"),
    axis.text.y = element_text(size = 15, colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
    ) 

  
