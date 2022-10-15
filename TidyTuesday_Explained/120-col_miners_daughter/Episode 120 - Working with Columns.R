
# TidyX Episode 120 - working with columns

library(tidyverse)
library(rvest)

## load Seattle Mariners historical Season data from baseball-reference.com

team_stats <- read_html("https://www.baseball-reference.com/teams/SEA/") %>% 
  html_table(header = TRUE) %>% 
  pluck(1)

## Selecting columns ----

### Unquoted names
team_stats %>% 
  select(Year, `W-L%`, `pythW-L%`, R, RA, Finish, GB)

#### Numeric position
team_stats %>% 
  select(1, 8,9,13,14,10,11)

### Tidyselect statements
selected_team_stats <- team_stats %>% 
  select(Year, ends_with("W-L%"), starts_with("R"), Finish, GB)

selected_team_stats

## ends_with, starts_with, contains are all literal strings
## matches is a regex (see episodes 62/63: bit.ly/TidyX_Ep62 or  bit.ly/TidyX_Ep63)

team_stats %>% 
  select(Year, matches("W[-]L[%]$"), matches("^R"), Finish, GB)

## where() allows you to select on conditions in column

team_stats_wide <- team_stats %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(
    cols = 3:ncol(.),
    names_to = "stat",
    values_to = "value"
  ) %>% 
  pivot_wider(
    names_from = Year,
    values_from = value
  )

team_stats_wide %>% 
  select(
    Tm, stat,
    where(function(x){
      suppressWarnings(value <- as.numeric(x[6]))
      isTRUE(value > .5)
    })
  )

## Renaming ----

### During selection
team_stats %>% 
  select(
    season = Year, 
    win_loss_pct = `W-L%`,
    win_loss_pythag = `pythW-L%`,
    runs_scored = R,
    runs_allowed = RA,
    finish = Finish, 
    games_back = GB
    )

### Rename functions
selected_renamed_team_stats <- selected_team_stats %>% 
  rename(
    season = Year, 
    win_loss_pct = `W-L%`,
    win_loss_pythag_pct = `pythW-L%`,
    runs_scored = R,
    runs_allowed = RA,
    finish = Finish, 
    games_back = GB
  )

selected_team_stats %>% 
  rename_with(
    function(x){
      str_replace_all(x, "%","_pct") %>% 
      str_replace_all("W-L","win_loss")
    },
    ends_with("W-L%")
  ) %>% 
  rename_with(
    function(x){
      str_replace_all(x, "R","runs")
    },
    starts_with("R")
  )

### Janitor 

selected_team_stats %>% 
  janitor::clean_names()


## Moving columns ----

### Using Select

selected_renamed_team_stats %>% 
  select(season, finish, games_back, everything())

## can lead to some weird behavior if you try to do multiple things
## This 100% makes sense if you break down what this is doing though.
selected_renamed_team_stats %>% 
  select(season,  finish, -games_back, everything())

### Relocate to move columns *** 

selected_renamed_team_stats %>% 
  relocate(finish, games_back, .after = season)




## Pull vs Pluck ----

### pluck is a wrapper around `[[`
team_stats %>% 
  pluck(1)

### pull is designed to be a bit more...interesting
team_stats %>% 
  pull(1)

#### apply names to pulled column via another column!
team_stats %>% 
  pull(`W-L%`, name = Year)

team_stats %>% 
  pull(`W-L%`)

#### Defaults to pulling last column
team_stats %>% pull()
team_stats %>% pluck()






