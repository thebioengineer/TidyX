## code to prepare `lahman_batting_and_team_runs` dataset goes here

library(tidyverse)
library(Lahman)

lahman_batting_2010 <- Batting %>%
  filter(yearID >= 2010) %>%
  mutate(batting_avg = H / AB) %>%
  select(playerID, yearID, AB, H, batting_avg)

lahman_team_runs_2010 <- Teams %>%
  filter(yearID >= 2010) %>%
  mutate(runs_per_game = R / G) %>%
  select(yearID, teamID, G, R, runs_per_game)


usethis::use_data(lahman_batting_2010, overwrite = TRUE)
usethis::use_data(lahman_team_runs_2010, overwrite = TRUE)
