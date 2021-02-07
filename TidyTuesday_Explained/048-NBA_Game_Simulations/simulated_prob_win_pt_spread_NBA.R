
library(tidyverse)
library(rvest)


#### Getting Data #####
pull_nba_data <- function(month){
  
  url <- paste0(
    "https://www.basketball-reference.com/leagues/NBA_2021_games-",
    tolower(month),
    ".html"
  )
  
  html_page <- read_html(url)

  html_page %>%
    html_table(fill = TRUE) %>%
    .[1] %>%
    as.data.frame() %>%
    select(visitor = Visitor.Neutral,
           v_pts = PTS,
           home = Home.Neutral,
           h_pts = PTS.1) %>% 
    filter(!is.na(v_pts))
}

nba <- c("december","january","february") %>% 
  map_dfr(
    ~pull_nba_data(.x)
  )

nba %>% head()

all_teams <- unique(c(nba$visitor, nba$home))

all_team_stats <- all_teams %>% 
  
  map_dfr(function(team, nba_data){
  
  home_games <- nba_data$home == team
  away_games <- nba_data$visitor == team
  
  points_for <- mean(
    c(nba_data$h_pts[home_games],
      nba_data$v_pts[away_games])
  )
  
  points_for_sd <- sd(
    c(nba_data$h_pts[home_games],
      nba_data$v_pts[away_games])
  )
  
  points_against <- mean(
    c(nba_data$v_pts[home_games],
      nba_data$h_pts[away_games])
  )
  
  data.frame(
    team = team,
    pf = points_for,
    pf_sd = points_for_sd,
    pa = points_against,
    n_games = sum(c(home_games,away_games))
  )
}, nba)

#### Example of the simulation ####
## Get Pts For, Pts Against, and SD of Pts For, for the two teams of interest

team1 <- "Golden State Warriors"
team2 <- "Los Angeles Lakers"

tm1_pf <- all_team_stats[all_team_stats$team == team1, "pf"]
tm1_pa <- all_team_stats[all_team_stats$team == team1, "pa"]
tm1_sd_pf <- all_team_stats[all_team_stats$team == team1, "pf_sd"]

tm2_pf <- all_team_stats[all_team_stats$team == team2, "pf"]
tm2_pa <- all_team_stats[all_team_stats$team == team2, "pa"]
tm2_sd_pf <- all_team_stats[all_team_stats$team == team2, "pf_sd"]

## adjust points for for both teams based on opponent points against

tm1_adj_pts <- sqrt(tm1_pf * tm2_pa)
tm2_adj_pts <- sqrt(tm2_pf * tm1_pa)

## Let's get some simulated scores for team 1 and team 2

# simulated score for tm1
qnorm(
  runif(1),
  mean = tm1_adj_pts,
  sd = tm1_sd_pf)

# simulated score for tm2
qnorm(
  runif(1),
  mean = tm2_adj_pts,
  sd = tm2_sd_pf)


## Run the simulation 10,000 times to get probability that one team beats the other
N <- 1e4
outcome <- vector("character", length = N)

for(i in 1:N){
  
  d <- qnorm(runif(1), mean = tm1_adj_pts, sd = tm1_sd_pf) - 
    qnorm(runif(1), mean = tm2_adj_pts, sd = tm2_sd_pf)
  
  d <- ifelse(d > 0, team1, team2)
  
  outcome[i] <- d
  
}

table(outcome)
prop.table(table(outcome))
barplot(prop.table(table(outcome)))

## Run simulation and get the estimated point spread of the game

## make a function to simulate a single game

simulate_game <- function( tm1_mean, tm1_sd, tm2_mean, tm2_sd){
  
  tm1 <- qnorm(runif(1), mean = tm1_mean, sd = tm1_sd)
  tm2 <- qnorm(runif(1), mean = tm2_mean, sd = tm2_sd)
  
  data.frame(
    tm1 = tm1,
    tm2 = tm2,
    point_diff = tm1 - tm2,
    winner = ifelse(tm1 > tm2, "tm1", "tm2")
  )
}

## Run sumulation N times (10000)
N <- 1e4

simulated_games <- seq_len(N) %>% 
  map_dfr(
    ~simulate_game(
      tm1_adj_pts, tm1_sd_pf,
      tm2_adj_pts, tm2_sd_pf
    )
  )


head(simulated_games)

hist(simulated_games$point_diff, col = "pale green")

abline(v = mean(simulated_games$point_diff), col = "red", lty = "dashed", lwd = 4)

quantile(simulated_games$point_diff)

mean(simulated_games$point_diff)



