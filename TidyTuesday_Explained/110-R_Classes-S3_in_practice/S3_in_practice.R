## Problem statement:
##
## You are the Data Scientist for a a local sports betting company. The 
## season has just ended for a local sports league. And we want to predict who
## will will the whole enchilada
##
## First we need to sort out how we will simulate a single game
##

## First we need to make a function that defines a team. We have some numbers
## indicating the "strength" and "sd" of a team. 
##
## Note: This is related to our past Bayesian series
##
new_team <- function(team_name = "", strength, sd){
  
  structure(
    .Data = list(
      stength = strength, 
      sd = sd
    ),
    team_name = team_name,
    class = "team"
  )
  
}

## Create new teams. These are representations of the 8 teams in the league of interest
## The Strength of each team is based on their performance across the season and 
## the sd of their performance

Jayhawks <- new_team("Jayhawks", 5, 3)
Chickenhawks <- new_team("Chickenhawks", 4, 2)
Riverhawks <- new_team("Riverhawks", 5, 1.5)
Fauxhawks <- new_team("Fauxhawks", 6, 5)
greyhawks <- new_team("Greyhawks", 5, 3)
orangehawks <- new_team("Orangehawks", 3, 1)
hawkietalkies <- new_team("Hawkietalkies", 6, 8)
hawkeye <- new_team("Hawkeye", 8, 6)


## To estimate the distribution of a teams, we are pulling a random simulation 
## using their seasonal average strength and sd.
##
## We assume performance is normally distributed 
##
predict_team_performance <- function(x, nsims = 100){
  rnorm(n = nsims,
        mean = x$stength,
        sd = x$sd)
}

predict_team_performance(Jayhawks, 1000)

## To estimate which team will win a game, we run simulations.
## This function takes in two teams, and the number of simulations we plan on running/
## it ges the predicted performance of nsims games, and then figures out which team
## wins the most of the simulations.

run_sim_game <- function(team1, team2, nsims = 10000){
  
    team1_sim <- predict_team_performance(team1, nsims)
    team2_sim <- predict_team_performance(team2, nsims)
    
    winner_t1 <- mean(team1_sim > team2_sim) >= .5
    
    if(winner_t1){
      pct_wins <- scales::percent(mean(team1_sim > team2_sim))
    }else{
      pct_wins <- scales::percent(mean(team2_sim > team1_sim))
    }
    
    game_winner <- ifelse(winner_t1, attr(team1, "team_name"), attr(team2, "team_name"))
    
    cat(
      paste0(
        attr(team1, "team_name"),
        " vs ",
        attr(team2, "team_name"),
        ", Winner: ",
        game_winner,
        "; Won ",
        pct_wins,
        " of simulated games\n"
      )
    )
    
    if(winner_t1){
      invisible(team1)
    }else{
      invisible(team2)
    }
}

run_sim_game(Jayhawks, Chickenhawks, 10000)
run_sim_game(Riverhawks, Fauxhawks, 10000)


## what would we do if we wanted to see a random game between two teams
random_game <- function(...){
  teams <- list(...)
  team_game <- sample(seq_along(teams), 2, replace = FALSE)
  run_sim_game(teams[[team_game[[1]]]], teams[[team_game[[2]]]], 10000)
}

random_game(
  Jayhawks,
  Chickenhawks,
  Riverhawks,
  Fauxhawks,
  greyhawks, 
  orangehawks,
  hawkietalkies,
  hawkeye
)
