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
  
  stopifnot(is.numeric(strength))
  stopifnot(is.numeric(sd))
  stopifnot(strength > 0)
  stopifnot(sd > 0)
  
  structure(
    .Data = list(
      strength = strength, 
      sd = sd
    ),
    team_name = team_name,
    class = "team"
  )
  
}

print.team <- function(x,..){
  
  cat(paste0(
    "Team name: ", attr(x, "team_name"),"\n",
    "Strength: ", x$strength,"\n",
    "SD: ", x$sd,"\n"
  ))
  
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
        mean = x$strength,
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



## Episode 112 --------------------------------------

matchup <- function(team1, team2){
  structure(
    .Data = list(team1, team2),
    class = "matchup"
  )
}

run_sim_game(
  run_sim_game(Jayhawks, Chickenhawks, 10000),
  run_sim_game(Riverhawks, Fauxhawks, 10000),
10000)

run_tournament_round <- function(
    matchup_list,
    nsim =10000
){
  
  round_winner <- lapply(
    matchup_list,
    function(x){
      run_sim_game(x[[1]], x[[2]], nsim = nsim)
    }
  )
  
  round_winner
  
}

run_tournament_round(list(
  matchup(Jayhawks, Chickenhawks),
  matchup(Riverhawks, Fauxhawks)
))

run_tournament <- function(..., nsim = 10000){
  
  matchup_list <- list(...)
  
  rounds <- 1
  
  while(length(matchup_list) >= 1){
    
    cat(paste0("\nRound: ", rounds, "\n"))
    
    round_winners <- run_tournament_round(matchup_list, nsim = nsim)
    
    if(length(round_winners) > 1){
      matchup_list <- lapply(seq(1, length(round_winners), by = 2),function(i){
        matchup(round_winners[[i]], round_winners[[i+1]])
      })
      rounds <- rounds + 1
    }else{
      break
    }
  }
  
  cat(paste("\nTournament winner:", attr(round_winners[[1]], "team_name"),"\n"))
  
  invisible(round_winners[[1]])
  
}

#EAST
run_tournament(
  matchup(Jayhawks, Chickenhawks),
  matchup(Riverhawks, Fauxhawks),
  matchup(greyhawks, orangehawks),
  matchup(hawkietalkies,hawkeye)
)









