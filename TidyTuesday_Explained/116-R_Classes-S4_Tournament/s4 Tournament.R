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

# Episode 115 ---- 

#define team object
setClass(
  Class = "team",
  ## what sort of data can be entered
  slots = c(team_name = "character", strength = "numeric", sd = "numeric"),
  ## default values
  prototype = list(strength = 0, sd = 1)
)

new_team <- function(team_name = "", strength, sd){
  new(
    "team",
    team_name = team_name, 
    strength = strength,
    sd = sd
  )
}

## Create new teams. These are representations of the 8 teams in the league of interest
## The Strength of each team is based on their performance across the season and 
## the sd of their performance

## exactly like s3 since we created a constructor function!

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

predict_team_performance <- function(object, nsims = 100){
  
  rnorm(n = nsims,
        mean = object@strength,
        sd = object@sd)
}


predict_team_performance(Jayhawks, 1000)

## To estimate which team will win a game, we run simulations.
## This function takes in two teams, and the number of simulations we plan on running/
## it ges the predicted performance of nsims games, and then figures out which team
## wins the most of the simulations.

## define "game_results" object - based on a suggestion from a viewer

setClass("game_results",
         slots = c(team1 = "team", team2 = "team", predicted_results = "data.frame", seed = "numeric")
)

setMethod("show",signature = signature(object = "game_results"),
          function(object){
            cat(
              paste0(
                object@team1@team_name,
                " vs ",
                object@team2@team_name,
                ", Winner: ",
                get_winner(object)@team_name,
                "; Won ",
                get_winner_pct(object),
                " of simulated games\n"
              )
            )
            invisible(object)
          })


# creating our own generics
setGeneric("get_winner", function(object) {
  standardGeneric("get_winner")
})

setMethod("get_winner", signature(object = "game_results"),
          function(object) {
            winner_t1 <-
              mean(object@predicted_results$team1_sim > object@predicted_results$team2_sim) >= .5
            if (winner_t1) {
              object@team1
            } else{
              object@team2
            }
          })

setGeneric("get_winner_pct", function(object) {
  standardGeneric("get_winner_pct")
})

setMethod("get_winner_pct", signature(object = "game_results"), function(object) {
  t1_win_pct <-
    mean(object@predicted_results$team1_sim > object@predicted_results$team2_sim)
  
  if (t1_win_pct >= .5) {
    scales::percent(t1_win_pct)
  } else{
    scales::percent(1 - t1_win_pct)
  }
})

run_sim_game <-function(team1, team2, nsims = 10000, seed = sample(1:10000,1)){
  
  set.seed(seed)
  
  sims <- data.frame(
    team1_sim = predict_team_performance(team1, nsims),
    team2_sim = predict_team_performance(team2, nsims)
  )
  
  new("game_results", team1 = team1, team2 = team2, predicted_results = sims, seed = seed)
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


# Episode 116 --------------------------------------

## use generics
setGeneric("run_sim_game", function(object, ...) {
  standardGeneric("run_sim_game")
})

setMethod("run_sim_game", signature(object = "team"), 
          function(object, team2, nsims = 10000, seed = sample(1:10000,1)){
            
            set.seed(seed)
            
            sims <- data.frame(
              team1_sim = predict_team_performance(object, nsims),
              team2_sim = predict_team_performance(team2, nsims)
            )
            
            new("game_results", team1 = object, team2 = team2, predicted_results = sims, seed = seed)
          })

run_sim_game(Jayhawks, Chickenhawks, 10000)
run_sim_game(Riverhawks, Fauxhawks, 10000)


# If we wanted to simulate a four team tournament rounds: 

run_sim_game(
  get_winner(show(run_sim_game(Jayhawks, Chickenhawks, 10000))),
  get_winner(show(run_sim_game(Riverhawks, Fauxhawks, 10000))),
10000)


## Thats klunky, lets make a "matchup" representation
setClass("matchup",
         slots = c(team1 = "team", team2 = "team")
)

matchup <- function(team1, team2){
  new("matchup",
      team1 = team1,
      team2 = team2
  )
}

# create generic for running sim games on a matchup object

setMethod("run_sim_game", signature(object = "matchup"), 
          function(object, nsims = 10000, seed = sample(1:10000,1)){
            run_sim_game(object@team1, object@team2, nsims = nsims, seed = seed)
          })

run_sim_game(matchup(Jayhawks, Chickenhawks))
run_sim_game(Jayhawks, Chickenhawks)


run_tournament_round <- function(
    matchup_list,
    nsim =10000 
    ){
  
  round_winner <- lapply(
    matchup_list,
    function(x){
      game_sim <- run_sim_game(x, nsim = nsim)
      show(game_sim)
    }
  )
  
  invisible(round_winner)
  
}

run_tournament_round(list(
  matchup(Jayhawks, Chickenhawks),
  matchup(Riverhawks, Fauxhawks)
))

run_tournament <- function(..., nsim = 10000){
  
  matchup_list <- list(...)
  
  rounds <- 1
  
  round_list <- list()
  
  while(length(matchup_list) >= 1){
    
    cat(paste0("\nRound: ", rounds, "\n"))
    
    round_results <- run_tournament_round(matchup_list, nsim = nsim)
    
    round_list[[rounds]] <- round_results
    
    round_winners <- lapply(round_results, get_winner)
    
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
  
  invisible(round_list)
  
}

#EAST
run_tournament(
  matchup(Jayhawks, Chickenhawks),
  matchup(Riverhawks, Fauxhawks),
  matchup(greyhawks, orangehawks),
  matchup(hawkietalkies,hawkeye)
) -> tournament_details


tournament_details[[1]]

library(ggplot2)

tournament_details[[3]][[1]]@predicted_results

ggplot(data = tournament_details[[3]][[1]]@predicted_results) + 
  geom_histogram(aes(x = team1_sim),fill = "green", alpha = .5) + 
  geom_histogram(aes(x = team2_sim),fill = "blue", alpha = .5) + 
  ggtitle(
    label = paste0(tournament_details[[3]][[1]]@team1@team_name, " (green) vs ",
                   tournament_details[[3]][[1]]@team2@team_name, " (blue)")
  )


sim_tournament <- function(..., nsims = 1000){
  
  sim_winner <- c()
  
  for(i in seq_len(nsims)){
    
    shh <- capture.output({
    tournament_results <- run_tournament(...,nsim = 1)
    })
    
    winner <- get_winner(tournament_results[[length(tournament_results)]][[1]])
    sim_winner <- c(
      sim_winner, winner@team_name)
  }
  
  
  sim_winner
  
}

res <- sim_tournament(
  matchup(Jayhawks, Chickenhawks),
  matchup(Riverhawks, Fauxhawks),
  matchup(greyhawks, orangehawks),
  matchup(hawkietalkies,hawkeye)
) 

prop.table(table(res))

