# TidyX Episode 155 - Writing Functions

library(tidyverse)
library(Lahman)

theme_set(theme_light())

batting <- Batting %>%
  filter(yearID >= 2010) %>%
  mutate(batting_avg = H / AB) %>%
  select(playerID, yearID, AB, H, batting_avg)

teams <- Teams %>%
  filter(yearID >= 2010) %>%
  mutate(runs_per_game = R / G) %>%
  select(yearID, teamID, G, R, runs_per_game)

### Functions
#
# Functions are useful for encapsulating ideas
# 
# Given input, get specific result (value, object, side effect**)
#
# Functions should be independent - nothing it uses should be 
# defined outside the function. If it is derived elsewhere, pass it as an argument
#
# FUNCTIONNAME <- function(PARAMS/ARGUMENTS){
#   FUNCTION BODY
# }
#
#

# Example of a useful function:
# Get alpha and beta parameters for a population using beta distribution
# 
# - Function name, get_beta_parameters is clear
# - parameters are clearly named for what is expected
# - returns a list object with 2 values
# 

# Code is from TidyX Episode 100 for more information on the beta distribution
get_beta_parameters <- function(population_avg, population_var){
  
  alpha <- population_avg * (population_avg * (1 - population_avg)/population_var - 1)
  beta <- alpha * (1 - population_avg)/population_avg
  
  list(alpha = alpha, beta = beta)
  
}

pop_avg <- batting %>% 
  filter(
    AB >= quantile(AB, probs = 0.75)) %>% 
  pull(batting_avg) %>%
  mean()
pop_var <- batting %>% 
  filter(AB >= quantile(AB, probs = 0.75)) %>% 
  pull(batting_avg) %>%
  var()

pop_avg
pop_var
sqrt(pop_var)

batting_avg_params <- get_beta_parameters(
  population_avg = pop_avg,
  population_var = pop_var
  )

batting_avg_params

ba_alpha_prior <- batting_avg_params$alpha
ba_beta_prior <- batting_avg_params$beta

ba_alpha_prior

# Document your functions within your R scripts using basic comments
# explaining _what they do_

# Functions to calculate mean and standard deviation from alpha and beta
# parameters of a beta distribution
beta_mu <- function(alpha, beta){
  
  mu <- alpha / (alpha + beta)
  return(mu)
}
  
beta_sd <- function(alpha, beta){
  
  s <- sqrt((alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1)))
  return(s)
}

beta_mu(alpha = ba_alpha_prior,
        beta = ba_beta_prior)

beta_sd(alpha = ba_alpha_prior,
        beta = ba_beta_prior)

# create function to calculate the posterior for the beta distribution
posterior_beta_mu <- function(prior_alpha, prior_beta, n, success){
  
  posterior_alpha <- prior_alpha + success
  posterior_beta <- prior_beta + (n - success)
  
  mu <- posterior_alpha / (posterior_alpha + posterior_beta)
  return(mu)
}

posterior_beta_sd <- function(prior_alpha, prior_beta, n, success){
  
  posterior_alpha <- prior_alpha + success
  posterior_beta <- prior_beta + (n - success)
  
  s <- sqrt((posterior_alpha * posterior_beta) / ((posterior_alpha + posterior_beta)^2 * (posterior_alpha + posterior_beta + 1)))
  return(s)
}


# Writing functions also means that you can encapsulate ideas, and also name
# your functions similarly to make it easier to remember which functions 
# do what

## Get alpha and beta parameters for population using gamma distribution
# Functions from Episode 101 for more information on the gamma distribution

get_gamma_parameters <- function(population_avg, population_var){
  
  alpha <- population_avg^2 / population_var
  beta <- population_avg / population_var
  
  list(alpha = alpha, beta = beta)
  
}

rpg_mean <- mean(teams$runs_per_game)
rpg_var <- var(teams$runs_per_game)

rpg_mean
rpg_var
sqrt(rpg_var)

rpg_params <- get_gamma_parameters(population_avg = rpg_mean,
                 population_var = rpg_var)

rpg_alpha_prior <- rpg_params$alpha
rpg_beta_prior <- rpg_params$beta

## Functions to calculate mean and standard deviation from alpha and beta parameters 
# of a gamma distribution

gamma_mu <- function(alpha, beta){
  mu <- alpha / beta
  return(mu)
}

gamma_sd <- function(alpha, beta){
  s <- alpha / beta^2
  return(s)
}

gamma_mu(rpg_alpha_prior, rpg_beta_prior)
gamma_sd(rpg_alpha_prior, rpg_beta_prior)

#### Applying the functions
set.seed(559)
small_batting <- batting %>%
  sample_n(size = 10)

small_batting %>%
  mutate(posterior_batting_avg = posterior_beta_mu(ba_alpha_prior, ba_beta_prior, n = AB, success = H),
         posterior_batting_sd = posterior_beta_sd(ba_alpha_prior, ba_beta_prior, n = AB, success = H))


teams %>%
  mutate(alpha_posterior = rpg_alpha_prior + R,
         beta_posterior = rpg_beta_prior + G,
         posterior_rpg_avg = gamma_mu(alpha_posterior, beta_posterior),
         posterior_rpg_sd = gamma_sd(alpha_posterior, beta_posterior)) %>%
  head()
