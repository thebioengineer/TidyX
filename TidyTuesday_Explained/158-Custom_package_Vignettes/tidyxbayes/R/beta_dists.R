#' Get beta distribution parameters
#'
#' Based on the population average and variance, calculate the alpha and beta
#' parameters that describe the beta distribution.
#'
#' The beta distribution should be used when the distribution being predicting
#' is a value between 0 and 1
#'
#' @param population_avg the population average, a length 1 numeric value
#'   describing the average (mean) value of the population
#' @param population_var the population variance, a length 1 numeric value
#'   describing the variance of the population
#'
#' @export
#'
#' @examples
#'
#' ## Set up the data
#'
#' pop_avg <- lahman_batting_2010 %>%
#'   filter(AB >= quantile(AB, probs = 0.75)) %>%
#'   pull(batting_avg) %>%
#'   mean()
#' pop_var <- lahman_batting_2010 %>%
#'   filter(AB >= quantile(AB, probs = 0.75)) %>%
#'   pull(batting_avg) %>%
#'   var()
#'
#' pop_avg
#' pop_var
#' sqrt(pop_var)
#'
#' ## Using the function
#'
#' batting_avg_params <- get_beta_parameters(
#'   population_avg = pop_avg,
#'   population_var = pop_var
#' )
#'
#' batting_avg_params
#'
get_beta_parameters <- function(population_avg, population_var){

  alpha <- population_avg * (population_avg * (1 - population_avg)/population_var - 1)
  beta <- alpha * (1 - population_avg)/population_avg

  list(alpha = alpha, beta = beta)

}

#' @title Beta Distribution Parameter - mu & sd
#'
#' @description Calculate mean from alpha and beta parameters of a beta
#'   distribution.
#'
#' @details See these functions in action in the
#'   \link[tidyxbayes::posterior_beta]{posterior_beta} help page
#'
#' @param alpha alpha parameter of a beta distribution
#' @param beta beta parameter of a beta distribution
#'
#'
#' @noRd
#'
beta_mu <- function(alpha, beta){

  mu <- alpha / (alpha + beta)
  return(mu)
}

#' @description Calculate standard deviation from alpha and beta parameters of a
#' beta distribution
#'
#' @noRd
#'
beta_sd <- function(alpha, beta){

  s <- sqrt((alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1)))
  return(s)
}


#' Calculate Posterior mu based on Beta distribution
#'
#' Calculate posterior mu and sd given priors, observations, and n success to
#' calculate
#'
#' @param prior_alpha beta distribution alpha parameter from population
#' @param prior_beta beta distribution beta parameter from population
#' @param n numeric value representing number of observations
#' @param success numeric value representing the number of success (positive) observations
#'
#' @rdname posterior_beta
#'
#' @export
#'
#' @examples
#'
#' ## Set up
#'
#' library(tidyverse)
#'
#' pop_avg <- lahman_batting_2010 %>%
#'   filter(AB >= quantile(AB, probs = 0.75)) %>%
#'   pull(batting_avg) %>%
#'   mean()
#' pop_var <- lahman_batting_2010 %>%
#'   filter(AB >= quantile(AB, probs = 0.75)) %>%
#'   pull(batting_avg) %>%
#'   var()
#'
#' pop_avg
#' pop_var
#' sqrt(pop_var)
#'
#' batting_avg_params <- get_beta_parameters(
#'   population_avg = pop_avg,
#'   population_var = pop_var
#' )
#'
#' ba_alpha_prior <- batting_avg_params$alpha
#' ba_beta_prior <- batting_avg_params$beta
#'
#' ## Using the functions
#'
#' set.seed(559)
#'
#' small_batting <- lahman_batting_2010 %>%
#'  sample_n(size = 10)
#'
#' small_batting %>%
#'    mutate(
#'       posterior_batting_avg = posterior_beta_mu(ba_alpha_prior, ba_beta_prior, n = AB, success = H),
#'       posterior_batting_sd = posterior_beta_sd(ba_alpha_prior, ba_beta_prior, n = AB, success = H)
#'       )
#'
#'
posterior_beta_mu <- function(prior_alpha, prior_beta, n, success){

  posterior_alpha <- prior_alpha + success
  posterior_beta <- prior_beta + (n - success)

  mu <- posterior_alpha / (posterior_alpha + posterior_beta)
  return(mu)
}

#' @rdname posterior_beta
#' @export
posterior_beta_sd <- function(prior_alpha, prior_beta, n, success){

  posterior_alpha <- prior_alpha + success
  posterior_beta <- prior_beta + (n - success)

  s <- sqrt((posterior_alpha * posterior_beta) / ((posterior_alpha + posterior_beta)^2 * (posterior_alpha + posterior_beta + 1)))
  return(s)
}
