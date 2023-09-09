#' @title Gamma Distribution
#'
#' @description Functions for the gamma distribution to calculate priors and
#'   posteriors.
#'
#'   The gamma distribution should be used as a rate per event, the distribution
#'   being predicting is a value between 0 and inf. Think of this as runs/game
#'   like in baseball.
#'
#' @param population_avg the population average, a length 1 numeric value
#'   describing the average (mean) value of the population
#' @param population_var the population variance, a length 1 numeric value
#'   describing the variance of the population
#' @param alpha alpha parameter of a gamma distribution
#' @param beta beta parameter of a gamma distribution
#' @param n numeric value representing number of observations
#' @param successes numeric value representing the total number of successes across all the n observations.
#'
#' @rdname gamma
#'
#' @export
#'
#' @examples
#'
#' rpg_mean <- mean(lahman_team_runs_2010$runs_per_game)
#' rpg_var <- var(lahman_team_runs_2010$runs_per_game)
#'
#' rpg_params <- get_gamma_parameters(population_avg = rpg_mean,
#'                                    population_var = rpg_var)
#'
#' rpg_alpha_prior <- rpg_params$alpha
#' rpg_beta_prior <- rpg_params$beta
#'
#' lahman_team_runs_2010 %>%
#'    mutate(
#'      posterior_rpg_avg = posterior_gamma_mu(rpg_alpha_prior, rpg_beta_prior, G, R),
#'      posterior_rpg_sd = posterior_gamma_sd(rpg_alpha_prior, rpg_beta_prior, G, R)
#'    )
#'
#' @seealso [posterior_beta_mu()], [posterior_beta_sd()] which is the beta distribution posterior calculations
#'
get_gamma_parameters <- function(population_avg, population_var){

  alpha <- population_avg^2 / population_var
  beta <- population_avg / population_var

  list(alpha = alpha, beta = beta)

}

#' @rdname gamma
#' @export
gamma_mu <- function(alpha, beta){
  mu <- alpha / beta
  return(mu)
}

#' @rdname gamma
#' @export
gamma_sd <- function(alpha, beta){
  s <- alpha / beta^2
  return(s)
}

#' @rdname gamma
#' @export
posterior_gamma_mu <- function(alpha, beta, n, successes){

  alpha_posterior <- alpha + successes
  beta_posterior <- beta + n

  gamma_mu(alpha_posterior, beta_posterior)

}

#' @rdname gamma
#' @export
posterior_gamma_sd <- function(alpha, beta, n, successes){

  alpha_posterior <- alpha + successes
  beta_posterior <- beta + n

  gamma_sd(alpha_posterior, beta_posterior)

}
