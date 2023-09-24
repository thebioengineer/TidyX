
test_that("get_beta_paramters works", {

  pop_avg <- lahman_batting_2010 %>%
    dplyr::filter(AB >= quantile(AB, probs = 0.75)) %>%
    dplyr::pull(batting_avg) %>%
    mean()
  pop_var <- lahman_batting_2010 %>%
    dplyr::filter(AB >= quantile(AB, probs = 0.75)) %>%
    dplyr::pull(batting_avg) %>%
    var()

  ## Using the function
  batting_avg_params <- get_beta_parameters(
    population_avg = pop_avg,
    population_var = pop_var
  )


  expect_equal(batting_avg_params$alpha, 43.48505)
  expect_equal(batting_avg_params$beta, 126.444348)

})

test_that("re-deriving beta mu and sd works", {

  pop_beta_params <- list(alpha = 43.48505, beta = 126.4443)

  expect_equal(
    beta_mu(pop_beta_params$alpha,pop_beta_params$beta),
    0.2559007,
    tolerance = .001
    )

  expect_equal(
    beta_sd(pop_beta_params$alpha,pop_beta_params$beta),
    sqrt(0.001114001),
    tolerance = .000001
  )

})

test_that("deriving posterior beta mu and sd works",{

  pop_beta_params <- list(alpha = 43.48505, beta = 126.4443)
  n_success_lt_pop_avg <- 10
  n_success_gt_pop_avg <- 80
  n_attempts <- 100

  ## general expectation testing as opposed to known quantities
  expect_true(
    posterior_beta_mu(
      pop_beta_params$alpha,
      pop_beta_params$beta,
      n = n_attempts,
      success = n_success_lt_pop_avg
    ) < 0.2559007
  )

  expect_true(
    posterior_beta_mu(
      pop_beta_params$alpha,
      pop_beta_params$beta,
      n = n_attempts,
      success = n_success_lt_pop_avg
    ) > n_success_lt_pop_avg/n_attempts
  )

  expect_true(
    posterior_beta_mu(
      pop_beta_params$alpha,
      pop_beta_params$beta,
      n = n_attempts,
      success = n_success_gt_pop_avg
    ) > 0.2559007
  )

})

test_that("beta dist processing - regression test",{

   pop_avg <- lahman_batting_2010 %>%
     dplyr::filter(AB >= quantile(AB, probs = 0.75)) %>%
     dplyr::pull(batting_avg) %>%
     mean()
   pop_var <- lahman_batting_2010 %>%
     dplyr::filter(AB >= quantile(AB, probs = 0.75)) %>%
     dplyr::pull(batting_avg) %>%
     var()

   batting_avg_params <- get_beta_parameters(
     population_avg = pop_avg,
     population_var = pop_var
   )

   ba_alpha_prior <- batting_avg_params$alpha
   ba_beta_prior <- batting_avg_params$beta

   ## Using the functions

   set.seed(559)

   small_batting <- lahman_batting_2010 %>%
    dplyr::slice(1,2,4,6,7,8,9)

   small_batting %>%
     dplyr::mutate(
         posterior_batting_avg = posterior_beta_mu(ba_alpha_prior, ba_beta_prior, n = AB, success = H),
         posterior_batting_sd = posterior_beta_sd(ba_alpha_prior, ba_beta_prior, n = AB, success = H)
         )

   expect_snapshot(
     small_batting
   )

})

test_that("get_beta_paramters throws errors with non numeric values", {


  ## test generically for an error
  expect_error(
    get_beta_parameters(
      population_avg = "test",
      population_var = "value"
      )
  )

  ## tests for specific error pattern
  expect_error(
    get_beta_parameters(
      population_avg = "test",
      population_var = "value"
    ),
    "is.numeric\\(population_avg\\) is not TRUE"
  )

  ## Use Regex
  expect_error(
    get_beta_parameters(
      population_avg = "test",
      population_var = 42
    ),
    "is.numeric\\(.*?\\) is not TRUE"
  )

  expect_error(
    get_beta_parameters(
      population_avg = 42,
      population_var = "test"
    ),
    "is.numeric\\(.*?\\) is not TRUE"
  )

})

