test_that("get_beta_paramters works", {

  pop_avg <- lahman_batting_2010 %>%
    filter(AB >= quantile(AB, probs = 0.75)) %>%
    pull(batting_avg) %>%
    mean()
  pop_var <- lahman_batting_2010 %>%
    filter(AB >= quantile(AB, probs = 0.75)) %>%
    pull(batting_avg) %>%
    var()

  ## Using the function
  batting_avg_params <- get_beta_parameters(
    population_avg = pop_avg,
    population_var = pop_var
  )


  pop_avg
  pop_var
  sqrt(pop_var)

})
