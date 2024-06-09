## TidyX Episode 182: Nested for loops for simulation: Parallelizing

### Updating TidyX Episode 144: Nested for Loops for Simulation

library(tidyverse)
library(snowfall)
library(tictoc)

### we will be simulating data that looks like this
dat <- tibble(
  athlete = LETTERS[1:10],
  var1 = rnorm(n = 10, mean = 20, sd = 3),
  var2 = rnorm(n = 10, mean = 15, sd = 4),
  var3 = rnorm(n = 10, mean = 40, sd = 10)
)

dat

### Create some models that we want to run on our data
all_models <- c(
  mod_1 = "var1 ~ var2",
  mod_2 = "var1 ~ var3",
  mod_3 = "var3 ~ var2"
)

## An example of calling each model on our data one by one
lm(all_models[1], data = dat)
lm(all_models[2], data = dat)
lm(all_models[3], data = dat)

## We want to simulate new data N number of times and run models over it in a for() loop
n_sims <- 1000

## start timer
tic()

### Create a list for simulated data
sim_datasets <- list()

### Create a list for storing model outputs
sim_fitted_values <- list()

### Nested for() loop -  Episode 144
for(i in 1:n_sims) {
  
  ## Create simulation dataset. This can be as complicated as you need for your
  ## situation
  dat_sim <- tibble(
    athlete = LETTERS[1:10],
    var1 = rnorm(n = 10, mean = 20, sd = 3),
    var2 = rnorm(n = 10, mean = 15, sd = 4),
    var3 = rnorm(n = 10, mean = 40, sd = 10)
  )
  
  ## Create new output_df_list for every simulation to
  ## hold results of each model
  output_list <- vector("list", length = length(all_models))
  
  for (j in seq_along(all_models)) {
    
    ## fit linear model
    fit <- lm(all_models[j], data = dat_sim)
    
    ## this is for pulling out content from an lm. 
    ## may need to change for the model used
    output_list[[j]] <- data.frame(
      model = all_models[j],
      sim_intercept = fit$coef[1],
      sim_slope = fit$coef[2],
      fitted_vals = fit$fit,
      simulation = i, # we can reference for loop indexes above them
      row.names = paste0("sim_",i,"_model_",j,"_",seq_along(fit$fit))
    )
  }
  
  ## combine the output from every model into a single df and store in
  ## model_fitted_values for every simulation
  sim_fitted_values[[i]] <- output_list
  
  ## store simulated data
  sim_datasets[[i]] <- dat_sim
  
}

## End timer
toc()

sim_fitted_values

sim_datasets

## Updating to parallelize across n simulations!

## create parallel processes
sfInit(parallel = TRUE, cpus=6)

## call tidyverse on all parallel processes
sfLibrary(tidyverse)

## start timer
tic()

## run simulations
parallel_simulated_outputs <- seq_len(n_sims) %>% 
  snowfall::sfLapply(function(iteration, models){
    
    set.seed(iteration)
    
    ## Create simulation dataset. This can be as complicated as you need for your
    ## situation
    dat_sim <- tibble(
      athlete = LETTERS[1:10],
      var1 = rnorm(n = 10, mean = 20, sd = 3),
      var2 = rnorm(n = 10, mean = 15, sd = 4),
      var3 = rnorm(n = 10, mean = 40, sd = 10)
    )
    
    ## Create new output_df_list for every simulation to
    ## hold results of each model
    output_list <- vector("list", length = length(models))
    
    for (j in seq_along(models)) {
      
      ## fit linear model
      fit <- lm(models[j], data = dat_sim)
      
      ## this is for pulling out content from an lm. 
      ## may need to change for the model used
      output_list[[j]] <- data.frame(
        model = models[j],
        sim_intercept = fit$coef[1],
        sim_slope = fit$coef[2],
        fitted_vals = fit$fit,
        simulation = iteration, # we can reference for loop indexes above them
        row.names = paste0("sim_",iteration,"_model_",j,"_",seq_along(fit$fit))
      )
    }
    
    
    list(
      # Store the output from every model into a single df and store in
      ## model_fitted_values for every simulation
      sim_fitted_values = output_list,
      ## store simulated data
      sim_dataset = dat_sim
    )
  }, models = all_models)

## End timer
toc()

### issues: 

## How can we use these values now?

## combine all fitted values together

parallel_simulated_outputs %>% 
  map(~.x[["sim_fitted_values"]]) %>% 
  bind_rows()

## get full simulated datasets with sim index

seq_along(parallel_simulated_outputs) %>% 
  map(function(x) {
    parallel_simulated_outputs[[x]] %>% 
      pluck("sim_dataset") %>% 
      mutate(sim_number = x)
  }) %>% 
  bind_rows()
