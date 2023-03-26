

library(tidyverse)

## Fake Data
dat <- tibble(
  year = c(2023, 2020, 2020, 2022, 2023, 2021, 2019, 2020, 2023),
  value = c(15, 33, 25, 12, 18, 13, 10, 21, 28)
)


## We want to standardize our data prior to analysis
# We want to normalize the values for this years data relative to the mean and standard
# deviation of previous years data.

# Create a function that calculates a z-score for this years data with the previous years mean and SD

###### THE WRONG WAY TO DO THIS ######
bad_z_score <- function(x){
  
  a <- dat %>%
    filter(year < 2023) %>%
    summarize(a = mean(x, na.rm = TRUE)) %>%
    pull(a)
  
  s <- dat %>%
    filter(year < 2023) %>%
    summarize(s = sd(x, na.rm = TRUE)) %>%
    pull(s)

  z <- (x - a) / s
  return(z)

}


## Use the function
dat %>%
  mutate(
    scaled = bad_z_score(value),
    scaled_check = (value - mean(value[year < 2023])) /  sd(value[year < 2023])
    )

## That didn't work?
# create a column with the mean and SD we want to use and then calculate from there
dat %>%
  mutate(a = mean(value[year < 2023]),
         s = sd(value[year < 2023]),
         scaled3 = (value - a) / s)

## That looks correct. It seems like the function is taking the mean and SD including 2023?
dat %>%
  mutate(scaled_func = bad_z_score(value),
         scaled_val = (value - mean(value)) / sd(value))

##### Turn the average and SD in to a vector and work from there

z_score <- function(x){
  
  a <- mean(x[dat$year < 2023], na.rm = TRUE)
  s <- sd(x[dat$year < 2023], na.rm = TRUE)
  
  z <- (x - a) / s
  return(z)
}


dat %>%
  mutate(
    avg = z_score(value),
    scaled_check = (value - mean(value[year < 2023])) /  sd(value[year < 2023])
    )


# Worked!

## What if we had several columns of data?
## Fake Data
set.seed(4)
dat <- tibble(
  
  year = c(2023, 2020, 2020, 2022, 2023, 2021, 2019, 2020, 2023),
  value1 = runif(n = 9, min = 10, 30),
  value2 = runif(n = 9, min = 1, 15),
  value3 = runif(n = 9, min = 50, 100)
  
)

dat %>%
  mutate(
    across(.cols = value1:value3, ~z_score(.x)),
    value1_check = (value1 - mean(value1[year < 2023])) /  sd(value1[year < 2023]),
    value2_check = (value2 - mean(value2[year < 2023])) /  sd(value2[year < 2023]),
    value3_check = (value3 - mean(value3[year < 2023])) /  sd(value3[year < 2023])
    )

## Predefine the Z score function with generator function to 

##### Turn the average and SD in to a vector and work from there
gen_z_score_func <- function(dat, var, rows){
  
  a <- mean(dat[[var]][rows], na.rm = TRUE)
  s <- sd(dat[[var]][rows], na.rm = TRUE)
  
  function(x){
    z <- (x - a) / s
    return(z)
  }
  
}

z_score_value1 <- gen_z_score_func(dat, "value1", dat$year < 2023)
z_score_value2 <- gen_z_score_func(dat, "value2", dat$year < 2023)
z_score_value3 <- gen_z_score_func(dat, "value3", dat$year < 2023)

dat %>%
  mutate(
    value1_z_score = z_score_value1(value1),
    value2_z_score = z_score_value2(value2),
    value3_z_score = z_score_value3(value3),
    value1_check = (value1 - mean(value1[year < 2023])) /  sd(value1[year < 2023]),
    value2_check = (value2 - mean(value2[year < 2023])) /  sd(value2[year < 2023]),
    value3_check = (value3 - mean(value3[year < 2023])) /  sd(value3[year < 2023])
  )
