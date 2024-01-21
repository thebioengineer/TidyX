# TidyX Episode 170 - Tidy Expressions and For loops

library(tidyverse)

set.seed(6923)

fake_dat <- data.frame(
  athlete = c("Tom", "Tom", "Kevin", "Kevin", "Hank", "Hank", "Bob", "Andy", "Sam", "Sam"),
  season = c(2021, 2022, 2021, 2022, 2021, 2022, 2021, 2022, 2021, 2022),
  stat1 = rnorm(n = 10, mean  = 20, sd = 5),
  stat1_n = round(runif(n = 10, min = 20, max = 100)),
  stat2 = rnorm(n = 10, mean  = 200, sd = 150),
  stat2_n = round(runif(n = 10, min = 20, max = 100)),
  stat3 = rnorm(n = 10, mean  = 100, sd = 25),
  stat3_n = round(runif(n = 10, min = 20, max = 100)),
  stat4 = rnorm(n = 10, mean  = 50, sd = 15),
  stat4_n = round(runif(n = 10, min = 20, max = 100)),
  stat5 = rnorm(n = 10, mean  = 120, sd = 35),
  stat5_n = round(runif(n = 10, min = 20, max = 100))
)

fake_dat


## We want a weighted score for each stat
# let's try stat 1
fake_dat %>%
  group_by(athlete) %>%
  summarize(
    total_obs = sum(stat1_n),
    wgt_stat = weighted.mean(stat1, stat1_n),
    .groups = "drop"
    )


## Let's write a function so that we don't need do this for every column 
## (we might have hundreds of columns!)
##
## we use {{}} to allow us to pass the values of variable and N just
## as we would if we were to write the expression as normal (see above)
##
get_wgt_score <- function(dat, variable, N){
  
  dat %>%
    group_by(athlete) %>%
    summarize(
      total_obs = sum({{N}}),
      wgt_stat = weighted.mean({{variable}}, {{N}}),
      .groups = "drop"
      )
}

get_wgt_score(fake_dat, stat1, stat1_n)

get_wgt_score(fake_dat, stat4, stat4_n)


get_wgt_score(fake_dat, "stat4", "stat4_n")

## Can we do a for loop to get each score?

stats_of_concern <- colnames(fake_dat) %>% 
  str_subset(".+\\d$")
  
## this doesnt work though
## get_wgt_score(fake_dat, "stat4", "stat4_n")

## create empty list to store results
output <- list()

## run loop
for(i in seq_along(stats_of_concern)){
  
  stat <- stats_of_concern[i]
  
  ## convert into "name" objects here
  ## essesntially think of this as R code values not executed yet
  var_expr <- str2lang(stat)
  sample_size_expr <- str2lang(paste0(stat,"_n"))
  
  ## do.call allows us to pass these values
  ## as expressions to evaluate, which means when it actually 
  ## runs it sees this as get_wgt_score(dat = fake_dat, variable = VALUE, N = VALUE2)
  
  output[[stat]] <- do.call(
    get_wgt_score,
    list(
      dat = fake_dat, 
      variable = var_expr,
      N = sample_size_expr
    )
  ) %>% 
    mutate(
      name = stat
    )
  
}

bind_rows(output) %>% 
  relocate(name, .after = athlete) %>% 
  arrange(athlete, name)



### tidyverse solution
fake_dat %>%
  select(athlete, season, !ends_with("_n")) %>%
  pivot_longer(cols = stat1:stat5) %>%
  left_join(
    fake_dat %>%
      select(athlete, season, ends_with("_n")) %>%
      pivot_longer(cols = stat1_n:stat5_n) %>%
      rename(sample_size = value) %>%
      mutate(name = gsub("_n", "", name))
  ) %>%
  group_by(athlete, name) %>%
  summarize(total_obs = sum(sample_size),
            wgt_stat = weighted.mean(value, sample_size),
            .groups = "drop")



