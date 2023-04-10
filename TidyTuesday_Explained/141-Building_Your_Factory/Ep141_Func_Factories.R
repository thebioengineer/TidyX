# Function Factories

library(tidyverse)
library(scales)

## A function that returns another function

## For more details see Chapter 10 Function Factories - Advanced R - by Hadley Wickham
## https://adv-r.hadley.nz/function-factories.html


## Base R - approxfun, exdf, and creating gradients!
royg_palette <- colorRampPalette(c("red","orange","yellow","green"))

show_col(royg_palette(10))
show_col(royg_palette(20))
show_col(royg_palette(100))

## ggplot2 - your formatting functions!
## Copied/Edited from Advanced R - https://adv-r.hadley.nz/function-factories.html#labelling

df <- data.frame(x = 1, y = c(12345, 123456, 1234567))

core <- ggplot(df, aes(x, y)) + 
  geom_point() + 
  scale_x_continuous(breaks = 1, labels = NULL) +
  labs(x = NULL, y = NULL) + 
  scale_y_continuous(
    labels = comma_format() # this is a function factory!
   )

## Reduce values by 1000, but suffix to mark as thousands
core + scale_y_continuous(
  labels = comma_format(scale = .001, suffix = "K", prefix =  "$") 
  )

comma_format()
comma_format()(df$y)
comma_format(scale = .001, suffix = "K")(df$y)

## Back to the use-case we showed in Episode 139 (bit.ly/TidyX_Ep139)

## Fake Data
dat <- tibble(
  year = c(2023, 2020, 2020, 2022, 2023, 2021, 2019, 2020, 2023),
  value = c(15, 33, 25, 12, 18, 13, 10, 21, 28)
)

gen_z_score_func <- function(dat, var, rows){
  
  a <- mean(dat[[var]][rows], na.rm = TRUE)
  s <- sd(dat[[var]][rows], na.rm = TRUE)
  
  function(x){
    z <- (x - a) / s
    return(z)
  }
  
}

z_score_value <- gen_z_score_func(dat, "value", dat$year < 2023)

z_score_value
function(x){
  z <- (x - a) / s
  return(z)
}


z_score_value_env <- environment(z_score_value)

ls(z_score_value_env)

z_score_value_env$a
z_score_value_env$s

mean(dat[dat$year < 2023,][["value"]])
sd(dat[dat$year < 2023,][["value"]])


## what else do you want to know about function factories?
