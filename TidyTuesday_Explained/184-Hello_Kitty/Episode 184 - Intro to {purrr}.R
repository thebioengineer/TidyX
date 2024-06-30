
## Episode 184: Hello Kitty: Intro to {purrr}

## Load packages
library(tidyverse)
library(palmerpenguins)

## get data
data("penguins")
d <- penguins

d %>%
  head()

## Split data into a list
d_list <- d %>%
  split(.$island)

# call first element of list
d_list[1]
d_list$Biscoe

## {purrr} helps us with iteration via the map_* functions
# simply pass a list and then a function

# how many rows in each data frame?

map(.x = d_list, .f =  nrow) ##ambigious

map(.x = d_list, .f =  ~nrow(.x)) ## tilde application with .x

map(.x = d_list, .f =  function(x){nrow(x)}) # anonymous function

map(.x = d_list, .f =  \(x){nrow(x)}) ## R >= 4.1

# What is the average bill length on each island?
map(.x = d_list, .f = ~mean(.x$bill_length_mm, na.rm = TRUE))

# Get the results returned in a vector
map_dbl(.x = d_list, .f = ~mean(.x$bill_length_mm, na.rm = TRUE))

## what if you want it as a character vector?
map_chr(.x = d_list, .f = ~mean(.x$bill_length_mm, na.rm = TRUE))

# How many different species on each island?
map(.x = d_list, .f = ~table(.x$species))

# Summarize each data frame in the list
map(.x = d_list, .f = ~summary(.x))

# building a linear model
d_list %>%
  map(~lm(bill_length_mm ~ flipper_length_mm, data = .x)) %>%
  map(summary)

d_list %>%
  tibble(islands = .) %>% 
  mutate(
    model = map(islands, ~lm(bill_length_mm ~ flipper_length_mm, data = .x)),
    model_summary = map(model, summary)
  ) %>% 
  mutate(
    model_f_stat = map_dbl(model_summary, ~.x$fstatistic[["value"]])
  )
  
# Find rows where the species = Adelie
map(d_list, ~.x[['species']] == 'Adelie')
map(d_list, ~.x$species == 'Adelie')

# get only those rows
d_list %>%
  keep(~any(.x$species == 'Adelie'))  

d_list %>%
  map(~filter(.x, species == 'Adelie'))  


# get the Torgersen island
d_list %>%
  keep(~any(.x$species == 'Adelie')) %>%
  pluck("Torgersen")

d_list %>%
  keep(~any(.x$species == 'Adelie')) %>%
  pluck("Torgersen") %>%
  ggplot(aes(x = flipper_length_mm, y = bill_length_mm)) +
  geom_point(aes(color = as.factor(year)),
             size = 3) +
  geom_smooth(method = "lm",
              color = "black") +
  theme_classic()
