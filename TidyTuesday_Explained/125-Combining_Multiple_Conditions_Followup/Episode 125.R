# TidyX Episode 125 - Combining Multiple Conditions Follow-up

library(tidyverse)


## Suggestion raised on our github by @datadavidz

lst1 <- list(
  winter = c("apple", "orange", "banana"), 
  summer = c("pear", "kiwi", "grape", "apple"),
  spring = c("cherry", "apple", "grape", "banana")
  )

lst1 %>%
  enframe(name = "season", value = "fruit") %>% ## converts list into a data.frame
  unnest(fruit) %>%
  group_by(fruit) %>%
  summarize(season = toString(season))


## Another Question was around rowwise and un-doing it

row_mtcars <- mtcars %>% 
  rowwise()

## Note a rowwise_df is implicitly grouped by row but is _not_ a grouped_df
class(row_mtcars)

### To un-rowise, call ungroup, or apply your own group_by

row_mtcars %>% 
  ungroup() %>%  
  class()

row_mtcars %>% 
  group_by(cyl) %>% 
  class()


## Complicated multiple conditions problem submitted by 
## Jeff Rothschild asking how to use a join (alluded to in Ep 124)

## Problem:

### I have RPE data using the Borg 6-20 scale, that I’d like to transform 
### to a different scale.

### Here is a table that converts from the original numbers on the 6-20 
### scale to the other 1-10 scale.

rpe_key <- tibble(
  borg = c(6, 7,   8, 9,  10, 11,  12, 13, 14,  15,  16,  17,  18,  19, 20), 
  CR10 = c(0, 1, 1.5, 2, 2.5,  3, 3.5,  4,  5, 5.5, 6.5, 7.5, 8.5, 9.5, 10)
)

### Then I have some sample data.

subject_data <- tibble(
  subject = c(201, 202, 203, 204, 205, 206, 207, 209),
  rpe = c(9, 8, 10, 12, 7, 13, 9, 7)
  )

### I’d like a third column of my subject_data tbl to be the 
### transformed RPE number (so 2, 1.5, 2.5, etc)

subject_data %>% 
  left_join(
    rpe_key, 
    by = c("rpe" = "borg")
  ) %>% 
  rename(
    `Transformed RPE` = CR10
  )

