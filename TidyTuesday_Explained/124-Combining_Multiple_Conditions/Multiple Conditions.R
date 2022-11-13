### TidyX Episode 124 Combining Multiple Conditions

library(tidyverse)


d <- tibble(
  fruit = c("apple", "pear", "kiwi", "banana", "grape", "cherry", "orange")
)

## Desired output -- a string column returned, meeting several conditions -----------------
# Desired output

tribble(
  ~fruit, ~fruit_season,
  "apple", "winter, summer, spring",
  "pear",  "summer",
  "kiwi", "summer",
  "banana", "winter, summer",
  "grape", "summer, spring",
  "cherry", "spring",
  "orange", "winter")

## case_when() --------------------------------------------------------------------------------

d %>%
  mutate(
    fruit_season = case_when(
      fruit %in% c("apple", "orange", "banana") ~ "winter",
      fruit %in% c("pear", "kiwi", "grape", "apple") ~ "summer",
      fruit %in% c("cherry", "apple", "grape", "banana") ~ "spring"
      )
    )

# Apple should have all seasons!
# Other fruits have two seasons
# Things are being over written by their first classification. case_when() retains the first

## what about ifelse() -------------------------------------------------------------------------

d %>%
  mutate(fruit_season = 
           ifelse(fruit %in% c("apple", "orange", "banana"), "winter",
           ifelse(fruit %in% c("pear", "kiwi", "grape", "apple"), "summer",
           ifelse(fruit %in% c("cherry", "apple", "grape", "banana"), "spring", NA))))


# same issue!


### Create separate mutates for your conditions -----------------------------------------
# paste them altogether and then remove the NA's to get the final product
d %>%
  mutate(
    fruit_season1 = 
      case_when(fruit %in% c("apple", "orange", "banana") ~ "winter")
    ) %>%
  mutate(
    fruit_season2 = 
      case_when(fruit %in% c("pear", "kiwi", "grape", "apple") ~ "summer")) %>%
  mutate(
    fruit_season3 = 
      case_when(fruit %in% c("cherry", "apple", "grape", "banana") ~ "spring")
  ) %>%
  mutate(
    fruit_season4 = 
      case_when(fruit %in% c("apple", "kiwi",) ~ "fall")
  ) 
  mutate(
    fruit_season = paste(fruit_season1, fruit_season2, fruit_season3,fruit_season4, sep = ", ")
  ) %>%
  mutate(fruit_season = str_replace_all(fruit_season, "NA, ", replacement = "")) %>%
  mutate(fruit_season = str_replace_all(fruit_season, ", NA", replacement = "")) %>%
  select(-fruit_season1, -fruit_season2, -fruit_season3, -fruit_season4)


# paste them altogether and then remove the NA's to get the final product
d %>%
  mutate(fruit_season1 = case_when(fruit %in% c("apple", "orange", "banana") ~ "winter")) %>%
  mutate(fruit_season2 = case_when(fruit %in% c("pear", "kiwi", "grape", "apple") ~ "summer")) %>%
  mutate(fruit_season3 = case_when(fruit %in% c("cherry", "apple", "grape", "banana") ~ "spring")) %>% 
  rowwise() %>% 
  summarize(
    fruit = fruit,
    fruit_season = paste(setdiff(c(fruit_season1, fruit_season2, fruit_season3),NA), collapse = ", ")
  ) 

d %>%
  rowwise() %>% 
  mutate(
    fruit_season = paste(c(
      if(fruit %in% c("apple", "orange", "banana")){"winter"},
      if(fruit %in% c("pear", "kiwi", "grape", "apple")){"summer"},
      if(fruit %in% c("cherry", "apple", "grape", "banana")){"spring"}
      ),
      collapse= ", ")
  )


         


