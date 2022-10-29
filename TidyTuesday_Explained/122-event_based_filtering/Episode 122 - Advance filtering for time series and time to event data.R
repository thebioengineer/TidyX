
## Episode 122: Advance filtering for time series and time to event data

library(tidyverse)

## simulate data with observations and some event of interest
set.seed(35)

## participant = unique identifier for individual
## obs_number = observation number
## event = binary indicating if the event occurred or not during observation

df <- tibble(
  participant = c(rep(LETTERS[1:2], each = 20), rep(LETTERS[3], times = 8)),
  obs_number = c(rep(1:20, times = 2), rep(1:8, times = 1)),
  event = sample(0:1, size = 48, replace = T, prob = c(0.75, 0.25))
  )

df

## how many events per participant
df %>%
  filter(event == 1) %>%
  count(participant, event, sort = TRUE)

df %>% 
  group_by(participant) %>% 
  summarize(sum(event))

## Create a running count of each time an event occurs
df %>%
  group_by(participant) %>%
  mutate(event_group = cumsum(event == 1))

## find the observation two after the event of interest

## get last time before 

df %>%
  group_by(participant) %>%
  mutate(event_group = cumsum(event == 1)) %>%
  group_by(event_group, .add = TRUE) %>%          # use .add to keep the previous grouping as well as the new
  filter(row_number() <= 2) %>%
  print(n = nrow(.))

## Filter every subjects time from start to their first observation
# Use the same concept of cumsum() to filter when the event_group = 1 and the event = 1
df %>%
  group_by(participant) %>% 
  filter(cumsum(event) == 1 & event == 1)

## duration between events
df %>%
  group_by(participant) %>% 
  mutate(event_number = cumsum(event)) %>% 
  group_by(event_number, .add = TRUE) %>% 
  summarize(duration = n()) %>% 
  filter(event_number != 0 )



## Grab observations between events
df %>%
  group_by(participant) %>% 
  mutate(
    event_number = cumsum(event),
    obs_type =
      case_when(
        event_number == 1 & event == 1 ~ "stand",
        event_number == 2 & event == 1 ~ "start activity",
        event_number == 3 & event == 1 ~ "end activity",
        event_number == 4 & event == 1 ~ "lie down"
      )
  ) %>% 
  filter(
    between(
      row_number(), 
      left = which(obs_type == "start activity"), 
      right = min(which(obs_type == "lie down"), n())
    )
  )
  