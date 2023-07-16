## TidyX Episode 152: Data Packaging Part 1 - Getting the data

library(tidyverse)
library(rvest)

## F1 Racing Statistics
## https://www.racing-statistics.com/

## The website states this data is from the Ergast API. IF there is interest, we
## can demonstrate how API calls work in R, but for now we will keep this simple
## and pull via rvest

## Lets pick a year
x <- read_html("https://www.racing-statistics.com/en/seasons/1950")

## get the section from the webpage that had the data we want
## there were multiple divs with "blocks" classes. but once we call html_table on
## them, the ones without a table will be NULL

year_tables <- x %>% 
  html_nodes(".blocks") %>% 
  html_table()

Championship_Results <- year_tables[[1]] %>% 
  janitor::clean_names() %>% 
  select(-driver, -driver_3) %>% 
  rename(driver = driver_2)

Event_Results <- year_tables[[2]] %>% 
  janitor::clean_names() %>% 
  select(-winning_driver) %>% 
  rename(driver = winning_driver_2 )


## Lets functionalize this now

f1_season_records <- function(year){
  
  x <- read_html(file.path("https://www.racing-statistics.com/en/seasons",year))
  
  ## get the section from the webpage that had the data we want
  ## there were multiple divs with "block" and "blocks" classes, but only 1 with
  ## "block2". from there we can call html table
  
  year_tables <- x %>% 
    html_nodes(".blocks") %>% 
    html_table()
  
  Championship_Results <- year_tables[[1]] %>% 
    janitor::clean_names() %>% 
    select(-driver, -driver_3) %>% 
    rename(driver = driver_2)
  
  Event_Results <- year_tables[[2]] %>% 
    janitor::clean_names() %>% 
    select(-winning_driver) %>% 
    rename(driver = winning_driver_2 )
  
  list(year = year, Championship_Results = Championship_Results, Event_Results = Event_Results)
}

## this can take a few minute to run
historical_f1_data <- 1950:2020 %>% 
  map(f1_season_records)


## Bonus: data viz!

library(ggplot2)

## winning_constructor over the years 
constructors <- historical_f1_data %>% 
  map(function(year_data){
    year_data[["Championship_Results"]] %>% 
      group_by(constructor) %>% 
      summarize(points = sum(points), .groups = "drop") %>% 
      arrange(desc(points)) %>% 
      slice(1)%>% 
      mutate(year = year_data[["year"]])
  }) %>% 
  bind_rows() %>% 
  arrange(year)

## Make a waffle
theme_set(theme_void())

n_col = 10

constructors %>% 
  arrange(as.numeric(year)) %>% 
  mutate(
    col_idx = (row_number()-1) %% (n_col),
    row_idx = -floor((row_number()-1)/(n_col))
  ) %>% 
  ggplot(
    aes(
      x = col_idx,
      y = row_idx
      )   
  ) + 
  geom_tile(
    aes(
      fill = constructor
      )
  ) + 
  geom_text(
    aes(
      label = year
    )
  ) + 
  scale_fill_manual(
    name = NULL,
    labels = c("Alfa Romeo", "Benetton", "Brabham-Repco","Brawn", "BRM",
               "Cooper-Climax", "Ferrari", "Lotus-Climax", "Lotus-Ford", "Maserati",
               "Matra-Ford", "McLaren", "Mercedes", "Red Bull", "Renault", "Team Lotus",
               "Tyrrell", "Williams"),
    ## Hex Codes based on Asking ChatGPT for hex codes best representative of
    ## the livery of the team
    values = c("#9B0000", "#0066CC", "#006600", "#C30000", "#FF0000",
               "#003399", "#DC0000", "#006400", "#0000CD", "#1F4096",
               "#FFD700", "#FF8700", "#00D2BE", "#0800FF", "#F9FF00",
               "#006400", "#002055", "#FFFFFF")
  ) +
  labs(
    title = "Eras of Dominance",
    subtitle = "F1 Constructors Champions from 1950 to 2020"
  )

