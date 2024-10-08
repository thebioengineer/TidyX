## code to prepare `f1_championship_data` dataset goes here
library(tidyverse)
library(rvest)

## F1 Racing Statistics
## https://www.racing-statistics.com/


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
historical_f1_data <- 2000:2020 %>%
  map(f1_season_records)

usethis::use_data(historical_f1_data, overwrite = TRUE)
