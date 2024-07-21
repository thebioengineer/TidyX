## TidyX Episode 185: Independence Days with more {purrr}

library(tidyverse)
library(rvest)
library(janitor)


## read the independence days Wikipedia Page
independence_wiki <- read_html(
  "https://en.wikipedia.org/wiki/List_of_national_independence_days"
  )

## Get the table with purrr::pluck
independence_table_html <- independence_wiki %>% 
  rvest::html_elements("table") %>% 
  pluck(2)

## let rvest make this table for us
independence_table_raw <- independence_table_html %>% 
  html_table() %>% 
  clean_names() %>% 
  mutate(
    across( 
      c("date_of_holiday", "year_of_event"),
      ~gsub("\\[\\d+\\]","",.x)
      )
  )


## Using purrr, lets make date objects

## run function, if it errors return NULL. to get results, get the "results" slot
safe_as_date <- safely(as.Date,otherwise = NULL,quiet = TRUE)
safe_as_date("4 July 2024", format = "%e %B %Y")
safe_as_date("99 July 2024", format = "%e %B %Y")

## other options include:
##   possibly (returns value instead of error), 
##   quietly (capture result and any printed/output values)


independence_table <- independence_table_raw %>% 
  mutate(
    ## _vec allows you to pass more complex object types than the 
    ## normal map_* functions (map_chr, map_dbl...)
    holiday_date =
      map_vec(
      date_of_holiday,
      ~safe_as_date(.x, format = "%e %B")$result
    ),
    event_date = 
      ## pass two vectors to run over each as entries to a function
      map2_vec(
        .x = date_of_holiday,
        .y = year_of_event,
        ~safe_as_date(
          paste(.x, .y), format = "%e %B %Y"
          )$result
      ),
    event_from_description = 
      ## pass n vectors to run over as entries
      pmap(
        list(year_of_event,
             independence_from,
             date_of_holiday,
             name_of_holiday
             ),
        function(a,b,c,d){
          paste0("In ", a, " independence from ", b, " was acheived on ", c, ", and is called ", d)
        }
      )
  )


## for side effects, use "walk"
independence_table$event_from_description %>%
  walk(print)
  

### compose functions together mean(diff(sort(x)))
m_d_s <- compose(mean, diff, sort)

## there is a saying that Every 4 days a country celebrates independence from the UK.
## Is this true?
independence_froms <- independence_table %>% 
  group_split(independence_from,.keep = TRUE) %>% 
  map(\(x){
    tibble(
      independence_from = unique(x$independence_from),
      n_countries = nrow(x),
      start_indeps = min(x$event_date, na.rm = TRUE),
      latest_indeps = max(x$event_date, na.rm = TRUE),
      mean_days_between = m_d_s(c(x$holiday_date[nrow(x)]-365, x$holiday_date)),
      data = list(x)
    )
  }) %>% 
  bind_rows() %>% 
  arrange(mean_days_between)

independence_froms