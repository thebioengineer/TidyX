
### Episode 134 - shorts: Conditional Formatting with {DT}

#### Shorts - Categorical Formatting


### Load packages and simulate data -------------------------------------------
library(tidyverse)
library(DT)

set.seed(109)

dat <- tibble(
  athlete = LETTERS[1:15],
  pos = rep(c("Center", "Fwd", "G"), each = 5),
  day1 = round(rnorm(n = length(athlete), mean = 60, sd = 10), 0),
  day2 = round(rnorm(n = length(athlete), mean = 100, sd = 13), 0),
  day3 = round(rnorm(n = length(athlete), mean = 45, sd = 5), 0),
) %>% 
  rowwise() %>% 
  mutate(
    Week_Avg = round(mean(c_across(day1:day3)),0),
    Week_SD = round(sd(c_across(day1:day3)),0),
    CV_Pct = round(Week_SD / Week_Avg, 3)
  )


### Basic DT table ---------------------------------------------------------------------------------------

basic_tbl <- dat %>%
  datatable(
    caption = "Weekly Training Load",
    rownames = FALSE,
    class = 'cell-border stripe',
    options = list(columnDefs = list(
      list(className = "dt-center", targets = 0:7)
    )),
    colnames = c(
      "Athlete",
      "Pos",
      "Day 1",
      "Day 2",
      "Day 3",
      "Weekly Avg",
      "Weekly SD",
      "CV%"
    )
  ) 


### Conditional Formatting based on categorical condition -----------------------------------------------
## Color two different athletes background and text
basic_tbl %>%
  formatStyle(
    columns = "athlete",
    color = styleEqual(
      levels = c("B", "F"),
      values = c("red", "red")
    ),
    backgroundColor = styleEqual(
      levels = c("B", "F"), 
      values= c("lightgrey", "lightgrey")
    )
  )
