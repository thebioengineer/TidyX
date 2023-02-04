
### Episode 134: Conditional Formatting with {DT}

#### Shorts - Rowwise Colors

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

## Row wise styling --------------------------------------------------

basic_tbl %>%
  formatStyle(
    columns = "pos",
    target = 'row',
    backgroundColor = styleEqual(
      levels = c("Center", "Fwd", "G"), 
      values = c("palegreen","orange", "pink")
    )
  )
