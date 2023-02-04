
### Episode 134 - shorts: Conditional Formatting with {DT}

#### Shorts - Conditional Formatting


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


### Conditional Formatting based on a continuous variable -----------------------------------------------
# Color the text of all athletes with a CV% greater than 40%

basic_tbl %>%
  formatStyle(
    columns = "CV_Pct",
    color = styleInterval(
      cuts = 0.4, 
      values = c("black", "red")
    )
  )

# Color the background of all athletes with a CV% greater than 40%
basic_tbl %>%
  formatStyle(
    columns = "CV_Pct",
    backgroundColor = styleInterval(
      cuts =  0.4, 
      values = c("white", "red")
    )
  )
