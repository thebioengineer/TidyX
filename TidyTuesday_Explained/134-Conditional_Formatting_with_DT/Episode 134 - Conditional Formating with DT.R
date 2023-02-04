
### Episode 134: Conditional Formatting with {DT}

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
  ) %>% 
  ungroup()


### Basic DT table ---------------------------------------------------------------------------------------

basic_tbl <- dat %>%
  datatable(
    caption = "Weekly Training Load",
    rownames = FALSE,
    class = 'cell-border stripe',
    options = list(
      columnDefs = list(
        list(className = "dt-center", targets = 0:7)
        )
    ),
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

basic_tbl

### Percentage formatting DT table ---------------------------------------------------------------------------------------

basic_tbl %>% 
  formatPercentage(
    columns = "CV_Pct",
    digits = 1
  )

?DT::formatPercentage

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
      ),
  )

# Color the position groups background differently 
basic_tbl %>%
  formatStyle(
    columns = "pos",
    backgroundColor = styleEqual(
      levels = unique(dat$pos),
      values = c("lightgreen", "yellow", "lightblue")
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
      cuts = 0.4, 
      values = c("white", "#FF9999")
    )
  )

# Add a color bar instead of shading the background
basic_tbl %>%
  formatStyle(
    columns = "Week_Avg",
    background = styleColorBar(
      data = dat$Week_Avg,
      color = "#99CCFF"
    )
  )


### Advanced Conditional Formatting ----------------------------------------------------------
## Create a color bar based on a hidden column
# Assume the normal weekly training load is 70 with a standard deviation of 10
# highlight rows in green if they are above 0.4 SD and red if they are below -0.4

dat2 <- dat %>% 
  mutate(
    weekly_z = (Week_Avg - 70) /  10
  )


basic_tbl2 <- dat2 %>%
  datatable(
    caption = "Weekly Training Load",
    rownames = FALSE,
    class = 'cell-border stripe',
    options = list(columnDefs = list(
      list(className = "dt-center", targets = 0:7),
      list(visible = FALSE, targets = 8)
    )),
    colnames = c("Athlete",
                 "Pos",
                 "Day 1",
                 "Day 2",
                 "Day 3",
                 "Weekly Avg",
                 "Weekly SD",
                 "CV%",
                 "z"))

## Styling on multiple intervals

basic_tbl2 %>%
  formatStyle(
    columns = "Week_Avg",
    valueColumns = "weekly_z",
    background = styleInterval(
      cuts = c(-0.4, 0.4), 
      values = c("#FF9999", "white", "#CCFF99")
    )
  )

# Alternative approach

dat2 %>%
  mutate(
    flag = case_when(
      weekly_z < -0.4 ~ "low",
      weekly_z > 0.4 ~ "high",
      TRUE ~ "normal")
  ) %>%
  datatable(
    caption = "Weekly Training Load - Manual Coloring",
    rownames = FALSE,
    class = 'cell-border stripe',
    options = list(columnDefs = list(
      list(className = "dt-center", targets = 0:7),
      list(visible = FALSE, targets = 8:9)
    )),
    colnames = c(
      "Athlete",
      "Pos",
      "Day 1",
      "Day 2",
      "Day 3",
      "Weekly Avg",
      "Weekly SD",
      "CV%",
      "z"
    )
  ) %>%
  formatStyle(
    columns = "Week_Avg", 
    valueColumns = "flag",
    background = styleEqual(
      levels = c("low", "normal","high"), 
      values = c("#FF9999", "white", "#CCFF99")
    )
  )


## Gradient Colors Manually

col_ramp <- colorRampPalette(c("#FF9999", "white", "#CCFF99"))
col_brk <- sort(dat2$weekly_z)
clr <- col_ramp(length(col_brk)+1)

basic_tbl2 %>%
  formatStyle(
    columns = "Week_Avg", 
    valueColumns = 'weekly_z',
    background = styleInterval(
      cuts = col_brk,
      values = clr
    )
  )


## Row wise styling --------------------------------------------------

basic_tbl2 %>% 
  formatStyle(
    columns = "pos",
    target = 'row',
    backgroundColor = styleEqual(
        levels = c("Center", "Fwd", "G"),
        values = c("white", "palegreen", "pink")
        )
    )
