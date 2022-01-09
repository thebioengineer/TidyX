
## packages
library(tidyverse)
library(broom)
library(gt)

theme_set(theme_bw())

## data
df <- mtcars %>%
  mutate(vs = as.factor(vs)) 

head(df)

## model
fit <- df %>%
  lm(
    mpg ~ cyl + wt + as.factor(am),
    data = .)

fit %>%
  tidy() %>%
  mutate(across(
    .cols = estimate:statistic,
    ~round(.x, 2)),
  ) %>%
  
  ## start GT Table
  gt() %>%
  
  gt::fmt_scientific(
    p.value, decimals = 2,
  ) %>% 
  
  tab_source_note(
    source_note = md("**Table 1.** Linear regression model coefficients for predicting miles per gallon")
  ) %>%
  
  # tab_header(
  #   title = md("**Table 1.** Linear regression model coefficients for predicting miles per gallon")
  # ) %>%
  # opt_align_table_header(
  #   align = "left"
  # ) %>%

  cols_label(
    term = md("**Term**"),
    estimate = md("**Coefficients**"),
    std.error = md("**Standard Error**"),
    statistic = md("**t-value**"),
    p.value = md("**p-value**")
  ) %>%
  
  tab_footnote(
    footnote = "Transmission (0 = automatic, 1 = manual)",
    locations = cells_body(
      columns = term, 
      rows = 4
    )
  ) %>%
  
  tab_style(
    style = list(
      cell_borders(
        sides = "top",
        color = "black",
        weight = px(2)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = everything()
      )
    )
  ) %>%
  
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(2)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = everything()
      )
    )
  )


### Make a plot of a different variable
mtcars %>% 
  mutate(gear = as.factor(gear)) %>%
  group_by(gear) %>%
  summarise(
    quantile = c("low", "median", "high"),
    mpg = quantile(mpg, probs = c(0.25, 0.5, 0.75)),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = quantile,
              values_from = mpg) %>%
  
  ## plot
  ggplot(aes(y = reorder(gear, median))) +
  
  geom_linerange(aes(xmin = low, xmax = high),
                 size = 10,
                 color = "grey",
                 alpha = 0.5) +
  geom_point(aes(x = median),
             color = "orange",
             size = 5) +
  
  
  geom_curve(x = 22.5,
             xend = 19.8,
             y = 2.7,
             yend = 2.1,
             arrow = arrow(length = unit(0.3,"cm")),
             curvature = 0.5) +
  annotate(geom = "text",
           x = 23.2,
           y = 2.7,
           label = "Median\nMPG") +
  
  
  labs(caption = "Figure 1. Median MPG by car gears",
       x = NULL,
       y = "Car Gears") +
  
  theme(axis.text = element_text(size = 13, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 15, vjust = -0.5, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold", vjust = 1.5),
        axis.line = element_line(color = "black"))
  
