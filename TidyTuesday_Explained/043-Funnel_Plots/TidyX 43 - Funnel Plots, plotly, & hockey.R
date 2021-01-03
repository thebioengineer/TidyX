## TidyX 43: funnel plots, plotly & hockey

### packages & functions --------------------------------------------
library(tidyverse)
library(plotly)
library(rvest)

### Webscrape & Data Clean Up ----------------------------------------

## create object from reading your html as a separate step
## prevents re-requesting the html page every time
hockey_url <- "https://www.hockey-reference.com/leagues/NHL_2020_skaters.html"
hockey_html <- hockey_url %>%
  read_html() 

hockey_table <- hockey_html %>% 
  html_node('table') %>%
  html_table(fill = TRUE) %>%
  setNames(1:ncol(.)) 

nhl <- hockey_table %>% 
  select(2,5,6,7,20) %>% 
  rename(player = 1,
         pos = 2,
         games = 3,
         goals = 4,
         shots = 5) %>%
  filter(player != "Player") %>%
  group_by(player) %>%
  summarize(
    across(.cols = c(games:shots),
           ~sum(as.numeric(.x))),
    pos = pos[which.max(games)],
    positions = paste0(pos," (Games:", games,", Goals:", goals,", Shots:", shots,")", collapse= "; "),
    .groups = "drop"  
  ) %>%
  mutate(shooting_pct = goals / shots)

nhl %>% head()


### EDA --------------------------------------------

## What is the distribution of shots taken?
quantile(nhl$shots)

# by position
by(nhl$shots, nhl$pos, quantile)

# visuals
nhl %>%
  ggplot(aes(x = shots, fill = pos)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~pos) +
  ggtitle("Shots by Position") +
  theme(legend.position = "none")

nhl %>%
  mutate(pos = as.factor(pos)) %>%
  ggplot(aes(x = shots, y = reorder(pos, shots), fill = pos)) +
  geom_boxplot(alpha = 0.7) +
  ggtitle("Shots by Position") +
  theme(legend.position = "none")

## Add a league average for shooting% based on the median number of shots for the population

nhl <- nhl %>%
  mutate(
    lg_avg_shooting_pct = median(shooting_pct[shots >= median(shots)])
    )


### Player Plot with Confidence Intervals -------------------------

nhl %>%
  mutate(CL_95 = 2 * sqrt((shooting_pct * (1 - shooting_pct)) / shots),
         CL_99 = 3 * sqrt((shooting_pct * (1 - shooting_pct)) / shots)) %>%
  sample_n(size = 15) %>%
  ggplot(aes(x = shooting_pct, y = reorder(player, shooting_pct))) +
  geom_vline(aes(xintercept = lg_avg_shooting_pct),
             color = "red",
             linetype = "dashed",
             size = 1.1) +
  geom_point(aes(size = shots)) +
  geom_errorbar(aes(xmin = shooting_pct - CL_99,
                    xmax = shooting_pct + CL_99),
                width = 0) +
  geom_errorbar(aes(xmin = shooting_pct - CL_95,
                    xmax = shooting_pct + CL_95),
                size = 1.5,
                width = 0) +
  scale_x_continuous(labels = scales::percent)

### Funnel Plot ---------------------------------------------------

# calculate 95% and 99% CI for funnel plots

nhl_funnel <- nhl %>%
  mutate(shooting_pct = coalesce(shooting_pct, 0),
         CL_95 = 2 * sqrt((lg_avg_shooting_pct * (1 - lg_avg_shooting_pct)) / shots),
         CL_99 = 3 * sqrt((lg_avg_shooting_pct * (1 - lg_avg_shooting_pct)) / shots)) %>%
  arrange(desc(shots))


nhl_funnel %>%
  ggplot(aes(x = shots, y = shooting_pct)) +
  geom_point(alpha = 0.8) +
  geom_hline(aes(yintercept = lg_avg_shooting_pct)) +
  geom_line(aes(y = lg_avg_shooting_pct + CL_95),
            linetype = "dashed",
            color = "blue") +
  geom_line(aes(y = lg_avg_shooting_pct - CL_95),
            linetype = "dashed",
            color = "blue") +
  geom_line(aes(y = lg_avg_shooting_pct + CL_99),
            linetype = "dashed",
            color = "red") +
  geom_line(aes(y = lg_avg_shooting_pct - CL_99),
            linetype = "dashed",
            color = "red") +
  labs(x = "shots",
       y = "shooting%",
       title = "Shooting% Funnel Plot") +
  scale_y_continuous(labels = scales::percent)


nhl_funnel %>%
  mutate(CI_95_Low = lg_avg_shooting_pct - CL_95,
         CI_95_High = lg_avg_shooting_pct + CL_95,
         CI_99_Low = lg_avg_shooting_pct - CL_99,
         CI_99_High = lg_avg_shooting_pct + CL_99) %>%
  plot_ly() %>%
  add_trace(
    mode = "markers",
    x = ~shots,
    y = ~shooting_pct,
    hovertemplate = ~paste0(
      "Player: ", player, "<br>",
      "Position: ", pos, "<br>",
      "Shots: ", shots, "<br>",
      "Goals: ", goals, "<br>",
      "Shooting%: ", paste0(round(shooting_pct*100, 1), "%")
    )
  ) %>%
  add_trace(
    mode = "line",
    line = list(dash = "dot",
                color = "blue"),
    x = ~shots,
    y = ~CI_95_Low
  ) %>%
  add_trace(
    mode = "line",
    line = list(dash = "dot",
                color = "blue"),
    x = ~shots,
    y = ~CI_95_High
  ) %>%
  add_trace(
    mode = "line",
    line = list(dash = "dot",
                color = "red"),
    x = ~shots,
    y = ~CI_99_Low
  ) %>%
  add_trace(
    mode = "line",
    line = list(dash = "dot",
                color = "red"),
    x = ~shots,
    y = ~CI_99_High
  ) %>% 
  add_trace(
    mode = "line",
    line = list(color = "black"),
    x = ~shots,
    y = ~lg_avg_shooting_pct
  ) %>% 
  layout(
    showlegend = FALSE
  )