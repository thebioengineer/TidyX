## Data
# https://www.kaggle.com/drgilermo/nba-players-stats?select=Seasons_Stats.csv

## Inspiration
# https://projects.fivethirtyeight.com/carmelo/

## Packages -------------------------------------------------------------------
library(tidyverse)
library(here)
library(FNN)

theme_set(theme_bw() +
            theme(axis.text = element_text(size = 12, face = "bold"),
                  axis.title = element_text(size = 16),
                  panel.border = element_blank(),
                  panel.grid.major = element_line(color = "grey")))


## Data ----------------------------------------------------------------------

season_stats <- read.csv(here("TidyTuesday_Explained/028-Nearest_Neighbor_and_ReactiveValues/Seasons_Stats.csv"), header = TRUE ) 


## Process Data --------------------------------------------------------------

vorp_ts <- season_stats %>%
  filter(Year >= 2000) %>%
  select(-X) %>%
  group_by(Player, Year) %>%
  summarize(across(c("VORP"), sum, na.rm = T)) %>%
  mutate(
    Experience = seq_along(Player),
    Max_Years = max(Experience)
  )


## KNN ---------------------------------------------------------------------

## What is KNN, Patrick?

## ML technique - K nearest neighbors
## Non parametric
## Classify or predict outcome
## Size is dependent on K

## Convert to wide
vorp_wide <- vorp_ts  %>%
  select(-Year) %>%
  pivot_wider(
    names_from = Experience,
    values_from = VORP
  )


## Select a player of interest
test <- vorp_wide %>%
  filter(Player == "O.J. Mayo") %>%
  select( where( ~sum(!is.na(.x)) > 0 ) )

## Get the max number of years for the player
yrs <- test %>% pull(Max_Years)

## select all players with +2 years > the number of years for the player of interest
train <- vorp_wide %>%
  filter( Max_Years >= (yrs + 2) ) %>%
  select( where( ~sum(!is.na(.x)) > 0 ) )


## Create KNN to find the 4 closest players to the player of interest
knn_similarity <- knn(
         train[,3:ncol(test)],
         test[,3:ncol(test)],
         cl = train$Player,
         k = 4,
         algorithm = "kd_tree")

knn_similarity

## Get the row index for the players in the cluster
new_df <- bind_rows(
  test,
  train[c(attr(knn_similarity, "nn.index")), ]
  )

## Forecast the next two years
forecast <- new_df %>%
  ungroup() %>% 
  pivot_longer(cols = matches("\\d+"),
               names_to = "Experience",
               values_to = "VORP") %>%
  mutate(Experience = as.numeric(Experience)) %>%
  filter(
    Player != "O.J. Mayo",
    Experience > max(Max_Years[Player == "O.J. Mayo"], na.rm=TRUE)
  ) %>%
  group_by(Experience) %>%
  summarize(
    VORP = mean(VORP, na.rm = TRUE)
  ) %>% 
  filter(!is.na(VORP))

player <- test %>%
  pivot_longer(cols = 3:last_col(),
               names_to = "Experience",
               values_to = "VORP") %>% 
  bind_rows(
    data.frame(
      Player = "O.J. Mayo",
      VORP = forecast$VORP
    )
  ) %>% 
  mutate(
    Experience = row_number(),
    Max_Years = unique(as.numeric(na.exclude(Max_Years))),
    Pred_Years = ifelse(Experience > Max_Years, 1, 0)
  ) %>% 
  bind_rows(
    .[.$Experience == .$Max_Years,] %>% 
      mutate(Pred_Years = 1)
  ) %>% 
  arrange(Experience, Pred_Years)


## plot the players career VORP
player %>%
  mutate(
    Projection = factor(Pred_Years, levels = c("0","1"))
  ) %>% 
  ggplot(aes(
    x = Experience,
    y = VORP,
    group = Pred_Years,
    color = Projection
  )) +
  geom_hline(
    yintercept = 0,
    size = 1, 
    color = "#383838"
  ) + 
  geom_vline(
    aes(xintercept = Max_Years),
    size = 1, 
    color = "#383838"
  ) +
  geom_line(aes(
    linetype = Projection
  ),
  size = 2
  ) +
  geom_point(size = 3, shape = 1) +
  scale_linetype_manual( values = c("solid", "dashed") ) +
  scale_color_manual( values = c("Black", "Grey") ) +
  ylim(c(-4, 15))+
  labs(
    title = "Career Value over Replacement",
    y = "",
    x = "Years in NBA") + 
  theme(
    legend.position = "none",
    title = element_text(size = 20, face = "bold")
  )
