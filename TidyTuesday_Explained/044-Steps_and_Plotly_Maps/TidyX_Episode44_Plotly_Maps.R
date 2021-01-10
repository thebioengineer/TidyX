library(tidyverse)
library(plotly)

## install airball
# devtools::install_github("abresler/nbastatR")
# devtools::install_github("josedv82/airball")
library(airball)

## airball provices the function to be able to pull the data for a season

games <- nba_travel(
  team = "Golden State Warriors",
  start_season = 2015, 
  end_season = 2015)

games$game <- 1:nrow(games)

travel <- games %>% 
  filter(Route != "No Travel")

stadiums_away <- games %>% 
  filter(Location == "Away") %>% 
  ungroup() %>% 
  select(Stadium = Opponent, Latitude, Longitude) %>% 
  distinct()
  
stadiums_home <- games %>%
  filter(Location == "Home") %>%
  ungroup() %>%
  select(Latitude, Longitude) %>%
  distinct() %>% 
  mutate(
    Stadium = "Home"
  )

stadiums <- bind_rows(
  stadiums_away, stadiums_home
)

## Plotly maps!

## plot the stadiums
map_data("state") %>%
  plot_mapbox(x = ~long, y = ~lat) %>%
  add_markers(
    data = stadiums,
    x = ~Longitude,
    y = ~Latitude,
    color = ~I(case_when(
      Stadium == "Home" ~ "orange",
      TRUE ~ "blue"
    )), 
    size = I(20),
    text = ~paste0("Stadium: ", Stadium),
    hoverinfo = "text"
  )


# map projection
geo <- list(
  scope = 'north america',
  projection = list(type = 'azimuthal equal area'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)

plot_geo(
  locationmode = 'USA-states',
  color = I("red")
  ) %>%
  add_markers(
    data = stadiums,
    x = ~ Longitude,
    y = ~ Latitude,
    text = ~ paste0("Stadium: ", Stadium),
    color = ~I(case_when(
      Stadium == "Home" ~ "orange",
      TRUE ~ "blue"
    )), 
    size = I(20),
    text = ~paste0("Stadium: ", Stadium),
    hoverinfo = "text"
  ) %>% 
  
  layout(
    title = paste(unique(games$Team),
                  unique(games$Season),
                  "Season Travel"),
    geo = geo,
    showlegend = FALSE
  ) %>% 
  
  add_segments(
    data = travel,
    x = ~ Longitude,
    xend = ~ d.Longitude,
    y = ~ Latitude,
    yend = ~ d.Latitude,
    alpha = 0.3,
    size = I(1),
    text = ~ paste0(
      "Opponent: ", Opponent, "<br>",
      "Win/Loss: ",    `W/L`, "<br>",
      "Flight Time: ", `Flight Time`
    ),
    hoverinfo = "text"
  )
