library(rvest)
library(tidyverse)
library(statebins) 
library(patchwork)
library(ggforce)
library(geofacet)

  
beer_stats_url <- "https://www.brewersassociation.org/statistics-and-data/state-craft-beer-stats"

beer_html <- read_html(beer_stats_url)
  
state_data <- beer_html %>% 
  html_nodes(".stat-container") %>% 
  map_dfr(
    function(state_div){
      
      state_abbr <- state_div %>% html_attr("data-id")
      state_name <- state_div %>% html_node("h1") %>% html_text()
      
      craft_breweries_n <- state_div %>% html_nodes("#per-state") %>% html_nodes(".total") %>% html_nodes(".count") %>% html_text()
      craft_breweries_n_rank <- state_div %>% html_nodes("#per-state")  %>% html_nodes(".total") %>% html_nodes(".rank") %>% html_text()
      craft_breweries_pc <- state_div %>% html_nodes("#per-state") %>% html_nodes(".per-capita") %>% html_nodes(".count") %>% html_text()
      craft_breweries_pc_rank <- state_div %>% html_nodes("#per-state")  %>% html_nodes(".per-capita") %>% html_nodes(".rank") %>% html_text()
      
      econ_impact_n <- state_div %>% html_nodes("#economic-impact") %>% html_nodes(".total") %>% html_nodes(".count") %>% html_text()
      econ_impact_n_rank <- state_div %>% html_nodes("#economic-impact") %>% html_nodes(".total") %>% html_nodes(".rank") %>% html_text()
      econ_impact_pc <- state_div %>% html_nodes("#economic-impact") %>% html_nodes(".per-capita") %>% html_nodes(".count") %>% html_text()
      econ_impact_pc_rank <- state_div %>% html_nodes("#economic-impact") %>% html_nodes(".per-capita") %>% html_nodes(".rank") %>% html_text()
      
      prod_n <- state_div %>% html_nodes("#production") %>% html_nodes(".total") %>% html_nodes(".count") %>% html_text()
      prod_n_rank <- state_div %>% html_nodes("#production") %>% html_nodes(".total") %>% html_nodes(".rank") %>% html_text()
      prod_pc <- state_div %>% html_nodes("#production") %>% html_nodes(".per-capita") %>% html_nodes(".count") %>% html_text()
      prod_pc_rank <- state_div %>% html_nodes("#production") %>% html_nodes(".per-capita") %>% html_nodes(".rank") %>% html_text()  
      
      tibble(
        state = state_name,
        state_abbr = state_abbr,
        breweries_n_value = craft_breweries_n,
        breweries_n_rank = craft_breweries_n_rank,
        breweries_pc_value = craft_breweries_pc,
        breweries_pc_rank = craft_breweries_pc_rank,
        econ_n_value = econ_impact_n,
        econ_n_rank = econ_impact_n_rank,
        econ_pc_value = econ_impact_pc,
        econ_pc_rank = econ_impact_pc_rank,
        prod_n_value = prod_n,
        prod_n_rank = prod_n_rank,
        prod_pc_value = prod_pc,
        prod_pc_rank = prod_pc_rank,
      )
    }
  ) %>% 
  pivot_longer(
    cols = c(ends_with("value"),ends_with("rank")),
    names_sep = "_",
    names_to = c("measure","reference","type"),
    values_to = "value"
  ) %>% 
  pivot_wider(
    names_from = type,
    values_from = value
  )


beer <- state_data %>% 
  mutate(
    measure = case_when(
      measure == "breweries" ~ "craft breweries",
      measure == "econ" ~ "economic impact",
      measure == "prod" ~ "production"
    ),
    reference = case_when(
      reference == "n" ~ "total",
      reference == "pc" ~ "per-capita"
    ),
    units = case_when(
      measure == "craft breweries" ~ "total",
      measure == "economic impact" ~ "million dollars",
      measure == "production" & reference == "total" ~ "barrels",
      measure == "production" & reference == "per-capita" ~ "gallons"
    ),
    rank = as.numeric(gsub(".*\\s(\\d+).*","\\1",rank)), ## extract rank into numeric
    value = readr::parse_number(value) ## convert to numeric
  )

write_csv(beer, here::here("TidyTuesday_Explained/033-Beer_and_State_Maps/craft_beer_data.csv"))


beer_prod_total <- beer %>%
  filter(
    measure == "production",
    reference == "total"
  ) %>% 
  ggplot(aes(state = state, fill = value)) + # aesthetics passed on to statebins
  geom_statebins(radius = grid::unit(5, "pt"), # more rounded bins
                 na.rm = FALSE, # keep states with no beers
                 border_size = 2, 
                 size = 1, 
                 border_col = "#242F40") +
  theme_statebins(base_family = "Bahnschrift") +
  scale_fill_gradient(
    high = "#CF7200", # pale ale
    low = "#4B0002" # Brown Ale
  ) +
  labs(title = "Overall Beer Production - Total")

beer_prod_per_capita <- beer %>%
  filter(
    measure == "production",
    reference == "per-capita"
  ) %>% 
  ggplot(aes(state = state, fill = value)) + # aesthetics passed on to statebins
  geom_statebins(radius = grid::unit(5, "pt"), # more rounded bins
                 na.rm = FALSE, # keep states with no beers
                 border_size = 2, 
                 size = 1, 
                 border_col = "#242F40") +
  theme_statebins(base_family = "Bahnschrift") +
  scale_fill_gradient(
    high = "#CF7200", # pale ale
    low = "#4B0002" # Brown Ale
  ) +
  labs(title = "Overall Beer Production - Per-Capita")

beer_impact_total <- beer %>%
  filter(
    measure == "economic impact",
    reference == "total"
  ) %>% 
  ggplot(aes(state = state, fill = value)) + # aesthetics passed on to statebins
  geom_statebins(radius = grid::unit(5, "pt"), # more rounded bins
                 na.rm = FALSE, # keep states with no beers
                 border_size = 2, 
                 size = 1, 
                 border_col = "#242F40") +
  theme_statebins(base_family = "Bahnschrift") +
  scale_fill_gradient(
    high = "#CF7200", # pale ale
    low = "#4B0002" # Brown Ale
  ) +
  labs(title = "Overall Beer Economic Impact - Total")

beer_impact_per_capita <- beer %>%
  filter(
    measure == "economic impact",
    reference == "per-capita"
  ) %>% 
  ggplot(aes(state = state, fill = value)) + # aesthetics passed on to statebins
  geom_statebins(radius = grid::unit(5, "pt"), # more rounded bins
                 na.rm = FALSE, # keep states with no beers
                 border_size = 2, 
                 size = 1, 
                 border_col = "#242F40") +
  theme_statebins(base_family = "Bahnschrift") +
  scale_fill_gradient(
    high = "#CF7200", # pale ale
    low = "#4B0002" # Brown Ale
  ) +
  labs(title = "Overall Beer Economic Impact - Per-Capita")

breweries_total <- beer %>%
  filter(
    measure == "craft breweries",
    reference == "total"
  ) %>% 
  ggplot(aes(state = state, fill = value)) + # aesthetics passed on to statebins
  geom_statebins(radius = grid::unit(5, "pt"), # more rounded bins
                 na.rm = FALSE, # keep states with no beers
                 border_size = 2, 
                 size = 1, 
                 border_col = "#242F40") +
  theme_statebins(base_family = "Bahnschrift") +
  scale_fill_gradient(
    high = "#CF7200", # pale ale
    low = "#4B0002" # Brown Ale
  ) +
  labs(title = "Craft Breweries - Total")

breweries_per_capita <- beer %>%
  filter(
    measure == "craft breweries",
    reference == "per-capita"
  ) %>% 
  ggplot(aes(state = state, fill = value)) + # aesthetics passed on to statebins
  geom_statebins(radius = grid::unit(5, "pt"), # more rounded bins
                 na.rm = FALSE, # keep states with no beers
                 border_size = 2, 
                 size = 1, 
                 border_col = "#242F40") +
  theme_statebins(base_family = "Bahnschrift") +
  scale_fill_gradient(
    high = "#CF7200", # pale ale
    low = "#4B0002" # Brown Ale
  ) +
  labs(title = "Craft Breweries - Per-Capita")

((beer_prod_total / beer_prod_per_capita) |
  (beer_impact_total / beer_impact_per_capita) |
  (breweries_total / breweries_per_capita)) + 
  plot_annotation(
    title = "Craft Beer and Breweries in America",
    theme = theme(
      plot.title = element_text(size = 20),
      plot.background = element_rect(color = "grey", fill = "grey")
      
      )
  )
  
  



## Bonus...




### bspline beer bottle
beer_bottle_half <- tribble(
  ~x,     ~y, 
   0,    0.0,
   0,    0.1,
   0,    0.1,
   0,    0.5,
  0,    0.65,
  0,    0.70,
  .06,   0.71, 
  .0801,  0.74, 
  .0802,  0.76, 
  .085,  0.78, 
  .1005,  0.95,
  .098,  0.95,
  .098,  0.96,
  .1005,  0.97,
  .1005,  0.97,
  .1005,  0.97,
  .098,  0.97,
  .098,  0.98,
  .097,  0.98,
  .0995,  0.99,
  .1005,     1
  ) %>% mutate(
    order = row_number()
  )

beer_bottle <- bind_rows(list(
  beer_bottle_half ,
  beer_bottle_half %>% 
    arrange(desc(order)) %>% 
    mutate(
      x = .3 - x
      ),
  tribble(
    ~x,     ~y, 
    0,    0.0,
    0,    0.1
    ))) %>% 
  mutate(
    x = x-.15
  )

ggplot(beer_bottle) +
  geom_bspline_closed(aes(
    x = x,
    y = y
  ),
  fill = "grey",
  n = 300) +
  theme_void()


### beer-bottle plots

scale_between <- function(x, from = 0, to = 1){
  ((x - min(x)) / (max(x) - min(x)) * (to - from)) + (from)
}

beer_scaled_dat <- beer %>% 
  group_by(
    measure,
    reference
  ) %>% 
  mutate(
    value_scaled = scale_between(value, from = 0 , to = 1),
  ) %>% 
  ungroup %>% 
  mutate(
    beer_bottle_scaled = lapply(value_scaled, function(scale){beer_bottle * scale})
  ) %>% 
  unnest(beer_bottle_scaled)

top_craft_breweries_states <- beer %>% 
  filter(
    reference == "total",
    measure == "craft breweries"
  ) %>% 
  slice_max(order_by = value, n = 6) %>% 
  arrange(desc(value)) %>% 
  pull(state)

side_by_side <- function(x, measure){
  
  widths <- lapply(unique(measure),function(m){
    c(
      min = min(x[measure == m]),
      max = max(x[measure == m])
    )
  })
  
  new_x_disp <- c(0)
  
  for(i in 2:length(unique(measure))){
    new_x_disp[[i]] <- new_x_disp[[i-1]] + widths[[i-1]][2] + abs(widths[[i]][2])
  }
  
  for(i in seq_along(unique(measure))){
    x[measure == unique(measure)[i]] <- 
      x[measure == unique(measure)[i]] + new_x_disp[i]
  }
  
  x - ((abs(min(x))+max(x))/2) # center
}

beer_scaled_dat %>%
  filter(
    state %in% top_craft_breweries_states,
    reference == "total"
    ) %>%
  mutate(
    spline_groups = paste(measure, state),
    state = factor(state, top_craft_breweries_states)
    ) %>%
  group_by(state) %>% 
  mutate(
    x = side_by_side(x, measure)
  ) %>% 
  ggplot() +
  geom_bspline_closed(aes(
    x = x,
    y = y,
    fill = measure,
    group = spline_groups
  ),
  n = 300) +
  facet_wrap(~ state) +
  scale_fill_manual(
   values = c("black","#85bb65","#f28e1c"),
   
  ) +
  labs(
    title = "Top 6 Beer Producing States"
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(color = "grey", fill = "grey"),
    plot.title = element_text(size = 20)
  )



