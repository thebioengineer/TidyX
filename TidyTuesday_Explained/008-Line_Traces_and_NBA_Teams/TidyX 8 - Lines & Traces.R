### TidyX 8 - Lines & Traces
### Load Packages -------------------------------------------

## Load Libraries
library(tidyverse)
library(rvest)
library(here)
library(cowplot)

### Scrape Team Stats -------------------------------------------

## get teams

NBA_teams_html <- read_html("https://www.basketball-reference.com/teams/") %>% 
  html_nodes(".table_outer_container") %>% 
  html_node("table")

NBA_teams_code <- NBA_teams_html %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  basename()

NBA_team_Name <- NBA_teams_html %>% 
  html_nodes("a") %>% 
  html_text

## Determine active teams

active_teams <- NBA_teams_html %>% 
  html_table() %>% 
  purrr::pluck(1) %>% 
  filter(To == "2019-20") %>% 
  distinct(Franchise) %>% 
  pull(Franchise)

NBA_Teams <- tibble(
  code = NBA_teams_code,
  team = NBA_team_Name
) %>% 
  filter( team %in% active_teams)


## Set up function for scraping
scrape_history <- function(team_table){
  
  message(paste("scraping team:",team_table$team))
  
  url <- file.path("https://www.basketball-reference.com/teams",team_table$code,"stats_basic_totals.html")
  tab <- read_html(url) %>% 
    html_table() %>% 
    purrr::pluck(1)
  tab[,which(colnames(tab) == "")] <- NULL
  tab %>% 
    mutate_all(as.character) %>% 
    filter(Season != "Season") %>% 
    tibble(team_table)
}

active_NBA_stats <- NBA_Teams %>% 
  split(.$code) %>% 
  map_dfr(~scrape_history(.x))


### Data Munging -------------------------------------------

stats_of_interest <- active_NBA_stats %>%
  select(
    "team_id" = code,
    "team_name" = team,
    Season,
    "Two Point Attempts" = `2PA`,
    "Three Point Attempts" = `3PA`,
    "Free Throw Attempts" = `FTA`,
    "Wins" = W,
    "Losses" = L,
    Finish
  ) %>%
  mutate_at(
    vars(
      `Two Point Attempts`,
      `Three Point Attempts`,
      `Free Throw Attempts`,
      Wins,
      Losses
    ),
    as.numeric
  ) %>%
  mutate(
    "Win Percent" = (Wins / (Wins + Losses)) * 100,
    "Win Division" = Finish == 1,
    Season = as.numeric(gsub("(\\d+)-(\\d+)", "\\1", Season))
    ) %>%  # keep only the start year) %>%
  gather(
    "Statistic",
    "Value",
    `Two Point Attempts`,
    `Three Point Attempts`,
    `Free Throw Attempts`,
    Wins,
    Losses,
    `Win Percent`
  ) %>%
  mutate(Statistic = factor(
    Statistic,
    levels = c(
      "Win Percent",
      "Two Point Attempts",
      "Three Point Attempts",
      "Free Throw Attempts",
      "Wins",
      "Losses"
    )
  ))



### Create Plot -------------------------------------------

TOI <- "GSW"

plotting_data <- stats_of_interest %>%
  filter( ! Statistic %in% c("Wins","Losses"))

Won_division <- plotting_data %>% 
  filter(`Win Division` == TRUE,
         team_id == TOI)

hist_plot <- plotting_data %>% 
  
  ## Set aes for any data to be added to data
  ggplot(aes(x = Season,
             y = Value)) +
  
  ## Add lines, alpha and coloring are for the "ghosting" of lines
  geom_line(aes(
    color = team_id == TOI,
    group = team_id,
    alpha = ifelse(team_id == TOI, 1, 0.8)
  )) + 
  
  ## Add points of note, such as winning the division
  geom_point(
    data = Won_division,
    shape = 23,
    color = "gold"
  ) +
  
  ## Set the colors for where we don't match or do match the TOI
  scale_color_manual(values = c("light grey", "lightblue")) +
  
  ## Set the Labels
  labs(x = "Season",
       y = "",
       title = "Active NBA Team Trends",
       subtitle = NBA_Teams[["team"]][NBA_Teams$code == TOI],
       caption = "Source: https://basketball-reference.com/") +
  
  scale_y_continuous(labels = scales::comma) + 
  
  facet_wrap(
    ~ Statistic,
    ncol = 1,
    scales = "free_y"
  ) +
  
  ## Setting themes
  theme(panel.background = element_rect(fill = "#333333", color = "#333333"),
        plot.background = element_rect(fill = "#333333", color = "#333333"),
        panel.grid.major = element_line(color = "#333333"),
        panel.grid.minor = element_line(color = "#333333"),
        panel.border = element_rect(color = "white", fill = NA),
        legend.position = "none",
        axis.text.x = element_text(color = "white", 
                                   face = "bold", 
                                   size = 12),
        axis.text.y = element_text(color = "white",
                                   face = "bold", 
                                   size = 12),
        axis.title.x = element_text(color = "white",
                                    face = "bold", 
                                    size = 13,
                                    vjust = 2),
        plot.title = element_text(color = "white",
                                  size = 20),
        plot.subtitle = element_text(color = "white", 
                                     size = 14),
        plot.caption = element_text(color = "white",
                                    face = "bold"),
        panel.grid.major.y = element_line(color = "#e6e6e6"),
        panel.grid.major.x = element_blank())

hist_plot

## Add NBA Logo
logo_file <- here("TidyTuesday_Explained/008-Line_Traces_and_COVID/img/National_Basketball_Association_logo.svg")
ggdraw() +
  draw_plot(hist_plot) +
  draw_image(logo_file,  x = -.45, y = 0.44, scale = .1)
  

ggsave(here(
  paste0(
    "TidyTuesday_Explained/008-Line_Traces_and_COVID/",
    TOI,
    "_History.png"
    )
  ),
  height = 10,
  width = 8,
  units = "in")
