
########### TidyX Episode 7: Heatmaps & Facet Wraps ##########
##############################################################


### Load packages & functions ---------------------------------------------
library(tidyverse)
library(reshape2)
library(rvest)
library(conflicted)
theme_set(theme_bw())

# set conflicted
conflict_prefer("pluck","purrr")
conflict_prefer("filter","dplyr")

# z-score function
z_score <- function(x){
  z = (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
  return(z)
}


### Get Data ---------------------------------------------------------------
# get URL
url <- read_html("https://www.basketball-reference.com/leagues/NBA_2019_totals.html")

# Extract the table of interest form the per webpage

nba <- url %>% 
  html_table(fill = T) %>% 
  pluck(1) %>%
  filter(Rk != "Rk")

### Data Pre-process ------------------------------------------------------------------------
# Chanage columns to numeric
numeric_cols <- c(1, 4, 6:30)
nba[,numeric_cols] <- apply(nba[,numeric_cols], 2, function(x) as.numeric(as.character(x)))

# Some players played for multiple teams so aggregate over the season BY Position
nba_main <- nba %>%
  group_by(Player, Pos, Age) %>%
  summarize_at(vars(G:PTS), .funs = sum)

# Get stats for heat map
nba_main <- nba_main %>%
  ungroup() %>%
  select(Player, Pos, FG_Pct = 'FG%', Three_Pt_Pct = '3P%', AST, TRB, STL, BLK, TOV) %>%
  mutate(Pos = case_when(Pos == "C-PF" ~ "C",
                         Pos %in% c("PF-C", "PF-SF") ~ "PF",
                         Pos == "SF-SG" ~ "SF",
                         Pos %in% c("SG-PF", "SG-SF") ~ "SG",
                         TRUE ~ Pos)) %>%
  group_by(Pos) %>%
  mutate_at(vars(FG_Pct:TOV), .funs = z_score) %>%
  mutate(TOV = TOV * -1)
  
nba_main

# Convert to long data frame
nba_long <- melt(nba_main, id = c("Player", "Pos"))

### Plot a heatmap by position ----------------------------------------
# Get a random sample of 8 players for each position group (smaller sample for plotting)

set.seed(5555)
c_sample <- nba_long %>%
  filter(Pos == "C") %>%
  sample_n(., 8) %>%
  pull(Player)

set.seed(5555)
pf_sample <- nba_long %>%
  filter(Pos == "PF") %>%
  sample_n(., 8) %>%
  pull(Player)

set.seed(5555)
pg_sample <- nba_long %>%
  filter(Pos == "PG") %>%
  sample_n(., 8) %>%
  pull(Player)

set.seed(5555)
sf_sample <- nba_long %>%
  filter(Pos == "SF") %>%
  sample_n(., 8) %>%
  pull(Player)

set.seed(5555)
sg_sample <- nba_long %>%
  filter(Pos == "SG") %>%
  sample_n(., 8) %>%
  pull(Player)

# function to select a subset of players n
subset_players <- function(player, n, seed ){
  if(!missing(seed)){
    set.seed(seed)
  }
  player %in% sample(unique(player), size = n, replace = FALSE)
}

## Visualize heat map
nba_long %>%
  group_by(Pos) %>%
  mutate(
    keep_player = subset_players(Player, 8, seed = 5555)
  ) %>%
  ungroup() %>%
  filter( keep_player == TRUE ) %>%
  # filter(Player %in% c(c_sample, pf_sample, pg_sample, sf_sample, sg_sample)) %>% # this way can keep a player that was listed in muliple positions (Jimmy Butler)
  ggplot(aes(x = variable, y = Player)) +
    geom_tile(aes(fill = value), color = "white") +
    geom_text(aes(label = round(value,1))) +
    scale_fill_gradient2(low = "blue", high = "red") + # pick color-blind friendly color scales. also picking color scales that highlight important values
    # scale_fill_gradient( low = "red", high = "green") + 
    facet_wrap(~Pos, scale = "free_y") +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      strip.background = element_rect(fill = "black"),
      strip.text = element_text(face = "bold", color = "white"),
      legend.position = "bottom"
      ) +
    labs(title = "2019 NBA Heat Map",
       subtitle = "Absolute Stats (not per minute)",
       x = "",
       y = "",
       caption = "https://www.basketball-reference.com/leagues/NBA_2019_totals.html")
