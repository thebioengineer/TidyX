
#####################################
#### Regex 201 - Regex Lookarounds
####################################

### Covered in Episode 61 ----
## bit.ly/TidyX_Ep61

library(tidyverse)
library(lubridate)
library(rvest)
library(patchwork)

theme_set(theme_light())

#### scrape Miami Heat @ Milwaukee Bucks (1st Rd | Game 2 Eastern Conference Playoffs, 2021) ####
## html
pbp_html <- read_html("https://www.espn.com/nba/playbyplay/_/gameId/401327878")

## scrape elements for time stamps, play details, and score
game_data <- map_dfr(1:4,function(q){
  
  time_stamps <- pbp_html %>%
    html_nodes("div") %>%
    html_nodes(paste0("#gp-quarter-",q)) %>%
    html_nodes(".time-stamp") %>%
    html_text() %>%
    as_tibble() %>%
    rename(time = value)
  
  possession_details <- pbp_html %>%
    html_nodes("div") %>%
    html_nodes(paste0("#gp-quarter-",q)) %>%
    html_nodes(".logo") %>%
    html_nodes("img") %>% 
    html_attr("src") %>%
    gsub(".*(mil|mia).*","\\1",.) %>% 
    as_tibble() %>%
    rename(possession = value)
  
  play_details <- pbp_html %>%
    html_nodes("div") %>%
    html_nodes(paste0("#gp-quarter-",q)) %>%
    html_nodes(".game-details") %>%
    html_text() %>%
    as_tibble() %>%
    rename(play_details = value)
  
  score <- pbp_html %>%
    html_nodes("div") %>%
    html_nodes(paste0("#gp-quarter-",q)) %>%
    html_nodes(".combined-score") %>%
    html_text() %>%
    as_tibble() %>%
    rename(score = value)
  
  ## bind data together
  bind_cols(time_stamps, possession_details, play_details, score) %>%
    mutate(quarter = q)
})

####################################
######## data cleaning #############
####################################

# Get the score in separate columns for the respective team
game_data_score <- game_data %>%
  mutate(
    Heat  = as.numeric(sub("-.*", replacement = "", score)), ## using sub
    Bucks = as.numeric(gsub(".*-", replacement = "", score)), ## using gsub same
    score_diff = Heat - Bucks)

# adjust the time from a character, into minutes

game_data_time <- game_data_score %>%
  mutate(
    quarter_time = ifelse(
      grepl(pattern = ":", time),
      minute(ms(time)) + second(ms(time)) / 60, 
      as.numeric(as.character(time)) / 60),
    quarter_time = 12 - quarter_time,
    game_time = (12 * (quarter-1)) + quarter_time,
    team_col = if_else(score_diff <= 0, "Bucks", "Heat")
  )

### Episode 63 ---- Regex Lookarounds

score_worm <- game_data_time %>%
  ggplot(aes(x = game_time, y = score_diff)) +
  geom_hline(yintercept = 0,
             size = 1,
             linetype = "dashed") +
  geom_step(size = 1.2,
            aes(color = team_col)) +
  scale_color_manual(values = c(Bucks = "darkgreen", Heat = "red")) +
  scale_y_continuous(limits = c(-40,40),
                     breaks = seq(from = -40, to = 40, by = 10),
                     labels = abs(seq(from = -40, to = 40, by = 10))) +
  scale_x_continuous(
    breaks = seq(from = 0, to = 48, by = 12),
    labels = c(paste0("Q",1:4), "End Game")
  ) +
  labs(x = "Time",
       y = "Score Difference") +
  annotate(geom = "text", x = 1,  y = -20, label = "Bucks", size = 10) +
  annotate(geom = "text", x = 1,  y = 20, label = "Heat", size = 10) +
  theme(legend.position = "none")

### Substitutions by team
# Which players were subbed out and which players came in

subs <- game_data_time %>%
  filter(grepl(pattern = "enters the game for", play_details)) %>%
  mutate(
    player_in = trimws(str_extract(play_details, ".*(?=enters the game for)")),
    player_out = trimws(str_extract(play_details, "(?<=enters the game for).*"))
  ) %>% 
  select(game_time, player_in, player_out, team = possession)

### Chart of player time in game

starters <- tribble(
  ~ game_time, ~ player_in, ~ team,
  0, "Giannis Antetokounmpo", "mil",
  0, "Khris Middleton", "mil",
  0, "Brook Lopez", "mil",
  0, "Jrue Holiday", "mil",
  0, "Donte DiVincenzo", "mil",
  0, "Trevor Ariza", "mia",
  0, "Jimmy Butler", "mia",
  0, "Bam Adebayo", "mia",
  0, "Kendrick Nunn", "mia",
  0, "Duncan Robinson", "mia"
)


## create data.frame listing players in and subs
player_flow <- bind_rows(starters, subs) %>% 
  arrange(game_time)

players <- unique(player_flow$player_in)

player_stints <- players %>% 
  
  map_dfr(function(player){
    
    stints <- player_flow %>% 
      filter(player_in == player | player_out == player) %>% 
      mutate(
        direction = case_when(
          player_in == player ~ "sub_in",
          player_out == player ~ "sub_out"
        ),
        player = player
      ) %>% 
      select(game_time, direction, player, team) %>% 
      pivot_wider(
        names_from = direction, 
        values_from = game_time
      )
    
    if(!"sub_out" %in% colnames(stints)){
      stints$sub_out <- list(NA_real_)
    }
    
    stints <- unnest(stints, c(sub_in, sub_out))
    
    if(is.na(stints[nrow(stints),"sub_out"])){
      stints[nrow(stints),"sub_out"] <- 48
    }
    
    stints
    
  })



# How many substitutions per team/player?

player_stints %>%
  count(team, player) %>%
  ggplot(aes(y = fct_reorder(player,n), x = n)) +
  geom_col(aes(fill = team),
           color = "black",
           alpha = 0.75) +
  facet_wrap( team ~ .,scales = "free_y") +
  scale_fill_manual(values = c(mil = "darkgreen", mia = "red")) +
  scale_x_continuous(breaks = seq(from = 0, to = 10, by = 2)) +
  labs(y = NULL,
       x = "Substitutions",
       title = "",
       subtitle = "Miami Heat @ Milwaukee Bucks (1st Rd | Game 2 Eastern Conference Playoffs, 2021)",
       caption = "Data: https://www.espn.com/nba/playbyplay/_/gameId/401327878") +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 13),
        axis.text = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 13, face = "bold"),
        legend.position = "none",
        plot.caption = element_text(face = "bold", color = "blue"))


# How many n duration per team/player?

player_stints %>%
  group_by(team, player) %>%
  summarize(
    duration = mean(sub_out - sub_in)
  ) %>% 
  ggplot(aes(y = fct_reorder(player,duration), x = duration)) +
  geom_col(aes(fill = team),
           color = "black",
           alpha = 0.75) +
  facet_wrap( team ~ .,scales = "free_y") +
  scale_fill_manual(values = c(mil = "darkgreen", mia = "red")) +
  labs(y = NULL,
       x = "Mean Duration",
       title = "",
       subtitle = "Miami Heat @ Milwaukee Bucks (1st Rd | Game 2 Eastern Conference Playoffs, 2021)",
       caption = "Data: https://www.espn.com/nba/playbyplay/_/gameId/401327878") +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 13),
        axis.text = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 13, face = "bold"),
        legend.position = "none",
        plot.caption = element_text(face = "bold", color = "blue"))

## When in the game did the Bucks make substitutions?

player_gantt <- player_stints %>%
  group_by(team, player) %>%
  mutate(
    duration = sum(sub_out - sub_in),
    player = fct_reorder(player, duration)
  ) %>% 
  ggplot() +
  geom_segment(
    aes(
      y = tidytext::reorder_within(player,duration, team),
      yend = tidytext::reorder_within(player,duration, team), 
      x = sub_in,
      xend = sub_out,
      color = team),
    size = 4) +
  facet_wrap( ~ team , scales = "free_y", nrow = 2) +
  scale_color_manual(values = c(mil = "darkgreen", mia = "red")) +
  scale_x_continuous(
    breaks = seq(from = 0, to = 48, by = 12),
    labels = c(paste0("Q",1:4), "End Game")
    ) +
  tidytext::scale_y_reordered() +
  labs(title = "Heat @ Bucks",
       subtitle = "Miami Heat @ Milwaukee Bucks (1st Rd | Game 2 Eastern Conference Playoffs, 2021)",
       x = NULL,
       y = NULL) +
  theme(legend.position = "none")

(player_gantt + theme(axis.text.x = element_blank())) / score_worm + 
  plot_layout(ncol = 1, nrow = 2, heights = c(3,2))



