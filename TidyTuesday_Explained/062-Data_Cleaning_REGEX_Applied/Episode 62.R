
#####################################
#### Regex 101 - Applied Regex
####################################

### Covered in Episode 61 ----
## bit.ly/TidyX_Ep61

library(tidyverse)
library(lubridate)
library(rvest)

theme_set(theme_light())

#### scrape Miami Heat @ Milwaukee Bucks (1st Rd | Game 2 Eastern Conference Playoffs, 2021) ####
## html
espn_html <- read_html("https://www.espn.com/nba/playbyplay/_/gameId/401327878")

## scrape elements for time stamps, play details, and score
time_stamps <- espn_html %>%
  html_nodes("div") %>%
  html_nodes("#gp-quarter-1") %>%
  html_nodes(".time-stamp") %>%
  html_text() %>%
  as_tibble() %>%
  rename(time = value)

possession_details <- espn_html %>%
  html_nodes("div") %>%
  html_nodes("#gp-quarter-1") %>%
  html_nodes(".logo") %>%
  html_nodes("img") %>% 
  html_attr("src") %>%
  gsub(".*(mil|mia).*","\\1",.) %>% 
  as_tibble() %>%
  rename(possession = value)

play_details <- espn_html %>%
  html_nodes("div") %>%
  html_nodes("#gp-quarter-1") %>%
  html_nodes(".game-details") %>%
  html_text() %>%
  as_tibble() %>%
  rename(play_details = value)

score <- espn_html %>%
  html_nodes("div") %>%
  html_nodes("#gp-quarter-1") %>%
  html_nodes(".combined-score") %>%
  html_text() %>%
  as_tibble() %>%
  rename(score = value)

## bind data together
df <- bind_cols(time_stamps, possession_details, play_details, score)
df %>% head()

####################################
######## data cleaning #############
####################################

# Get the score in separate columns for the respective team
df <- df %>%
  mutate(
    Heat  = as.numeric(sub("-.*", replacement = "", score)), ## using sub
    Bucks = as.numeric(gsub(".*-", replacement = "", score)), ## using gsub same
    score_diff = Heat - Bucks)

# adjust the time from a character, into minutes

df <- df %>%
  mutate(
    quarter_time = ifelse(
      grepl(pattern = ":", time),
      minute(ms(time)) + second(ms(time)) / 60, 
      as.numeric(as.character(time)) / 60),
    quarter_time = 12 - quarter_time,
    team_col = if_else(score_diff <= 0, "Bucks", "Heat")
  )

### Episode 62 ----

## Scoring Worm
df %>%
  ggplot(aes(x = quarter_time, y = score_diff)) +
  geom_hline(yintercept = 0,
             size = 1,
             linetype = "dashed") +
  geom_step(size = 1.2,
            aes(color = team_col)) +
  scale_color_manual(values = c(Bucks = "darkgreen", Heat = "red")) +
  scale_y_continuous(limits = c(-30, 30),
                     breaks = seq(from = -30, to = 30, by = 10),
                     labels = c(30, 20, 10, 0, 10, 20, 30)) +
  labs(title = "Heat @ Bucks",
       subtitle = "Quarter 1",
       x = "Time",
       y = "Score Difference") +
  annotate(geom = "text", x = 1,  y = -20, label = "Bucks", size = 10) +
  annotate(geom = "text", x = 1,  y = 20, label = "Heat", size = 10) +
  theme(legend.position = "none")

#### Identifying all non-free throw shots
## Count the number of shots in the first quarter
# Set up a "pattern" of shot types to check for
shot_types <- "shot|jump|layup|dunk|three pointer"
Free_throws <- "free throw"
df %>%
  mutate(
    shot = case_when(
      grepl(shot_types, play_details, ignore.case = TRUE) ~ 1,
      grepl(Free_throws, play_details, ignore.case = TRUE) ~ 2,
      TRUE ~ 0)
  ) %>% 
  summarize(total_shots = sum(shot, na.rm = TRUE))


# alternative approach -- put the pattern into grepl()
df %>%
  mutate(
    shot = case_when(
    grepl("([Ss]hot|[Jj]ump|[Ll]ayup|[Dd]unk|[Tt]hree pointer)",play_details ) ~ 1,
    TRUE ~ 0
  )) %>%
  summarize(total_shots = sum(shot, na.rm = TRUE))

df %>%
  mutate(shot = case_when(
    str_detect(play_details, regex(shot_types, ignore_case = TRUE)) ~ 1,
    TRUE ~ 0)) %>% 
  summarize(total_shots = sum(shot, na.rm = TRUE))


## How many shots per team?
df %>%
  mutate(shot = case_when(grepl(shot_types, play_details, ignore.case = TRUE) ~ 1,
                          TRUE ~ 0)) %>%
  group_by(possession) %>%
  summarize(shots = sum(shot))


### Substitutions by team
# Which players were subbed out and which players came in

subs <- df %>%
  filter(grepl(pattern = "enters the game for", play_details)) %>%
  mutate(player_out = gsub(".* enters the game for ", replacement = "", play_details),
         player_in = gsub(" enters the game for .*", replacement = "", play_details),
         sub_detail = paste(player_in, player_out, sep = " in for "))

subs_stringr <- df %>%
  filter(grepl(pattern = "enters the game for", play_details)) %>%
  mutate(player_out = str_remove(play_details,".* enters the game for "),
         player_in = str_remove(play_details," enters the game for .*"),
         sub_detail = paste(player_in, player_out, sep = " in for "))


subs2 <- df %>%
  filter(grepl(pattern = "enters the game for", play_details)) %>%
  mutate(player_out = gsub("(.*) enters the game for (.*)", replacement = "\\2", play_details),
         player_in = gsub("(.*) enters the game for (.*)", replacement = "\\1", play_details),
         sub_detail = paste(player_in, player_out, sep = " in for "))

# How many substitutions per team?

df %>%
  filter(grepl(pattern = "enters the game for", play_details)) %>%
  count(possession) %>%
  ggplot(aes(x = possession, y = n)) +
  geom_col(aes(fill = possession),
           color = "black",
           alpha = 0.75) +
  scale_fill_manual(values = c(mil = "darkgreen", mia = "red")) +
  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 2)) +
  labs(x = NULL,
       y = "Substitutions",
       title = "First Quarter Substitutions",
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

df %>%
  ggplot(aes(x = quarter_time, y = score_diff)) +
  geom_hline(yintercept = 0,
             size = 1,
             linetype = "dashed") +
  geom_step(size = 1.2,
            aes(color = team_col),
            alpha = 0.3) +
  geom_point(
    data = subs %>% filter(possession == "mil"),
    color = "darkgreen"
    ) +
  ggrepel::geom_text_repel(
              data = subs %>% filter(possession == "mil") ,
              aes(label = sub_detail),
              color = "darkgreen",
              size = 3) +
  scale_color_manual(values = c(Bucks = "darkgreen", Heat = "red")) +
  scale_y_continuous(limits = c(-30, 30),
                     breaks = seq(from = -30, to = 30, by = 10),
                     labels = c(30, 20, 10, 0, 10, 20, 30)) +
  labs(title = "Heat @ Bucks",
       subtitle = "Quarter 1 - Bucks Substitutions",
       x = "Time",
       y = "Score Difference") +
  annotate(geom = "text", x = 1,  y = -20, label = "Bucks", size = 10) +
  annotate(geom = "text", x = 1,  y = 20, label = "Heat", size = 10) +
  theme(legend.position = "none")



