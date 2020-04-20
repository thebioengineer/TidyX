
#### Load Packages ----------------------------------------------

# remotes::install_github("adror1/nwslR")
library(nwslR)
library(tidyverse)
library(patchwork)
library(wordcloud2)
library(ggimage)

theme_set(theme_bw())

#### Load Data -------------------------------------------------

# get 2019 data
season <- game %>% filter(season == "2019")
season %>% head()

# get 2019 stadium data
game_grounds <- stadium %>% filter(season == "2019")

# get 2019 advanced statistics
adv_stats <- adv_team_stats %>% 
  filter(season == "2019")

#### Create Home Point Margin ----------------------------------

home_advantage <- season %>%
  mutate(home_margin = home_pts - away_pts) %>%
  group_by(home_team) %>%
  summarize(home_edge = mean(home_margin)) %>%
  arrange(desc(home_edge))

edge_plot <- home_advantage %>%
  mutate(
    home_team = fct_reorder(home_team, home_edge)
  ) %>% 
  ggplot(aes(x = home_team, y = home_edge)) +
  geom_hline(aes(yintercept = 0), size = 1.2) +
  geom_segment(aes(x = home_team, xend = home_team, y = 0, yend = home_edge)) +
  geom_point(size = 12, color = "pale green") +
  geom_text(aes(label = round(home_edge, 1)), color = "black") +
  labs(title = "NWSL 2019 Season Home Edge",
       subtitle = "NC won the Championship",
       caption = "Data: https://github.com/adror1/nwslR",
       x = "Home Team",
       y = "Home Edge") +
  coord_flip() +
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10),
        panel.background = element_rect(fill = "light grey"),
        panel.grid.major = element_line(color = "light grey"),
        plot.background = element_rect(fill = "light grey"))

edge_plot


attendance_plot <- game_grounds %>%
  mutate(
    team_id = fct_reorder(team_id,avg_attendance,mean)
  ) %>% 
  ggplot(aes(x = team_id, y = avg_attendance)) +
  geom_segment(aes(x = team_id, xend = team_id, y = 0, yend = avg_attendance)) +
  geom_label(aes(label = scales::comma(avg_attendance))) +
  labs(title = "NWSL 2019 Season Average Attendance",
       caption = "Data: https://github.com/adror1/nwslR",
       x = "Home Team",
       y = "Home Attendance") +
  coord_flip() +
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18),
        plot.caption = element_text(size = 10),
        panel.background = element_rect(fill = "light grey"),
        panel.grid.major = element_line(color = "light grey"),
        plot.background = element_rect(fill = "light grey"))

attendance_plot

shots_on_goal <- adv_stats %>% 
  select(
    team_id, shots = total_scoring_att, goals
  ) %>%
  group_by(team_id) %>%
  summarize(total_shots = sum(shots) + sum(goals)) %>%
  mutate(
    team_id = fct_reorder(team_id, total_shots)
  ) %>% 
  arrange(desc(team_id)) %>% 
  mutate(image = 
           case_when

shots_plot <- shots_on_goal %>%
  ggplot(aes(x = team_id, y = total_shots)) +
  geom_hline(aes(yintercept = 0), size = 1.2) +
  geom_segment(aes(x = team_id, xend = team_id, y = 0, yend = total_shots)) +
  geom_image(aes(image = image), size = .13) +
  geom_label(aes(label = scales::comma(total_shots))) +
  labs(title = "NWSL 2019 Season Shots on Goal",
       subtitle = "NC won the Championship",
       caption = "Data: https://github.com/adror1/nwslR",
       x = NULL,
       y = "Shots") +
  coord_flip() +
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10),
        panel.background = element_rect(fill = "light grey"),
        panel.grid.major = element_line(color = "light grey"),
        plot.background = element_rect(fill = "light grey"))

shots_plot

#### Patchwork the plots together ---------------------------------------
total_plot <- edge_plot / attendance_plot | shots_plot



