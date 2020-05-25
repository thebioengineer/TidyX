
library(tidyverse)
library(janitor)
library(ggtext)
library(lubridate)
library(RcppRoll)
library(extrafont)

#setting colors
sand <- '#f4e7c5'
blue <- "#041e42"
red <- "#c8102e"

vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

mk_tbl <- vb_matches %>%
  select(-matches("rank")) %>%
  rename(w_p1_name = w_player1, w_p2_name = w_player2,
         l_p1_name = l_player1, l_p2_name = l_player2) %>%
  mutate(id = row_number()) %>%
  mutate_at(vars(-c("id")), as.character) %>%
  pivot_longer(cols = matches("w_|l_"),
               names_to = "name",
               values_to = "vals") %>%
  separate(name, c("winner_loser", "player", "key"),
           sep = "_",
           extra = "merge",
           fill = "right") %>%
  pivot_wider(names_from = key,
              values_from = vals,
              names_repair = "unique") %>%
  filter(str_detect(name, "Misty|Kerri Walsh"))


mk_clean <- mk_tbl %>%
  mutate(date = as_date(date)) %>%
  filter(!is.na(tot_attacks)) %>% #this to retain only matches with more detailed stats (kills, attacks, digs, etc)
  pivot_longer(cols = starts_with("tot_"),
               names_to = "stat",
               values_to = "vals") %>%
  filter(!is.na(vals)) %>% #similar to the previous !is.na() filter but will get anything the previous step missed -- possibly redundant
  arrange(date) %>%
  mutate(mk_id = group_indices(., date, match_num),
         name = str_remove_all(name, " .*"),
         vals = as.numeric(vals),
         vals = if_else(name == "Kerri", -1*vals, vals)) %>%
  select(-c("player", "birthdate", "age", "hgt")) %>%
  group_by(name, stat) %>%
  mutate(rolled_val = roll_mean(vals, n = 10L, na.rm = FALSE, fill = NA_real_, align = "right")) %>% #calculating 10-match rolling averages to smooth out some variability and reduce # of gaps
  ungroup()

labels <- range(mk_clean$date) %>% as.character()
brks <- range(mk_clean$mk_id)
family <- "Bahnschrift"

mk_clean %>%
  filter(str_detect(stat, "kills|aces|blocks|digs")) %>%
  mutate(stat = str_replace_all(stat, c("tot_kills" = "Kills", "tot_aces" = "Aces", "tot_digs" = "Digs", "tot_blocks" = "Blocks"))) %>%
  ggplot(aes(x = mk_id, y = rolled_val, fill = name)) +
  geom_col() +
  facet_wrap(~stat, scales = "free_y", ncol = 1) +
  hrbrthemes::theme_ipsum() +
  labs(
    y = "",
    x = "",
    title = "America's Dynamic Duo",
    subtitle = "This plot shows match-level statistics for <span style='color:#041e42'>**Kerri Walsh Jennings**</span> and <span style='color:#c8102e'>**Misty May-Treanor**</span><br>from 2003 to 2016. Not surprisingly, Kerri tends to get more blocks, and<br>Misty tends to get more digs.",
    caption = "Data: BigTimeStats | Viz: Eric Ekholm (@ekholm_e)"
  ) +
  scale_fill_manual(
    values = c(blue, red)
  ) +
  scale_x_continuous(
    breaks = brks,
    labels = labels 
  ) +
  scale_y_continuous(
    n.breaks = 3,
    labels = abs #these are superfluous now bc I dropped y-axis text, but this formatted the number of breaks and made the text absolute value
  ) +
  theme(
    plot.background = element_rect(fill = sand),
    text = element_text(family = family),
    plot.title = element_markdown(hjust = .5,
                                  size = 28,
                                  family = family),
    plot.subtitle = element_markdown(hjust = .5,
                                     size = 11,
                                     family = family,
                                     face = "italic"),
    plot.title.position = "plot",
    strip.text = element_markdown(hjust = .5,
                                  size = 14,
                                  face = "bold",
                                  family = family),
    legend.position = "none",
    panel.spacing = unit(.5, "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank()
  )

ggsave(here::here("2020 - 21 - beach volleyball/may_walsh.png"), device = "png", height = 9)