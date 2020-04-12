library(tidyverse)
library(gganimate)

tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')

tdf_winningest <- tdf_winners %>%
  count(winner_team) %>%
  top_n(10) %>%
  pull(winner_team)

tdf_cum <- tdf_winners %>%
  count(start_date, winner_team) %>%
  complete(start_date, winner_team, fill = list(n = 0)) %>%
  arrange(start_date) %>%
  group_by(winner_team) %>%
  mutate(cum_wins = cumsum(n)) %>%
  ungroup() %>%
  filter(winner_team %in% tdf_winningest) %>%
  mutate(winner_team = str_replace(winner_team, "–", "-"))

#Make an unanimated plot to test the idea
ggplot(data = tdf_cum) +
  geom_line(aes(x = start_date, y = cum_wins, colour = winner_team), size = 2) + 
  scale_y_continuous(breaks = seq(0, max(tdf_cum$cum_wins), len = 5)) + 
  theme_minimal()

#Set some colours that match the jersey colours of each team. 
cols_1 <- c("Alcyon-Dunlop" = "blue", 
            "Automoto-Hutchinson" = "purple", 
            "Banesto" = "red", 
            "Belgium" = "red", 
            "France" = "red", 
            "Italy" = "darkgreen", 
            "La Sportive" = "yellow", 
            "Molteni" = "orange", 
            "Peugeot-Wolber" = "black", 
            "Team Sky" = "turquoise", 
            "U.S. Postal Service" = "darkblue")

#Based on the code here: https://github.com/thomasp85/gganimate/wiki/Temperature-time-series

anim <- ggplot(tdf_cum, aes(start_date, cum_wins, colour = winner_team)) + 
  geom_line(size = 2, show.legend = FALSE) + 
  geom_point(size = 5, show.legend = FALSE, shape = 21, fill = "white") +
  geom_text(aes(x = start_date, label = winner_team), hjust = 0, show.legend = FALSE, position = position_nudge(y = .5)) + 
  scale_colour_manual(values = cols_1, aesthetics = c("colour", "fill")) + 
  scale_y_continuous(breaks = seq(0, max(tdf_cum$cum_wins), len = 5)) + 
  transition_reveal(start_date) + 
  coord_cartesian(clip = 'off') + 
  labs(title = "Winningest teams in Tour de France history", 
       x = "Tour date",
       y = "Cumulative wins", 
       colour = "Team") + 
  theme_minimal()+ 
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))

anim_save("tdf.gif", anim)