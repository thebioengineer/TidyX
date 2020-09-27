# description of script --------------------------------------------------------
# TidyTuesday  23rd September 2020week 39 Himalayan Climbing Expeditions
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-09-22/readme.md
#
# input data on Himalayan climbers and exepiditions
# outputs -A stacked bar chart of termination reasons. Looked at using colorfindr which I came across this week thanks to @AnnaHenschel.

# set up ------------------------------------------------------------------------
library(colorfindr)
library(tidyverse)

# load data --------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2020-09-22')
tuesdata <- tidytuesdayR::tt_load(2020, week = 39)

expeditions <- tuesdata$expeditions

# extract colours from image

## MINOR EDIT FROM TIDYX TO MAKE WORK FOR US

colour_palatte <- get_colors(here::here("TidyTuesday_Explained/029-Palettes_and_Random_Effects/week39_everest_image.jpg")) %>%
  make_palette(n = length(unique(expeditions$termination_reason)) + 5)

colour_palatte_filter <- colour_palatte[5:length(colour_palatte)]

# stacked bar chart ------------------------------------------------------------
termination_plot <- expeditions %>%
  mutate(round_year = plyr::round_any(year, 5)) %>%
  group_by(round_year, termination_reason) %>%
  count() %>%
  group_by(termination_reason) %>%
  mutate(termination_reason_count = sum(n)) %>%
  arrange(desc(termination_reason_count)) %>%
  ungroup() %>%
  mutate(termination_reason = factor(termination_reason, levels = unique(termination_reason), ordered = TRUE))%>%
  ggplot() +
  geom_bar(aes(x = round_year, y = n, group = termination_reason, fill = termination_reason), stat = "identity", position = "stack") +
  scale_fill_manual(values = colour_palatte_filter) +
  theme_classic() +
  theme(panel.background = element_rect(fill=colour_palatte[1], colour=colour_palatte[1]),
        plot.background = element_rect(fill=colour_palatte[1], color = colour_palatte[1]),
        axis.line = element_line(color=colour_palatte[19], size = 1),
        legend.background = element_rect(color=colour_palatte[1], fill= colour_palatte[1]),
        plot.title = element_text(hjust = 0.5, size = 24, family = "Calibri", color = "white"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Calibri", color = "white"),
        legend.text=element_text(color="white",size=12, family = "Calibri"),
        legend.title=element_text(color="white",size=12, family = "Calibri"),
        axis.text = element_text(color=colour_palatte[19],size=12, family = "Calibri"),
        axis.title= element_text(color=colour_palatte[19],size=12, family = "Calibri")) +
  
  xlab("Year") +
  ylab("Count") +
  labs(fill="Termination Reason") +
  ggtitle("Himalayan  expeditions termination reasons", subtitle = "Aggregated to 5 years")

ggsave("Week39_Himalayan.png",plot = termination_plot)

