### loading libraries and setting theme

library(tidyverse)
library(extrafont)
theme_set(theme_minimal())

### loading data, doing a bit of cleaning, removed cafe furniture so the facet_wrap fits nicely

ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv') %>%
  select(-X1) %>%
  mutate(category = case_when(
    category == "Sideboards, buffets & console tables" ~ "Sideboards",
    category == "Chests of drawers & drawer units" ~ "Drawer Units",
    TRUE ~ category)) %>%
  filter(category != "Café furniture")

### the plot

ikea %>%
  ggplot(aes(x = width, y = height)) +
  geom_rect(aes(xmin = 0, ymin = 0, xmax = width, ymax = height), alpha = 0.1, fill = "#FFDA1A", colour = "#0051ba", size = 1.2) +
  scale_x_continuous(limits = c(0,200)) +
  scale_y_continuous(limits = c(0,200)) +
  facet_wrap(~category, ncol = 4) +
  labs(title = "The many shapes of IKEA furniture",
       x = "Width (cm)",
       y = "Height (cm)",
       caption = "Source | https://www.kaggle.com/ahmedkallam/ikea-sa-furniture-web-scraping\nTwitter | @henrywrover2\nGithub | henrywrover\n 3rd November 2020") +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5,
                                  size = 36,
                                  face = "bold",
                                  family = "Gadugi"),
        text = element_text(family = "Gadugi"),
        plot.caption = element_text(size = 6),
        plot.margin = unit(c(1,1,1,1), "cm")) + 
ggsave(filename = "ikea.png", type = "cairo-png", height = 10, width = 10, dpi = 180)
