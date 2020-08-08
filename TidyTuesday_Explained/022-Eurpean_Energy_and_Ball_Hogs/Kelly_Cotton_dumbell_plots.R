# Setup----
library(tidyverse)
library(tidytuesdayR)
library(ggalt)
library(scales)
library(patchwork)

theme_set(theme_light())

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 32)

energy_types <- tuesdata$energy_types
country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv')

# Plot I abandoned
# energy_types %>%
#   pivot_longer(
#     cols = c(5:7),
#     names_to = "year",
#     values_to = "amount"
#   ) %>%
#   ggplot(aes(x = year,y = amount, fill = type)) +
#   geom_col() +
#   facet_grid(vars(type), vars(country)) + 
#   coord_flip()

top_countries <- country_totals %>%
  subset(type == "Total net production") %>%
  slice_max(order_by = `2018`, n = 5)

energy_types$type <- factor(energy_types$type, c("Conventional thermal", "Nuclear", "Hydro", "Wind", "Solar", "Geothermal", "Other"))
energy_types$country_name <- replace_na(energy_types$country_name, "United Kingdom") 

# Create theme for plots
dumbell_theme <-   theme(plot.title = element_text(hjust=0.5, face="bold"),
                         plot.background=element_rect(fill="#f7f7f7"),
                         panel.background=element_rect(fill="#f7f7f7"),
                         panel.grid.minor=element_blank(),
                         panel.grid.major.y=element_blank(),
                         panel.grid.major.x=element_blank(),
                         axis.ticks=element_blank(),
                         panel.border=element_blank())

# Plots----
p1 <- energy_types %>%
  subset((country %in% top_countries$country) & (level == "Level 1") & (type != "Conventional thermal") & (type != "Other") ) %>%
  ggplot(aes(x = `2016`, xend = `2018`, y = country_name, group = country_name)) +
  geom_dumbbell(colour_x ="#882255",
                size_x = 2.5,
                size = 0.75, 
                color = "#888888",
                colour_xend ="#6699CC",
                size_xend = 2.5
  ) + 
  scale_x_continuous(labels = comma) +
  facet_wrap(~type, ncol = 1) +
  labs(
    x = element_blank(), 
    y = element_blank()
  ) +
  theme(strip.background = element_rect(fill = "#84E296")) +
  dumbell_theme

p2 <- energy_types %>%
  subset((country %in% top_countries$country) & (level == "Level 1") & (type == "Conventional thermal") ) %>%
  ggplot(aes(x = `2016`, xend = `2018`, y = country, group = country)) +
  geom_dumbbell(colour_x ="#882255",
                size_x = 2.5,
                size = 0.75, 
                color = "#888888",
                colour_xend ="#6699CC",
                size_xend = 2.5
  ) + 
  geom_point(x = 300000, y = 4, color = "#882255", size = 2.5, inherit.aes = FALSE) +
  annotate("text", label = "2016", x = 330000, y = 4) +
  geom_point(x = 300000, y = 3.8, color = "#6699CC", size = 2.5, inherit.aes = FALSE) +
  annotate("text", label = "2018", x = 330000, y = 3.8) +
  scale_x_continuous(labels = comma) +
  facet_wrap(~type, ncol = 1) +
  labs(
    x = element_blank(), 
    y = element_blank()
  ) +
  theme(strip.background = element_rect(fill = "#3C362A"))+
  dumbell_theme

# Put the plots together
p1 + p2 + plot_annotation(
  title = "Top 5 Largest European Energy Producers", 
  subtitle =  "Change in amount produced between 2016 and 2018", 
  caption = "Created by @kllycttn, Data from Eurostat, #TidyTuesday"
)
