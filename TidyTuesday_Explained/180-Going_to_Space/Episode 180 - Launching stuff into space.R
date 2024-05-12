
library(tidyverse)
library(trelliscopejs)

tt_data <- tidytuesdayR::tt_load('2024-04-23')

## Preview Readme
tt_data

## Pull object
space_objects <- tt_data$outer_space_objects

space_objects
head(space_objects)

## Stratify by Categories in the data
space_objects %>%
  count(Entity, sort = TRUE)

space_objects %>%
  count(Code, sort = TRUE)

## How many things did we send to space?
space_objects %>% 
  group_by(Year) %>% 
  summarise(objs = sum(num_objects, na.rm = TRUE))

# Visualizing
space_objects %>%
  group_by(Year) %>% 
  summarise(n = sum(num_objects, na.rm = TRUE)) %>% 
  ggplot(aes(x = Year, y = n)) +
  geom_line()

## same same but different
space_objects %>%
  ggplot(aes(x = Year, y = num_objects)) +
  stat_summary(fun = sum, geom = "line")

## countries with a longest number of years
countries_of_interest <- space_objects %>%
  count(Entity, sort = TRUE) %>%
  filter(Entity != "World") %>%
  filter(n >= 39) %>%
  pull(Entity)

countries_of_interest

space_objects_small <- space_objects %>%
  filter(Entity %in% countries_of_interest)


space_objects_small %>%
  ggplot(aes(x = Year, y = num_objects)) +
  geom_line(aes(color = Entity)) +
  theme_classic() +
  labs(
    title = "Number of Objects Released into Space per Year",
    color = "Country",
    y = "Number of Objects"
    ) +
  theme(legend.position = "top")

## Facet to make many small plots
space_objects_small %>%
  ggplot(aes(x = Year, y = num_objects)) +
  geom_line(aes(color = Entity)) +
  facet_wrap(~Entity, scales = "free_y") +
  theme_classic() +
  labs(
    title = "Number of Objects Released into Space per Year",
    color = "Country",
    y = "Number of Objects") +
  theme(legend.position = "top")


space_objects %>%
  ggplot(aes(x = Year, y = num_objects)) +
  geom_line(aes(color = Entity)) +
  facet_trelliscope(
    ~Entity,  scales = "free_y"
    ) +
  theme_classic() +
  labs(title = "Number of Objects Released into Space per Year",
       color = "Country",
       y = "Number of Objects") +
  theme(legend.position = "top")
