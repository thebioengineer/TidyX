# I first load the data and the packages I am using.

library(tidytuesdayR)
library(tidyverse)
library(ggrepel)
library(grid)
library(png)
library(scales)

tuesdata <- tt_load('2021-01-05')
transit_cost <- tuesdata$transit_cost

# modified for TidyX
country_codes <- read_csv(
  here::here("TidyTuesday_Explained/044-Steps_and_Plotly_Maps/",
  "country_codes.csv")
)


# I merge the data with a reference dataset of country codes as I am not always sure what these 2-letter codes refer to.

transit_cost <- transit_cost %>%
  mutate(Code = if_else(country == "UK", "GB", country)) %>%
  select(-country) %>% 
  left_join(country_codes, by  = "Code") %>% 
  rename(country = Name,
         code = Code,
         id = e) %>% 
  select(id, country, code, city, everything())

# Now, here is the skeleton of what I am looking for. The country seems to be the most natural candidate to connect the observations.

transit_cost %>%
  mutate(start_year = as.numeric(str_extract(start_year, "[0-9]+"))) %>%
  filter(start_year > 1950) %>% 
  ggplot(aes(x = start_year, y = cost_km_millions)) +
  geom_line(aes(group = country)) +
  geom_point()

# A few things stand out: first, the plot is very crowded, I need to remove some observations. Second, a log scale on the y axis would be more appropriate. I’ll use log10 for ease of interpretation. Third, connecting the dots directly doesn’t look like a subway map at all. I should use geom_step instead.
# 
# I correct these below by excluding only including a few of the most represented countries (I exclude the PRC because it has 20 times as many observations as the second most represented country and would crowd the map).

transit_cost %>%
  mutate(country = fct_lump(country, 10),
         cost_km_millions = round(cost_km_millions, -1),
         start_year = as.numeric(str_extract(start_year, "[0-9]+"))) %>%
  filter(!country %in% c("Other", "China", "Taiwan, Province of China", "India", "Turkey"),
         rr == 0) %>%
  ggplot(aes(x = start_year, y = cost_km_millions)) +
  geom_step(aes(group = country)) +
  geom_point() +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  scale_y_log10(labels = unit_format(unit = "M", big.mark = ","))

# This is starting to look like something. I now customize the plot to make it look like subway lines on a map, and add the names of the “stops”.

transit_cost %>%
  mutate(country = fct_lump(country, 10),
         cost_km_millions = round(cost_km_millions, -1),
         start_year = as.numeric(str_extract(start_year, "[0-9]+"))) %>%
  filter(!country %in% c("Other", "China", "Taiwan, Province of China", "India", "Turkey"),
         rr == 0) %>% 
  ggplot(aes(x = start_year, y = cost_km_millions)) +
  geom_step(aes(color = country, group = country), size = 2) +
  geom_point(shape = 21, size = 1.5, fill = "white") +
  geom_text_repel(aes(label = paste(line, " (", city, ")", sep = "")), size = 2) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  scale_y_log10(labels = unit_format(unit = "M", big.mark = ",")) +
  labs(y = "", x = "", color = "Country", title = "Transit Costs Project: Evolution of the cost per km (in USD) per country and year of construction") +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.background = element_blank(),
    legend.position = c(0.07, 0.82),
    legend.background = element_blank(),
    legend.key = element_blank(),
    text = element_text(family = "sans")
  )


# The only thing left is to add the map in the background, using annotation_custom().

#edited for TidyX
generated_map <- readPNG(
  here::here("TidyTuesday_Explained/044-Steps_and_Plotly_Maps/","map.png")
  )

transit_cost %>%
  mutate(country = fct_lump(country, 10),
         cost_km_millions = round(cost_km_millions, -1),
         start_year = as.numeric(str_extract(start_year, "[0-9]+"))) %>%
  filter(!country %in% c("Other", "China", "Taiwan, Province of China", "India", "Turkey"),
         rr == 0) %>%
  ggplot(aes(x = start_year, y = cost_km_millions)) +
  annotation_custom(rasterGrob(generated_map,
                               width = unit(1,"npc"),
                               height = unit(1,"npc")),
                    -Inf, Inf, -Inf, Inf) +
  geom_step(aes(color = country, group = country), size = 2) +
  geom_point(shape = 21, size = 1.5, fill = "white") +
  geom_text_repel(aes(label = paste(line, " (", city, ")", sep = "")), size = 2) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  scale_y_log10(labels = unit_format(unit = "M", big.mark = ",")) +
  labs(y = "", x = "", color = "Country", title = "Transit Costs Project: Evolution of the cost per km (in USD) per country and year of construction") +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.background = element_blank(),
    legend.position = c(0.07, 0.82),
    legend.background = element_blank(),
    legend.key = element_blank(),
    text = element_text(family = "sans")
  )