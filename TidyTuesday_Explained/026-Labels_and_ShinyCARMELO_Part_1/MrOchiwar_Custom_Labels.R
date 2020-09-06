library(tidytuesdayR)
library(tidyverse)
library(patchwork)
library(scales)
library(ggthemes)
library(viridis)
library(ggtext)
library(extrafont)
library(hrbrthemes)
library(lubridate)
library(forcats)

tuesdata <- tidytuesdayR::tt_load('2020-09-01')

key_crop_yields <- tuesdata$key_crop_yields

arable_land <- tuesdata$arable_land_pin


shortener <- function(string) {
  abb <- str_extract(string, '[^_]+')
  abb <- toupper(abb)
}


nigeria_crop_yeilds <- key_crop_yields %>% 
  janitor::clean_names() %>% 
  filter(entity == "Nigeria")

p1 <- nigeria_crop_yeilds %>% 
  select(-beans_tonnes_per_hectare,-peas_tonnes_per_hectare,
         -barley_tonnes_per_hectare,-bananas_tonnes_per_hectare, -code,
         -entity) %>% 
  pivot_longer(cols = !year,
               names_to = "crop_productivity",
               values_to = "tonnes_per_hectre") %>% 
  mutate(year = year %>% as.factor(),
         crop_productivity = crop_productivity %>% as.factor()
         )


p1 %>% ggplot(aes(x = year, y= tonnes_per_hectre, group = 1)) +
  geom_line() + 
  geom_point(size = 1) +
  
  labs(x = "",
       y = "Tonnes Per Hectre",
       title = "Nigeria Key Crop Yields",
       caption = "Source:https://ourworldindata.org/crop-yields\nGraphics by @MrOrchiwar") +
  
  scale_x_discrete (breaks = seq(1961, 2018, 9)) +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(size = 10)) +
  facet_wrap( ~ crop_productivity, scales = "free",
              labeller = labeller(crop_productivity = shortener)
  )
