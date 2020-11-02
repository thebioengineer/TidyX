library(tidyverse)
library(mapcan)
library(rgdal)
library(rmapshaper)
library(sp)
library(sf)
library(leaflet)
library(ggtext)

# This  week, I plotted a map of Nova Scotia, New Brunswick and Prince Edward Island's federal electoral ridings.
# I then merged tidytuesday's featured dataset to plot the position of wind turbines on the map.
# And to get things even more interesting, I also included Mildenberger et al.'s data, created from a survey of 
# Canadians on the topic of climate change. Here's the source: 
# Mildenberger, M., Howe, P.D., Lachapelle, E., Stokes, L.C., Marlon, J., and Gravelle, T. 
# “The distribution of climate change public opinion in Canada.” PLoS ONE 11(8) eo159774. 
# The data is available here on the University of Montreal website: https://www.umontreal.ca/climat/index.html
# There are a few opinion questions in the Mildenberger's data, including questions on the consequences of climate
# change and on policies to tackle climate change.

# wind turbines data
wind_turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv') %>% 
  filter(province_territory == "Nova Scotia" |
           province_territory == "New Brunswick" |
           province_territory == "Prince Edward Island") %>% 
  mutate(count = 1) %>% 
  dplyr::group_by()

class(wind_turbine$latitude)

# mildenberger's data
mildenberger <- read.csv(here::here("TidyTuesday_Explained/034-Wind_and_Maps/mildenberger-data.csv"), sep = ";",stringsAsFactors = TRUE) %>% # get the opinion data
  filter(!GEOID == "National") %>% # remove Canada obs
  mutate(GEOID = as.numeric(levels(GEOID)[as.numeric(GEOID)])) %>% 
  filter(!GEOID < 100) %>% # remove provinces
  filter(GEOID >= 11000 & GEOID <= 13999) # keep NS, NB and PEI only

# Canada shapefiles
boundaries <- readOGR(
  dsn = here::here("TidyTuesday_Explained/034-Wind_and_Maps/shapefiles/"),
  layer = "maritimes_boundaries")

# I did a custom export from this website: http://geo1.scholarsportal.info/#r/details/_lang=fr&_uri@=2907074758 (you can select a zone by drawing on the map and export only this location)

boundaries <- spTransform(boundaries,"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # important step to transform coordinates to decimal coordinates !

maritimes_plot <- ms_simplify(input = boundaries, # this was too long if I used the entire Canada shapefile, that's why I did a custom export to get the 3 provinces only
                              keep = 0.1, # keep only 10% of the original points
                              keep_shapes = TRUE) # make sure to keep all polygons

plot(maritimes_plot)

boundaries_df <- broom::tidy(x = maritimes_plot, # transform to a data frame
                             region = "FEDNUM") %>%  # identifies lat and long of each fed elec district
  filter(!id > 13999) %>%  #  keep NS, NB and PEI only (when I did the custom export I wasn't so careful and accidently got other regions)
  mutate(id = as.numeric(id))

head(boundaries_df)

# merge the map df with the opinion data
ridings_opinion <- left_join(boundaries_df, mildenberger,
                             by = c("id" = "GEOID")) # id and GEOID should be the same variable

# plot
ggplot(ridings_opinion, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = carbon_tax_bin, color = "white")) +
  coord_map() +
  theme_mapcan() +
  geom_point(data = wind_turbine, aes(y = latitude, x = longitude), col="midnightblue", alpha = .4, size = 7, shape = 8)+
  scico::scale_fill_scico(palette = "lajolla", 
                          name = "", 
                          limits = c(35,65),
                          guide = guide_colourbar(direction = "vertical")) +
  scale_color_manual(values = "white") +
  theme(panel.border = element_blank(),
        legend.position = c(0.12, 0.06),
        legend.background = element_rect(fill = "white"),
        legend.key.height  = unit(2, "line"),
        legend.text = element_text(colour = "midnightblue", size = 12),
        legend.title = element_text(colour = "midnightblue", size = 10),
        plot.title = element_text(colour = "midnightblue", size = 20, hjust = .5, face = "bold"),
        plot.subtitle = element_text(colour = "midnightblue", size = 17, hjust = .5),
        plot.caption = element_text(colour = "midnightblue", size = 10)) +
  guides(color = FALSE) +
  labs(title = "Location of Wind Turbines in New Brunswick, Nova Scotia and Prince Edward Island",
       subtitle = "Colours indicate the level of support for an increase in taxes on carbon based fuels.\nRegions are federal electoral districts.",
       caption = "Wind turbines data: Government of Canada\nOpinion data: Mildenberger et al., in collaboration with the CSEE\nand the CCOM project. The CSEE and CCOM project bear no responsibility\nfor the analyses or interpretations of the data presented here.\nSource: https://www.umontreal.ca/climat/index.html.\nData viz: @florencevdubois") 

# +
#   ggsave(filename = "plot-2020.10.27.png", width = 14, height = 10)

################################################################################
# you can also use the mapcan package to plot the Canada map
# like this 
# provinces = mapcan(boundaries = ridings, 
#                    type = standard) 

# but I could not get the coordinates from the df to match the format of the coordinates in the wind turbines df
# that's why I used raw shapefiles instead and converted the boundaries using this line of code:
# spTransform(boundaries,"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 