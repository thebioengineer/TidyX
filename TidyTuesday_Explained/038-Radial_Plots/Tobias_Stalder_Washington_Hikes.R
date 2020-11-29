#tidytuesday
#data on washington hiking routes scraped by tidyx
#script by tobias stalder
#nov 2020
#tobias-stalder.netlify.app



# load libraries ----------------------------------------------------------
library(tidyverse)
library(stringr)
library(prismatic) #package to view colors in console output
library(Cairo) #for export type
library(scales)
library(extrafont) #additional fonts

loadfonts(device = "win")




# load data ---------------------------------------------------------------

hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))



# data manipulation -------------------------------------------------------

#1) extract 'region' (entry before "--" of location data)
word(hike_data$location,1,sep = " -- ") -> hike_data$region
hike_data$region <- as.factor(hike_data$region)

#2)extract miles
as.numeric(sapply(strsplit(hike_data$length, " "), "[[", 1)) -> hike_data$length_num

#3) transform everything necessary to numeric type
hike_data$gain <- as.numeric(hike_data$gain)
hike_data$highpoint <- as.numeric(hike_data$highpoint)
as.numeric(row.names(hike_data))-> hike_data$trackNr

hike_data <- data.frame(hike_data) #convert to dataframe (could also be done anywhere before)



#4) Calculate cummulative length, mean(gain)
hike_data %>%
  group_by(region) %>%
  summarise(sum_length = sum(length_num),
            mean_gain = mean(gain),
  ) %>%
  mutate(mean_gain = round(mean_gain, digits = 0))-> summary_stats

#5) cummulative nr of tracks per region.
hike_data %>%
  group_by(region) %>%
  count() -> trackNrs #we can just count the number of entries here since 1 row = 1 track.

#6) join nr of tracks back to summary_stats
left_join(summary_stats, trackNrs, by = "region") -> summary_all


# data visualisation ------------------------------------------------------

#color choice
prismatic::color(c( "#6C5B7B" ,"#C06C84","#F67280","#F8B195"))

#ggplot2
ggplot(summary_all) +
  
  #make custom panel grid
  geom_hline(yintercept = 0, color = "lightgrey") +
  geom_hline(yintercept = 1000, color = "lightgrey") +
  geom_hline(yintercept = 2000, color = "lightgrey") +
  geom_hline(yintercept = 3000, color = "lightgrey") +
  
  geom_col(aes(
    x = reorder(str_wrap(region,5),sum_length), #is numeric
    y = sum_length, #is numeric
    fill = n), #is a factor
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9) +
  
  #new fill and legend title for number of tracks per region
  scale_fill_gradientn("Amount of Tracks",
                       colours = c( "#6C5B7B","#C06C84","#F67280","#F8B195"))+
  
  #mean gain per region
  geom_point(aes(x = reorder(str_wrap(region,5),sum_length),
                 y = mean_gain),
             size = 3,
             color = "gray12")+
  
  #lollipop shaft for mean gain per region
  geom_segment(aes(
    x = reorder(str_wrap(region,5),sum_length),
    y = 0,
    xend = reorder(str_wrap(region,5),sum_length),
    yend = 3000),
    linetype = "dashed",
    color = "gray12") +
  
  
  #annotate the bars and the lollipops so the reader understands the scaling
  annotate(x = 11, y = 1300,
           label = "Mean Elevation Gain\n[FASL]",
           geom = "text",
           angle = -67.5,
           color = "gray12",
           size = 2.5,
           family = "Bell MT")+
  
  annotate(x = 11, y = 3150,
           label = "Cummulative Length [FT]",
           geom = "text",
           angle = 23,
           color = "gray12",
           size = 2.5,
           family = "Bell MT")+
  
  #annotate custom scale inside plot
  annotate(x = 11.7, y =1100, label = "1000", geom = "text", color = "gray12", family = "Bell MT")+
  annotate(x = 11.7, y =2100, label = "2000", geom = "text", color = "gray12", family = "Bell MT")+
  annotate(x = 11.7, y =3100, label = "3000", geom = "text", color = "gray12", family = "Bell MT")+
  
  #scale y axis so bars don't start in the center
  scale_y_continuous(limits = c(-1500, 3500),
                     expand = c(0,0),
                     breaks = c(0, 1000, 2000, 3000))+
  
  #add title, subtitle & caption
  labs(title = "\nHiking Locations in Washington",
       subtitle = paste("\nThis Visualisation shows the cummulative length of tracks,",
                        "the amount of tracks and the mean gain in elevation per location.\n",
                        "If you are an experienced hiker, you might want to go",
                        "to the North Cascades since there are a lot of tracks,",
                        "higher elevations and total length to overcome.",
                        sep = "\n"),
       caption = "\n\nData Visualisation by Tobias Stalder\ntobias-stalder.netlify.app\nSource: TidyX Crew (Ellis Hughes, Patrick Ward)\nLink to Data: github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-11-24/readme.md") +
  
  #transform to polar coordinate system
  coord_polar() +
  
  #theming
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "gray12",
                                   size = 12),
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(color = "gray12",
                            family = "Bell MT"),
        plot.title = element_text(face = "bold",
                                  size = 25,
                                  hjust = 0.05),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.05),
        plot.caption = element_text(size = 10,
                                    hjust = .5))+
  
  guides(fill = guide_colorsteps(barwidth = 15,
                                 barheight = .5, 
                                 title.position = "top",
                                 title.hjust = .5))

#+
  # #save
  # ggsave(path = r"(C:\Users\tobia\Desktop\ownprojects\hiking)",
  #        filename = "dataviz_hiking.png",
  #        width = 21, #din A4 width
  #        height = 29.7, #din A4 height
  #        units = "cm",
  #        type = "cairo-png",
  #        dpi = 300)