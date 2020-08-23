## Tidy Tuesday Week 34 - Extinct Plants
## Data Source: IUCN Red List

#Load packages
library(tidyverse)
library(waffle) ## hrbrmstr/waffle
library(ggtext)
library(here)
library(patchwork)
library(extrafont)

#Import data
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')

#Inspect data
View(threats)

#Wrangle data
threat_data <- threats %>%
  filter(continent =="Africa", threatened == 1 ) %>%
  group_by(continent) %>%
  count(threat_type) %>%
  ungroup() %>% 
  mutate(pct = round(n/sum(n)*100,0)) %>% 
  mutate("threat type" = factor(case_when(
    threat_type == "Agriculture & Aquaculture" ~ "Agriculture & Aquaculture",
    threat_type == "Biological Resource Use" ~ "Biological Resource Use",
    threat_type == "Commercial Development" ~ "Commercial Development",
    threat_type == "Energy Production & Mining" ~ "Energy Production & Mining", 
    threat_type == "Natural System Modifications" ~ "Natural System Modifications",  
    TRUE ~ "Other")))

threat_data <-threat_data %>% 
  select(`threat type`, pct) %>% 
  group_by(`threat type`) %>% 
  summarise(pct = sum(pct)) %>% 
  mutate(`threat type` = fct_reorder(`threat type`, pct, sum, .desc=F))  

#Set theme
font_family <- 'Century Gothic'
background <- "#1E1E24"
text_colour <- "white"
axis_colour <- "white"
plot_colour <- "black"
theme_style <- theme(text = element_text(family = font_family),
                     rect = element_rect(fill = background),
                     plot.background = element_rect(fill = background, color = NA),
                     #plot.title = element_markdown(hjust = .1, size = 12, colour = text_colour),
                     plot.title = element_text(face = 'bold', hjust = .3, vjust = -2.3, size = 30, colour = text_colour),
                     plot.subtitle = element_text(size = 14, hjust = .1, vjust = -3.8, colour = text_colour),
                     plot.caption = element_text(size = 10, colour = text_colour),
                     panel.background = element_rect(fill = background, color = NA),
                     plot.margin = unit(rep(1, 4), "cm"),
                     panel.border = element_blank(),
                     panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     axis.title = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks = element_blank(),
                     axis.line = element_blank(),
                     legend.position="none",
                     strip.text.x = element_text(family =font_family, size = 10, colour= text_colour),
                     strip.background = element_blank())

theme_set(theme_classic() + theme_style)

cols <- c("#EF2D56", "#3A6EA5", "#F35B04","#EEE82C", "#35CE8D","#2BD9FE")

col_filter <- "#D7CDCC"
r <- 4

#Plot data
#All values
p1 <- ggplot(threat_data, aes(fill = `threat type`, values = pct))  +
  geom_waffle(n_rows = 10, size = .5, color=NA, 
              radius = unit(9, "pt"), height = 0.8, 
              width = 0.8, flip = T) +
  scale_fill_manual(values= alpha(cols, 1/3))+
  labs(title = "Extinct Plants in Africa",
       subtitle = "What were the causes of extinction?") +
  theme(plot.margin = unit(rep(1, 4), "cm"))

#Agriculture & Aquaculture
p5 <- ggplot(threat_data, aes(fill = `threat type`, values = pct))  +
  geom_waffle(n_rows = 10, size = .5, color=NA, 
              radius = unit(r, "pt"), height = 0.8, 
              width = 0.8, flip = T) +
  scale_fill_manual(values=c( alpha(col_filter, 1/3), 
                              alpha(col_filter, 1/3), 
                              alpha(col_filter, 1/3),
                              alpha(col_filter, 1/3), 
                              alpha(col_filter, 1/3),
                              "#2BD9FE")) +
  labs(title = "**Agriculture & Aquaculture**
  <span style='font-size:20pt;'>32%
       </span>")+ 
  theme(plot.title = element_markdown(hjust = .1, size = 10, colour = text_colour))

#Biological Resource Use
p6 <- ggplot(threat_data, aes(fill = `threat type`, values = pct))  +
  geom_waffle(n_rows = 10, size = .5, color=NA, 
              radius = unit(r, "pt"), height = 0.8, 
              width = 0.8, flip = T) +
  scale_fill_manual(values=c(alpha(col_filter, 1/3),
                             alpha(col_filter, 1/3), 
                             alpha(col_filter, 1/3),
                             alpha(col_filter, 1/3),
                             "#35CE8D",
                             alpha(col_filter, 1/3))) +
  labs(title = "**Biological Resource Use**
       <span style='font-size:20pt;'>23%
       </span>") +
  theme(plot.title = element_markdown(hjust = .1, size = 10, colour = text_colour))

#Natural System Modifications 
p7 <- ggplot(threat_data, aes(fill = `threat type`, values = pct))  +
  geom_waffle(n_rows = 10, size = .5, color=NA, 
              radius = unit(r, "pt"), height = 0.8, 
              width = 0.8, flip = T) +
  scale_fill_manual(values=c(alpha(col_filter, 1/3), 
                             alpha(col_filter, 1/3),
                             alpha(col_filter, 1/3),
                             "#EEE82C",
                             alpha(col_filter, 1/3),
                             alpha(col_filter, 1/3))) +
  labs(title = "**Natural System Modifications**
       <span style='font-size:20pt;'>16%
       </span>") +
  theme(plot.title = element_markdown(hjust = .1, size = 10, colour = text_colour))

#Other
p2 <- ggplot(threat_data, aes(fill = `threat type`, values = pct))  +
  geom_waffle(n_rows = 10, size = .5, color=NA, 
              radius = unit(r, "pt"), height = 0.8, 
              width = 0.8, flip = T) +
  scale_fill_manual(values=c(alpha(col_filter, 1/3), 
                             alpha(col_filter, 1/3),
                             "#F35B04",
                             alpha(col_filter, 1/3), 
                             alpha(col_filter, 1/3),
                             alpha(col_filter, 1/3))) +
  labs(title = "**Other**
       <span style='font-size:20pt;'>14%
       </span>") +
  theme(plot.title = element_markdown(hjust = .1, size = 10, colour = text_colour))

#Commercial Development
p3 <- ggplot(threat_data, aes(fill = `threat type`, values = pct))  +
  geom_waffle(n_rows = 10, size = .5, color=NA, 
              radius = unit(r, "pt"), height = 0.8, 
              width = 0.8, flip = T) +
  scale_fill_manual(values=c(alpha(col_filter, 1/3), 
                             "#3A6EA5",
                             alpha(col_filter, 1/3),
                             alpha(col_filter, 1/3), 
                             alpha(col_filter, 1/3), 
                             alpha(col_filter, 1/3))) +
  labs(title = "**Commercial Development**
       <span style='font-size:20pt;'>8%
       </span>") +
  theme(plot.title = element_markdown(hjust = .1, size = 10, colour = text_colour))

#Energy Production & Mining 
p4 <- ggplot(threat_data, aes(fill = `threat type`, values = pct))  +
  geom_waffle(n_rows = 10, size = .5, color=NA, 
              radius = unit(r, "pt"), height = 0.8, 
              width = 0.8, flip = T) +
  scale_fill_manual(values=c("#EF2D56",
                             alpha(col_filter, 1/3), 
                             alpha(col_filter, 1/3),
                             alpha(col_filter, 1/3), 
                             alpha(col_filter, 1/3), 
                             alpha(col_filter, 1/3))) +
  labs(title = "**Energy Production & Mining**
       <span style='font-size:20pt;'>7%
       </span>") +
  theme(plot.title = element_markdown(hjust = .1, size = 10, colour = text_colour))

final <- (
  ((p1 + plot_layout(widths = c(1, 2), nrow = 1)) |
     
          (p2 + p3 + p4 + plot_layout(nrow = 1)) / (p5 + p6 + p7 + plot_layout(nrow = 1))
   
           + plot_layout(guides = 'keep'))) + 
  
  plot_annotation(caption = "Visualisation: @JaredBraggins | Source: IUCN Red List")

#Export plot
ggsave("Extinct Plants.png", final, width = 17, height = 8, type = "cairo")