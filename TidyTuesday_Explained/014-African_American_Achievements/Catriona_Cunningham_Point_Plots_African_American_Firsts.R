#Packages
library(tidyverse)
library(dplyr)
library(rvest)
library(janitor)
library(plotly)
library(extrafont)
# font_import()
loadfonts(device="win")

#Importing data
firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')

#Creating colour scheme
Mycolours <- c("dodgerblue3", "darkorange2", "hotpink2", "magenta3", "red", "skyblue1", "gold", "grey57")


#Plotting number of accomplishments per year, grouped by sex
firsts %>% 
  ggplot(aes(year, category, group=gender, color=gender, person=person)) +
  geom_point(size=2) + #increasing point size
  theme_bw() + #removing grey background
  theme(legend.title = element_blank(), #removing legend title
        plot.title = element_text(hjust = 0.5), #centering graph title
        text = element_text(family = "Franklin Gothic Book", size=16),     #changing font
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removing grid lines
  scale_color_manual(values=Mycolours, labels=c("Male", "Female")) +   #changing legend labels and colour scheme
  xlab("Year") +    #Changing axis labels
  ylab("") +
  scale_x_continuous(breaks=seq(1700,2019,50)) +  #changing intervals on x axis
  ggtitle("African American Accomplishments")

ggsave("African American Accomplishments.png")

#Generating interactive plot with hover text using ggplotly
fig <- ggplotly(tooltip = c("x", "person"))
fig

#ggplotly didn't use my custom legend labels in scale_color_manual
#my hacky solution was to replace all
mfirsts <- mutate_if(firsts, 
                     is.character, 
                     str_replace_all, pattern = "Female African American Firsts", replacement = "Female")
m2firsts <- mutate_if(mfirsts, 
                      is.character, 
                      str_replace_all, pattern = "African-American Firsts", replacement = "Male")


m2firsts %>% 
  ggplot(aes(year, category, group=gender, color=gender, person=person, accomplishment=accomplishment)) +
  geom_point(size=2) + #increasing point size
  theme_bw() + #removing grey background
  theme(legend.title = element_blank(), #removing legend title
        plot.title = element_text(hjust = 0.5), #centering graph title
        text = element_text(family = "Franklin Gothic Book", size=16),     #changing font
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removing grid lines
  scale_color_manual(values=Mycolours) +   #changing legend text and colour scheme
  xlab("Year") +    #Changing axis labels
  ylab("") +
  scale_x_continuous(breaks=seq(1700,2019,50)) +  #changing intervals on x axis
  ggtitle("African American Accomplishments")


fig2 <- ggplotly(tooltip = c("x", "person"))
fig2




#Making dot plot of number of accomplishments per year, grouped by category
firsts %>% 
  ggplot(aes(x=year, group=category, fill=category, person=person)) +
  geom_dotplot(aes(y=..count..),
               stackdir = "up", stackratio = 1,stackgroups = TRUE,  #stacking dots
               binpositions="all",binwidth=5,
               dotsize = 1) + 
  theme_bw() + #removing grey background
  theme(legend.title = element_blank(), #removing legend title
        plot.title = element_text(hjust = 0.5), #centering graph title
        text = element_text(family = "Franklin Gothic Book", size=16),     #changing font
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #removing grid lines
        axis.text.y=element_blank(), axis.ticks.y=element_blank()) + #removing y axis units and ticks
  scale_fill_manual(values=Mycolours) +   #changing legend text and colour scheme
  xlab("Year") +    #Changing axis labels
  ylab("") +
  scale_x_continuous(breaks=seq(1700,2019,50)) +  #changing intervals on x axis
  ggtitle("African American Accomplishments")

#Saving plot
ggsave("African American Accomplishments Dotplot.png", dpi=600, height=5.5, width=10)