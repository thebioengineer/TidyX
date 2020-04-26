# Week 17 of TIdy Tuesday
# https://github.com/rfordatascience/tidytuesday

# clear working directory
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(lubridate)
library(plyr)
library(scales)
library(zoo)

# bring in data
gdpr_violations <- read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')

# Calendar Heatmap
# Violations by date - do violations or certain violations occur at higher frequencies certain times of the year?
# resource: http://margintale.blogspot.in/2012/04/ggplot2-time-series-heatmaps.html
gdpr_violations$date<-gdpr_violations$date%>%
  parse_date(format="%m/%d/%Y",na=character(),locale=default_locale(),trim_ws = TRUE) # format date

# create specific date columns for plotting
gdpr_violations<-gdpr_violations%>% # create new columns extracting portions of the date
  mutate(Year=year(date))%>% # extract the year as.numeric
  mutate(Week=week(date))%>% # extract the week as.numeric
  mutate(Monthf=months(date))%>% # extract month name as.character
  mutate(Weekdayf=weekdays(date))%>% # extract weekday name as.character
  mutate(yearmonth=as.yearmon(date))%>% # extract the Month and Year as.numeric
  mutate(yearmonthf=factor(yearmonth))%>% # parse yearmonth to as.factor
  ddply(.(yearmonthf), transform, monthweek=1+Week-min(Week))  # compute the week number of each month, called monthweek

# filter data for plotting
gdpr_violations<-gdpr_violations %>%
  mutate(price.div=price/1000)%>% # divided price by $1K euros for simpler figure legend
  filter(Year >= 2018)%>%  # filter read years
  filter(price.div > 0) # filter for actual fines paid

# select for only the necessary columns
gdpr_violations <- gdpr_violations%>%
  select("Year","Monthf","monthweek","Weekdayf","price.div")

# View data
head(gdpr_violations) # sample view of data frame
#View(gdpr_violations) # or view full data frame

# Plot
ggplot(gdpr_violations, aes(monthweek, Weekdayf, fill = price.div)) + 
  geom_tile(colour = "white") + # background color
  facet_grid(Year~Monthf) + # lay out panels in a grid by year and month
  scale_fill_gradient(low="red", high="green", trans="log") + # color scales and log transform to see a broader spectrum of fine amounts
  labs(x="Week of Month",
       y="",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Violation Fines Through the Year in Europe", 
       fill="Fines divided by 1K Euros")+ # legend heading
  ggsave("Data/Week17/Calendar_Heatmap_Fines.png") # save plot as png
