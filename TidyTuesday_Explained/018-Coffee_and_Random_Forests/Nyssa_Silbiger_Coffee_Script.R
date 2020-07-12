## Tidy Tuesday Coffee ####
## 7/7/2020 ######
## By Nyssa Silbiger ####

##### load library #####
library(tidyverse)
library(ggimage)
library(hrbrthemes)
library(RColorBrewer)

### load data ####
coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

### let's make a lollipop plot of sweetness by country of origin
## and try to make the points beans!

# get mean scores for the middle line
mean_data<-coffee_ratings %>%
  summarise_at(.vars = c("sweetness", "aroma", "flavor", "body", "aftertaste"), .funs = c("mean")) %>%
  pivot_longer(cols = sweetness:aftertaste, names_to = "category", values_to = "endpoints")


### get the average by country 
coffee_data<-coffee_ratings %>%
  group_by(country_of_origin)%>%
  summarise_at(.vars = c("sweetness", "aroma", "flavor", "body", "aftertaste"), .funs = c("mean")) %>%
  pivot_longer(cols = sweetness:aftertaste, names_to = "category", values_to = "values") %>%
  left_join(mean_data) %>% # join with the data for the means
  mutate(pos_neg = ifelse(endpoints - values>0, "pos", "neg")) %>% # add values for colors of positive and negatives
  drop_na(country_of_origin) # drop the unknown country

# coffee bean image
coffee<-"https://www.unlvfreepress.com/wp-content/uploads/2018/01/Coffee-Bean-1.png"

# make the lollipop plot
coffee_data %>%
  ggplot(aes(x = country_of_origin, y = values))+
  geom_point()+
  coord_flip()+
  geom_hline(aes(yintercept = endpoints), lwd = 1.5, color = "grey")+
  geom_segment(aes(x=country_of_origin, xend=country_of_origin, y=endpoints, yend=values, color = pos_neg), lwd = 1.2)+
  geom_image(aes(image=coffee), size=.05)+ # this add the coffee bean image
  ylab("score")+
  xlab("country")+
  scale_color_brewer(palette = "BrBG")+
  facet_wrap(~category)+
  theme_ft_rc()+
  theme(legend.position = "none")+
  ggsave(filename = "Coffee_070720/coffeeplot.png", width = 14, height = 10)