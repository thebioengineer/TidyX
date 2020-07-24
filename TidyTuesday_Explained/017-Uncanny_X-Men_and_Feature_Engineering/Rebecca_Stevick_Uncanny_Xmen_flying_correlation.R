# Claremont Run of X-men
# TidyTuesday 2020 week 27
# Rebecca Stevick updated 6/30/2020

# Load libraries
library(tidyverse)
library(ggpubr)
library(hrbrthemes)

# Load data - use the characters data set. Many others available in the Claremont Run package: remotes::install_github("malcolmbarrett/claremontrun")
characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/characters.csv')

# let's look at when characters flew!
flying<-characters %>%
  # remove rows that aren't flying
  drop_na(flying_with_another_character) %>%
  group_by(character) %>%
  # count up number of flights per character
  count(name="fly") %>%
  arrange(desc(fly))

# and when they hugged!
hugging<-characters %>%
  # remove actions that aren't hugging
  drop_na(hugging_with_which_character) %>%
  group_by(character) %>%
  # count up number of hugs per character
  count(name="hug") %>%
  arrange(desc(hug))

# join hugging and flying counts per character
flyinghugging<-full_join(flying, hugging) %>% 
  # add in a 0 if they didn't hug or fly
  replace_na(list("hug"=0, "fly"=0))

# plotting time
ggplot(flyinghugging, aes(x=fly, y=hug))+
  # add diamond points for each character
  geom_point(shape=18, size=6, 
             # highlight characters who hug more than 20 times
             color=ifelse(flyinghugging$hug > 20, "royalblue3", "gray60"))+
  # add regression line
  geom_smooth(method="lm", color="red3", fill="red2", alpha=0.1)+
  # show regression equation, R2 and p-value
  stat_regline_equation(label.x = 24, label.y=12, color="red3") + stat_cor(label.x=24, label.y=11, color="red3")+
  # add label and arrow for Storm
  geom_label(aes(x = 24, y = 21, label = "Storm \n(Ororo Munroe)"),alpha=0, hjust = 1, vjust = 0.5, lineheight = 0.8, colour = "gray70", label.size = NA, size = 4)+
  geom_curve(aes(x = 24, y = 21, xend = 27, yend = 22), colour = "gray70", size=0.5, curvature = -0.2, arrow = arrow(length = unit(0.03, "npc")))+
  # add label and arrow for Shadowcat
  geom_label(aes(x = 5, y = 21, label = "Ariel/Sprite/Shadowcat \n(Kitty Pryde)"), alpha=0, hjust = 0, vjust = 0.5, lineheight = 0.8, colour = "gray70", label.size = NA, size = 4)+
  geom_curve(aes(x = 5, y = 21, xend = 3, yend = 23), colour = "gray70", size=0.5, curvature = -0.3,arrow = arrow(length = unit(0.03, "npc")))+
  # change the overall theme
  theme_ft_rc()+
  # edit the text sizes
  theme(plot.title=element_text(size=20),plot.subtitle = element_text(color="royalblue", face="bold"), axis.title.x = element_text(size=14, face="bold"), axis.title.y = element_text(size=14, face="bold"), plot.caption = element_text(size=12))+
  # add those labels
  labs(x="Number of issues where character flew", y="Number of issues where \ncharacter hugged someone", 
       title="X-Men characters who fly more also hug more",
       subtitle="Frequent huggers are highlighted in blue.",
       caption = "Source: @ClarementRun | Plot by @rjstevick for #TidyTuesday")


# Saving -----------------------------
ggsave("ClaremontRun_plot.png", width = 12, height = 6.5, dpi=400)