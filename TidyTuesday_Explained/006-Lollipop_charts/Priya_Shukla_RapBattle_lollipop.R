library(tidyverse)
library(hrbrthemes)
library(patchwork)

#wordcloud packages
library(wordcloud2)
library(tm)
library(webshot)
# webshot::install_phantomjs(force = TRUE)
library(htmlwidgets)


# Get the Data

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
# PLEASE NOTE TO USE 2020 DATA YOU NEED TO USE tidytuesdayR version from GitHub

# Either ISO-8601 date or year/week works!

# Install via devtools::install_github("thebioengineer/tidytuesdayR")

as.data.frame(polls)
glimpse(polls)

## only look at data in 1989 - the year I was born
polls89 <- polls %>% 
  filter (year == 1989)

glimpse(polls89)

plot.89 <- ggplot(data = polls89, aes(x = artist, y = rank)) +
  geom_segment( aes(x= artist, xend= artist, y= 0, yend = 5), color="grey") +
  geom_point( color="skyblue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank()
  ) +
  labs(title="Top Rap Artist rankings in 1989", subtitle = "The Year I was Born", x = " ", y = "Ranking") +
  theme_ft_rc()

print(plot.89)

## only look at data in 2005 - the year I was 16
polls05 <- polls %>% 
  filter (year == 2005)

glimpse(polls05)

plot.05 <- ggplot(data = polls05, aes(x = artist, y = rank)) +
  geom_segment( aes(x= artist, xend= artist, y= 0, yend = rank), color="grey") +
  geom_point( color="springgreen", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(), 
    axis.title.x = element_blank()
  ) +
  labs(title="Top Rap Artist rankings in 2005", subtitle = "The Year I Turned 16", x = " ", y = "Ranking") +
  theme_ft_rc()

print(plot.05)


## most recent year
max(polls$year) #2019

#only look at data in 2019
polls19 <- polls %>% 
  filter (year == 2019)

glimpse(polls19)

plot.19 <- ggplot(data = polls19, aes(x = artist, y = rank)) +
  geom_segment( aes(x= artist, xend= artist, y= 0, yend = rank), color="grey") +
  geom_point( color="salmon", size=4) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank()
  ) +
  labs(title="Top Rap Artist rankings in 2019", subtitle = "The Year I Turned 30", x = " ", y = "Ranking") +
  theme_ft_rc()

print(plot.19)

plot.89 / plot.05 / plot.19


### Rapper Word Cloud!

# #Create a vector containing only the text
tb <- table(polls$artist)

WC <- wordcloud2(data = tb, size = 2.3, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1, color='random-light', backgroundColor="black")

WC

# save it in html & and in png
saveWidget(WC,"fig_output/RapWordcloud.html", selfcontained = F)

webshot("fig_output/RapWordcloud.png", delay =5, vwidth = 480, vheight=480)



### Another couple wordclouds!

library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)

# Create a corpus  
docs <- Corpus(VectorSource(tb))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) #%>%
#tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234) # for reproducibility 
wordcloud(names(tb),as.numeric(tb), scale=c(8,.3),min.freq=1,max.words=175, random.order=FALSE, rot.per=0.35, family = "Roboto", font = 2, colors=brewer.pal(8, "RdBu"))
