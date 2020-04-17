# https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html

###############################
# Setup
###############################
library(tidyr)
library(stringr)
library(schrute)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(reshape2)

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')
schrute <- schrute::theoffice

# Take a look at the data
dplyr::glimpse(schrute)
dplyr::glimpse(office_ratings)

###############################
# Prep language data
###############################
# Tokenize dialogue 
token.schrute = schrute %>%
  tidytext::unnest_tokens(word, text)
dplyr::glimpse(token.schrute) #570,450 observations

# Remove stop words (e.g. words that are not useful for analyses like "the", "a")
stop_words = tidytext::stop_words
tidy.token.schrute = token.schrute %>%
  dplyr::anti_join(stop_words, by = 'word') # return all rows from x where there are not matching values in y, keeping just columns from x.

# Most common words
tidy.token.schrute %>% # 169,835 observations
  dplyr::count(word, sort = TRUE)

tidy.token.schrute %>%
  dplyr::count(word, sort = TRUE) %>%
  dplyr::filter(n > 400) %>%
  dplyr::mutate(word = stats::reorder(word, n)) %>%
  ggplot2::ggplot(ggplot2::aes(word, n)) +
  ggplot2::geom_col() +
  ggplot2::xlab(NULL) +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal()


###############################
# Sentiments
###############################
sentiments=get_sentiments("bing") # Codes words as positive or negative (Bing Liu). NA for neutral. 
dplyr::glimpse(sentiments)
unique(sentiments$sentiment)

# Sentiment by season
schrute.sentiment = tidy.token.schrute %>%
  dplyr::left_join(sentiments) %>%
  dplyr::count(episode_name, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%# fill missing values w/ 0
  mutate(sentimentc = positive - negative) %>% # pos value means more words had positive connatation than neg
  dplyr::select(episode_name,sentimentc,negative,positive,neutral=`<NA>`)

###############################
# Most common positive and negative words 
###############################
bing_word_counts = tidy.token.schrute %>%
  inner_join(sentiments%>%filter(sentiment=='positive'|sentiment=='negative')) %>%
  count(word, sentiment, sort = TRUE)

p1 =bing_word_counts %>%
  filter(n>150) %>%
  mutate(n = ifelse(sentiment =='negative', -n, n)) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col() +
  coord_flip() + 
  labs(y='Contribution to sentiment analysis', x = 'Word') +
  theme_bw() + 
  theme(legend.position='none', axis.text.x=element_text(size=12), axis.title.x = element_text(size=14, face = 'bold'),
        axis.text.y=element_text(size=12), axis.title.y=element_text(size=14, face='bold'))

# word cloud
pdf('/Users/sifre002/Box/sifre002/7_Rscripts/TidyTuesday/20200318-The-Office/comparison_cloud.pdf', width =4, height =4)
bing_word_counts %>%
  acast(word~sentiment, value.var='n', fill = 0) %>%
  comparison.cloud(colors=c("#F8766D", "#00BFC4"), 
                   max.words=100)
dev.off()  

########################################
# Is there a relationship between word sentiment and episode rating?
# Not really
########################################
office_ratings = office_ratings %>%
  dplyr::select(season, episode, episode_name = title, imdb_rating)

sent.rating = schrute.sentiment %>%
  inner_join(office_ratings, by ='episode_name') %>%
  mutate(season = as.factor(season))

ggplot(data = sent.rating, aes(x=sentimentc, y = imdb_rating, color = season)) + 
  geom_point()

sent.rating %>%
  group_by(season) %>%
  summarize(rating_ave = mean(imdb_rating),
            sentiment_ave = mean(sentimentc)) %>%  
  ggplot(data = ., aes(x=sentiment_ave, y = rating_ave, color = season)) + 
  geom_point()

########################################
# Descriptive fig of pos & neg characters
########################################
glimpse(tidy.token.schrute)
# sentiment by character
schrute.sentiment = tidy.token.schrute %>%
  dplyr::left_join(sentiments) %>%
  dplyr::count(episode_name, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%# fill missing values w/ 0
  mutate(sentimentc = positive - negative) %>% # pos value means more words had positive connatation than neg
  dplyr::select(episode_name,sentimentc,negative,positive,neutral=`<NA>`)

char.sentiment= tidy.token.schrute %>%
  inner_join(sentiments, by = 'word') %>% 
  count(character, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentimentc = positive-negative)

p2 = char.sentiment %>% 
  filter(negative +positive >300) %>%
  mutate(sent_dummy = ifelse(sentimentc<0, 'More Negative', 'More Positive')) %>%
  mutate(character = reorder(character, sentimentc)) %>%
  ggplot(aes(character, sentimentc, fill = sent_dummy)) +
  geom_col() +
  coord_flip() + 
  labs(y='Emotional Charge of Dialogue \n (Positive - Negative Words)', x = 'Character') +
  theme_bw() + 
  theme(legend.position='none', axis.text.x=element_text(size=12), axis.title.x = element_text(size=14, face = 'bold'),
        axis.text.y=element_text(size=12), axis.title.y=element_text(size=14, face='bold'))


ggsave(filename = '/Users/sifre002/Box/sifre002/7_Rscripts/TidyTuesday/20200318-The-Office/word_bar.pdf', plot = p1, width=4.5,height=4)
ggsave(filename = '/Users/sifre002/Box/sifre002/7_Rscripts/TidyTuesday/20200318-The-Office/characters_sentiment.pdf', plot = p2, width=9,height=6)

########################################
# list of techniques/functions that I learned 
########################################
# 1. dplyr::anti_join, dplyr::left_join
# 2. Re-ordering "character" variables for plotting
# 3. count (with multiple argument)
# 4. Coding negative valuence for ggplot2
