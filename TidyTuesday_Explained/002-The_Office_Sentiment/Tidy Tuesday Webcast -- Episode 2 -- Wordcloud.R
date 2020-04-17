### Tidy Tuesday Webcast -- Episode 2 -- Wordcloud

## Basic functions used for text analysis

library(tidyverse)
library(tidytext)

## Key functions to explain

# unnest_tokens()
# stop_words()
# get_sentiments()

## Create some text data
text <- data.frame(text= c("Ice cream is the best", "Gloomy days are not fun", "The red car is cool", "The water was too dirty", "I wish I had a donut"), stringsAsFactors = FALSE)
df <- tibble(line = 1:5, text = text)
df <- mutate(df, text = text$text)
df

## Get each word in each sentence as its own row with unnest_tokens()

df_unnest <- df %>% 
  unnest_tokens(word, text) %>%
  mutate(word = tolower(word))

df_unnest

## filter out stop words

unecessary_words <- stop_words

df_filtered <- df_unnest %>%
  anti_join(unecessary_words, by = "word")


## Get sentiments for each word

sentiment <- get_sentiments("bing")
sentiment %>% head()
table(sentiment$sentiment)

df_sentiment <- df_filtered %>%
  inner_join(sentiment)

df_sentiment








