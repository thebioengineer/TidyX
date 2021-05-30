
#####################################
#### Regex 101
####################################

## functions

## base R, will cover packages in a future episode

### searching
?grep ## index or values
?grepl ## true/false

## string manipulation
?sub ## replaces first instance
?gsub ## replaces all instances

## string extraction
?substr ## specific locations 

## string splitting on character
?strsplit #separating chars


## regex keywords

## ^ beginning of a string
## $ end of a string 
## \\d digits
## \\s white spaces
## \\w alphanumeric words
## . anything

## * match 0 - infinity times
## + match 1 to infinity times
## ? match 0-1 times

## [] match inside these brackets "exactly"
##    [A-Za-z] upper and lower case letters
##    [0-9] all numbers between 0-9
##    [Ss]hot (example- shot with caps or not)

## () used to identify groupings for gsub work


library(tidyverse)
library(lubridate)
library(rvest)

theme_set(theme_light())

#### scrape Miami Heat @ Milwaukee Bucks (1st Rd | Game 2 Eastern Conference Playoffs, 2021) ####
## html
espn_html <- read_html("https://www.espn.com/nba/playbyplay/_/gameId/401327878")

## scrape elements for time stamps, play details, and score
time_stamps <- espn_html %>%
  html_nodes("div") %>%
  html_nodes("#gp-quarter-1") %>%
  html_nodes(".time-stamp") %>%
  html_text() %>%
  as_tibble() %>%
  rename(time = value)

possession_details <- espn_html %>%
  html_nodes("div") %>%
  html_nodes("#gp-quarter-1") %>%
  html_nodes(".logo") %>%
  html_nodes("img") %>% 
  html_attr("src") %>%
  gsub(".*(mil|mia).*","\\1",.) %>% 
  as_tibble() %>%
  rename(possession = value)

play_details <- espn_html %>%
  html_nodes("div") %>%
  html_nodes("#gp-quarter-1") %>%
  html_nodes(".game-details") %>%
  html_text() %>%
  as_tibble() %>%
  rename(play_details = value)

score <- espn_html %>%
  html_nodes("div") %>%
  html_nodes("#gp-quarter-1") %>%
  html_nodes(".combined-score") %>%
  html_text() %>%
  as_tibble() %>%
  rename(score = value)

## bind data together
df <- bind_cols(time_stamps, possession_details, play_details, score)
df %>% head()

####################################
######## data cleaning #############
####################################

# Get the score in separate columns for the respective team
df %>%
  mutate(Heat = substring(score, first = 1, last = 1),
         Bucks = substring(score, first = 5, last = 5))

# WAIT! That wont work once they get to double digits -- need to parse out values before and after the hyphen

df <- df %>%
  mutate(
    Heat  = as.numeric( sub("-.*", replacement = "", score)), ## using sub
    Bucks = as.numeric(gsub(".*-", replacement = "", score)), ## using gsub same
    score_diff = Heat - Bucks)


# adjust the time from a character, into minutes

df <- df %>%
  mutate(
    quarter_time = ifelse(
      grepl(pattern = ":", time),
      minute(ms(time)) + second(ms(time)) / 60, 
      as.numeric(as.character(time)) / 60),
    quarter_time = 12 - quarter_time,
    team_col = if_else(score_diff <= 0, "Bucks", "Heat")
    )

## Giannis shots/misses

df <- df %>% 
  mutate(
    giannis_shots = grepl("^giannis.*(makes|misses)", play_details,ignore.case = TRUE),
    giannis_makes = grepl("^Giannis.*([Mm]ake)", play_details)
  )
