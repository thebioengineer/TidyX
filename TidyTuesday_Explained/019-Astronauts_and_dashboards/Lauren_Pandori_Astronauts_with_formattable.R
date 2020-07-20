################################################################
# Title: Astronauts Tidy Tuesday 
# Purpose: Make a cool table 
# Created by: L Pandori
# Created: 07/14/2020
# Last edited: 07/14/2020
################################################################

##### Package upload #####
library(tidyverse) # it is Tuesday
library(formattable) # nice tables

# asked for reactable tutorial suggestions on twitter - thank you to folks who replied 

##### Data upload and tidy #####

# upload
astro <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

# tidy 
# goal: get stats by nationality to put in table
# percent of total astronauts from each country
# year of first mission
# most common occupation

# Function to calculate mode of occupation (Stack Overflow - https://stackoverflow.com/questions/46845848/statistical-mode-of-a-categorical-variable-in-r-using-mlv)

calculate_mode <- function(x) {
  # get list of unique values in x
  uniqx <- unique(x)
  # find entry that occurs most frequently
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# function to make 1st letter of occupation capitalized 
# https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html

CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

# apply across occupations 
astro$occupation2 <- sapply(astro$occupation, CapStr)


# calculate summary data for table 
astro <- astro %>%
  # summarize across nationality of astronauts
  group_by(nationality) %>%
  # calculate summary data for table
  summarise(
    # get percent of astronauts rounded to 2 decimal places
    'Percent of Astronauts' = round((length(nationality)/1277), digits = 2),
    # get year of first mission
    'First Mission' = min(year_of_mission),
    # get most common occupation using calculate_mode f(n)
    'Most Common Occupation' = calculate_mode(occupation2))

# clean up summary table
astro <- astro %>% 
  arrange(desc(`Percent of Astronauts`)) %>% 
  filter(`Percent of Astronauts` >= 0.01) %>%
  rename('Nationality' = 'nationality') %>%
  mutate(`Percent of Astronauts` = `Percent of Astronauts`*100)

##### Use formattable to make a Nice Table #####
# tutorial thank you: https://www.littlemissdata.com/blog/prettytables

formattable(astro,
            # R justify first column, center all others
            align = c('l','c','l','l'),
            # column list for custom formatting
            list(
              # create bar for % of astronauts
              `Percent of Astronauts` = color_bar('cadetblue')))


