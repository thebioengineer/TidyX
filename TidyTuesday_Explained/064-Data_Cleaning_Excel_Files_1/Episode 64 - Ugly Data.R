
library(tidyverse)
library(lubridate)
library(readxl)

excel_file <- file.choose()

## load ugly data -----------------------------------------------------
dat_1 <- read_excel(excel_file, sheet = 1, skip = 4, col_types = c("date", "text", "numeric", "date", "numeric"))
dat_1

vitals_1 <- read_excel(excel_file, sheet = 1)

dat_2 <- read_excel(excel_file, sheet = 2, skip = 4, col_types = c("date", "text", "numeric", "date", "numeric"))
dat_2

vitals_2 <- read_excel(excel_file, sheet = 2)

## Dat_1 -------------------------------------------------------------

jog_1 <- dat_1 %>%
  rename(date = 1,
         duration = 2,
         distance = 3) %>%
  select(date, duration, distance) %>%
  mutate(exercise = "jogging",
         duration = as.numeric(duration),
         date = substr(date, start = 1, stop = 10)) %>%
  relocate(exercise, .before = "date") %>%
  na.omit()
  
swim_1 <- dat_1 %>%
  rename(date = 4,
         duration = 5) %>%
  select(date, duration) %>%
  mutate(exercise = "swim",
         date = substr(date, start = 1, stop = 10)) %>%
  relocate(exercise, .before = "date") %>%
  na.omit()


exercise_1 <- jog_1 %>%
  bind_rows(swim_1)

hgt <- vitals_1 %>% select(2) %>% slice(1) %>% unlist(., use.names = FALSE)
wgt <- vitals_1 %>% select(2) %>% slice(2) %>% unlist(., use.names = FALSE)
age <- vitals_1 %>% select(2) %>% slice(3) %>% unlist(., use.names = FALSE)

name <- rep(colnames(vitals_1)[2], nrow(exercise_1))
hgt <- rep(hgt, nrow(exercise_1))
wgt <- rep(wgt, nrow(exercise_1))
age <- rep(age, nrow(exercise_1))

vitals <- data.frame(name, hgt, wgt, age)

athlete_1 <- bind_cols(vitals, exercise_1)
athlete_1


## Dat_2 -------------------------------------------------------------
jog_2 <- dat_2 %>%
  rename(date = 1,
         duration = 2,
         distance = 3) %>%
  select(date, duration, distance) %>%
  mutate(exercise = "jogging",
         duration = as.numeric(duration),
         date = substr(date, start = 1, stop = 10)) %>%
  relocate(exercise, .before = "date") %>%
  na.omit()

swim_2 <- dat_2 %>%
  rename(date = 4,
         duration = 5) %>%
  select(date, duration) %>%
  mutate(exercise = "weight_lifting",
         duration = as.numeric(duration),
         date = substr(date, start = 1, stop = 10)) %>%
  relocate(exercise, .before = "date") %>%
  na.omit()

exercise_2 <- jog_2 %>%
  bind_rows(swim_2) %>%
  mutate(distance = as.numeric(distance))

hgt <- vitals_2 %>% select(2) %>% slice(1) %>% unlist(., use.names = FALSE)
wgt <- vitals_2 %>% select(2) %>% slice(2) %>% unlist(., use.names = FALSE)
age <- vitals_2 %>% select(2) %>% slice(3) %>% unlist(., use.names = FALSE)

name <- rep(colnames(vitals_2)[2], nrow(exercise_2))
hgt <- rep(hgt, nrow(exercise_2))
wgt <- rep(wgt, nrow(exercise_2))
age <- rep(age, nrow(exercise_2))

vitals <- data.frame(name, hgt, wgt, age)

athlete_2 <- bind_cols(vitals, exercise_2)
athlete_2

### full data -------------------------------------------------------------

df_training <- athlete_1 %>%
  bind_rows(athlete_2)

df_training

######################
###### Function ######
######################

n_sheets <- 2

total_data <- data.frame(
  name = character(),
  hgt = character(),
  wgt = character(),
  age = character(),
  exercise = character(),
  date = character(),
  duration = numeric(),
  distance = numeric()
)

for(i in 1:n_sheets){
  
  # get data from sheets
  training_dat <- read_excel(excel_file, sheet = i, skip = 4, col_types = c("date", "text", "numeric", "date", "numeric"))
  athlete_id <- read_excel(excel_file, sheet = i)
  
  # get jogging data
  jog <- training_dat %>%
    rename(date = 1,
           duration = 2,
           distance = 3) %>%
    select(date, duration, distance) %>%
    mutate(exercise = colnames(training_dat)[1],
           duration = as.numeric(duration),
           date = substr(date, start = 1, stop = 10)) %>%
    relocate(exercise, .before = "date") %>%
    na.omit()
  
  swim <- training_dat %>%
    rename(date = 4,
           duration = 5) %>%
    select(date, duration) %>%
    mutate(exercise = colnames(training_dat)[4],
           date = substr(date, start = 1, stop = 10)) %>%
    relocate(exercise, .before = "date") %>%
    na.omit()
  
  # put togteher all of the exercise data
  exercise <- jog %>%
    bind_rows(swim)
  
  # get the athlete id data
  hgt <- athlete_id %>% select(2) %>% slice(1) %>% unlist(., use.names = FALSE)
  wgt <- athlete_id %>% select(2) %>% slice(2) %>% unlist(., use.names = FALSE)
  age <- athlete_id %>% select(2) %>% slice(3) %>% unlist(., use.names = FALSE)
  
  name <- rep(colnames(athlete_id)[2], nrow(exercise))
  hgt <- rep(hgt, nrow(exercise))
  wgt <- rep(wgt, nrow(exercise))
  age <- rep(age, nrow(exercise))
  
  vitals <- data.frame(name, hgt, wgt, age)
  
  total_data <- rbind(total_data,bind_cols(vitals, exercise))
  
}


total_data
