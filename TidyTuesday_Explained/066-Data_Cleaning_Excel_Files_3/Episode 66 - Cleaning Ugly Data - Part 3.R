library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)


excel_file <- file.choose()

sheets <- excel_sheets(excel_file)


dat <- read_excel(excel_file,
                  sheet = sheets[1],
                  col_names = FALSE)

vital_rows <- 1:(which(is.na(dat[[1]]))[1]-1)
workout_rows <- (which(is.na(dat[[1]]))[1]+1):nrow(dat)

vitals <- setNames(
  data.frame(matrix(unlist(dat[vital_rows, 2]),nrow = 1)),
  unlist(dat[vital_rows,c(1)])) %>% 
  clean_names()

fitness <- dat[workout_rows, ]

workout <- setdiff(unique(unlist(fitness[1,])), NA)

fitness_colnames <- fitness[c(1,2),] %>% 
  t() %>% 
  data.frame() %>% 
  fill(1) %>% 
  unite(
    col = "colnames",
    everything(),
    sep = "_"
  ) %>% 
  pull(colnames)

fitness_dat <- fitness[-c(1,2),]
colnames(fitness_dat) <- fitness_colnames

workout_dat_long <- map_dfr(workout, function(w){
  fitness_dat %>% 
    select(starts_with(w)) %>% 
    mutate(
      idx = 1:nrow(.)
    ) %>% 
    pivot_longer(
      cols = starts_with(w),
    ) %>% 
    separate(
      name, sep = "_", into = c("workout","detail")
    ) %>% 
    pivot_wider(names_from = detail,values_from = value) %>% 
    select(-idx) %>% 
    filter(!is.na(Date))
})

player_data <- bind_cols(vitals, workout_dat_long)


all_player_data <- map_dfr(sheets, function(s){
  
  dat <- read_excel(excel_file,
                    sheet = s,
                    col_names = FALSE)
  
  vital_rows <- 1:(which(is.na(dat[[1]]))[1]-1)
  workout_rows <- (which(is.na(dat[[1]]))[1]+1):nrow(dat)
  
  vitals <- setNames(
    data.frame(matrix(unlist(dat[vital_rows, 2]),nrow = 1)),
    unlist(dat[vital_rows,c(1)])) %>% 
    clean_names()
  
  fitness <- dat[workout_rows, ]
  
  workout <- setdiff(unique(unlist(fitness[1,])), NA)
  
  fitness_colnames <- fitness[c(1,2),] %>% 
    t() %>% 
    data.frame() %>% 
    fill(1) %>% 
    unite(
      col = "colnames",
      everything(),
      sep = "_"
    ) %>% 
    pull(colnames)
  
  fitness_dat <- fitness[-c(1,2),]
  colnames(fitness_dat) <- fitness_colnames
  
  workout_dat_long <- map_dfr(workout, function(w){
    fitness_dat %>% 
      select(starts_with(w)) %>% 
      mutate(
        idx = 1:nrow(.)
      ) %>% 
      pivot_longer(
        cols = starts_with(w),
      ) %>% 
      separate(
        name, sep = "_", into = c("workout","detail")
      ) %>% 
      pivot_wider(names_from = detail,values_from = value) %>% 
      select(-idx) %>% 
      filter(!is.na(Date))
  })
  
  bind_cols(vitals, workout_dat_long)
  
})


#######################################################
############## New Stuff Starts Here ##################
#######################################################

all_player_data %>%
  head()

all_player_data %>%
  str()

## Cool function from janitor package: if all values were numeric
excel_numeric_to_date(as.numeric(as.character(all_player_data$Date)), date_system = "modern") 

## Fix Dates
training_data_cleaned <- all_player_data %>%
  mutate(training_date = 
           if_else(grepl("-", Date),
                  as.Date(Date, "%m-%d-%Y" ), 
                  as.Date(as.numeric(Date), origin = "1899-12-30"))
        ) %>%
  select(-Date) %>%
  as.tibble()

# convert to tibble so we can see the variable types at the top
training_data_cleaned

# clean the numeric values
training_data_cleaned <- training_data_cleaned %>%
  mutate(height_cm = as.numeric(as.character(height_cm)),
         weight_kg = as.numeric(as.character(weight_kg)),
         age_yrs = as.numeric(as.character(age_yrs)),
         Distance = as.numeric(as.character(Distance)))

training_data_cleaned %>% head()

## Need to handle duration
# Gotta get the values that are less than 10 (the scientific notation values) to be in minutes. Maybe something like the hm() function?
# The values are not externally accurate coming from the scientific notation.

training_data_cleaned <- training_data_cleaned %>%
  mutate(
    duration_num = parse_number(training_data_cleaned$Duration)
  ) %>% 
  mutate(
    duration_min = ifelse(
      duration_num < 1, 
      duration_num*(24*60), ## excel units for time is number fraction of 24 hours so to convert to minutes 24hrs/unit * 60 minutes/24 hrs
      duration_num)
  ) %>% 
  select(-Duration) %>% 
  relocate(
    Distance, .after = duration_min
  )
