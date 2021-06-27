
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

all_player_data
