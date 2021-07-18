
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)

library(writexl)
library(openxlsx)

output_dir <- here::here("TidyTuesday_Explained/068-Data_Cleaning_Excel_Files_4")

###### Get and Clean Data --------------------------------------------------------------
excel_file <- here::here("TidyTuesday_Explained/068-Data_Cleaning_Excel_Files_4/ugly_data.xlsx")

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

## Fix Dates
training_data_cleaned <- all_player_data %>%
  mutate(training_date = 
           if_else(grepl("-", Date),
                   as.Date(Date, "%m-%d-%Y" ), 
                   as.Date(as.numeric(Date), origin = "1899-12-30"))
  ) %>%
  select(-Date) %>%
  as.tibble()


# clean the numeric values
training_data_cleaned <- training_data_cleaned %>%
  mutate(height_cm = as.numeric(as.character(height_cm)),
         weight_kg = as.numeric(as.character(weight_kg)),
         age_yrs = as.numeric(as.character(age_yrs)),
         Distance = as.numeric(as.character(Distance)))

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
      duration_num*(24*60), 
      duration_num)
  ) %>% 
  select(-Duration) %>% 
  relocate(
    Distance, .after = duration_min
  )

###### Write data out to different source --------------------------------------------------

## write to csv
write.csv(
  x = training_data_cleaned,
  file = file.path(output_dir,"player_data.csv"),
  row.names = FALSE
  )

write_csv(x = training_data_cleaned, file = file.path(output_dir,"player_data_readr.csv"))


## writexl
## write to excel
write_xlsx(training_data_cleaned, file.path(output_dir,"player_data.xlsx"), col_names = TRUE)

## write to excel on separate sheets for each athlete
athlete_1 <- training_data_cleaned %>%
  filter(player_number == "Aaron Jacob")

athlete_2 <- training_data_cleaned %>%
  filter(player_number == "Peter Mills")
 
athlete_list <- list(athlete_1, athlete_2)

write_xlsx(athlete_list, file.path(output_dir,"player_data2.xlsx"), col_names = TRUE)

## That isn't great:
# 1) Doesn't provide unique names to each sheet
# 2) Would be a massive time killer if you had many athletes

## User split() to automatically split on the athlete's names
# This creates a named list that can then be used to uniquely name each tab
athlete_list2 <- training_data_cleaned %>%
  split(.$player_number)

write_xlsx(athlete_list2, file.path(output_dir,"player_data3.xlsx"), col_names = TRUE)


## openxlsx

### create workbook

wb <- createWorkbook()

hs1 <- createStyle(
  fgFill = "#DCE6F1", halign = "CENTER", textDecoration = "italic",
  border = "Bottom"
)

for( player in unique(training_data_cleaned$player_number)){
  
  addWorksheet(wb, sheetName = player)
  
  writeData(wb,
            player,
            athlete_list2[[player]],
            startCol = 2,
            startRow = 3,
            rowNames = FALSE,
            borders = "surrounding",
            borderColour = "black",
            headerStyle = hs1
            )
  
}

saveWorkbook(wb, file.path(output_dir,"player_data4.xlsx"),overwrite = TRUE)

