# TidyX Episode 128
#
# Advent of Code 2022 Day 1
# https://adventofcode.com/2022/day/1
# Question 1: Which elf has the _most_ calories
# Question 2: Total calories for top three elves

library(tidyverse)

elf_calories_set <- readLines(
  here::here("TidyTuesday_Explained/128-AOC_Day1/AOC_Day1_Data.txt")
  )

# Lists and Loop solution ----
n_elves <- sum(elf_calories_set == "") + 1
elf_level_calories <- vector("numeric", length = n_elves)
elf_idx <- 1
for(i in elf_calories_set){
  if(i == ""){
    elf_idx <- elf_idx + 1
  }else{
    elf_level_calories[[elf_idx]] <- elf_level_calories[[elf_idx]] + as.numeric(i)
  }
}
## answer #1 - most calories
max(elf_level_calories)
## answer #2 - top 3 elf calories sum
sum(sort(elf_level_calories, decreasing = TRUE)[1:3])

# Tidyverse Solution ----

elves <- read.delim(
  here::here("TidyTuesday_Explained/128-AOC_Day1/AOC_Day1_Data.txt"),
  header = FALSE, 
  blank.lines.skip = FALSE
  )

head(elves, 20)

elf_cals <- elves %>%
  rename(calories = V1) %>%
  mutate(
    new_elf = ifelse(is.na(calories), 1, NA)
  ) %>%
  group_by(new_elf) %>%
  mutate(
    elf_id = seq_along(new_elf) + 1,
    elf_id = ifelse(is.na(new_elf), NA, elf_id)
  ) %>%
  ungroup() %>%
  fill(elf_id, .direction = "up") %>%
  mutate(elf_id = elf_id - 1) %>%
  select(-new_elf) %>%
  group_by(elf_id) %>%
  summarize(total_cals = sum(calories, na.rm = TRUE)) %>%
  arrange(desc(total_cals))

## Answer 1
elf_cals[1,]

## Answer 2
elf_cals %>% 
  slice(1:3) %>% 
  summarize(
    sum(total_cals)
  )
