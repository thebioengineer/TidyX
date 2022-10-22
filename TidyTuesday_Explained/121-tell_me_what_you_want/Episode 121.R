
library(tidyverse)
library(readxl)

ref_file <- here::here(file.path(
  "TidyTuesday_Explained",
  "121-tell_me_what_you_want",
  "episode_121_data.xlsx"))

what_ive_got <- read_excel(ref_file, sheet = 1)
what_i_want <- read_excel(ref_file, sheet = 2)

# Original is what I've got (wide data frame)
# What I want is a data frame lin long format for each test over time

what_ive_got
what_i_want

### Looks like the main columns we need are:
# patient_id
# The test columns, which have the test values under them
# The post columns that have the post operation date
# We need to also create a "week
# The "what I want" example only has right sided test outputs. I'm assuming the view wants both right
# and left, so we will get both of them

## some of the test columns are not numeric but should be
glimpse(what_ive_got)

right_side <- what_ive_got %>%
  select(-type) %>%
  mutate(across(.cols = starts_with("test"),
                ~as.numeric(.x))) %>%
  pivot_longer(cols = -c(patient_id, surgical_limb, starts_with("post"))) %>%
  mutate(test_side = case_when(grepl("_rt", name) ~ "right",
                               grepl("_lt", name) ~ "left")) %>%
  filter(test_side == "right") %>%
  rename(right_test = value) %>%
  select(-test_side) %>%
  pivot_longer(cols = starts_with("post"),
               names_to = "post_op",
               values_to = "weeks_post_op") %>%
  group_by(patient_id) %>% 
  mutate(test_num = parse_number(name),
         week_num = parse_number(post_op),
         weeks_post_op = weeks_post_op - weeks_post_op[1]) %>%
  filter(test_num == week_num) %>%
  select(-week_num, -name)
  
  
left_side <- what_ive_got %>%
  select(-type) %>%
  mutate(across(.cols = starts_with("test"),
                ~as.numeric(.x))) %>%
  pivot_longer(cols = -c(patient_id, surgical_limb, starts_with("post"))) %>%
  mutate(test_side = case_when(grepl("_rt", name) ~ "right",
                               grepl("_lt", name) ~ "left")) %>%
  filter(test_side == "left") %>%
  rename(left_test = value) %>%
  select(-test_side) %>%
  pivot_longer(cols = starts_with("post"),
               names_to = "post_op",
               values_to = "weeks_post_op") %>%
  group_by(patient_id) %>% 
  mutate(test_num = parse_number(name),
         week_num = parse_number(post_op),
         weeks_post_op = weeks_post_op - weeks_post_op[1]) %>%
  filter(test_num == week_num) %>%
  select(-week_num, -name)


## join together

new_df <- right_side %>%
  full_join(left_side, by = c("patient_id", "surgical_limb", "post_op", "weeks_post_op", "test_num")) %>%
  relocate(right_test, .before = left_test)


## conduct statistical test
new_df %>%
  lm(right_test ~ weeks_post_op, data = .) %>%
  summary()

### Using advanced pivoting -------

other_approach <- what_ive_got %>%
  select(-type) %>%
  mutate(across(.cols = starts_with("test"),
                ~as.numeric(.x))) %>%
  rename_with(
    ~str_remove(.x, "postop"),
    starts_with("postop")
  ) %>% 
  pivot_longer(
    cols = starts_with("test"),
    names_prefix = "test",
    names_pattern = "(\\d+)_*(.*)",
    names_to = c("test_num", "record_type"),
    values_to = "value"
  ) %>% 
  pivot_wider(
    names_from = record_type,
    values_from = value
  ) %>% 
  rename(
    right_test = peak_rt,
    left_test = peak_lt
  ) %>%
  group_by(patient_id) %>% 
  mutate(
    weeks_post_op = weeks - min(weeks)
  ) %>% 
  ungroup() %>% 
  arrange(
    patient_id, weeks_post_op
  )


other_approach %>%
  lm(right_test ~ weeks_post_op, data = .) %>%
  summary()
