
### load packages ----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)

### read in data -----------------------------------------------------------
dat <- read_excel(here::here("TidyTuesday_Explained/067-Data_Cleaning_User_Submission/ugly_covid_data.xlsx"))
dat %>% head(20)

### get the rows of interest -----------------------------------------------
deaths_data <- dat %>% 
  slice(5, 17:80)

### specify column names ----------------------------------------------------
col_names <- deaths_data %>% slice(1)  
colnames(deaths_data) <- col_names
colnames(deaths_data)[1:2] <- c("group", "age_group")

deaths_data <- deaths_data[-1, ] %>%
  filter(!age_group %in% c("Deaths by age group", "Females 2", "Males 2"))


### add a new column for the "group" being measured -------------------------
deaths_data <- deaths_data  %>%
  mutate(group = rep(c("all", "males", "females"), each = 20))


### get rid of NA columns that are place holders for future measurement -----
deaths_data <- deaths_data %>% 
  select_if(~ !all(is.na(.)))

### Convert numeric date to actual date -------------------------------------
deaths_data_cleaned <- deaths_data %>%
  pivot_longer(cols = which(!is.na(as.numeric(colnames(.))))) %>%
  mutate(name = as.Date(as.numeric(name), origin = "1899-12-30")) %>%
  pivot_wider(names_from = name)

deaths_data_cleaned

### Create a simple plot ---------------------------------------------------
deaths_data_long <- deaths_data %>%
  pivot_longer(cols = 3:ncol(.)) %>%
  mutate(name = as.Date(as.numeric(name), origin = "1899-12-30"))

deaths_data_long %>%
  filter(age_group == "60-64") %>%
  ggplot(aes(x = name, y = value, color = group)) +
  geom_line()

