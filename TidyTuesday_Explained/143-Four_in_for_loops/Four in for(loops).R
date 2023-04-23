
# TidyX Episode 143 - Four in for(loops)

library(tidyverse)

## Set up - Simulate data
set.seed(698)

dat <- tibble(
  athlete = rep(LETTERS[1:9], each = 2),
  team = rep(c("team 1", "team 2", "team 3"), each = 6),
  var = rep(c("var1", "var2"), times = 9)
  ) %>%
  mutate(val = case_when(
    var == "var1" ~ rnorm(n = 18, mean = 20, sd = 3),
    var == "var2" ~ rnorm(n = 18, mean = 25, sd = 2) + rnorm(n = 1, mean = 0, sd = 1)
  ))

dat

## Put the two vars side-by-side
wide_dat <- dat %>%
  filter(var == "var1") %>%
  pivot_wider(names_from = "var",
              values_from = "val") %>%
  rename(var1 = `var1`) %>%
  full_join(

    t2 <- dat %>%
      filter(var == "var2") %>%
      pivot_wider(names_from = "var",
                  values_from = "val") %>%
      rename(var2 = `var2`),
    
    by = c("athlete", "team")
  )

wide_dat


## 1) iterating over a generated numeric sequence
teams <- unique(wide_dat$team)
cor_coef <- data.frame(team = teams, COR = NA)

## what about a case where 
for(i in 1:length(teams)){
  
  ## get team
  t <- wide_dat %>% filter(team == teams[i])
  
  ## store the correlation coefficient
  cor_coef[i, 2] <- cor(t$var1, t$var2)
  
}

cor_coef


## 2) iterating over a generated numeric sequence - safely
teams <- unique(wide_dat$team)
cor_coef <- data.frame(team = teams, COR = NA)

## what about a case where 
for(i in seq_len(length(teams))){
  
  ## get team
  t <- wide_dat %>% filter(team == teams[i])
  
  ## store the correlation coefficient
  cor_coef[i, 2] <- cor(t$var1, t$var2)
  
}

cor_coef

## 3) iterating over a generated numeric sequence - safely & simpler
teams <- unique(wide_dat$team)
cor_coef <- data.frame(team = teams, COR = NA)

for(i in seq_along(teams)){
  
  ## get team
  t <- wide_dat %>% filter(team == teams[i])
  
  ## store the correlation coefficient
  cor_coef[i, 2] <- cor(t$var1, t$var2)
  
}

cor_coef


## 4) iterating over a vector of values
teams <- unique(wide_dat$team)
cor_coef <- data.frame(team = teams, COR = NA, row.names = teams)

for(team_name in teams){
  
  print(team_name)
  
  ## get team
  t <- wide_dat %>% filter(team == team_name)
  
  ## store the correlation coefficient
  cor_coef[team_name , 2] <- cor(t$var1, t$var2)
  
}

cor_coef

## 5) iterating over a list 

teams <- unique(wide_dat$team)
cor_coef <- data.frame(team = teams, COR = NA, row.names = teams)

wide_dat_list <- wide_dat %>% 
  group_split(team)

for(team_dat in wide_dat_list){
  
  ## store the correlation coefficient
  cor_coef[unique(team_dat$team), 2] <- cor(team_dat$var1, team_dat$var2)

}

cor_coef

wide_dat_list <- wide_dat %>% 
  group_split(team)

for(i in seq_along(wide_dat_list)){
  
  team_dat <- wide_dat_list[[i]]
  
  ## store the correlation coefficient
  cor_coef[i, 2] <- cor(team_dat$var1, team_dat$var2)
  
}

