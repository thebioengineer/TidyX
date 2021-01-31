

### load packages --------------------------------------
library(tidyverse)
library(rvest)
library(gt)

theme_set(theme_light())

### get nhl game data -----------------------------------


url2020 <- read_html("https://www.hockey-reference.com/leagues/NHL_2020_games.html")

nhl <- url2020 %>% 
  html_table(fill = T) %>% 
  .[1] %>% 
  as.data.frame() %>%
  select(date = Date,
         visitor = Visitor,
         v_goals = 3,
         home = Home,
         h_goals = 5) %>%
  mutate(h_win = ifelse(h_goals > v_goals, 1, 0),
         a_win = ifelse(v_goals < h_goals, 1, 0),
         tie = ifelse(h_goals == v_goals, 1, 0))

nhl %>% head()
nhl %>% tail()


### data prep ------------------------------------------

# vector of team names

teams <- unique(nhl$visitor)
length(teams)

# create a rankings vector
tm_ranks <- rep(1, length(teams) + 1)
names(tm_ranks) <- c(teams, "hie")
tm_ranks


### functions & optimization------------------------------------------
## game forecast function
game_forecast <- function(home_rank, visitor_rank, home_ice_edge){
  
  1 / (1 + exp(-(home_ice_edge + home_rank - visitor_rank)))
  
}

## likelihood function
log_likelihood <- function(tm_ranks){
  
  nhl %>%
    mutate(prediction = game_forecast(home_rank = tm_ranks[home],
                                       visitor_rank = tm_ranks[visitor],
                                       home_ice_edge = tm_ranks[32])) %>%
    mutate(game_result = ifelse(h_win == 1, prediction, (1-prediction))) %>% 
    summarize(sum_log_like = sum(log(game_result))) %>% 
    pull(sum_log_like)
}


## optimize ratings
tm_optim <- optim(
  par = tm_ranks, 
  fn = log_likelihood,
  method = "BFGS",
  control = list(fnscale = -1))

## get ratings
updated_ranks <- tm_optim$par
updated_ranks

## plot
updated_ranks_df <- stack(updated_ranks) %>%
  rename(team_rank = values,
         team = ind)

updated_ranks_df %>%
  filter(
    team != "hie"
  ) %>% 
  ggplot(aes(x = team_rank, y = reorder(team, team_rank))) +
  geom_col(width = 0.1) +
  geom_point(size = 3)


## forecast a game
game_forecast(home_rank = updated_ranks_df[updated_ranks_df$team == "Boston Bruins", 1],
              visitor_rank = updated_ranks_df[updated_ranks_df$team == "Ottawa Senators", 1],
              home_ice_edge = updated_ranks[updated_ranks_df$team == "hie"])

## fun summary table!

url_team_summary_2020 <- read_html("https://www.hockey-reference.com/leagues/NHL_2020.html")

nhl_summary_2020 <- url_team_summary_2020 %>% 
  html_table(fill = T) %>% 
  .[1:2] %>% 
  bind_rows() %>% 
  as.data.frame() %>%
  select(tm = ...1,
         GP,
         W,
         L,
         GF,
         GA) %>%
  mutate(
    division = case_when(
      tm %in% c("Atlantic Division", "Metropolitan Division", "Central Division", "Pacific Division") ~ tm,
      TRUE ~ NA_character_
    )
  ) %>% 
  fill(
    division
  ) %>% 
  filter(!(tm %in% c("Atlantic Division", "Metropolitan Division", "Central Division", "Pacific Division")))  %>%
  mutate(across(.cols = c(GP:GA), ~as.numeric(.))) %>% 
  mutate(
    playoff_team = grepl("*",tm, fixed = TRUE),
    tm = gsub("\\*.*", "", tm)
  ) %>% 
  mutate(
    win_percent = W/GP
  )

updated_ranks_df %>% 
  filter(team != "hie") %>% 
  arrange(desc(team_rank)) %>% 
  left_join(
    nhl_summary_2020,
    by = c("team" = "tm")
  ) %>% 
  group_by(division) %>%
  gt(
    rowname_col = "team"
  ) %>% 
  tab_header(
    title = md("**NHL** Team Ranks"),
    subtitle = md("*2019-2020*")
    ) %>%  
  cols_label(
    team_rank = html("Team Strength"),
    playoff_team = html("Playoffs"),
    win_percent = html("Win %")
  ) %>% 
  data_color(
    vars(team_rank),
    colors = scales::col_numeric(palette = c("red","white","blue"),domain = NULL)
  ) %>% 
  data_color(
    vars(win_percent),
    colors = scales::col_numeric(palette = c("red","white","blue"),domain = NULL)
  ) %>% 
  fmt_number(
    vars(team_rank),
    decimals = 2
  ) %>% 
  fmt_percent(
    vars(win_percent)
  )
