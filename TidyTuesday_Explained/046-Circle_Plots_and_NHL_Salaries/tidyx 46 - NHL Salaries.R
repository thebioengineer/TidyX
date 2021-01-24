### Tidy 46: NHL Salaries

### packages & functions --------------------------------------------
library(tidyverse)
library(rvest)

theme_set(theme_light())

### Webscrape & Data Clean Up ----------------------------------------
skater_url <- "https://www.hockey-reference.com/leagues/NHL_2021_skaters.html"
goalie_url <- "https://www.hockey-reference.com/leagues/NHL_2021_goalies.html"
salary_url <- "https://www.hockey-reference.com/friv/current_nhl_salaries.cgi"

## skater table
skater_html <- skater_url %>%
  read_html() 

skater_table <- skater_html %>% 
  html_node('table') %>%
  html_table(fill = TRUE) %>%
  setNames(1:ncol(.)) %>%
  select(2:5) %>%
  rename(Player = 1,
         Age = 2,
         Tm = 3,
         Pos = 4) %>%
  filter(Player != "Player")

## goalie table
goalie_html <- goalie_url %>%
  read_html() 

goalie_table <- goalie_html %>% 
  html_node('table') %>%
  html_table(fill = TRUE) %>%
  setNames(1:ncol(.)) %>%
  select(2:4) %>%
  rename(Player = 1,
         Age = 2,
         Tm = 3) %>%
  filter(Player != "Player") %>%
  mutate(Pos = "G")

## player table
player_pos <- bind_rows(skater_table, goalie_table)

## salaries
salary_html <- salary_url %>%
  read_html() 

salary_table <- salary_html %>% 
  html_node('table') %>%
  html_table(fill = TRUE) %>%
  rename(Cap_Hit = 'Cap Hit') %>%
  filter(Player != "Player") %>%
  mutate(Salary = parse_number(Salary),
         Cap_Hit = parse_number(Cap_Hit))

## Join
salary_table %>% anti_join(player_pos, by = c("Player", "Tm"))

nhl <- salary_table %>%
  inner_join(player_pos, by = c("Player", "Tm"))

#### EDA -----------------------------------------------
nhl %>%
  janitor::tabyl(Pos) %>%
  ggplot(aes(x = percent, y = reorder(Pos, percent))) +
  geom_col()


nhl %>%
  ggplot(aes(x = Salary)) +
  geom_histogram() +
  scale_x_continuous(labels = scales::comma)

nhl %>%
  ggplot(aes(x = Salary, y = reorder(Pos, Salary), fill = Pos)) +
  geom_boxplot() +
  scale_x_log10(labels = scales::comma) +
  theme(legend.position = "none")
  
nhl %>%
  ggplot(aes(x = Salary, y = reorder(Pos, Salary), fill = Pos)) +
  ggridges::geom_density_ridges() +
  scale_x_log10(labels = scales::comma) +
  theme(legend.position = "none")

  
################################  
##### The Cost of Winning ######

years <- seq(from = 2014, to = 2018, by = 1)

### get skaters
skater_stats <- years %>% 
  
  map_dfr(function(y){
    
    url <- paste("https://www.hockey-reference.com/leagues/NHL_", y, "_skaters.html", sep = "")
    
    # loop over each webpage
    webpage <- read_html(url)
    
    # extract tables
    table_info <- html_nodes(webpage, 'table')
    
    # Get the table you need
    data.frame(html_table(table_info)[[1]], 
               season = y) %>%
      select(Var.2:Var.5, season) %>%
      rename(Player = Var.2,
             Age = Var.3,
             Tm = Var.4,
             Pos = Var.5) %>%
      filter(Player != "Player")
    
  })


## get goalies
goalie_stats <- years %>% 
  
  map_dfr(function(y){
    
    url <- paste("https://www.hockey-reference.com/leagues/NHL_", y, "_goalies.html", sep = "")
    
    # loop over each webpage
    webpage <- read_html(url)
    
    # extract tables
    table_info <- html_nodes(webpage, 'table')
    
    # Get the table you need
    data.frame(html_table(table_info)[[1]], 
               season = y)  %>%
       select(Var.2:Var.4, season) %>%
       rename(Player = Var.2,
              Age = Var.3,
              Tm = Var.4) %>%
       filter(Player != "Player") %>%
       mutate(Pos = "G")

  })


## players
players <- bind_rows(skater_stats, goalie_stats) 

## salaries
salary <- read_csv(file.choose())

salary %>% head()

## player salaries
player_salaries <- players %>%
  inner_join(salary, by = c("Player" = "player",
                            "Tm" = "tm",
                            "season" = "season"))

head(player_salaries)

## spending for each team in 2015
tm_spending <- player_salaries %>%
  group_by(Tm) %>%
  summarize(total_spending = sum(salary)) %>%
  mutate(tm = case_when(
    Tm == "ANA" ~ "Anaheim Ducks",
    Tm == "ARI" ~ "Arizona Coyotes",
    Tm == "BOS" ~ "Boston Bruins",
    Tm == "BUF" ~ "Buffalo Sabres",
    Tm == "CAR" ~ "Carolina Hurricanes",
    Tm == "CBJ" ~ "Columbus Blue Jackets",
    Tm == "CGY" ~ "Calgary Flames",
    Tm == "CHI" ~ "Chicago Blackhawks",
    Tm == "COL" ~ "Colorado Avalanche",
    Tm == "DAL" ~ "Dallas Stars",
    Tm == "DET" ~ "Detroit Red Wings",
    Tm == "EDM" ~ "Edmonton Oilers",
    Tm == "FLA" ~ "Florida Panthers", 
    Tm == "LAK" ~ "Los Angeles Kings",
    Tm == "MIN" ~ "Minnesota Wild",
    Tm == "MTL" ~ "Montreal Canadiens",
    Tm == "NJD" ~ "New Jersey Devils",
    Tm == "NSH" ~ "Nashville Predators",
    Tm == "NYI" ~ "New York Islanders",
    Tm == "NYR" ~ "New York Rangers",
    Tm == "OTT" ~ "Ottawa Senators",
    Tm == "PHI" ~ "Philadelphia Flyers",
    Tm == "PIT" ~ "Pittsburgh Penguins",
    Tm == "SJS" ~ "San Jose Sharks",
    Tm == "STL" ~ "St. Louis Blues",
    Tm == "TBL" ~ "Tampa Bay Lightning",
    Tm == "TOR" ~ "Toronto Maple Leafs",
    Tm == "VAN" ~ "Vancouver Canucks",
    Tm == "WPG" ~ "Winnipeg Jets",
    Tm == "WSH" ~ "Washington Capitals"
  ))

### Get team wins

url2015 <- read_html("https://www.hockey-reference.com/leagues/NHL_2015.html")

nhl2015 <- url2015 %>% 
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
  filter(!(tm %in% c("Atlantic Division", "Metropolitan Division", "Central Division", "Pacific Division")))  %>%
  mutate(across(.cols = c(GP:GA), ~as.numeric(.))) %>% 
  mutate(
    playoff_team = grepl("*",tm, fixed = TRUE),
    tm = gsub("\\*.*", "", tm)
  )

### join everything together
nhl_df <- nhl2015 %>%
  left_join(tm_spending, by = c("tm" = "tm")) %>%
  mutate(spending_per_millions = total_spending / 1000000)

nhl_df %>%
  ggplot(aes(x = playoff_team, y = total_spending)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma)

## model
fit_wins <- glm(
  cbind(W, L) ~ spending_per_millions, 
  data = nhl_df, 
  family = "binomial"
  )

summary(fit_wins)

mean(nhl_df$spending_per_millions)

predict(
  fit_wins, 
  newdata = data.frame(spending_per_millions = mean(nhl_df$spending_per_millions)),
  type = "response") * 82

predict(fit_wins, 
        newdata = data.frame(spending_per_millions = mean(nhl_df$spending_per_millions) + 10),
        type = "response") * 82

53.3 - 46.9

spending_range <- seq(
  from = min(nhl_df$spending_per_millions),
  to = max(nhl_df$spending_per_millions),
  length.out = 10)

predictions <- predict(fit_wins,
                       newdata = data.frame(spending_per_millions = spending_range),
                       type = "response")

diff(predictions)

plot(x = spending_range,
     y = predictions,
     pch = 19)


plot(x = spending_range,
     y = predictions*82,
     pch = 19)


nhl_df$Pred <- predict(fit_wins, type = "response")*82

plot(x = nhl_df$Pred,
     y = nhl_df$W,
     pch = 19,
     xlab = "Predicted wins from spending",
     ylab = "Season Wins")

abline(a = 0,
       b = 1,
       col = "red",
       lwd = 2)
