## TidyX Episode 169 - Predicting MLB HOF - Class of 2024

library(Lahman)
library(tidyverse)

# Generate Datasets ----

## Batting data, keep everyone who started playing since 1970 and is eligible for 
## HOF: Retired pre 2018, played 10 seasons

## id players who played pre 1970
batters_pre1970 <- Batting %>% 
  filter(yearID < 1970) %>% 
  pull(playerID) %>% 
  unique()

## drop players pre 1970 
batters_post1970 <- Batting %>% 
  filter(!playerID %in% batters_pre1970)

## drop pitchers
batters_post1970 <- batters_post1970 %>% 
  filter(!playerID %in% Pitching$playerID)


## drop players ineligible for HOF (still played into 2019+)
batters_post1970_eligible <- batters_post1970 %>% 
  group_by(playerID) %>% 
  filter(
    max(yearID) < 2019,
    max(yearID)-min(yearID) >= 9, #(2005-2014 is 10 seasons) Had to play at least 10 years
    sum(AB) > 1 ## have batted at least once?
  )

## Generate Career stats
batters_post1970_eligible_career_stats <- batters_post1970_eligible %>% 
  ## summarize for the year the players stats
  group_by(playerID, yearID) %>% 
  summarize(
    across(G:GIDP, sum)
  ) %>% 
  ## Calculate Players Career Stats
  group_by(playerID) %>% 
  summarize(
    best_ba = max(H/AB, na.rm = TRUE),
    career_ba = sum(H)/sum(AB),
    best_scoring = max(R),
    across(G:GIDP, sum),
    final_season = max(yearID)
  )



## Add Hall of Fame Metadata
hof <- HallOfFame %>% 
  select(playerID, inducted) %>% 
  ## PLayers are eligible for a number of years, so summarizing to remove nominations votes
  group_by(playerID) %>% 
  summarize(
    inducted = sum(inducted == "Y") > 0
  )

### Merge Player name and HOF metadata into dataset
batters_post1970_eligible_career_stats_HOF <- batters_post1970_eligible_career_stats %>% 
  left_join(hof, by = "playerID")


## Pull out the new 2018 eligible players

batters_eligible_career_stats_HOF_2018 <- batters_post1970_eligible_career_stats_HOF %>% 
  filter(final_season == 2018)


## Players have 10 years of eligibility (Starting 2015 election),
## so if they have not been inducted and its 
## been 10 years, too bad
batters_eligible_career_stats_HOF_sans_2018_all <- batters_post1970_eligible_career_stats_HOF %>% 
  filter(final_season < 2018) %>% 
  mutate(
    inducted = case_when(
      ## if not been inducted, marking as wont be if its been greater than 10 years
      is.na(inducted) & final_season < 2009 ~ FALSE, 
      ## Preserve current values
      !is.na(inducted) ~ inducted
    )
  ) 

batters_eligible_career_stats_HOF_sans_2018_final <- batters_eligible_career_stats_HOF_sans_2018_all%>% 
  filter(!is.na(inducted)) %>% 
  filter(final_season < 2009)
  
batters_eligible_career_stats_HOF_sans_2018_tbd <- batters_eligible_career_stats_HOF_sans_2018_all%>% 
  filter((inducted == FALSE | is.na(inducted)) & final_season > 2009)

# Model Fitting ----

## glm using all the covariates

trained_glm_all <- glm(
  inducted ~ best_ba + career_ba + best_scoring + G + AB + R + H + X2B + X3B + 
    HR + RBI + SB + CS + BB + SO + IBB + HBP + SH + SF + GIDP,
  family = binomial(link = "logit"),
  data = batters_eligible_career_stats_HOF_sans_2018_final
  )

summary(trained_glm_all)

trained_glm_select <- glm(
  inducted ~ best_ba + career_ba + R,
  family = binomial(link = "logit"),
  data = batters_eligible_career_stats_HOF_sans_2018_final
)

summary(trained_glm_select)

exp(trained_glm_select$coefficients)

## Model Inspection

## How does an average player perform?
new_player <- data.frame(
  best_ba = mean(batters_eligible_career_stats_HOF_sans_2018_final$best_ba),
  career_ba = mean(batters_eligible_career_stats_HOF_sans_2018_final$career_ba),
  R = mean(batters_eligible_career_stats_HOF_sans_2018_final$R)
)

### what if they had a great career BA?
new_player2 <- data.frame(
  best_ba = mean(batters_eligible_career_stats_HOF_sans_2018_final$best_ba),
  career_ba = mean(batters_eligible_career_stats_HOF_sans_2018_final$career_ba)+.05,
  R = mean(batters_eligible_career_stats_HOF_sans_2018_final$R)
)

predict(trained_glm_select, newdata = new_player, type = "response")
predict(trained_glm_select, newdata = new_player2, type = "response")



# Who might be inducted from 2018? ----

### predicted by glm in 2018 retiring class
pred_glm_2018_HOF <- predict(
  object = trained_glm_select, 
  newdata = batters_eligible_career_stats_HOF_2018,
  type = "response"
)

sum(pred_glm_2018_HOF >= 0.1)

batters_eligible_career_stats_HOF_2018 %>% 
  mutate(
    pred = pred_glm_2018_HOF
  ) %>% 
  filter(pred >= .1) %>% 
  left_join(People) %>% 
  select(
    nameFirst,
    nameLast,
    pred,
    best_ba, 
    career_ba,
    G,
    AB,
    R,
    H,
    HR
  )

### predicted by glm in TBD
pred_glm_tbd_HOF <- predict(
  object = trained_glm_select, 
  newdata = batters_eligible_career_stats_HOF_sans_2018_tbd,
  type = "response"
)

sum(pred_glm_tbd_HOF >= 0.5)

batters_eligible_career_stats_HOF_sans_2018_tbd %>% 
  mutate(
    pred = pred_glm_tbd_HOF
  ) %>% 
  left_join(People) %>% 
  select(
    nameFirst,
    nameLast,
    pred,
    best_ba, 
    career_ba,
    G,
    AB,
    R,
    H,
    HR,
    final_season
  ) %>% 
  arrange(desc(pred))


