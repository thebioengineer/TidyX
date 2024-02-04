
## TidyX Episode 172 - Predicting MLB HOF Pitchers - Class of 2024

library(Lahman)
library(tidyverse)

## id players who played pre 1970
pitchers_pre1970 <- Pitching %>% 
  filter(yearID < 1970) %>% 
  pull(playerID) %>% 
  unique()

## drop players pre 1970 
pitchers_post1970 <- Pitching %>% 
  filter(!playerID %in% pitchers_pre1970)


## drop players ineligible for HOF (still played into 2019+)
pitchers_post1970_eligible <- pitchers_post1970 %>% 
  group_by(playerID) %>% 
  filter(
    max(yearID) < 2019,
    max(yearID)-min(yearID) >= 9 #(2005-2014 is 10 seasons) Had to play at least 10 years
  )

## Generate Career stats
pitchers_post1970_eligible_career_stats <- pitchers_post1970_eligible %>% 
  ## summarize for the year the players stats
  group_by(playerID) %>% 
  select(-BAOpp, -ERA) %>% 
  summarize(
    across(W:GIDP, sum),
    final_season = max(yearID),
    .groups = "drop"
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
pitchers_post1970_eligible_career_stats_HOF <- pitchers_post1970_eligible_career_stats %>% 
  left_join(hof, by = "playerID")


## Pull out the new 2018 eligible players

pitchers_eligible_career_stats_HOF_2018 <- pitchers_post1970_eligible_career_stats_HOF %>% 
  filter(final_season == 2018)


## Players have 10 years of eligibility (Starting 2015 election),
## so if they have not been inducted and its 
## been 10 years, too bad
pitchers_eligible_career_stats_HOF_sans_2018_all <- pitchers_post1970_eligible_career_stats_HOF %>% 
  filter(final_season < 2018) %>% 
  mutate(
    inducted = case_when(
      ## if not been inducted, marking as wont be if its been greater than 10 years
      is.na(inducted) & final_season < 2009 ~ FALSE, 
      ## Preserve current values
      !is.na(inducted) ~ inducted
    )
  ) 

pitchers_eligible_career_stats_HOF_sans_2018_final <- pitchers_eligible_career_stats_HOF_sans_2018_all%>% 
  filter(!is.na(inducted)) %>% 
  filter(final_season < 2009 | inducted == TRUE)

pitchers_eligible_career_stats_HOF_sans_2018_tbd <- pitchers_eligible_career_stats_HOF_sans_2018_all%>% 
  filter((inducted == FALSE | is.na(inducted)) & final_season >= 2009)


head(pitchers_eligible_career_stats_HOF_sans_2018_final) %>%
  as.data.frame()

table(pitchers_eligible_career_stats_HOF_sans_2018_final$inducted)


# Model Fitting ----

## glm using all the covariates

trained_glm_all <- glm(
  inducted ~ W + L + G + GS + CG + SHO + SV + IPouts + H + ER + HR + BB + SO + 
    IBB + WP + HBP + BK + BFP + GF + R + SH + SF + GIDP,
  family = binomial(link = "logit"),
  data = pitchers_eligible_career_stats_HOF_sans_2018_final
)

summary(trained_glm_all)

trained_glm_select <- glm(
  inducted ~ IPouts + SO + R,
  family = binomial(link = "logit"),
  data = pitchers_eligible_career_stats_HOF_sans_2018_final
)

summary(trained_glm_select)

exp(trained_glm_select$coefficients)


### predicted by glm in 2018 retiring class
pitchers_eligible_career_stats_HOF_sans_2018_final$preds <- predict(
  object = trained_glm_select, 
  type = "response"
)


## Calibration Plot
pitchers_eligible_career_stats_HOF_sans_2018_final %>% 
  select(playerID, inducted, preds) %>% 
  ggplot(aes(x = preds, y=as.numeric(inducted))) + 
  geom_smooth() +
  geom_abline(col = "red")
