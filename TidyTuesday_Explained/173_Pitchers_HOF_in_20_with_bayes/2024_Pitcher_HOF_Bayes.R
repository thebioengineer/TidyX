## TidyX Episode 173 - Pitching HOF with Bayes


### Data processing from 172 ----

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

### Fitting a Bayes Model 173 ----


#### Load Packages
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(broom.mixed)

#### Fit Model

fit_bayes <- stan_glm(
  inducted ~ IPouts + SO + R,
  data = pitchers_eligible_career_stats_HOF_sans_2018_final,
  family = binomial,
  seed = 3344
  )

print(fit_bayes, digits = 4)

round(exp(fit_bayes$coef), digits = 4)


mcmc_trace(fit_bayes)
mcmc_dens_overlay(fit_bayes)
mcmc_pairs(fit_bayes, pars = c("IPouts", "SO","R"))

## Explore Uncertainty in Coefficients
post_coefs <- as.data.frame(fit_bayes) %>%
  setNames(c("intercept", "IPouts", "SO", "R")) 

head(post_coefs)

par(mfrow = c(2,2))
hist(post_coefs[, 2], main = "Posterior Draws for IPouts")
hist(post_coefs[, 3], main = "Posterior Draws for SO")
hist(post_coefs[, 4], main = "Posterior Draws for R")

dev.off()


apply(X = post_coefs, MARGIN = 2, FUN = mean)
apply(X = post_coefs, MARGIN = 2, FUN = sd)

post_coefs %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  reframe(
    pct_tile = c(0.05, 0.5, 0.95),
    val = quantile(value, probs = pct_tile)
    ) %>%
  pivot_wider(
    names_from = pct_tile,
    values_from = val
  ) %>% 
  knitr::kable()

#### Make Point Predictions on the Fitted Data
player_pred_fitted <- pitchers_eligible_career_stats_HOF_sans_2018_final %>%
  select(playerID, IPouts, SO, R, inducted) %>%
  mutate(pred_inducted = predict(fit_bayes, newdata = ., type = "response"))

player_pred_fitted %>%
  select(playerID, inducted, pred_inducted) %>%
  arrange(desc(pred_inducted)) %>%
  head(10)


#### Posterior Uncertainty in Predictions
set.seed(4)
pitcher_sample <- pitchers_eligible_career_stats_HOF_sans_2018_final %>%
  sample_n(size = 3) %>%
  select(playerID, IPouts, SO, R, inducted)
  

pitcher_sample

## Posterior probabilities
pred_pitcher_sample <- posterior_epred(fit_bayes, newdata = pitcher_sample) %>%
  as.data.frame() %>%
  setNames(c(pitcher_sample %>% pull(playerID)))

head(pred_pitcher_sample)

pred_pitcher_sample %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = value, fill = name)) +
  geom_density() +
  facet_wrap(~name, scales = "free")

apply(pred_pitcher_sample, MARGIN = 2, FUN = mean)
apply(pred_pitcher_sample, MARGIN = 2, FUN = sd)
apply(pred_pitcher_sample, MARGIN = 2, FUN = quantile, c(.05, 0.95))

apply(pred_pitcher_sample, MARGIN = 2, FUN = \(x){sum(x>.02)/length(x)})

pred_pitcher_sample %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = value, fill = name)) +
  geom_density() +
  facet_wrap(~name, scales = "free") + 
  geom_vline(xintercept = 0.02, linetype = "dashed", linewidth = 1.2)

## Predict binary outcomes
pred_pitcher_sample_outcome <- posterior_predict(
  fit_bayes, newdata = pitcher_sample)  %>%
  as.data.frame() %>%
  setNames(c(pitcher_sample %>% pull(playerID)))

head(pred_pitcher_sample_outcome)
apply(pred_pitcher_sample_outcome, MARGIN = 2, FUN = mean)


#### Make predictions on the final data frame
player_pred_tbd <- pitchers_eligible_career_stats_HOF_sans_2018_tbd %>%
  select(playerID, IPouts, SO, R, inducted) %>%
  mutate(pred_inducted = predict(fit_bayes, newdata = ., type = "response"))

player_pred_tbd %>%
  select(playerID, inducted, pred_inducted) %>%
  arrange(desc(pred_inducted)) %>%
  head()

## specific players?

## For the Mariners fans of the last 10 years (Félix Hernández)
People %>% 
  filter(
    nameFirst == "Felix",
    nameLast == "Hernandez"
  )

Felix_pitching <- pitchers_post1970 %>% 
  filter(playerID == "hernafe02") %>%
  group_by(playerID) %>% 
  select(-BAOpp, -ERA) %>% 
  summarize(
    across(W:GIDP, sum),
    final_season = max(yearID),
    .groups = "drop"
  )

Felix_pitching_pred <- predict(fit_bayes, newdata = Felix_pitching, type = "response")
Felix_pitching_epred <- posterior_epred(fit_bayes, newdata = Felix_pitching)

mean(Felix_pitching_epred)
sd(Felix_pitching_epred)
quantile(Felix_pitching_epred, probs = c(.05, 0.95))
sum(Felix_pitching_epred > 0.5)/4000

hist(Felix_pitching_epred, main = "King Felix HOF Pred")
abline(v=0.5, col = "red", lwd = 2, lty = 2)
