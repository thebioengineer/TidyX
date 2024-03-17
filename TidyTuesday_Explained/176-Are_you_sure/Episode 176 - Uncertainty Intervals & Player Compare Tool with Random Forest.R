## TidyX Episode 176 - Are you sure?

#### Packages -------------------------
library(Lahman)
library(tidyverse)
library(broom)
library(rsample)
library(Metrics)
library(randomForest)

#### Data Prep (TidyX Episode 172) -------------------------
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
  summarize(
    across(W:GIDP, sum),
    final_season = max(yearID),
    .groups = "drop"
  )

## add batting data (if it exists)
pitchers_post1970_eligible_career_stats_plus_batting <- Batting %>% 
  filter(playerID %in% pitchers_post1970$playerID) %>% 
  ## summarize for the year the players stats
  group_by(playerID, yearID) %>% 
  summarize(
    across(G:GIDP, sum)
  ) %>% 
  ## Calculate Players Career Stats
  group_by(playerID) %>% 
  summarize(
    career_ba = sum(H)/sum(AB),
    across(G:GIDP, sum)
  ) %>% 
  mutate(
    career_ba = ifelse(is.nan(career_ba), NA, career_ba)
    # career_ba = case_when(
    #   is.nan(career_ba) ~ NA, 
    #   TRUE ~ career_ba
    # )
  ) %>% 
  rename_with(\(x){paste0("batting_",x)}) %>%
  {left_join(
    pitchers_post1970_eligible_career_stats,
    .,
    by = c("playerID" = "batting_playerID" )
  ) }


## Add Hall of Fame Metadata
hof <- HallOfFame %>% 
  select(playerID, inducted) %>% 
  ## PLayers are eligible for a number of years, so summarizing to remove nominations votes
  group_by(playerID) %>% 
  summarize(
    inducted = sum(inducted == "Y") > 0
  )

### Merge Player name and HOF metadata into dataset
pitchers_post1970_eligible_career_stats_plus_batting_HOF <- pitchers_post1970_eligible_career_stats_plus_batting %>% 
  left_join(hof, by = "playerID")


## Pull out the new 2018 eligible players

pitchers_eligible_career_stats_HOF_2018 <- pitchers_post1970_eligible_career_stats_plus_batting_HOF %>% 
  filter(final_season == 2018)


## Players have 10 years of eligibility (Starting 2015 election),
## so if they have not been inducted and its 
## been 10 years, too bad
pitchers_eligible_career_stats_HOF_sans_2018_all <- pitchers_post1970_eligible_career_stats_plus_batting_HOF %>% 
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

#### Train the Random Forest (TidyX Episode 175)-----------------------------------------

## rename the training set so it is less typing
train <- pitchers_eligible_career_stats_HOF_sans_2018_final

## set outcome to 0 or 1
train$inducted_class <- ifelse(train$inducted == TRUE, 1, 0)
train$inducted_class <- as.factor(train$inducted_class)

table(train$inducted_class)

## Cross Validation Folds
set.seed(2024)
d_cv <- vfold_cv(train, strata = inducted_class, v = 5)
d_cv

cv_data <- d_cv %>%
  mutate(train = map(splits, ~training(.x)),
         validate = map(splits, ~testing(.x)))

cv_data

## Tuning Parameters
cv_tune <- cv_data %>%
  crossing(mtry = c(2:4),
           ntrees = c(100, 200, 300, 400)) %>%
  mutate(tune_param = paste(mtry, ntrees, sep = ", "))   

cv_tune

## Tune the RF
rf <- cv_tune %>%
  mutate(fit = map2(.x = train, .y = tune_param,
                    ~randomForest(inducted_class ~ ER + BB + SO + SV,
                                  data = .x,
                                  mtry = as.numeric(str_split(.y, pattern = ", ")[[1]]),
                                  ntrees = as.numeric(str_split(.y, pattern = ", ")[[2]]))))

rf

## Make predictions on the validation set
rf_validate <- rf %>%
  mutate(pred_hof = map2(.x = fit, .y = validate, ~predict(.x, .y)))

rf_validate

rf_validate %>%
  unnest(cols = train) %>%
  select(inducted_class, pred_hof) %>%
  unnest(pred_hof)

## get AUC
rf_auc <- rf_validate %>%
  mutate(auc = map2_dbl(.x = validate, .y = pred_hof, ~auc(actual = as.numeric(.x$inducted), predicted = as.numeric(.y))))

rf_auc

## get the parameters that have the highest AUC across the folds
rf_auc %>%
  group_by(tune_param) %>%
  summarize(avg_auc = mean(auc)) %>%
  slice_max(avg_auc)

# optimized model has mtry = 1, ntrees = 100

mtry <- 2
ntree <- 100


## Create the final model on all train data
final_rf <- randomForest(inducted_class ~ ER + BB + SO + SV,
                         data = train,
                         mtry = mtry,
                         ntree = ntree)

final_rf

#### Player Comparison (TidyX Episode 176) ###################

## Grab a single player to make a prediction on

new_player <- train %>% filter(playerID == "smoltjo01")
new_player

predict(final_rf, new_player, type = "prob")

## where does the probability come from?
as.numeric(as.vector(
  predict(final_rf, new_player, type = "prob", predict.all = TRUE)$individual)
  )
mean(as.numeric(as.vector(predict(final_rf, new_player, type = "prob", predict.all = TRUE)$individual)))

# The standard deviation of the predicted probability
sqrt((0.69 * (1-0.69)) / ntree)

# use simulation to obtain a distribution of possible outcomes
n_sims <- 1000

set.seed(2024)
player_sim <- rbinom(n = n_sims, size = ntree, prob = 0.69)/ntree
hist(player_sim)
quantile(player_sim, probs = c(0.5, 0.025, 0.975))

## predictions on the train set
# predicted probability
train_preds <- train %>% 
  cbind(
    predict(final_rf, newdata = train, type = "prob")
  ) %>%
  rename("Not Inducted" = "0", Inducted = "1") %>% 
  mutate(
    pred_hof = predict(final_rf, newdata = train)
  )

train_preds %>%
  select(playerID, inducted_class, pred_hof, 'Not Inducted', Inducted) %>%
  arrange(desc(Inducted)) %>%
  head(10)

## Pick two players to compare

pick_two <- function(playerID_1, playerID_2, train_set){
  
  n_sims <- 10000
  ntree <- 100
  
  ## get players
  p1 <- train_set %>%
    filter(playerID == playerID_1) %>%
    select(playerID, ER, BB, SO, SV, Inducted)
  
  p2 <- train_set %>%
    filter(playerID == playerID_2) %>%
    select(playerID, ER, BB, SO, SV, Inducted)
  
  ## Calculate the difference between the two players
  diff <- p1$Inducted - p2$Inducted
  se_diff <- sqrt((p1$Inducted * (1 - p1$Inducted)) / ntree + (p2$Inducted * (1 - p2$Inducted)) / ntree)
  
  ## simulations
  sim_p1 <- rbinom(n = n_sims, size = ntree, prob = p1$Inducted)/ntree
  sim_p2 <- rbinom(n = n_sims, size = ntree, prob = p2$Inducted)/ntree
  
  diff_sim <- rnorm(n = n_sims, mean = diff, sd = se_diff)
  
  ## plot
  par(mfrow = c(1, 2))
  plot(density(sim_p1),
       lwd = 3,
       col = "blue",
       xlim = c(0, 1),
       ylim = c(0, 15),
       main = "Player HOF\nProbability Comparisons")
  lines(density(sim_p2),
        lwd = 3,
        col = "red")
  legend("topleft",
         legend = c(p1$playerID, p2$playerID),
         lwd = c(2, 2),
         col = c("blue", "red"))
  
  hist(x = diff_sim,
       main = "Difference btw\ntwo players",
       xlab = "Difference in Making the HOF",
       col = "palegreen")
  abline(v = 0,
         col = 'red',
         lwd = 3,
         lty = 2)
  
  ## stats table
  stats_tbl <- rbind(p1, p2) %>%
    select(-Inducted) %>%
    t() %>%
    janitor::row_to_names(row_number = 1) %>%
    knitr::kable()

  ## quantile intervals
  p1_intervals <- quantile(sim_p1, probs = c(0.5, 0.025, 0.975))
  p2_intervals <- quantile(sim_p2, probs = c(0.5, 0.025, 0.975))
  diff_intervals <- quantile(diff_sim, probs = c(0.5, 0.025, 0.975))
  
  uncertainty_tbl <- bind_rows(
      p1_intervals, 
      p2_intervals,
      diff_intervals
    ) %>%
    as.data.frame() %>%
    setNames(c("Median", "Lower95", "Upper95"))  %>%
    mutate(
      Player = c(p1$playerID, p2$playerID, "Difference")
      ) %>%
    relocate(Player, .before = Median) %>%
    mutate(across(
      .cols = Median:Upper95,
      ~round(.x, 3))
      )
  
  ## Result
  diff_over0 <- mean(diff_sim >= 0)
  
  outcome <- ifelse(
    mean(diff_sim) >= 0, 
    paste0("Player 1 has a ", paste(round(diff_over0*100, 1), sep = "%"), " chance of making the HOF over Player 2" ),
    paste0("Player 2 has a ", paste(round((1-diff_over0)*100, 1), sep = "%"), " chance of making the HOF over Player 1" 
           )
  )
  
  ## Return list
  list(
    Stats = stats_tbl,
    'Probability of HOF' = uncertainty_tbl,
    Result = outcome
    )
}


pick_two("hoffmtr01", "morrija02", train_set = train_preds)

pick_two("eckerde01", "martipe02", train_set = train_preds)
