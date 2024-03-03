## Tidy Episode 175 - Predicting HOF Pitchers with Random Forests

#### Packages -------------------------
library(Lahman)
library(tidyverse)
library(broom)
library(rsample)
library(Metrics)
library(randomForest)

#### Data Prep From episode 172 -------------------------
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


#### Starts Episode 175 -----

## rename the training set so it is less typing
train <- pitchers_eligible_career_stats_HOF_sans_2018_final

## set outcome to 0 or 1
train$inducted_class <- as.numeric(train$inducted)
train$inducted_class <- as.factor(train$inducted_class)

table(train$inducted_class)

#### Random Forest -----------------------------------------

## Cross Validation Folds
set.seed(2024)
d_cv <- vfold_cv(train, strata = inducted_class, v = 5)
d_cv

cv_data <- d_cv %>%
  mutate(
    train = map(splits, ~training(.x)),
    validate = map(splits, ~testing(.x))
    )

cv_data

## Tuning Parameters
cv_tune <- crossing( # create Cartesian product of contents
    cv_data,
    mtry = c(2:4), # number of variables to use in each tree
    ntrees = c(100, 200, 300, 400) # number of trees
  ) %>%
  mutate(
    tune_param = paste0("Mtry: ", mtry,", ntrees: ", ntrees )
  )   

cv_tune

## Tune the RF
rf <- cv_tune %>%
  mutate(fit =
           pmap(
             list( x = train, mtry = mtry, trees = ntrees ),
            \(x, mtry, trees) {
               randomForest(
                 inducted_class ~ ER + BB + SO + SV,
                 data = x,
                 mtry = mtry,
                 ntrees = trees
               )
           }))

rf

## Make predictions on the validation set
rf_validate <- rf %>%
  mutate(pred_hof = map2(.x = fit, .y = validate, ~predict(.x, .y)))

rf_validate

rf_validate %>%
  unnest(cols = validate) %>%
  select(inducted_class, pred_hof) %>%
  unnest(pred_hof)

## get AUC
rf_auc <- rf_validate %>%
  mutate(auc = map2_dbl(
    .x = validate, 
    .y = pred_hof,
    ~auc(actual = as.numeric(.x$inducted), predicted = as.numeric(.y)))
    )

rf_auc

## get the parameters that have the highest AUC across the folds
best_model <- rf_auc %>%
  group_by(mtry, ntrees) %>%
  summarize(avg_auc = mean(auc)) %>%
  ungroup() %>% 
  slice_max(avg_auc)

# optimized model has mtry = 3, ntrees = 300

mtry <- best_model$mtry
ntree <- best_model$ntrees


## Create the final model on all train data
final_rf <- randomForest(inducted_class ~ ER + BB + SO + SV,
                         data = train,
                         mtry = mtry,
                         ntree = ntree)

final_rf

## predictions on the train set
# predicted class
train$pred_hof <- predict(final_rf, newdata = train)

# predicted probability
train <- train %>% 
  cbind(
    predict(final_rf, newdata = train, type = "prob")
    ) %>%
  rename("Not Inducted" = "0", Inducted = "1")

train %>%
  select(playerID, inducted_class, pred_hof, 'Not Inducted', Inducted) %>%
  head()

## mse of model
brier <- with(train, mean((as.numeric(inducted_class)-1) - Inducted)^2)
brier

table(Predicted = train$pred_hof, Actual = train$inducted)

train %>%
  select(playerID, inducted_class, pred_hof, `Not Inducted`, Inducted) %>% 
  filter(inducted_class == "0") %>%
  arrange(desc(Inducted)) %>%
  head()


#### Make predictions on the final data frame
test <- pitchers_eligible_career_stats_HOF_sans_2018_tbd %>%
  select(playerID:SO, inducted)

# predicted class
test$pred_hof <- predict(final_rf, newdata = test)

# predicted probability
test <- test %>% 
  cbind(predict(final_rf, newdata = test, type = "prob")) %>%
  rename("Not Inducted" = "0", Inducted = "1")

head(test)

table(Predicted = test$pred_hof, Actual = test$inducted)

test %>%
  select(playerID, inducted, pred_hof, `Not Inducted`, Inducted) %>% 
  arrange(desc(Inducted)) %>%
  head()

playerInfo

#### Make predictions on the Newly Eligible Folks
test2 <- pitchers_eligible_career_stats_HOF_2018 %>%
  select(playerID:SO, inducted)

# predicted class
test2$pred_hof <- predict(final_rf, newdata = test2)

# predicted probability
test2 <- cbind(test2, predict(final_rf, newdata = test2, type = "prob")) %>%
  rename("Not Inducted" = "0", Inducted = "1")

head(test2)

table(Predicted = test2$pred_hof) # at a 50% threshold

test2 %>%
  select(playerID, inducted, pred_hof, `Not Inducted`, Inducted) %>% 
  arrange(desc(Inducted)) %>%
  head()
