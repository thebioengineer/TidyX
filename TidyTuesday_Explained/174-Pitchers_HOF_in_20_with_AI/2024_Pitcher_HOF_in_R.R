# TidyX Episode 174 - Predicting MLB Pitching HOF with AI


## Data processing from 172 ----

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

## 174 - Fitting an AI Model ----

## Follow setup from https://tensorflow.rstudio.com/
library(tensorflow)
library(keras)

### Normalize Data ----

### value size can impact model significance, scaling them is important so no
### one variable has an out sized impact. 

scale_ref_values <- pitchers_eligible_career_stats_HOF_sans_2018_final %>% 
  select(W:GIDP) %>% 
  as.list() %>% 
  map(\(x){
    list(
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE)
      )
  })

scale_and_replace_na <- function(x){
  ## get passed name of column as a character
  x_col <- as.character(substitute(x))
  x_scales <- scale_ref_values[[x_col]]
  x <- (x - x_scales$mean)/x_scales$sd
  if(sum(is.na(x)) > 0){
    x[is.na(x)] <- -9999
  }
  x
}

pitchers_eligible_career_stats_HOF_sans_2018_final_scaled <- pitchers_eligible_career_stats_HOF_sans_2018_final %>% 
  select(-final_season) %>% 
  mutate(
    across(W:GIDP , scale_and_replace_na)
  ) 
  
pitchers_eligible_career_stats_HOF_2018_scaled <- pitchers_eligible_career_stats_HOF_2018 %>% 
  select(-final_season) %>% 
  mutate(
    across(W:GIDP , scale_and_replace_na)
  ) 

pitchers_eligible_career_stats_HOF_sans_2018_tbd_scaled <- pitchers_eligible_career_stats_HOF_sans_2018_tbd %>% 
  select(-final_season) %>% 
  mutate(
    across(W:GIDP , scale_and_replace_na)
  )

### Convert input data into matrices/arrays ----

pitchers_eligible_career_stats_HOF_sans_2018_final_scaled_matrix_stats <- 
  pitchers_eligible_career_stats_HOF_sans_2018_final_scaled %>% 
  select(-playerID, -inducted) %>% 
  as.matrix()

pitchers_eligible_career_stats_HOF_sans_2018_final_scaled_results <- 
  pitchers_eligible_career_stats_HOF_sans_2018_final_scaled %>% 
  select(inducted) %>% 
  mutate(
    inducted = as.numeric(inducted)
    ) %>% 
  as.matrix()



### Define the AI Model! ----

## Basic CNN using all the covariates

### Define basic CNN with 1 "hidden" layer, and an output layer
pitcher_hof_model <- keras_model_sequential(input_shape = c(23)) %>%
  layer_dense(64, activation = "relu") %>% # rectified linear unit -> max(0,X)
  layer_dense(1, activation = "sigmoid") ## bounds the outputs between 0 and 1

### Compile model to inform training procedure

pitcher_hof_model %>% compile(
  loss = "MeanSquaredError",
  optimizer = "adam",
  metrics = "accuracy"
  )


### Fitting the Model ----

## Using known "source of truth", train the model

history <- pitcher_hof_model %>% 
  fit(
  pitchers_eligible_career_stats_HOF_sans_2018_final_scaled_matrix_stats,
  pitchers_eligible_career_stats_HOF_sans_2018_final_scaled_results,
  epochs = 100,
  sample_weight =
    pitchers_eligible_career_stats_HOF_sans_2018_final_scaled_results[,1]*199 +1,
  verbose = TRUE
  )


history

### Model Inspection ----

#### Against training data ----

HOF_known <- predict(pitcher_hof_model, pitchers_eligible_career_stats_HOF_sans_2018_final_scaled_matrix_stats) 

HOF_pred_known_results <- pitchers_eligible_career_stats_HOF_sans_2018_final %>% 
  mutate(
    pred = HOF_known
  ) %>% 
  left_join(People) 

HOF_pred_known_results %>% 
  select(
    playerID,
    nameFirst,
    nameLast,
    final_season,
    inducted,
    pred
  ) %>% 
  arrange(desc(pred)) %>% 
  print(n = 20)

hist(HOF_known, breaks = 100)

HOF_pred_known_results%>% 
  select(
    playerID,
    nameFirst,
    nameLast,
    final_season,
    inducted,
    pred
  ) %>% 
  filter(inducted == TRUE) %>% 
  arrange(desc(pred))

HOF_pred_known_results%>% 
  select(
    playerID,
    nameFirst,
    nameLast,
    final_season,
    inducted,
    pred
  ) %>% 
  filter(inducted == FALSE) %>% 
  arrange(desc(pred))


#### How does an average player perform? ----
new_pitcher <- pitchers_eligible_career_stats_HOF_sans_2018_final_scaled %>% 
  select(W:GIDP) %>% 
  summarize(
    across(everything(), mean)
  ) %>% 
  as.matrix()

predict(pitcher_hof_model, new_pitcher) 

#### Who might be inducted from 2018? ----

HOF_probs_2018 <- pitchers_eligible_career_stats_HOF_2018_scaled %>% 
  select(W:GIDP) %>% 
  as.matrix() %>% 
  predict(pitcher_hof_model, .) 
  
pitchers_eligible_career_stats_HOF_2018 %>% 
  mutate(
    pred = HOF_probs_2018
  ) %>% 
  left_join(People) %>% 
  select(
    playerID,
    nameFirst,
    nameLast,
    pred
  ) %>% 
  arrange(desc(pred))

#### predicted in TBD ---

HOF_probs_TBD <- pitchers_eligible_career_stats_HOF_sans_2018_tbd_scaled %>% 
  select(W:GIDP) %>% 
  as.matrix() %>% 
  predict(pitcher_hof_model, .)

pitchers_eligible_career_stats_HOF_sans_2018_tbd %>% 
  mutate(
    pred = HOF_probs_TBD
  ) %>% 
  left_join(People) %>% 
  select(
    playerID,
    nameFirst,
    nameLast,
    final_season,
    pred,
  ) %>% 
  arrange(desc(pred))

top_predicted_tbd <- pitchers_eligible_career_stats_HOF_sans_2018_tbd[
  which.max(HOF_probs_TBD),
  "playerID",
  drop = TRUE
  ]

HallOfFame %>% 
  filter(playerID == top_predicted_tbd)



## For the Mariners fans (Félix Hernández)
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
    .groups = "drop"
  )


Felix_pitching %>% 
  mutate(
    across(W:GIDP, scale_and_replace_na)
    ) %>%
  select(-playerID,) %>% 
  as.matrix() %>% 
  predict(pitcher_hof_model, .)
  