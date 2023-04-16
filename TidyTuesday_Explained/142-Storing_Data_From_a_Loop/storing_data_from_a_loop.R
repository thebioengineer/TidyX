## TidyX Episode 142 - Storing data from a loop

library(tidyverse)


## setup
df <- mtcars
head(df)

## Sample Observations
n_obs <- nrow(df)
n_obs

## Set up the folds
k <- 5
k_fold <- floor(n_obs / k)
k_fold

k_fold * 5

## Create the groups
groups <- rep(1:k, each = k_fold)
length(groups)

## Add a random selection to any un accounted for groups
extra_groups <- sample(1:k, size = n_obs - length(groups))

groups <- c(groups, extra_groups)

rand_groups <- sample(groups, replace = FALSE, size = length(groups))

## put groups into the data frame for subsetting
df$k_group <- groups
df

##### K-fold version 1: lists ###########

group_id <- unique(df$k_group)
preds <- list()

## k-fold cross validation
for(i in group_id){
  
  dat <- df %>% filter(k_group != i)
  hold_out <- df %>% filter(k_group == i)
  
  fit <- lm(mpg ~ cyl + disp + hp, data = dat)
  preds[[i]] <- predict(fit, newdata = hold_out)

}

## Plot model results
plot(x = unlist(preds)[rownames(df)],
     y = df$mpg)
abline(a = 0, b = 1, col = "red", lwd = 3)



##### K-fold version 2: vector ###########

preds <- c()

## k-fold cross validation
for(i in group_id){
  
  dat <- df %>% filter(k_group != i)
  hold_out <- df %>% filter(k_group == i)
  
  fit <- lm(mpg ~ cyl + disp + hp, data = dat)
  
  preds <- c(preds, predict(fit, newdata = hold_out))
}

preds

## Plot model results
plot(x = preds[rownames(df)],
     y = df$mpg)
abline(a = 0, b = 1, col = "red", lwd = 3)

##### K-fold version 3: data.frames construction ###########

preds_df <- data.frame(
  fold = numeric(0),
  obs_mpg = numeric(0),
  pred_mpg = numeric(0),
  car = character(0)
  )

## k-fold cross validation
for(i in group_id){
  
  dat <- df %>% filter(k_group != i)
  hold_out <- df %>% filter(k_group == i)
  
  fit <- lm(mpg ~ cyl + disp + hp, data = dat)
  
  fold_df <- data.frame(
    fold = i,
    obs_mpg = hold_out$mpg,
    pred_mpg = predict(fit, newdata = hold_out),
    car = rownames(hold_out)
  )
  
  preds_df <- rbind(preds_df, fold_df)
  
}

preds_df

## Plot model residuals
hist(x = preds_df$obs_mpg - preds_df$pred_mpg )
abline(v = 0, col = "red", lwd = 3)


##### K-fold version 4: updating data.frame ref ###########

tmp_df <- df
tmp_df$preds <- NA_real_

## k-fold cross validation
for(i in group_id){
  
  dat <- df %>% filter(k_group != i)
  hold_out <- df %>% filter(k_group == i)
  
  fit <- lm(mpg ~ cyl + disp + hp, data = dat)
  
  preds_vec <- predict(fit, newdata = hold_out)

  tmp_df[names(preds_vec), "preds"] <- preds_vec
  
}

tmp_df

## Plot model residuals
hist(x = tmp_df$mpg - tmp_df$preds )
abline(v = 0, col = "red", lwd = 3)
