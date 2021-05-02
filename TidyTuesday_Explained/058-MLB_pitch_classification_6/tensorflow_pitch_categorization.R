---
  title: "TensorFlow pitchf/x Classification"
author: "TidyX"
date: "5/2/2021"
output: html_document
editor_options: 
  chunk_output_type: console
---
  
  **Data courtest of {mlbgameday}:** https://github.com/keberwein/mlbgameday
**Resource for understanding pitchf/x data:** https://library.fangraphs.com/misc/pitch-fx/
  **Resource on the features in the pitchf/x data:** Kagan, David. (2008). Fun with PitchFX Data. 


## Load Data

## References

**Data courtesy of {mlbgameday}:** https://github.com/keberwein/mlbgameday
**Resource for understanding pitchf/x data:** https://library.fangraphs.com/misc/pitch-fx/
  **Resource on the features in the pitchf/x data:** Kagan, David. (2008). Fun with PitchFX Data. 

## Problem Statement

The FBL (fantasy baseball league) wants to predict which pitches are thrown. 
Traditionally it was labeled by a human sitting in the stands.
They want to productionalize it to give the classifications faster. 

They hired us...

## Load Data

```{r setup, include=FALSE}

bp <- here::here("TidyTuesday_Explained/058-MLB_pitch_classification_6")

knitr::opts_chunk$set(echo = TRUE,
                      root.dir = bp)

suppressPackageStartupMessages({
  suppressWarnings({
    library(tidyverse)
  })
})


```

```{r load-data-and-clean}

## see Episode 53 - bit.ly/TidyX_Ep53 - for the background on this cleanup

# train
set1 <- readRDS(file.path(bp,"2016_04_21_to_2016_04_23_pitch.rds"))
set2 <- readRDS(file.path(bp,"2016_04_24_to_2016_04_26_pitch.rds"))
train <- bind_rows(set1, set2)

#test
test <- readRDS(file.path(bp,"2016_04_27_to_2016_04_30_pitch.rds"))

names(train)

#### Cleaning data based on EDA (below) ----------------------------------------------
train_cleaned <- train %>%
  filter(!pitch_type %in% c("KN", "IN", "PO", "EP", "FO"),
         !is.na(pitch_type)) %>%
  select(pitch_type, start_speed, end_speed, pfx_x, pfx_z, px, pz, x0, z0, vx0, vz0)

test_cleaned <- test %>%
  filter(!pitch_type %in% c("KN", "IN", "PO", "EP", "FO", "SC"),
         !is.na(pitch_type)) %>%
  select(pitch_type, start_speed, end_speed, pfx_x, pfx_z, px, pz, x0, z0, vx0, vz0)

## Check for any NA's

train_cleaned %>%
  summarize(across(.cols = everything(),
                   .fns = ~sum(is.na(.x)))) %>%
  t()

test_cleaned %>%
  summarize(across(.cols = everything(),
                   .fns = ~sum(is.na(.x)))) %>%
  t()


train_cleaned$pitch_type <- as.factor(train_cleaned$pitch_type)
test_cleaned$pitch_type <- as.factor(test_cleaned$pitch_type)
```


## Keras/TensorFlow Classifier

```{r}

library(keras)

## defining the model
model <- keras_model_sequential()

model %>% 
  
  ## two hidden layers
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  
  ## output layer
  layer_dense(units = 9, activation = 'softmax')


## define learning information and what model uses to figure out how to updae the weights next
model %>% compile(
  optimizer = optimizer_adam(.01), 
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

```

```{r}

# convert pitch type to a categorical array
train_cleaned_pitch_type_num <- to_categorical(as.numeric(train_cleaned$pitch_type)-1)
test_cleaned_pitch_type_num <- to_categorical(as.numeric(test_cleaned$pitch_type)-1)


## generate arrays

## create a function for scaling the data
scale_func <- function(x, a, s){
  z = (x - a) / s
  return(z)
}

## create training and test matrices and scale the data
train_norms <- train_cleaned %>% 
  select(-pitch_type) %>%
  summarize(across(.cols = everything(),
                   .fns = list(avg = mean, SD = sd),
                   .names = "{col}_{fn}"))

## scale the data & make into matrix
train_cleaned_x <- train_cleaned %>%
  select(-pitch_type) %>%
  mutate(start_speed = scale_func(start_speed, train_norms$start_speed_avg, train_norms$start_speed_SD),
         end_speed = scale_func(end_speed, train_norms$end_speed_avg, train_norms$end_speed_SD),
         pfx_x = scale_func(pfx_x, train_norms$pfx_x_avg, train_norms$pfx_x_SD),
         pfx_z = scale_func(pfx_z, train_norms$pfx_z_avg, train_norms$pfx_z_SD),
         px = scale_func(px, train_norms$px_avg, train_norms$px_SD),
         pz = scale_func(pz, train_norms$pz_avg, train_norms$pz_SD),
         x0 = scale_func(x0, train_norms$x0_avg, train_norms$x0_SD),
         z0 = scale_func(z0, train_norms$z0_avg, train_norms$z0_SD),
         vx0 = scale_func(vx0, train_norms$vx0_avg, train_norms$vx0_SD),
         vz0 = scale_func(vz0, train_norms$vz0_avg, train_norms$vz0_SD)) %>% 
  array() %>% 
  as.matrix

test_cleaned_x <- test_cleaned %>% 
  select(-pitch_type) %>%
  mutate(start_speed = scale_func(start_speed, train_norms$start_speed_avg, train_norms$start_speed_SD),
         end_speed = scale_func(end_speed, train_norms$end_speed_avg, train_norms$end_speed_SD),
         pfx_x = scale_func(pfx_x, train_norms$pfx_x_avg, train_norms$pfx_x_SD),
         pfx_z = scale_func(pfx_z, train_norms$pfx_z_avg, train_norms$pfx_z_SD),
         px = scale_func(px, train_norms$px_avg, train_norms$px_SD),
         pz = scale_func(pz, train_norms$pz_avg, train_norms$pz_SD),
         x0 = scale_func(x0, train_norms$x0_avg, train_norms$x0_SD),
         z0 = scale_func(z0, train_norms$z0_avg, train_norms$z0_SD),
         vx0 = scale_func(vx0, train_norms$vx0_avg, train_norms$vx0_SD),
         vz0 = scale_func(vz0, train_norms$vz0_avg, train_norms$vz0_SD)) %>% 
  array() %>% 
  as.matrix

```

```{r}

#training the model

model %>%
  fit(
    x = train_cleaned_x,
    y = train_cleaned_pitch_type_num,
    validation_split = .2,
    epochs = 50,
    verbose = 2
  )

```


```{r}

preds <- model %>% 
  predict( test_cleaned_x, type = "raw") %>% 
  data.frame() %>% 
  setNames(levels(train_cleaned$pitch_type)) %>% 
  mutate(
    idx = 1:nrow(.)
  ) %>% 
  pivot_longer(
    cols = 1:9,
    names_to = "type") %>% 
  group_by(idx) %>% 
  mutate(
    pred_pitch = type[which.max(value)]
  ) %>% 
  pivot_wider(
    names_from = type, 
    values_from = value
  ) %>% 
  ungroup() %>% 
  mutate(
    original_pitch_type = test_cleaned$pitch_type
  )

## confusion matrix
table(preds$original_pitch_type, preds$pred_pitch,
      dnn = c("Observed", "Predicted"))


sum(diag(table(preds$original_pitch_type, preds$pred_pitch))) / nrow(preds)

# within pitch type accuracy

preds %>%
  count(pred_pitch, original_pitch_type) %>%
  group_by(original_pitch_type) %>%
  summarize(N = sum(n),
            matches = sum(ifelse(pred_pitch == original_pitch_type, n, 0)),
            within_acc = matches / N)

```

