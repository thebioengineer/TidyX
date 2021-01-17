

library(tidyverse)
library(rvest)
library(ggalt)

theme_set(theme_light())

### Get NHL Game Data

url2018 <- read_html("https://www.hockey-reference.com/leagues/NHL_2019.html")
url2019 <- read_html("https://www.hockey-reference.com/leagues/NHL_2020.html")

## 2018 data
nhl2018 <- url2018 %>% 
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


## 2019 Data
nhl2019 <- url2019 %>% 
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

nhl2018 %>% head()
nhl2019 %>% head()

##### Find optimal coefficient for Pythagorean Wins formula in 2018 data ######

# Calculate Scoring Ratio and Win% for each Team
nhl2018 <- nhl2018 %>%
  mutate(
    scoring_ratio = GF / GA,
    win_pct = W / GP
    )


## optimize the pythagorean win exponent
## sequence of exponent options

exp.options <- seq(from = 0.5, to = 4, by = 0.1)
mae.results <- data.frame("Exp" = exp.options, "Results" = NA)

# key columns
key_columns <- nhl2018[, 2:6]
head(key_columns)

## for loop
for(i in seq_along(exp.options)){
  
  win.pct <- key_columns$W / key_columns$GP
  
  pred.win.pct <- (key_columns$GF/key_columns$GA)^exp.options[i] / (1 + (key_columns$GF/key_columns$GA)^exp.options[i])
  
  mae.results[i,2] <- mean(abs(win.pct - pred.win.pct))
  
}

## look at error data
mae.results %>% 
  head()

## visualize results
mae.results %>%
  ggplot(aes(x = Exp, y = Results)) +
  geom_line() +
  labs(x = "Exponent",
       y = "Mean Absolute Error",
       title = "MAE for Different Exponents in the Pythagorean Wins Formula")

## get the optimized exponent
subset(mae.results, Results == min(Results))

exp_optimal <- subset(mae.results, Results == min(Results))[[1]]

# 2.2 is the best exponent

## Add expected wins to the data frame

nhl2018 <- nhl2018 %>%
  mutate(pyth_win_pct = scoring_ratio^exp_optimal / (1 + scoring_ratio^exp_optimal),
         expected_W = pyth_win_pct * GP,
         error = W - expected_W)

hist(nhl2018$error)


### Use the exponent to project the 2019 data 

nhl2019 <- nhl2019 %>%
  mutate(scoring_ratio = GF / GA,
         win_pct = W / GP,
         pyth_win_pct = scoring_ratio^exp_optimal / (1 + scoring_ratio^exp_optimal),
         expected_W = pyth_win_pct * GP,
         error = W - expected_W)


hist(nhl2019$error)


### year-to-year comparisons ------------------------------------

nhl_compare <- nhl2018 %>%
  left_join(
    nhl2019,
    by = "tm",
    suffix = c("_2018", "_2019")
    )

nhl_compare %>%
  ggplot(aes(x = W_2018, y = W_2019)) +
  geom_point() +
  geom_smooth(method = "lm")


nhl_compare %>%
  ggplot(aes(x = win_pct_2018, win_pct_2019)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(method = "lm") +
  ggrepel::geom_text_repel(aes(label = tm))

nhl_compare %>%
  ggplot(aes(x = GF_2018, GF_2019)) +
  geom_point() +
  geom_text(aes(label = tm),
            data = nhl_compare %>% filter(GF_2019 < 150),
            vjust = -1) +
  geom_smooth(method = "lm")


#### Goals For Regression to the Mean ----------------------------------------

gf_comp <- nhl_compare %>%
  select(tm, GF_2018, GF_2019)


# Find the 2018 (GF_2018) difference to the mean

gf_comp <- gf_comp %>%
  mutate(diff_to_mean2018 = GF_2018 - mean(GF_2018))


gf_comp %>%
  ggplot(aes(x = diff_to_mean2018, y = reorder(tm, diff_to_mean2018))) +
  geom_col() +
  geom_vline(xintercept = 0, color = "red", size = 1.2) +
  geom_label(aes(label = round(diff_to_mean2018, 1)),
             size = 3)


gf_comp %>%
  ggplot(aes(x = GF_2018, GF_2019)) +
  geom_smooth(method = "lm") +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = tm))

## Linear model for regression to the mean between the 2018 and 2019 seasons

fit_gf <- lm(GF_2019 ~ diff_to_mean2018, data = gf_comp)
summary(fit_gf)

fit_gf_demo <- lm(GF_2019 ~ GF_2018, data = gf_comp)
summary(fit_gf_demo)


# add predictions into the data
gf_comp$Pred_GF <- predict(fit_gf)

# calculate residuals
gf_comp <- gf_comp %>%
  mutate(error = GF_2019 - Pred_GF)

gf_comp %>%
  ggplot(aes(x = Pred_GF, y = error)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  ggrepel::geom_text_repel(aes(label = tm))
  

gf_comp %>%
  ggplot(aes(x = error, y = reorder(tm, error))) +
  geom_col() +
  geom_vline(xintercept = 0, color = "red", size = 1.2) +
  geom_label(aes(label = round(error, 1)),
             size = 3)


gf_comp %>%
  ggplot(aes(
    x = Pred_GF, xend = GF_2019,
    y = reorder(tm, GF_2019))
    ) +
  geom_dumbbell(color = "black",
                size = 1.2,
                size_x = 5,
                size_xend = 5,
                colour_x =  "blue",
                colour_xend = "red") +
  annotate(geom = "text",
           x = 160,
           y = 27.5,
           label = "Predicted Goals",
           color = "blue") +
  annotate(geom = "text",
           x = 160,
           y = 26.5,
           label = "Actual Goals",
           color = "red") +
  geom_vline(
    aes(xintercept = mean(GF_2019)),
    linetype = "dashed"
  ) +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "#CCCCCC")
    )




