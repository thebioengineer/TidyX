## TidyX Episode 181 - I Likert Coffee

library(tidyverse)

## get tidytuesday coffee rating data and keep only age group and expertise columns
coffee <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv'
  )

filtered_coffee <- coffee %>% 
  filter(
    !is.na(age),
    !is.na(expertise)
    ) %>%
  select(age, expertise) %>%
  mutate(age = factor(age, levels = c(
    "<18 years old",
    "18-24 years old",
    "25-34 years old",
    "35-44 years old",
    "45-54 years old",
    "55-64 years old",
    ">65 years old"
  )))

head(filtered_coffee)

## what is the distribution of coffee expertise?
filtered_coffee %>%
  count(expertise) %>%
  ggplot(aes(x = as.factor(expertise), y = n)) +
  geom_col()

## what does this distribution look like by age?
filtered_coffee %>%
  count(age, expertise) %>%
  ggplot(aes(x = as.factor(expertise), y = n)) +
  geom_col() +
  facet_wrap(~age)

## some groups have smaller participants surveyed. What if we took a stratified sample for 1000
# for each group (with replacement)?
filtered_coffee %>%
  group_by(age) %>%
  slice_sample(n = 1000, replace = TRUE) %>%
  count(age, expertise) %>%
  ggplot(aes(x = as.factor(expertise), y = n)) +
  geom_col() +
  facet_wrap(~age)

## Look at the IQR for each group
filtered_coffee %>%
  select(age, expertise) %>%
  ggplot(aes(x = expertise, y = age)) +
  geom_boxplot()

## Summarize the data for each group
filtered_coffee %>%
  select(age, expertise) %>%
  group_by(age) %>%
  summarize(N = n(),
            avg = mean(expertise),
            sd = sd(expertise),
            middle = median(expertise),
            mad = mad(expertise),
            q25 = quantile(expertise, probs = 0.25),
            q75 = quantile(expertise, probs = 0.75),
            iqr = IQR(expertise)) %>%
  knitr::kable()

### Let's make some comparisons between groups
# A number of different ways to do this with Likert data. 
# Let's start by looking at a t-test, to keep things simple

# What if we compare just two groups?
coffee_t_results <- t.test(
  filtered_coffee$expertise[filtered_coffee$age %in% c("18-24 years old", "25-34 years old")] ~ 
    filtered_coffee$age[filtered_coffee$age %in% c("18-24 years old", "25-34 years old")]
  )
coffee_t_results

### What if we wanted to compare all groups -- Doing this one by one would be a drag!
# let's write a for() loop to do it for us!

## Get all possible comparisons?
comps <- expand.grid(
  grp1 = unique(filtered_coffee$age), 
  grp2 = unique(filtered_coffee$age)
  ) %>%
  filter(grp1 != grp2)

comps

## We have a problem!
comps %>%
  filter((grp1 == "<18 years old" & grp2 == "25-34 years old") |
         (grp1 == "25-34 years old" & grp2 == "<18 years old"))

# Same - Same!

## Walk through a practical way of dropping duplicates, with a smaller data frame
r <- c("a", "b", "c")
r <- factor(r, levels = c("b", "c", "a"))

r_df <- expand.grid(
  r1 = r,
  r2 = r
  ) %>%
  filter(r1 != r2)

r_df

# sort each row
apply(X = r_df, MARGIN = 1, FUN = sort)

# transpose the matrix
t(apply(X = r_df, MARGIN = 1, FUN = sort))

# identify duplicates by row
!duplicated(t(apply(X = r_df, MARGIN = 1, FUN = sort)))

# drop duplicates
r_df[!duplicated(t(apply(X = r_df, MARGIN = 1, FUN = sort))),]



## Now apply to our comps data set
comps <- comps[!duplicated(t(apply(X = comps, MARGIN = 1, FUN = sort))),]

# Sanity check
comps %>%
  filter((grp1 == "<18 years old" & grp2 == "25-34 years old") |
           (grp1 == "25-34 years old" & grp2 == "<18 years old"))

## What aspects of the t-test would we want returned to us from our for() loop?
coffee_t_results

# probably want the difference between the groups, 95% confidence interval, and p-value
names(coffee_t_results)


#### Let's write a for loop
# We need to correct the p-value since we have so many comparisons (21). So let's use
# a Bonferroni correction
alpha <- 0.05
n_comparisons <- nrow(comps)

# P.Value adjustment tool - bonferroni
bonf_correction <- 1 - (alpha / n_comparisons)
bonf_correction

## get a storage data frame
multiple_comps <- data.frame(
  grp1 = comps$grp1,
  grp2 = comps$grp2,
  difference = NA,
  low95 = NA,
  high95 = NA,
  p_value = NA
  )

head(multiple_comps)

for(i in 1:nrow(comps)){
  
  t_test <- filtered_coffee %>% 
    filter(age %in% c(multiple_comps$grp1[i], multiple_comps$grp2[i])) %>% 
    with(t.test(expertise ~ age, conf.level = bonf_correction))
  
  multiple_comps[i, 3] <- t_test$estimate[1] - t_test$estimate[2]
  multiple_comps[i, 4] <- t_test$conf.int[1]
  multiple_comps[i, 5] <- t_test$conf.int[2]
  multiple_comps[i, 6] <- t_test$p.value
}

multiple_comps %>%
  head()

## Apply P-value adjustment for multiple comparisons
multiple_comps %>%
  filter(p_value < 1-bonf_correction)


multiple_comps %>%
  mutate(
    p_adj = p.adjust(p_value, method = "bonferroni")
  ) %>% 
  filter(p_adj < 0.05)

