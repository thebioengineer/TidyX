
### Episode 119: Random sampling from groups in tidyverse

library(tidyverse)

## Create data
group <- c("A", "A", "A", 
           "B", "B", "B", "B", "B", "B", 
           "C", "C", "C", "C", "C", "C", "C", "C", "C", "C")
var1 <- rnorm(n = length(group), mean = 0, sd = 1)
var2 <- rnorm(n = length(group), mean = 10, sd = 5)
dat <- data.frame(group, var1, var2)

dat %>% 
  head()


## Randomly Sample 4 observations
dat %>%
  slice_sample(n = 4)

## Randomly Sample 4 observations from each group with replacement
dat %>%
  group_by(group) %>%
  slice_sample(n = 4, replace = TRUE)


## Take a random sample based on a percentage of observations (e.g., sample 1/4
## of the observations)
dat %>%
  count(group)

dat %>%
  group_by(group) %>% 
  sample_frac(size = 0.25, replace = TRUE)


dat %>%
  group_by(group) %>% 
  slice_sample(prop = 0.25, replace = TRUE)

?slice_sample

## Take different samples per group so that each group has 5 observations
set.seed(12346)

dat_group_samp <- bind_rows(
  
  dat %>%
    filter(group == "A") %>%
    slice_sample(n = 5, replace = TRUE), # w replacement
  
  dat %>%
    filter(group == "B") %>%
    slice_sample(n = 5, replace = TRUE), # w replacement
  
  dat %>%
    filter(group == "C") %>%
    slice_sample(n = 5, replace = FALSE) # w/o replacement
  )

dat_group_samp

dat_group_samp %>%
  count(group)

