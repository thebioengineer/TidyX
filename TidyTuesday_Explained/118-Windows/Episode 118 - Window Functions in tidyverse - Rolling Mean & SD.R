
## Episode 118: Window Functions in tidyverse: Rolling Mean & SD

library(Lahman)
library(tidyverse)
library(zoo)

theme_set(theme_light())

## Get career home runs for Albert Pujols (data is only through 2020)
pujols <- Batting %>%
  left_join(People) %>%
  filter(nameFirst == "Albert", nameLast == "Pujols") %>%
  mutate(Name = paste(nameFirst, nameLast, sep = " ")) %>%
  select(Name, yearID, AB, HR) %>%
  janitor::clean_names()

pujols

## plot home runs over career
pujols %>%
  ggplot(aes(x = year_id, y = hr)) +
  geom_line(size = 1.1,
            color = "red")

## Calculate a 3 year rolling average and rolling standard deviation and plot them

pujols %>%
  mutate(roll_avg = rollapply(hr, width = 3, FUN = mean, fill = NA, align = "right"),
         roll_sd = rollapply(hr, width = 3, FUN = sd, fill = NA, align = "right")) %>%
  ggplot(aes(x = year_id, y = hr)) +
  geom_ribbon(aes(ymin = roll_avg - roll_sd, ymax = roll_avg + roll_sd),
              fill = "light grey",
              alpha = 0.6) +
  geom_line(aes(y = roll_avg),
            linetype = "dashed",
            size = 1.1) +
  geom_line(size = 1,
            color = "red",
            alpha = 0.8) +
  geom_point(shape = 21,
             color = "red",
             alpha = 0.8,
             size = 4,
             fill = "white") +
  annotate(geom = "text",
           x = 2004,
           y = 20,
           label = "- Observed HR",
           color = "red") +
  annotate(geom = "text",
           x = 2004,
           y = 17,
           label = "-- 3yr Rolling Average ± SD",
           color = "black")

pujols %>%
  mutate(roll_avg = rollapply(hr, width = 3, FUN = mean, fill = NA, align = "right"),
         roll_sd = rollapply(hr, width = 3, FUN = sd, fill = NA, align = "right")) %>% 
  head()

pujols %>%
  mutate(roll_avg = rollapply(hr, width = 3, FUN = mean, fill = NA, align = "left"),
         roll_sd = rollapply(hr, width = 3, FUN = sd, fill = NA, align = "left")) %>% 
  tail()

## Calculate rolling average and standard deviation year to date
# This looks back all the way to the start so it won't capture acute trends as easily
# cumulative mean/sd over career
pujols %>%
  mutate(
    row_id = seq_along(year_id),
    roll_avg = rollapply(hr, width = row_id, FUN = mean, fill = NA, align = "right"),
    roll_sd = rollapply(hr, width = row_id, FUN = sd, fill = NA, align = "right")
  ) %>%
  ggplot(aes(x = year_id, y = hr)) +
  geom_ribbon(aes(ymin = roll_avg - roll_sd, ymax = roll_avg + roll_sd),
              fill = "light grey",
              alpha = 0.6) +
  geom_line(aes(y = roll_avg),
            linetype = "dashed",
            size = 1.1) +
  geom_line(size = 1,
            color = "red",
            alpha = 0.8) +
  geom_point(shape = 21,
             color = "red",
             alpha = 0.8,
             size = 4,
             fill = "white") +
  annotate(geom = "text",
           x = 2004,
           y = 20,
           label = "- Observed HR",
           color = "red") +
  annotate(geom = "text",
           x = 2006,
           y = 17,
           label = "-- Season-to-Date Rolling Average ± SD",
           color = "black")


## Calculate a 5 year non-overlapping window for rolling average and standard deviation
# add result to the original data set

pujols %>%
  mutate(
    roll_avg = rollapply(hr, width = 5, by = 5, FUN = mean, fill = NA, align = "right"),
    roll_sd = rollapply(hr, width = 5, by = 5, FUN = sd, fill = NA, align = "right")
    )

# add result to the original data set and fill values dowm
pujols %>%
  mutate(roll_avg = rollapply(hr, width = 5, by = 5, FUN = mean, fill = NA, align = "right"),
         roll_sd = rollapply(hr, width = 5, by = 5, FUN = sd, fill = NA, align = "right")) %>%
  fill(roll_avg, .direction = "down") %>%
  fill(roll_sd, .direction = "down") %>%
  ggplot(aes(x = year_id, y = hr)) +
  geom_ribbon(aes(ymin = roll_avg - roll_sd, ymax = roll_avg + roll_sd),
              fill = "light grey",
              alpha = 0.6) +
  geom_line(aes(y = roll_avg),
            linetype = "dashed",
            size = 1.1) +
  geom_line(size = 1,
            color = "red",
            alpha = 0.8) +
  geom_point(shape = 21,
             color = "red",
             alpha = 0.8,
             size = 4,
             fill = "white")


## create result in its own summarized data frame
pujols %>%
  summarize(
    roll_avg = rollapply(hr, width = 5, by = 5, FUN = mean, align = "right"),
    roll_sd = rollapply(hr, width = 5, by = 5, FUN = sd, align = "right")
  ) %>%
  mutate(obs_group_id = row_number())

## Calculate a 5 year window with a 2 year overlap for rolling average and standard deviation 
# add result to the original data set

pujols %>%
  mutate(roll_avg = rollapply(hr, width = 5, by = 2, FUN = mean, fill = NA, align = "right"),
         roll_sd = rollapply(hr, width = 5, by = 2, FUN = sd, fill = NA, align = "right"))

(37+34+43+46+41) / 5
(43+46+41+49+32) / 5

# add result to the original data set and fill values dowm
pujols %>%
  mutate(roll_avg = rollapply(hr, width = 5, by = 2, FUN = mean, fill = NA, align = "right"),
         roll_sd = rollapply(hr, width = 5, by = 2, FUN = sd, fill = NA, align = "right")) %>%
  fill(roll_avg, .direction = "down") %>%
  fill(roll_sd, .direction = "down") %>%
  ggplot(aes(x = year_id, y = hr)) +
  geom_ribbon(aes(ymin = roll_avg - roll_sd, ymax = roll_avg + roll_sd),
              fill = "light grey",
              alpha = 0.6) +
  geom_line(aes(y = roll_avg),
            linetype = "dashed",
            size = 1.1) +
  geom_line(size = 1,
            color = "red",
            alpha = 0.8) +
  geom_point(shape = 21,
             color = "red",
             alpha = 0.8,
             size = 4,
             fill = "white")

## create result in its own summarized data frame
pujols %>%
  summarize(roll_avg = rollapply(hr, width = 5, by = 2, FUN = mean, align = "right"),
            roll_sd = rollapply(hr, width = 5, by = 2, FUN = sd, align = "right")) %>%
  mutate(obs_group_id = row_number())
