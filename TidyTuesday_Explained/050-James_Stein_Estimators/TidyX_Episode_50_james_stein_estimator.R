## Packages -----------------------------------------------------------
library(tidyverse)
library(Lahman)
library(gt)
library(patchwork)
library(magick)

theme_set(theme_light())

?Lahman::Batting

df <- Lahman::Batting

df %>% head()


## Get Data to build model ---------------------------------------------------

player_df_2018 <- df %>%
  filter(yearID == 2018) %>%
  select(playerID, G, AB, H) %>%
  group_by(playerID) %>%
  summarize(
    across(.cols = c(G:H), .fns = sum),
    .groups = "drop"
    ) %>%
  mutate(batting_avg = ifelse(is.na(H / AB), 0, H / AB))

## James-Stein Estimator for BA
## paper link: https://www.jstor.org/stable/24954030?seq=1

mean(player_df_2018$AB)
quantile(player_df_2018$AB)

mean(player_df_2018$batting_avg)
sd(player_df_2018$batting_avg)

mean(player_df_2018$batting_avg[player_df_2018$AB >= quantile(player_df_2018$AB, probs = 0.5)])
sd(player_df_2018$batting_avg[player_df_2018$AB >= quantile(player_df_2018$AB, probs = 0.5)])


#' @title James Stein Estimator
#' 
#' @description Calculate the James Stein Estimator for Batting Average
#'   z = y_hat + c * (y - y_hat)
#'   c = shrinkage factor = 1 - ((n_obs - degrees_freedom) * sigma^2) / sum((y - y_hat)^2)
#'   degrees_freedom = 3
#'   1-c = shrinkage towards group mean
#' 
#' @param batting_average numeric vector of all players measured batting averages
#' @param at_bats numeric vector of all players number of at bats
#'  
#' @returns vector of equivalent length of estimated batting average for players
#' 
james_stein_est <- function(batting_average, at_bats){
  
  # make sure both vectors are of the same length
  stopifnot(length(batting_average) == length(at_bats))
  
  #which players were in at least the 50th percentile of at bats
  percentile_50 <- at_bats >= quantile(at_bats, probs = 0.5)
  
  # get group mean & sd of BA for for players who were at least in the 
  # 50th percentile of at bats
  group_mean <- mean(batting_average[percentile_50])
  group_sd <- sd(batting_average[percentile_50])
  
  # calc sum squares of players difference from group mean
  sum_sq_diff = sum((batting_average - group_mean)^2)
  
  # calc c
  c <- 1 - ((length(at_bats) - 3) * group_sd^2) / sum_sq_diff

  # given players measured BA and C, what is their estimated BA
  js_estimate <- group_mean + c * (batting_average - group_mean)
  
  return(js_estimate)
}


player_df_2018_js <- player_df_2018 %>%
  mutate(
    james_stein = james_stein_est(batting_avg, AB),
    js_sd = ifelse(AB == 0, 
                  sqrt((james_stein * (1 - james_stein)) / 1), 
                  sqrt((james_stein * (1 - james_stein)) / AB)
                  ),
    low = ifelse(james_stein - js_sd < 0, 0, james_stein - js_sd),
    high = james_stein + js_sd,
    )


player_df_2018_js  %>% head()

## add player information

player_df_2018_js_p <- player_df_2018_js %>%
  left_join(
    Lahman::People %>% 
      mutate(
        name = paste(nameFirst, nameLast)
      ) %>% 
      select(playerID,name),
    by = "playerID"
  )

## plot

set.seed(6723)

player_df_2018_js_p %>%
  filter(AB > 0 & batting_avg > 0) %>%
  sample_n(10) %>% 
  ggplot(
    aes(
      y = name,
      x = batting_avg
    )) +
  geom_errorbar(aes(xmin = low, xmax = high),
                width = 0,
                size = 5,
                color = "light grey",
                alpha = 0.6) +
  geom_point(color = "red",
             aes(size = AB)) +
  geom_point(color = "blue",
             size = 5,
             shape = 124,
             aes(x = james_stein)) +
  annotate(geom = "text",
           x = 0.05,
           y = 9.5,
           hjust = 0,
           label = "Observed Avg",
           color = "red") +
  annotate(geom = "text",
           x = 0.05,
           y = 9.0,
           hjust = 0,
           label = "Estimated True Avg",
           color = "blue")



### 2019 data

player_df_2019 <- df %>%
  filter(yearID == 2019) %>%
  select(playerID, G, AB, H) %>%
  group_by(playerID) %>%
  summarize(across(.cols = c(G:H), .fns = sum),.groups = "drop") %>%
  mutate(batting_avg = ifelse(is.na(H / AB), 0, H / AB)) %>%
  mutate(
    james_stein = james_stein_est(batting_avg, AB),
    js_sd= ifelse(AB == 0, 
                  sqrt((james_stein * (1 - james_stein))) / 1, 
                  sqrt((james_stein * (1 - james_stein)) / AB)),
    low = ifelse(james_stein - js_sd < 0, 0, james_stein - js_sd),
    high = james_stein + js_sd
  ) %>% 
  inner_join(
    player_df_2018_js_p, 
    by = "playerID",
    suffix = c("_2019", "_2018")
  )


player_df_2019 %>% head()


set.seed(84791)
player_df_2019 %>%
  filter(AB_2019 > 0 & batting_avg_2019 > 0) %>%
  sample_n(10) %>%
  ggplot(aes(y = name, x = batting_avg_2019)) +
  geom_errorbar(aes(xmin = low_2018, xmax = high_2018),
                width = 0,
                size = 6,
                color = "light grey",
                alpha = 0.6) +
  geom_point(color = "red",
             aes(size = AB_2019)) +
  
  geom_point(color = "lightblue",
             size = 5,
             shape = 124,
             aes(x = james_stein_2018)) +
  
  geom_point(color = "blue",
             size = 5,
             shape = 124,
             aes(x = james_stein_2019)) + 
  
  annotate(geom = "text",
           x = 0.05,
           y = 9.5,
           hjust = 0,
           label = "Observed Avg",
           color = "red") +
  
  annotate(geom = "text",
           x = 0.05,
           y = 9,
           hjust = 0,
           label = "Estimated True Avg - 2019",
           color = "blue") +
  
  annotate(geom = "text",
           x = 0.05,
           y = 8.5,
           hjust = 0,
           label = "Estimated True Avg - 2018",
           color = "lightblue")


## Make a combo of plots and gt into a single plot!

players <- player_df_2019 %>% 
  filter(AB_2019 > 0 & AB_2018 > 0,
         batting_avg_2019 > 0 & batting_avg_2018 > 0) %>% 
  pull(playerID) %>% 
  sample(size = 10)

vis_data <- player_df_2019 %>% 
  filter(playerID %in% players) %>% 
  arrange(playerID) %>% 
  mutate(
    name = factor(name, levels = name) # preserve 
  )

gt_table <- vis_data %>% 
  arrange(desc(name)) %>%  # order in plots are flipped
  select(
    name,
    G_2018,
    G_2019,
    AB_2018,
    AB_2019,
    batting_avg_2018,
    batting_avg_2019,
    james_stein_2018,
    james_stein_2019
  ) %>% 
  
  gt(
    rowname_col = "name"
  ) %>% 
  
  cols_label(
    G_2018 = "2018",
    G_2019 = "2019",
    AB_2018 = "2018",
    AB_2019 = "2019",
    batting_avg_2018 = "2018",
    batting_avg_2019 = "2019",
    james_stein_2018 = "2018",
    james_stein_2019 = "2019",
  ) %>% 
  
  tab_spanner(
    label = html("<br>Games"),
    columns = starts_with("G")
  ) %>% 
  
  tab_spanner(
    label = html(" <br>At Bats"),
    columns = starts_with("AB")
  ) %>% 
  
  tab_spanner(
    label = html("Batting<br>Average"),
    columns = starts_with("batting_avg")
  ) %>% 
  
  tab_spanner(
    label = html("James Stein<br>Batting Average"),
    columns = starts_with("james_stein")
  ) %>% 
  
  fmt_number(
    c(starts_with("batting_avg"),starts_with("james_stein")),
    decimals = 3
  ) %>% 
  
  tab_style(
    style = cell_text(color = "red"),
    locations = cells_body(
      columns = starts_with("batting_avg")
    )
  ) %>% 
  
  tab_style(
    style = cell_text(color = "blue"),
    locations = cells_body(
      columns = starts_with("james_stein")
    )
  )
  
gt_table

plot_2018 <- vis_data %>% 
  ggplot(aes(y = name, x = batting_avg_2018)) +
  geom_errorbar(aes(xmin = low_2018, xmax = high_2018),
                width = 0,
                size = 6,
                color = "light grey",
                alpha = 0.6) +
  geom_point(color = "red",
             aes(size = AB_2018)) +
  geom_point(color = "blue",
             size = 5,
             shape = 124,
             aes(x = james_stein_2018)) +
  xlim(0,.45) +
  labs(
    title = "2018 Batting Average"
  ) + 
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none"
  ) 

plot_2018

plot_2019 <- vis_data %>% 
  ggplot(aes(y = name, x = batting_avg_2019)) +
  geom_errorbar(aes(xmin = low_2019, xmax = high_2019),
                width = 0,
                size = 6,
                color = "light grey",
                alpha = 0.6) +
  geom_point(color = "red",
             aes(size = AB_2019)) +
  geom_point(color = "blue",
             size = 5,
             shape = 124,
             aes(x = james_stein_2019)) +
  labs(
    title = "2019 Batting Average"
  ) + 
  xlim(0,.45) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) 

plot_2019


### combine it all into a single plot.
## Note, this was inspired by Thomas Mock
## https://gist.github.com/jthomasmock/67892387f23708b94819ef12ee76dc70


### convert to image

gt_temp <- tempfile(fileext = ".png")

gt_table %>% 
  gtsave(
    filename = gt_temp
  )

gt_image <- magick::image_read(gt_temp) %>% 
  image_ggplot(interpolate = TRUE)

gt_image + plot_2018 + plot_2019 +
  plot_layout(
    ncol = 3,
    widths = c(2, 1, 1),
    guides = 'collect') +
  plot_annotation(
    title = "James Stein Estimates",
    subtitle = "2018 & 2019 MLB Batting Averages",
    caption = "Plot: @tidy_explained\nData: {Lahman}",
    theme = theme(
      plot.title = element_text(hjust = .5),
      plot.subtitle = element_text(hjust = .5)
    )
  )

ggsave(
  here::here("TidyTuesday_Explained/050-James_Stein_Estimators/MLB_James_Stein_batting_averages.png"),
  height = 10,
  width = 20,
  dpi = 300
)
