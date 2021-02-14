

## Packages -----------------------------------------------------------
library(tidyverse)
library(Lahman)
library(gt)
library(sparkline)

theme_set(theme_light())

data(Batting)
df <- Batting

df %>% head()


## Get Some Data ---------------------------------------------------
player_df <- df %>%
  filter(yearID == 2019) %>%
  select(playerID, G, AB, H) %>%
  group_by(playerID) %>%
  summarize(across(.cols = c(G:H), .fns = sum),.groups = "drop") %>%
  mutate(batting_avg = H / AB)

player_df  %>% head()


## Plat Batting Avg Distribution -------------------------------------

player_df %>%
  ggplot(
    aes(x = batting_avg)
    ) +
  geom_histogram(
    fill = "pale green",
    alpha = 0.6
    ) +
  geom_density(
    size = 1.2,
    color = "red"
    ) +
  geom_vline(
    aes(xintercept = mean(batting_avg,na.rm = TRUE)),
    color = "black",
    size = 1.2,
    linetype = "dashed") +
  geom_rug(
    size = 1.1
    ) +
  labs(
    x = "Batting Avg",
    y = "",
    title = "2019 Batting Average Distribution"
    )

player_df %>%
  filter( AB > 200) %>% 
  ggplot(
    aes(x = batting_avg)
  ) +
  geom_histogram(
    fill = "pale green",
    alpha = 0.6
  ) +
  geom_density(
    size = 1.2,
    color = "red"
  ) +
  geom_vline(
    aes(xintercept = mean(batting_avg,na.rm = TRUE)),
    color = "black",
    size = 1.2,
    linetype = "dashed") +
  geom_rug(
    size = 1.1
  ) +
  labs(
    x = "Batting Avg",
    y = "",
    title = "2019 Batting Average Distribution",
    subtitle = "Players wth > 200 AB"
  )

### Player simulation

player_ab <- 100
player_hits <- 20
batting_avg <- player_hits/player_ab

simulated_season_hits <- rbinom(n = 1e4, size = player_ab, prob = batting_avg)
mean(simulated_season_hits)/player_ab
quantile(simulated_season_hits/player_ab, prob = c(.05, 0.95))

### Simulate Each Player 10,000 times and get 90% confidence limits ----------------------

player_df_sim <- player_df %>%
  filter(AB > 0) %>% 
  rowwise() %>%
  mutate(
    simulations = list(rbinom(n = 1e4, size = AB, prob = batting_avg)),
    sim_avg = mean(simulations)/AB,
    Low_CI = quantile(simulations/AB, prob = 0.05),
    High_CI = quantile(simulations/AB, prob = 0.95)
    ) %>% 
  ungroup()

### Table
player_ab_gt <- player_df_sim %>% 
  sample_n(10,replace = FALSE, weight = AB/G) %>% 
  rowwise() %>% 
  mutate(
    confidence_interval = paste("[",round(Low_CI,3), ",", round(High_CI,3),"]"),
    spark_sim = spk_chr(
      simulations,type = "box"
    )
  ) %>% 
  ungroup() %>% 
  left_join(
    Lahman::People %>% 
      mutate(
        name = paste(nameFirst, nameLast)
      ) %>% 
      select(playerID,name),
    by = "playerID"
  ) %>%  
  select( -Low_CI, - High_CI, -playerID, -simulations) %>% 
  
  gt(
    rowname_col = "name",
  ) %>% 
  
  tab_header(
    title = md("**MLB** Player Batting Averages"),
    subtitle = md("*2019*")
  ) %>%  
  
  cols_label(
    batting_avg = html("Batting<br>Average"),
    sim_avg  = html("Simulated<br>Batting Average"),
    confidence_interval = html("90% CI"),
    spark_sim = html("Simulated<br>BA Boxplot")
  ) %>% 
  
  fmt_number(
    vars(batting_avg, sim_avg),
    decimals = 3
  ) %>% 
  
  data_color(
    vars(sim_avg),
    colors = scales::col_numeric(
      palette = c("red","white","blue"),
      domain = NULL)
  ) %>% 
  
  fmt_markdown(columns = vars(spark_sim))
  
player_ab_gt_html <- gt:::as.tags.gt_tbl(player_ab_gt)

player_ab_gt_html <- htmltools::attachDependencies(
  player_ab_gt_html,
  htmlwidgets::getDependency("sparkline")
  )

print(
  player_ab_gt_html,
  browse = interactive()
  )
  
  



