## TidyX Episode 123 - Criss Cross Apple Sauce

library(tidyverse)

## Crossing your data - vectors ----

full_cross <- crossing(
  my_vec_1 = c(1,2,3),
  my_vec_2 = c("A","B")
)

full_cross2 <- crossing(
  my_vec_1 = c(1,2,3),
  my_vec_2 = c("A","B"),
  my_vec_3 = c(.1,.2,.3)
)

## Recycling - drops duplicates
full_cross_recycle <- crossing(
  my_vec_1 = c(1,2,3),
  my_vec_2 = c("A","B"),
  my_vec_3 = c(.1,.2,.1)
)

### Crossing your data - data.frame ----

full_cross_df <- crossing(
  tibble(
    col_1 = c(1,2,3),
    col_2 = c("A","B","C")
  ),
  my_vec_3 = c(.1,.2,.3)
)

full_cross_full <- crossing(
    col_1 = c(1,2,3),
    col_2 = c("A","B","C"),
    my_vec_3 = c(.1,.2,.3)
)

full_cross_df2 <- crossing(
  tibble(
    col_1 = c(1,2,3),
    col_2 = c("A","B","C")
  ),
  tibble(
    my_vec_3 = c(.1,.2),
    my_vec_4 = c(TRUE, FALSE)
  )
)


## Expand & Nesting

subset_full_cross_df <- tibble(
  col_1 = c(1,1,1,2,2,2,3,3,3),
  col_2 = c("A","B","C","A","B","C","A","B","C"),
  vals = c(1,2,3,4,5,6,7,8,9)
  ) 

## all potential combinations
subset_full_cross_df %>%
  slice(-c(1,5)) %>% 
  expand(col_1, col_2)

## unique combinations
subset_full_cross_df %>%
  bind_rows(subset_full_cross_df) %>% 
  expand(nesting( col_1, col_2))

## 
subset_full_cross_df %>%
  bind_rows(subset_full_cross_df) %>% 
  distinct(col_1, col_2)

## useful for creating data quickly!
tracking <- tribble(
  ~play,    ~player,     ~team, ~x, ~y,
      1,      "tim", "bobcats",  3,  5,
      1,   "seneca", "bobcats",  6, 10,
      1, "chistine", "bobcats",  1, 12,
      1,    "ellis",  "sharks",  4, 17,
      1,  "patrick",  "sharks",  5, 20,
      1,  "melissa",  "sharks",  3, 18
)

cart_dists <- tracking %>%
  crossing(tracking, .name_repair = "universal") %>%
  setNames(c("play", "player", "team", "x", "y", "playsecond", "player_rel", "team_rel", "x_rel", "y_rel")) %>%
  select(-playsecond) %>%
  filter(player != player_rel) %>% 
  mutate(
    dist = sqrt((x-x_rel)^2 + (y-y_rel)^2)
  )

cart_dists

tracking %>%
  left_join(tracking, by = "play") %>%
  filter(player.x != player.y) %>%
  arrange(player.x) %>% 
  mutate(
    dist = sqrt((x.x-x.y)^2 + (y.x-y.y)^2)
  )


