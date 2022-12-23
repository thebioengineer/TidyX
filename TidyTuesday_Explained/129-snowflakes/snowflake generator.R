## TidyX Episode 129

library(tidyverse)

## code is inspired by 
## https://cloudfour.com/thinks/coding-a-snowflake-generator/

trunk <- function(length){
  
  tibble(
    part = "trunk",
    x_start = c(0),
    x_end = c(0),
    y_start = c(0),
    y_end = c(length)
  )
  
}

branch <- function(length, angle, start){
  
  angle <- angle * (pi/180)
  
  branch_right_x_end <- sin(angle) * length
  branch_left_x_end <- -branch_right_x_end
  branch_y_end <- (cos(angle) * length) + start
  
  tibble(
    part = "branch",
    x_start = c(0,0),
    x_end = c(branch_left_x_end, branch_right_x_end),
    y_start = c(start,start),
    y_end = c(branch_y_end,branch_y_end)
  )
}

trunk_df <- trunk(5)

branch_df <- map_df(
  seq_len(3),
  ~branch(
    length = .x,
    angle = 45,
    start = (5/4)*.x
  )
) 

snowflake_df_base <- rbind(trunk_df, branch_df)

ggplot(snowflake_df_base) + 
  geom_segment(
    aes(x = x_start, y = y_start,
        xend = x_end, yend = y_end)
  )


rotatate_angle <- function(angle, trunk_df){
  
  angle <- angle * (pi/180)
  
  trunk_df %>% 
    mutate(
      x_start_new = x_start*cos(angle) - y_start*sin(angle),
      x_end_new = x_end*cos(angle) - y_end*sin(angle),
      y_start_new = x_start*sin(angle) + y_start*cos(angle),
      y_end_new = x_end*sin(angle) + y_end*cos(angle),
    ) %>% 
    select(
      part,
      ends_with("new")
    ) %>% 
    rename_with(
      .cols = ends_with("new"),
      ~gsub("_new","", .x)
    )
}


generate_snowflake <- function(n_trunk, n_branches, seed = NULL){
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  trunk_l <- runif(1, 1, 5)
  
  trunk_df <- trunk(trunk_l)
  
  branch_df <- map_df(
    seq_len(n_branches),
    ~branch(
      length = runif(1, .1, trunk_l*.5),
      angle = runif(1, 10, 85),
      start = runif(1, .01, trunk_l)
      )
    ) 
  
  snowflake_df_base <- rbind(trunk_df, branch_df)
  
  trunk_angles_new <- setdiff(seq(0, 360, by = 360/n_trunk),c(0,360))
  
  rotated_trunks <-
    map_dfr(trunk_angles_new,
           ~ rotatate_angle(angle = .x, trunk_df = snowflake_df_base))
  
  snowflake_df <- rbind(
    snowflake_df_base, 
    rotated_trunks
    )
  
  ggplot(snowflake_df) + 
    geom_segment(
      aes(x = x_start, y = y_start,
          xend = x_end, yend = y_end)
    ) + 
    theme_void() +
    ggtitle("Happy Holidays from TidyX")
}

n_trunk <- sample(c(1:10), 1)
generate_snowflake(n_trunk = n_trunk, n_branch = 6)

library(camcorder)

gg_record(device = "png", width = 5, height = 5)

for(trunks in 2:10){
  for(branches in 3:10){
    
    snowflake_plot <- generate_snowflake(
      n_trunk = trunks,
      n_branch = branches
    )
    
    print(snowflake_plot)
    
  }
}

gg_playback(
  frame_duration = .15,
  background = "white"
)
