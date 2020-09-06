
## Data
# https://www.kaggle.com/drgilermo/nba-players-stats?select=Seasons_Stats.csv

## Inspiration
# https://projects.fivethirtyeight.com/carmelo/

## Packages -------------------------------------------------------------------
library(tidyverse)
library(shiny)
library(patchwork)
library(ggpubr)
library(here)
library(ggtext)

theme_set(theme_bw() +
            theme(axis.text = element_text(size = 12, face = "bold"),
                  axis.title = element_text(size = 16),
                  panel.border = element_blank(),
                  panel.grid.major = element_line(color = "grey")))

percentile <- function(x){
  a = (rank(x, ties.method = "average") / length(x)) * 100
  a
}

## Data ----------------------------------------------------------------------

stats <- read.csv(here("TidyTuesday_Explained/026-Labels_and_ShinyCARMELO_Part_1/Seasons_Stats.csv"), header = TRUE ) %>%
  filter(Year > 2009) %>%
  select(-X) %>%
  group_by(Player, Year) %>%
  summarize(
    Positions = paste(unique(Pos), collapse = "/"),
    Teams = paste(unique(Tm), collapse = "/"),
    across(c("G", "MP", "FG", "FGA", "X3P", "X3PA", "X2P", "X2PA", "FT", "FTA", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "VORP"), sum, na.rm = T))

vitals <- read.csv(here("TidyTuesday_Explained/026-Labels_and_ShinyCARMELO_Part_1/Players.csv"), header = TRUE) %>%
  select(-X)

## Data Prep ----------------------------------------------------------------

nba_stats <- stats %>%
  filter(between(Year, left = 2015, right = 2017)) %>%
  group_by(Player) %>%
  summarize(across(c("G", "MP", "FGA", "FG", "X3PA", "X3P", "FTA", "FT", "TRB", "AST", "STL", "BLK"), ~sum(.x))) %>%
  filter(MP >= 1000) %>%
  mutate(MPG = MP/G,
         FG_Pct = FG / FGA,
         Three_Pct = X3P / X3PA,
         FT_Pct = FT / FTA,
         TRB_48 = TRB / MP * 48,
         AST_48 = AST / MP * 48,
         STL_48 = STL / MP * 48,
         BLK_48 = BLK /  MP * 48,
         across(c("MPG":"BLK_48"), list(pctl = percentile))
         ) %>% 
  ungroup()

head(nba_stats)

## Helper Functions ----------------------------------------------------------

pivot_summarized_values <- function(.data, ...){
  .data %>%
    select(...) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "value"
    )
}

#### Shiny App -----------------------------------------------------------------------
## ui

ui <- fluidPage(
  
  sidebarPanel(
    width = 3,
    
    selectInput(inputId = "Player",
                label = "Player",
                choices = unique(nba_stats$Player),
                selected = "LeBron James")
    
  ),
  
  mainPanel(
        plotOutput(outputId = "plt_score"),
        plotOutput(outputId = "plt_def")
  )
)


server <- function(input, output){

  
  player_stats <- reactive({
  
    nba_stats %>%
      filter(Player == input$Player)
    
  })
  
  
  scoring_stats <- reactive({
    
    player_stats() %>%
      rename(
        FG_Pct_value = FG_Pct,
        FT_Pct_value = FT_Pct,
        Three_Pct_value = Three_Pct,
        AST_48_value = AST_48
      ) %>%
      pivot_summarized_values(
        FG_Pct_value,
        FT_Pct_value,
        Three_Pct_value,
        AST_48_value,
        FG_Pct_pctl,
        FT_Pct_pctl,
        Three_Pct_pctl,
        AST_48_pctl
      ) %>%
      mutate(
        stat = gsub("(.*_.*)_.*$", "\\1", variable),
        type = gsub(".*_.*_(.*)$", "\\1", variable)
      ) %>%
      pivot_wider(
        id_cols = stat,
        names_from = type,
        values_from = value
        ) %>% 
      mutate(
        value = case_when(
          stat == "AST_48" ~ as.character(round(value,1)),
          TRUE ~  scales::percent(value)
          ),
        stat = case_when(
          stat == "Three_Pct" ~ "3PT%",
          stat == "FT_Pct" ~ "FT%",
          stat == "FG_Pct" ~ "FG%",
          stat == "AST_48" ~ "AST/48"
        ),
        stat = factor(stat, rev(unique(stat))),
        color = case_when(pctl > 75 ~ "Good",
                          pctl < 45 ~ "Bad",
                          TRUE ~ "Average"),
        color = factor(color, levels = c("Bad","Average","Good"))
      )
        
  })
  
  
  defense_stats <- reactive({
    
    player_stats() %>%
      rename(
        TRB_48_value = TRB_48,
        BLK_48_value = BLK_48,
        STL_48_value = STL_48,
      ) %>% 
      pivot_summarized_values(
        TRB_48_value,
        BLK_48_value,
        STL_48_value,
        TRB_48_pctl,
        BLK_48_pctl, 
        STL_48_pctl) %>%
      mutate(
        stat = gsub("(.*_\\d+)_.*$","\\1",variable),
        type = gsub(".*_\\d+_(.*)$","\\1",variable)
      ) %>% 
      pivot_wider(
        id_cols = stat,
        names_from = type,
        values_from = value
      ) %>% 
      mutate(
        value = round(value,1),
        stat = case_when(
          stat == "TRB_48" ~ "TRB/48",
          stat == "BLK_48" ~ "BLK/48",
          TRUE ~ "STL/48"),
        stat = factor(stat, rev(unique(stat))),
        color = case_when(pctl > 75 ~ "Good",
                          pctl < 45 ~ "Bad",
                          TRUE ~ "Average"),
        color = factor(color, levels = c("Bad","Average","Good"))
      )
    
  })
  
  output$plt_score <- renderPlot({
    
    scoring_stats() %>%
      mutate(
        stat_pos = as.numeric(stat)
      ) %>% 
      ggplot() + 
      theme_void() +
      theme(legend.position = "none") +
      geom_text(
        aes(
          x =.1,
          y = stat_pos,
          label = stat
        ),
        fill = "black",
        label.size = 0,
        label.padding = unit(0.1, "lines"),
        hjust = 0,
        size = 8
      ) +
      geom_text(
        aes(
          x = 1.9,
          y = stat_pos,
          label = value
        ),
        fill = "black",
        label.size = 0,
        label.padding = unit(0.1, "lines"),
        hjust = 1,
        size = 8
      ) + 
      
      ## add table lines - vertical
      annotate("segment", 
               x = c( 0, 1, 2, 2.25, 4.25), 
               xend = c( 0, 1, 2, 2.25, 4.25),
               y = .5, yend = 4.5, size = 0.3) +
      
      ## add table lines - horizontal
      annotate("segment",
               y = c(.5, 1.5, 2.5, 3.5, 4.5),
               yend = c(.5, 1.5, 2.5, 3.5, 4.5),
               x = c(0), 
               xend = c(2),
               size = 1) +
      annotate("segment",
               y = c( 4.5),
               yend = c( 4.5),
               x = c(0), 
               xend = c(2),
               size = 2) +
      annotate("segment",
               y = c(.5, 1.5, 2.5, 3.5, 4.5),
               yend = c(.5, 1.5, 2.5, 3.5, 4.5),
               x = c(2.25), 
               xend = c(4.25),
               size = 1) +
      annotate("segment",
               y = c(4.5),
               yend = c(4.5),
               x = c(2.25), 
               xend = c(4.25),
               size = 2) +
      
      ## plot vertical lines
      annotate("segment", 
             x = c( 2.75, 3.75), 
             xend = c( 2.75, 3.75),
             y = .5, yend = 4.5, size = 0.2, color = "grey") +
      annotate("segment", 
               x = c( 3.25), 
               xend = c(3.25 ),
               y = .5, yend = 4.5, size = 2, color = "grey") +
      annotate("text", 
               x = c( 3.25), 
               y = 4.7, 
               label = "50%",
               size = 5, color = "grey") +
      
      ## plot percentiles
      geom_point(
        aes(
          x = (pctl / 50) + 2.25,
          y = stat_pos,
          fill = color,
          size = 10,
        ),
        color = "black",
        shape = 21
      ) +
      
      scale_fill_manual(
        breaks = c("Bad","Average","Good"),
        values = c("red","white","blue")
      ) +
      
      geom_text(
        data = data.frame(
          lab = c("Shooting Statistics","Shooting Percentile"),
          x = c(0,2.25),
          y = c(5,5)
        ),
        aes(
          x = x,
          y = y, 
          label = lab
        ),
        hjust = 0,
        size = 10
      ) 
    
  })
  
  output$plt_def <- renderPlot({
    
    defense_stats() %>%
      mutate(
        stat_pos = as.numeric(stat)
      ) %>% 
      ggplot() + 
      theme_void() +
      theme(legend.position = "none") +
      geom_text(
        aes(
          x =.1,
          y = stat_pos,
          label = stat
        ),
        fill = "black",
        label.size = 0,
        label.padding = unit(0.1, "lines"),
        hjust = 0,
        size = 8
      ) +
      geom_text(
        aes(
          x = 1.9,
          y = stat_pos,
          label = value
        ),
        fill = "black",
        label.size = 0,
        label.padding = unit(0.1, "lines"),
        hjust = 1,
        size = 8
      ) + 
      
      ## add table lines - vertical
      annotate("segment", 
               x = c( 0, 1, 2, 2.25, 4.25), 
               xend = c( 0, 1, 2, 2.25, 4.25),
               y = .5, yend = 3.5, size = 0.3) +
      
      ## add table lines - horizontal
      annotate("segment",
               y = c(.5, 1.5, 2.5, 3.5),
               yend = c(.5, 1.5, 2.5, 3.5),
               x = c(0), 
               xend = c(2),
               size = 1) +
      annotate("segment",
               y = c( 3.5),
               yend = c( 3.5),
               x = c(0), 
               xend = c(2),
               size = 2) +
      annotate("segment",
               y = c(.5, 1.5, 2.5, 3.5 ),
               yend = c(.5, 1.5, 2.5, 3.5 ),
               x = c(2.25), 
               xend = c(4.25),
               size = 1) +
      annotate("segment",
               y = c(3.5),
               yend = c(3.5),
               x = c(2.25), 
               xend = c(4.25),
               size = 2) +
      
      ## plot vertical lines
      annotate("segment", 
               x = c( 2.75, 3.75), 
               xend = c( 2.75, 3.75),
               y = .5, yend = 3.5, size = 0.2, color = "grey") +
      annotate("segment", 
               x = c( 3.25), 
               xend = c(3.25 ),
               y = .5, yend = 3.5, size = 2, color = "grey") +
      annotate("text", 
               x = c( 3.25), 
               y = 3.7, 
               label = "50%",
               size = 5, color = "grey") +
      
      ## plot percentiles
      geom_point(
        aes(
          x = (pctl / 50) + 2.25,
          y = stat_pos,
          fill = color,
          size = 10,
        ),
        color = "black",
        shape = 21
      ) +
      
      scale_fill_manual(
          breaks = c("Bad","Average","Good"),
          values = c("red","white","blue")
      ) +
      
      geom_text(
        data = data.frame(
          lab = c("Defensive Statistics","Defensive Percentile"),
          x = c(0,2.25),
          y = c(4,4)
        ),
        aes(
          x = x,
          y = y, 
          label = lab
        ),
        hjust = 0,
        size = 10
      ) 
      
  })
}


shinyApp(ui, server)
