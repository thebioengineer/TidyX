
## Data
# https://www.kaggle.com/drgilermo/nba-players-stats?select=Seasons_Stats.csv

## Inspiration
# https://projects.fivethirtyeight.com/carmelo/

## Packages -------------------------------------------------------------------
library(tidyverse)
library(shiny)
library(patchwork)
library(here)
library(FNN)

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

stats <- read.csv(here("TidyTuesday_Explained/028-Nearest_Neighbor_and_ReactiveValues/Seasons_Stats.csv"), header = TRUE ) %>%
  filter(Year > 2009) %>%
  select(-X) %>%
  group_by(Player, Year) %>%
  summarize(
    Positions = paste(unique(Pos), collapse = "/"),
    Teams = paste(unique(Tm), collapse = "/"),
    across(c("G", "MP", "FG", "FGA", "X3P", "X3PA", "X2P", "X2PA", "FT", "FTA", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "VORP"), sum, na.rm = T))

vitals <- read.csv(here("TidyTuesday_Explained/028-Nearest_Neighbor_and_ReactiveValues/Players.csv"), header = TRUE) %>%
  select(-X)

vorp_ts <- read.csv(here("TidyTuesday_Explained/028-Nearest_Neighbor_and_ReactiveValues/Seasons_Stats.csv"), header = TRUE ) %>%
  filter(Year >= 1990) %>%
  select(-X) %>%
  group_by(Player, Year) %>%
  summarize(across(c("VORP"), sum, na.rm = T)) %>%
  mutate(Experience = seq_along(Player),
         Max_Years = max(Experience)) %>% 
  ungroup()


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
source(here("TidyTuesday_Explained/028-Nearest_Neighbor_and_ReactiveValues/helper_funcs.R"))


#### Shiny App -----------------------------------------------------------------------
## ui

ui <- fluidPage(
  
  title = "2017 NBA Player Statistics",
  
  sidebarPanel(
    width = 3,
    selectizeInput(
      inputId = "Player",
      label = "Player",
      choices = unique(nba_stats$Player),
      selected = "LeBron James",
      multiple = FALSE
    )
  ), 
    
  mainPanel(
      
    # an html output that will update based on the input
    uiOutput(outputId = "PlayerProfile"),
    
    # Tabs for the shiny app
    tabsetPanel(
      
      ## Covered this in Part 1!
      tabPanel(
        title = "Player Data",
        fluid = T,
        plotOutput(outputId = "plt_score"),
        plotOutput(outputId = "plt_def")
      ),
      
      ## Part 2!
      tabPanel(
        title =  "VORP Career Comparison",
        fluid = T,
        column(
          width = 12,
          tags$div(style = "padding:20px;",
          selectizeInput(
            inputId = "Player_VORP_comparison",
            label = "Select Players",
            choices = unique(nba_stats$Player),  
            selected = "",
            multiple = T, 
            width = "100%"
            )),
          
          plotOutput(outputId = "vorp")
          
          )
        ),
      
      tabPanel(
        title =  "VORP Career Twins",
        fluid = T,
        column(
          width = 12,
          tags$div(style = "padding:20px;",
                   
          actionButton(
            inputId = "Player_VORP_KNN_calculate",
            label = "Determine Similar Players and Forecast",
            width = "100%",
            icon = icon("search-plus")
          )),
          
          plotOutput(outputId = "vorp_prediction", height = "400px"),
          plotOutput(outputId = "vorp_twins", height = "600px")
          
        )
      )
      
      )
  ))


server <- function(input, output, session){
  
  #### Player Profile ####
  
  output$PlayerProfile <- renderUI({
    
    if(input$Player != ""){
      
      vital_info <- vitals %>% 
        filter(Player == input$Player) %>% 
        mutate(
          age = 2017 - born
        )
      
      player_year_info <- stats %>%
        ungroup() %>% 
        filter(Player == input$Player) %>% 
        filter(Year == 2017) %>% 
        select(
          Positions, Teams, Games = G
        )
      
      player_name <- gsub("\\W","",strsplit(input$Player,split = " ")[[1]])
      
      tags$div(style = "display:flex; padding-bottom:10px",
               tags$div(
                 tags$h2(input$Player),
                 tags$p(toupper(player_year_info$Teams)),
                 tags$p(toupper(player_year_info$Positions)),
                 tags$p(toupper(paste(vital_info$age,"YEARS OLD"))),
                 tags$p(toupper(paste(player_year_info$Games,"Games Played in 2017")))
               ),
               tags$div(
                 tags$img(
                   src=file.path("https://nba-players.herokuapp.com/Players",player_name[[2]],player_name[[1]]),
                   style = "max-height: 180px; width: auto;"
                   
                 )
               )
      )
    }
    
  })
  
  #### Tab 1: Player Data #####
  player_stats <- reactive({
    
    nba_stats %>%
      filter(Player == input$Player)
  
  })
  
  scoring_stats <- reactive({
    
    if(input$Player != ""){
    
    player_stats() %>%
      rename(
        FG_Pct_value = FG_Pct,
        FT_Pct_value = FT_Pct,
        Three_Pct_value = Three_Pct,
        AST_48_value = AST_48
      ) %>%
      table_summarized_values(
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
                          pctl < 25 ~ "Bad",
                          TRUE ~ "Average"),
        color = factor(color, levels = c("Bad","Average","Good"))
      )
    }
  })
  
  defense_stats <- reactive({
  
    if(input$Player != ""){
    player_stats() %>%
      rename(
        TRB_48_value = TRB_48,
        BLK_48_value = BLK_48,
        STL_48_value = STL_48,
      ) %>% 
      table_summarized_values(
        TRB_48_value,
        BLK_48_value,
        STL_48_value,
        TRB_48_pctl,
        BLK_48_pctl, 
        STL_48_pctl) %>%
      mutate(
        value = round(value,1),
        stat = case_when(
          stat == "TRB_48" ~ "TRB/48",
          stat == "BLK_48" ~ "BLK/48",
          TRUE ~ "STL/48"),
        stat = factor(stat, rev(unique(stat))),
        color = case_when(pctl > 75 ~ "Good",
                          pctl < 25 ~ "Bad",
                          TRUE ~ "Average"),
        color = factor(color, levels = c("Bad","Average","Good"))
      )
    }
    
  })
  
  output$plt_score <- renderPlot({
    if(input$Player != ""){
    scoring_stats()  %>% 
      table_point_plots("Scoring")
    }
  })
  
  output$plt_def <- renderPlot({
    if(input$Player != ""){
    defense_stats() %>% 
      table_point_plots("Defense")
    }
  })
  
  #### Tab 2: VORP Career Comparison #####
  
  # make sure that we can't select a the same player twice!
  observeEvent(input$Player, {
    
    updateSelectizeInput(
      session,
      inputId = "Player_VORP_comparison",
      label = paste("Compare",input$Player,"Against:"),
      choices = setdiff(unique(nba_stats$Player), input$Player),
      selected = setdiff(input$Player_VORP_comparison, input$Player)
    )
    
  })
  
  df_tab2 <- reactive({
    
    d <- stats %>%
      filter(between(Year, left = 2000, right = 2017),
             Player %in% c(input$Player, input$Player_VORP_comparison)) %>%
      group_by(Player, Year) %>%
      summarize(VORP= sum(VORP)) %>% 
      ungroup %>% 
      mutate(
        Player = factor(Player, levels = c(input$Player, input$Player_VORP_comparison))
      )
    
    d
    
  })
  
  output$vorp <- renderPlot({
    
    df_tab2() %>%
      ggplot(aes(x = Year, y = VORP, color = Player)) +
      geom_point(size = 6, position = position_dodge(width = 0.4)) +
      geom_line(size = 1.1,
                linetype = "dashed", position = position_dodge(width = 0.4)) +
      ylim(c(-4, 14))
    
  })
  
  #### Tab 3: VORP Nearest Comparison #####
  
  player_vorp <- reactive({
    
    vorp_ts %>%
      filter(Player == input$Player) %>%
      select(-Year) %>%
      pivot_wider(names_from = Experience,
                  values_from = VORP) %>%
      select( where(~sum(!is.na(.x)) > 0) )
    
  })
  
  KNN_VORP <- reactiveValues(
    knn_df = data.frame(),
    forecast = data.frame(),
    player = ""
  )
  
  observeEvent( input$Player_VORP_KNN_calculate, {
    
    test <- player_vorp()
    
    train <- vorp_ts %>% 
      filter(
        Max_Years >= (unique(Max_Years[Player == input$Player]) + 2 )
      )%>%
      select(-Year) %>%
      pivot_wider(names_from = Experience,
                  values_from = VORP) %>%
      select( where(~sum(!is.na(.x)) > 0))
    

    knn_similarity <- knn(train[,3:(ncol(test))],
             test[,3:ncol(test)],
             cl = train$Player,
             k = 4,
             algorithm = "kd_tree")
    
    knn_similar_df <- bind_rows(test, train[c(attr(knn_similarity, "nn.index")), ])
    
    KNN_VORP$knn_df <- knn_similar_df
    
    forecast <- knn_similar_df %>%
      pivot_longer(cols = matches("\\d+"),
                   names_to = "Experience",
                   values_to = "VORP") %>%
      mutate(Experience = as.numeric(Experience)) %>%
      filter(
        Player != input$Player,
        Experience > max(Max_Years[Player == input$Player])
      ) %>%
      group_by(Experience) %>%
      summarize(
        VORP = mean(VORP,na.rm = TRUE)
        ) %>% 
      filter(!is.na(VORP))
    
    player_calc_vorp <- test %>%
      pivot_longer(cols = 3:last_col(),
                   names_to = "Experience",
                   values_to = "VORP") %>% 
      bind_rows(
        data.frame(
          Player = input$Player,
          VORP = forecast$VORP
        )
      ) %>% 
      mutate(
        Experience = row_number(),
        Max_Years = unique(as.numeric(na.exclude(Max_Years))),
        Pred_Years = ifelse(Experience > Max_Years, 1, 0)
      ) %>% 
      bind_rows(
        .[.$Experience == .$Max_Years,] %>% 
          mutate(Pred_Years = 1)
      ) %>% 
      arrange(Experience, Pred_Years)
    
    KNN_VORP$forecast <- player_calc_vorp
    KNN_VORP$player <- input$Player
    
  })
  
  output$vorp_prediction <- renderPlot({
    
    if(input$Player == KNN_VORP$player){
      
      plot_dat <- KNN_VORP$forecast
      
    }else{
      
      plot_dat <- vorp_ts %>%
        filter(Player == input$Player) %>% 
        mutate(
          Pred_Years = 0
        )
      
    }
  
    plot_dat %>%
      mutate(
        Projection = factor(Pred_Years, levels = c("0","1"))
      ) %>% 
      ggplot(aes(
        x = Experience,
        y = VORP,
        group = Pred_Years,
        color = Projection
      )) +
      geom_hline(
        yintercept = 0,
        size = 1, 
        color = "#383838"
      ) + 
      geom_vline(
        aes(xintercept = Max_Years),
        size = 1, 
        color = "#383838"
      ) +
      geom_line(aes(
        linetype = Projection
        ),
        size = 2
      ) +
      geom_point(size = 3,
                 shape = 1) +
      scale_linetype_manual( values = c("solid", "dashed") ) +
      scale_color_manual( values = c("Black", "Grey") ) +
      ylim(c(-4, 15))+
      labs(
        title = "Career Value over Replacement",
        y = "",
        x = "Years in NBA") + 
      theme(
        legend.position = "none",
        title = element_text(size = 20, face = "bold")
      )
  })
  
  
  output$vorp_twins <- renderPlot({
    
    if(input$Player == KNN_VORP$player){
      
      KNN_VORP$knn_df %>%
        filter(Player != input$Player) %>%
        mutate(
          Player = paste(as.numeric(factor(Player, levels = unique(Player))),"-", Player),
          Player = factor(Player, levels = unique(Player)) ## Preserve order
        ) %>% 
        pivot_longer(cols = 3:last_col(),
                     names_to = "Experience",
                     values_to = "VORP") %>%
        mutate(
          Experience = as.numeric(Experience)
        ) %>% 
        ggplot(aes(x = Experience,
                   y = VORP),
               color = "#383838") +
        geom_hline(
          yintercept = 0,
          size = 1, 
          color = "#383838"
        ) +
        geom_vline(
          xintercept = unique(KNN_VORP$forecast$Max_Years),
          size = 1, 
          color = "#383838"
        ) +
        geom_line(size = 2) +
        geom_point(size = 3) +
        facet_wrap( ~ Player, ncol = 2, scales = "free_x") +
        scale_color_manual(values = c("Black", "Grey")) +
        ylim(c(-4, 15)) +
        labs(
          title = "Most Similar Players Career Value over Replacement",
          y = "",
          x = "Years in NBA"
        ) +
        theme(
          strip.text = element_text(size = 15,face = "bold"),
          strip.background = element_rect(fill = "white"),
          title = element_text(size = 20, face = "bold")
        )
    
    }
  })
  
}


shinyApp(ui, server)
