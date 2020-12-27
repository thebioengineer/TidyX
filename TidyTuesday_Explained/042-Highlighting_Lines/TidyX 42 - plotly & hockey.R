

### packages & functions --------------------------------------------
library(tidyverse)
library(plotly)
library(shiny)
library(rvest)

z_score <- function(x){
  z = (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  return(z)
}

### Webscrape & Data Clean Up ----------------------------------------

## create object from reading your html as a separate step
## prevents re-requesting the html page every time
hockey_url <- "https://www.hockey-reference.com/leagues/NHL_2020_skaters.html"
hockey_html <- hockey_url %>%
  read_html() 

hockey_table <- hockey_html %>% 
  html_node('table') %>%
  html_table(fill = TRUE) %>%
  setNames(1:ncol(.)) 

nhl <- hockey_table %>% 
  select(2,5,6,7,8) %>% 
  rename(player = 1,
         pos = 2,
         games = 3,
         goals = 4,
         assists = 5) %>%
  filter(player != "Player") %>%
  group_by(player) %>%
  summarize(
    across(.cols = c(games:assists),
           ~sum(as.numeric(.x))),
    pos = pos[which.max(games)],
    positions = paste0(pos," (G:", games,", S:", goals,", A:", assists,")", collapse= "; "),
    n = n(),
    .groups = "drop"  ## new feature in dplyr, allows you to skip the ungroup
    ) %>%
  filter(games > 27) %>%
  mutate(assist_z = z_score(assists),
         goals_z = z_score(goals)) %>%
  distinct(player, .keep_all = TRUE) ## ASK PATRICK if intended, this will keep the first recorded value for the player

nhl %>% head()

### plotly -----------------------------------------------------

## select players in search box
player_search <- highlight_key(nhl, ~player)


## create the plot
nhl_plt <- 
  plot_ly(
    player_search,
    color = I("black")
    ) %>%
  group_by(player) %>%
  add_markers(
    x = ~assist_z,
    y = ~goals_z,
    marker = list(size = 10),
    symbol = ~pos,
    hovertemplate = ~paste0(
              "Player: ", player, "<br>",
              "Goals: ", goals, " (",round(goals_z,2),")","<br>",
              "Assists: ", assists, " (",round(assist_z,2),")"
            )) %>%
  layout(
    title = "NHL Goals & Assists\nQualifying: Minimum of 27 games",
    xaxis = list(title = "Assists"),
    yaxis = list(title = "Goals")
  )

## add the player search box
highlight(
  nhl_plt,
  on = "plotly_click",
  selectize = TRUE,
  dynamic = TRUE,
  persistent = TRUE
)


### Make a shiny with table data ####

ui <- fluidPage(
  plotlyOutput(outputId = "plt"),
  tableOutput(outputId = "tbl")
)


server <- function(input, output, session){
  

  # output plot
  output$plt <- renderPlotly({
    
    # if the player is selected, color him red
    
    highlighted_players <- nhl %>% 
      mutate(
        player_color = case_when(
          player %in% selected_players() ~ "red",
          TRUE ~ "grey")
      )
    
    highlighted_players %>% 
      filter(player_color == "red") %>% 
      print()
    
    highlighted_players %>% 
      plot_ly() %>%
      add_trace(
        mode = "markers",
        x = ~ assist_z,
        y = ~ goals_z,
        color = ~ I(player_color),
        customdata = ~ player,
        symbol = ~ pos,
        hovertemplate = ~ paste0(
          "Player: ",
          player,
          "<br>",
          "Goals: ",
          goals,
          " (",
          round(goals_z, 2),
          ")",
          "<br>",
          "Assists: ",
          assists,
          " (",
          round(assist_z, 2),
          ")"
        )
      ) %>% 
      layout(
        title = "NHL Goals & Assists\nQualifying: Minimum of 27 games",
        xaxis = list(title = "Assists"),
        yaxis = list(title = "Goals")
      )
  })
  
  # keep track of which players have been selected
  selected_players <- reactiveVal()
  
  # On click, the key field of the event data contains the car name
  # Add that name of newly selected players to the set of all players that have been selected
  observeEvent(event_data("plotly_click"), {
    new_selected <- event_data("plotly_click")$customdata
    all_players_selected <- c(selected_players(), new_selected)
    selected_players(unique(all_players_selected))
  })
  
  
  # clear the table when the plot is double clicked
  observeEvent(event_data("plotly_doubleclick"), {
    selected_players(NULL)
  })
  
  
  # table
  output$tbl <- renderTable({
    nhl %>%
      filter(player %in% selected_players()) %>%
      select(Player = player, Pos = pos, Games = games, Goals = goals, Assists = assists)
  })
  
}


shinyApp(ui, server)

