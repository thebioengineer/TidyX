


library(tidyverse)
library(rvest)
library(shiny)
library(DT)

### Get skater data

skaters <- read_html("https://www.hockey-reference.com/leagues/NHL_2024_skaters.html") %>% 
  html_table(fill = T) %>%
  as.data.frame()

skaters %>%
  head()

colnames(skaters) <- skaters[1, ]
skaters <- skaters[-1, ]

skaters <- skaters %>%
  janitor::clean_names() %>%
  filter(player != "Player")

skaters %>%
  head()

## Get player ids
player_id <- read_html("https://www.hockey-reference.com/leagues/NHL_2024_skaters.html") %>%
  html_nodes("table") %>%
  html_nodes("tbody") %>%
  html_elements("a") %>% 
  html_attr("href") %>%
  as.data.frame() %>%
  setNames("url_info") %>%
  mutate(get_players = ifelse(grepl(pattern = '/players/', url_info), 1, 0)) %>% 
  filter(get_players == 1) %>%
  select(-get_players) %>%
  mutate(player_id = gsub("\\..*","", url_info),
         player_id = gsub(".*/[a-z]/","", player_id))


player_id %>%
  head()

## quick logic check looking for players that may show up muliple times
skaters[skaters$player %in% skaters[which(duplicated(skaters$player)),"player"],]

duplicated(skaters$player)

## join together
skaters <- skaters %>%
  bind_cols(player_id) %>%
  mutate(url_info = paste0("https://www.hockey-reference.com", url_info)) %>%
  rename(link_to_player_page = url_info)

## quick logic check looking for players that may show up muliple times
skaters[skaters$player %in% skaters[which(duplicated(skaters$player)),"player"],]

duplicated(skaters$player)

skaters %>%
  head()

### Shiny Up

## ui

ui <- fluidPage(
  
  title = "2023-2024 NHL Skaters",
  
  sidebarPanel(
    
    selectInput(inputId = "player",
                label = "Players",
                choices = sort(unique(skaters$player)), ## assumes every player is unique
                selected = NULL,
                multiple = TRUE)
    
  ),
  
  mainPanel(
    DTOutput(outputId = "tbl")
  )
)

## server
server <- function(input, output){

  ## get skaters
  dat <- reactive({
    
    skaters %>%
      filter(player %in% input$player)
    
  })
  
  ## output table
  output$tbl <- renderDataTable({
    
    dat() %>% 
      mutate(
        link_to_player_page = paste0("<a href='",link_to_player_page,"'>",link_to_player_page,"</a>")
      ) %>%
      select(
        player, link_to_player_page, everything()
      ) %>% 
      datatable(escape = FALSE)
  })
}

shinyApp(ui, server)


