
library(tidyverse)
library(rvest)
library(shiny)
library(DT)
library(janitor)

### Get skater data

nhl_skaters <- function(year){
  
  skater_url <- paste0("https://www.hockey-reference.com/leagues/NHL_",year,"_skaters.html")
  
  skater_html <- read_html(skater_url) 
  
  skater_df <- skater_html %>% 
    html_elements("#stats") %>% 
    html_elements("tr") %>% 
    .[-1] %>%  ## Drop first element, which is the table spaning headers
    map(function(tr){
    
      table_row_cells <- tr %>% 
        html_children()
      
      row_text <- table_row_cells %>% 
        html_text() %>% 
        as.list()   
      
      skater_url <- table_row_cells %>%
        .[[2]] %>% 
        html_children() %>% 
        html_attr("href") %>% 
        as.list()
      
      if(length(skater_url) == 0){
        skater_url <- list("skater_url")
      }
      
      skater_stats <- as.data.frame(
        c(skater_url, row_text),
        ) %>% 
        suppressMessages()
      
      names(skater_stats) <- paste0("x", 1:ncol(skater_stats))
      
      skater_stats
      
    }) %>% 
    bind_rows() 
  
  colnames(skater_df) <- skater_df[1, ]
  skater_df <- skater_df[-1, ]
  
  skater_df %>% 
    clean_names() %>%  
    rename(
      skater = player
    ) %>% 
    mutate(
      skater_id = basename(skater_url) %>% tools::file_path_sans_ext(),
      skater_url = paste0("https://www.hockey-reference.com", skater_url),
      unique_skater_name_id = paste0(skater," - ",skater_id)
    )
  
}

### Shiny UI

## ui

ui <- fluidPage(
  
  titlePanel("NHL Skaters"),
  
  sidebarPanel(
    
    ## THIS IS NEW CODE
    ### <<<< FROM HERE
    
    selectInput(inputId = "year",
                label = "Select Season", 
                choices = 2024:1950),
    ### <<<< To HERE
    
    selectInput(inputId = "skater",
                label = "Skaters",
                choices = NULL,
                selected = NULL,
                multiple = TRUE)
  ),
  
  mainPanel(
    DTOutput(outputId = "tbl")
  )
)

## server
server <- function(input, output, session){
  
  ## THIS IS NEW CODE
  ### <<<< FROM HERE
  
  selectedSkater <- reactiveVal()
  
  ## update based on URL parameters
  observe({
    
    query <- parseQueryString(session$clientData$url_search)
    
    if(!is.null(query[['year']])){
      updateSelectInput(session, "year", selected = query[['year']])
    }
    
    if(!is.null(query[['skater']])){
      selectedSkater(query[["skater"]])
    }
  })
  
  
  ## given year, get set of available skaters
  skaters <- reactive({
    nhl_skaters(input$year)
  })
  

  ## Update skater options to reflect current season
  observeEvent(skaters(),{
    updateSelectInput(
      session = session,
      inputId = "skater",
      ## starting with "Choose Skater" prevents values from being selected initially
      choices = c(
        "Choose Skater"="",
        unique(skaters()$unique_skater_name_id)
      ),
      selected = selectedSkater()
    )
  })  

  ### <<<< To HERE

  ## get skaters
  dat <- reactive({
    skaters() %>%
      filter(unique_skater_name_id %in% input$skater)
  })
  
  ## output table
  output$tbl <- renderDataTable({
    
    dat() %>% 
      mutate(
        skater_url = paste0("<a href='",skater_url,"' target = '_blank'>",skater_url,"</a>")
      ) %>%
      select(
        skater, skater_id, skater_url, everything(), -unique_skater_name_id
      ) %>% 
      datatable(
        escape = FALSE
      )
    
  })
  
}

shinyApp(ui, server)


