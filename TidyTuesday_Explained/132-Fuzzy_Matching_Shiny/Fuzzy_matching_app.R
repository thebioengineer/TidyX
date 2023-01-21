#TidyX Episode 132

library(shiny)
library(tidyverse)
library(DT)

## Function performing fuzzy matching from TidyX Episode 127 (bit.ly/TidyX_Ep127)
get_preferred_name <- function(db_names, preferred_names, max_dist = .2){
  
  db_names_out <- vector("character",length(db_names))
  for(name in preferred_names){
    preferred_name_loc <- agrepl(
      pattern = name,
      x = db_names,
      max.distance = max_dist, ## the smaller the number, the less "fuzzy" 
      fixed = TRUE
    )
    db_names_out[preferred_name_loc] <- name
  }
  db_names_out %>% 
    na_if("")
}


# Define UI for application ----
ui <- fluidPage(

    # Application title
    titlePanel("Fuzzy Matching"),

    # Left hand panel for inputs 
    column(
      width = 4,
      fileInput(
        inputId = "raw_file",
        label = "Upload a csv of raw data to match",
        accept = c("csv")
      ),
      fileInput(
        inputId = "ref_file",
        label = "Upload reference file (txt) with a new line for reach name",
        accept = "txt"
      )
    ),
    
    column(
      width = 8,
      uiOutput("col_select_ui"),
      DT::dataTableOutput("fuzzy_match_preview"),
      uiOutput("download_ui")
    )
)

# Server function for the app ----
server <- function(input, output) {
  
  ## read in files
  raw_dat <- reactive({
    csv_file <- input$raw_file
    req(csv_file)
    validate(need(tools::file_ext(csv_file$datapath) == "csv", "Please upload a csv file"))
    read_csv(csv_file$datapath)
  })
  
  ref_vec <- reactive({
    ref_file <- input$ref_file
    req(ref_file)
    validate(need(tools::file_ext(ref_file$datapath) == "txt", "Please upload a txt file"))
    readLines(ref_file$datapath,warn = FALSE)
  })
  
  ## Select a column from the raw file to fuzzy match on and max dist
  output$col_select_ui <- renderUI({
    
    req(raw_dat())
    
    tagList(
        selectInput(
          inputId = 'raw_file_colname',
          label = "Select column to fuzzy match to",
          choices = colnames(raw_dat())
        ),
        numericInput(
          inputId = "fuzzy_match_dist",
          label = "Fuzzy Matching Distance (see ?agrep max.distance)",
          min = 0.01,
          max = 0.9,
          value  = 0.2,
          step = 0.05
        )
      )
  })
  
  ## Perform fuzzy match
  raw_dat_matched <- reactive({
    
    req(input$raw_file_colname)
    req(input$fuzzy_match_dist)
    
    dat <- raw_dat()
    
    raw_vec <- dat[[input$raw_file_colname]]
    preferred_names <- get_preferred_name(raw_vec, ref_vec(), max_dist = input$fuzzy_match_dist)
    
    
    
    ### add_column from tibble package!
    dat <- add_column(
      .data = dat,
      !!paste0(input$raw_file_colname,"_ref") := preferred_names, 
      .after = input$raw_file_colname
      )
    
    dat
  
  })
  
  ## Preview Match Results
  output$fuzzy_match_preview <- DT::renderDataTable({
    raw_dat_matched() %>% 
      DT::datatable()
  })
  
  ## Provide download handler
  output$download_ui <- renderUI({
    req(raw_dat_matched())
    downloadButton('fuzzy_match_download',label = "Download Matched Data")
  })
  
  output$fuzzy_match_download <- downloadHandler(
    filename = function(){
      input$raw_file$name %>% 
        basename() %>% 
        tools::file_path_sans_ext() %>% 
        paste0("_fuzzy_matched.csv")
    },
    content = function(file){
      write_csv(
        raw_dat_matched(),
        file = file
      )
    }
  )
}

# Run the application in the browser
shinyApp(ui = ui, server = server)
