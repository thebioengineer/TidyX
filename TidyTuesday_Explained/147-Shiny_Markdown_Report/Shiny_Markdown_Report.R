# TidyX Episode 147 - Shiny Markdown Report

## Load Libraries
library(tidyverse)
library(shiny)
library(rmarkdown)
library(shinyjs)
library(Lahman)


## Load data

lahman_dataset <- Batting %>%
  #Person information
  left_join(People) %>% 
  # Get the position played by the person that year
  left_join(Fielding %>% select(playerID, yearID, POS)) %>% 
  mutate(
    first_season = substr(debut, start = 1, stop = 4), # Had a full date 
    name = paste(nameLast, nameFirst, sep = ", ")
  ) %>%
  filter(first_season >= 2010) %>%
  mutate(
    obp = (H + BB + HBP) / (AB + BB + HBP + SF),
    batting_avg = H / AB
  ) %>%
  select(playerID, name, yearID, POS, AB, batting_avg, obp)

## Construct App

### UI

ui <- fluidPage(
  
  ## enables shinyjs
  useShinyjs(),
  
  sidebarLayout(
    
  sidebarPanel = sidebarPanel(
      
      ## Select Players
      selectizeInput(
        inputId = "selected_player",
        label = "Select Player ID",
        choices = unique(lahman_dataset$playerID),
        multiple = TRUE,
        width = "100%"
      ),
      
      hr(),

      ## Start these buttons off as disabled
      disabled(
        actionButton("render_report_button","Render Report",width = "100%")
      ),
      disabled(
        downloadButton("download_rendered_report", "Download Report", style = "width:100%;")
      )
      
    ),
  
  mainPanel = mainPanel(
      ## simple plots
      plotOutput(outputId = "ab_vs_obp"),
      plotOutput(outputId = "batting_avg_vs_obp")
    )
  )
)

### Server

server <- function(input, output, session){
  
  
  dat_players <- reactive({
    
    req(input$selected_player)
    
    lahman_dataset %>% 
      filter(playerID %in% input$selected_player)
  })

  ## Make the AB vs OBP Plot
  ab_vs_obp_plot <- reactive({
    dat_players() %>% 
      ggplot() + 
      geom_point(aes(x = AB, y = obp, color = name)) + 
      labs(
        x = "At Bats",
        y = "On Base %",
        title = "AB vs OBP"
      ) +
      theme_minimal()
  })
  
  ## Make the BA vs OBP Plot
  batting_avg_vs_obp_plot <- reactive({
    dat_players() %>% 
      ggplot() + 
      geom_point(aes(x = batting_avg, y = obp, color = name)) + 
      labs(
        x = "Batting Average",
        y = "On Base %",
        title = "AB vs OBP"
      ) +
      theme_minimal()
  })
  
  ## Send plots to output
  output$ab_vs_obp <- renderPlot({ab_vs_obp_plot()})
  output$batting_avg_vs_obp <- renderPlot({batting_avg_vs_obp_plot()})
  
  
  ## enable rendering if players are selected, else, disable everything
  observeEvent(input$selected_player,{
    if(!is.null(input$selected_player)){
      enable("render_report_button")
    }else{
      disable("render_report_button")
      disable("download_rendered_report")
      report_loc(NULL)
    }
  }, ignoreNULL = FALSE)
  
  
  ## Render report on click. store result to reactive val
  report_loc <- reactiveVal()
  observeEvent(input$render_report_button,{
    
    ## always write to a new dir
    output_dir <- tempfile()
    dir.create(output_dir)
    
    ## store data from shiny in env for report, getting values from reactive
    render_env <- new.env()
    render_env$dat_players <- dat_players()
    render_env$ab_vs_obp_plot <- ab_vs_obp_plot()
    render_env$batting_avg_vs_obp_plot <- batting_avg_vs_obp_plot()
    
    render(
      input = "TidyTuesday_Explained/147-Shiny_Markdown_Report/sample_markdown_report.Rmd", ## capitalization must match
      output_dir = output_dir,
      envir = render_env
    )
    
    ## update report_loc to have rendered file
    report_loc(list.files(output_dir, full.names = TRUE))
  })
  
  ## enable downloading if report is rendered
  observeEvent(report_loc(),{
    if(!is.null(report_loc())){
      enable("download_rendered_report")
    }else{
      disable("download_rendered_report")
    }
  }, ignoreNULL = FALSE)
  
  ## download rendered report!
  output$download_rendered_report <- downloadHandler(
    filename = function(){
      basename(report_loc())
    },
    content = function(file){
      file.copy(report_loc(), file, overwrite = TRUE)
    }
  )
  
}

shinyApp(ui = ui, server = server, options = c(launch.browser = TRUE))
