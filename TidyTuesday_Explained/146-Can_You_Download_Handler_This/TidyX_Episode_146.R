# TidyX Episode 146: Can you Download Handler This


#### packages ----------------------------------------------

library(tidyverse)

library(shiny)
library(shinyscreenshot)

library(gridExtra)
library(ggpubr)

## data ----------------------------------------------------
dat <- mtcars %>%
  mutate(
    cyl = as.factor(cyl),
    car_type = rownames(.)
  ) %>%
  relocate(car_type, .before = mpg)

######################################
########## V1 ########################

# print the same table that is shown

ui <- fluidPage(
  
  titlePanel(title = h1("Cars!", align = "center")),
  
  sidebarPanel(
    width = 2,
    
    ## User Input
    selectInput(
      "cyl",
      label = "Choose Cylinder:",
      choices = sort(unique(dat$cyl)),
      selected = NULL,
      multiple = FALSE
    ),
    
    ## Exporting Content from the shiny app
    
    ## Download Content
    downloadButton(outputId = 'export', label = "Download PDF"),
    
    ## Screenshot Shiny
    actionButton(inputId = "screenshot", label = "Screenshot Report"),
    
  ),
  
  
  mainPanel(
    
    ## Row 1
    fluidRow(
      column(width = 6,  plotOutput(outputId = "plt1")),
      column(width = 6, tableOutput(outputId = "tbl"))
    ),
    
    br(),
    
    ## Row 2
    fluidRow(column(
      width = 12, plotOutput(outputId = "plt2")
    ))
  )
  
)


server <- function(input, output){
  
  ## filter cylinder
  cyl_df <- reactive({
    req(input$cyl)
    d <- dat %>% filter(cyl == input$cyl)
    d
  })
  
  ## The Reactive to hold the plots and tables
  vals <- reactiveValues(
    plt1=NULL,
    plt2=NULL,
    tbl=NULL
    )
  
  ## output plt1
  output$plt1 <- renderPlot({
    
    vals$plt1 <- cyl_df() %>%
      ggplot(aes(x = wt, y = mpg)) +
      geom_point(size = 4) +
      theme_bw() +
      labs(x = "wt",
           y = "mpg",
           title = "mpg ~ wt") +
      theme(axis.text = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 15, face = "bold"),
            plot.title = element_text(size = 20))
    
    vals$plt1
    
    
  })
  
  ## output table
  output$tbl <- renderTable({
    
    tbl_df <- cyl_df() %>%
      setNames(
        c(
          "Car Type",
          "MPG",
          "CYL",
          "DISP",
          "HP",
          "DRAT",
          "WT",
          "QSEC",
          "VS",
          "AM",
          "GEAR",
          "CARB"
        )
      )
    
    # store table for printing
    vals$tbl <- ggtexttable(
      tbl_df,
      rows = NULL,
      cols = c(
        "Car Type",
        "MPG",
        "CYL",
        "DISP",
        "HP",
        "DRAT",
        "WT",
        "QSEC",
        "VS",
        "AM",
        "GEAR",
        "CARB"
      ),
      theme = ttheme('minimal', base_size = 12)
    )
    
    # return table for viewing
    tbl_df
    
  })
  
  ## output plt2
  output$plt2 <- renderPlot({
    
    vals$plt2 <-  cyl_df() %>%
      ggplot(aes(x = disp, y = hp)) +
      geom_point(size = 4) +
      theme_bw() +
      labs(x = "disp",
           y = "hp",
           title = "hp ~ disp") +
      theme(axis.text = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 15, face = "bold"),
            plot.title = element_text(size = 20))
    
    vals$plt2
    
  })
  
  ## clicking on the export button will generate a pdf file 
  ## containing all stored plots and tables
  output$export = downloadHandler(
    filename = function() {"plots.pdf"},
    content = function(file) {
      
      ## Init graphic device
      pdf(file, onefile = TRUE, width = 15, height = 9)
      
      ## Arrange and "print" plots
      grid.arrange(vals$plt1,
                   vals$tbl,
                   vals$plt2,
                   nrow = 2,
                   ncol = 2)
      
      ## stop device & save to file 
      dev.off()
      
    })
  
  ## Record screenshot of shiny app
  observeEvent(input$screenshot, {
    screenshot()
  })
  
  
}


shinyApp(ui, server)
