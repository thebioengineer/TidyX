## TidyX Episode 145 - Multi-Input Shiny

library(tidyverse)
library(shiny)
library(DT)

z_score <- function(x){
  
  z = (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  return(z)
  
}

set.seed(362)
d <- tibble(
    player = rep(c("Bob", "Jeff", "Kevin"), each = 35),
    day = rep(seq(1:35), times = 3),
    var1 = round(rnorm(n = 35*3, mean = 1200, sd = 300),0),
    var2 = round(rnorm(n = 35*3, mean = 100, sd = 20), 0),
    var3 = round(rnorm(n = 35*3, mean = 20, sd = 5), 1)
  ) %>%
  group_by(player) %>%
  mutate(
    across(.cols = var1:var3,~z_score(.x),.names = "{col}_z"),
    var1_flag = ifelse(abs(var1_z) >= 1.5, "flag", "no flag"),
    var2_flag = ifelse(abs(var2_z) >= 1.5, "flag", "no flag"),
    var3_flag = ifelse(abs(var3_z) >= 1.5, "flag", "no flag")
  ) %>% 
  ungroup()


d

#### shiny app

variables <- c("var1", "var2", "var3")

ui <- fluidPage(
  
  tabsetPanel(
    id = "my_tabsetpanel",
    
    tabPanel(
      "Flags Table",
      DTOutput(outputId = "flag_tbl")
    ),
    
    tabPanel(
      "Plots",
      sidebarPanel(
        
        selectInput(inputId = "player",
                    label = "Name",
                    choices = unique(d$player),
                    selected = NULL,
                    multiple = FALSE),
        
        selectInput(inputId = "variables",
                    label = "Variable",
                    choices = variables,
                    selected = NULL,
                    multiple = FALSE)
      ),
      
      mainPanel(
        plotOutput(outputId = "plt")
      )
      
    )
  )
)

############################
########## Server ##########
server <- function(input, output, session){
  
  ## Table Tab ----
  
  ### create flag table
  output$flag_tbl <- renderDataTable({
    d %>% 
      select(-var1, -var2, -var3, -ends_with("_z"))
    },server = FALSE)
  
  
  ### We'll come back to this in a second ----
  ### update plt_dat if selecting a row in the dt
  observeEvent(input$flag_tbl_rows_selected, {
    
    s <- input$flag_tbl_rows_selected
    
    playername <- d %>% 
      slice(s) %>% 
      pull(player)
    
    updateSelectInput(inputId = "player", selected = playername)
    updateTabsetPanel(inputId = "my_tabsetpanel",selected = "Plots")
    
  })
  
  
  ## Plotting Tab ----
  
  ### update player_dat to be the player level data ----
  player_dat <- reactive({
    
    d %>%
      filter(player == input$player) %>%
      select(day, var1, var2, var3) %>%
      pivot_longer(cols = c("var1", "var2", "var3"))
    
  })
  
  
  ### generate plotting data ----
  plt_dat <- reactive({
    player_dat() %>% 
      filter(name == input$variables)
  })
  
  
  ### plot the data ----
  output$plt <- renderPlot({
    plt_dat() %>%
      ggplot(aes(x = day, y = value)) +
      geom_line() + 
      ggtitle(label = paste0(input$player,", ",input$variables))
    
  })

 
}



shinyApp(ui, server)
