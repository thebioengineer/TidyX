
##### TidyX Episode 3: Polar Plots & Shiny Apps ######
######################################################

### Load packages ----------------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(rvest)
library(shiny)
library(conflicted)

#how to handle overloaded functions
conflict_prefer(name = "pluck", winner = "purrr")
conflict_prefer(name = "filter", winner = "dplyr")

theme_set(theme_bw())

### Scrape Data ------------------------------------------------------------------------
# www.basketball-reference.com
# Per minute data

# get URL
url <- read_html("https://www.basketball-reference.com/leagues/NBA_2019_totals.html")

# Extract the table of interest form the per webpage

nba <- url %>% 
  html_table(fill = T) %>% 
  .[[1]] %>% # purrr::pluck is a method for doing the same thing that is more "pipe-like"
  # pluck(1) %>% 
  filter(Rk != "Rk")


### Data Pre-process ------------------------------------------------------------------------

nba %>% glimpse()

# Chanage columns to numeric
numeric_cols <- c(1, 4, 6:30)
nba[,numeric_cols] <- apply(nba[,numeric_cols], 2, function(x) as.numeric(as.character(x)))
nba %>% glimpse()

# Get the necessary columns
nba_main <- nba %>%
  select(Player,
         G,
         MP,
         FG,
         FGA,
         Three_Pt = '3P',
         TRB,
         AST,
         BLK)

# Some players played for multiple teams so aggregate over the season
nba_main <- nba_main %>%
  group_by(Player) %>%
  summarize_at(vars(G:BLK), .funs = sum)

# What was the range of minutes played?
quantile(nba_main$MP)

# Put the play stats on a per-minute basis and only retain those who played at or above 1174 min (1174 / 82 = 14 min/game)
nba_main <- nba_main %>%
  group_by(Player) %>%
  summarize(MP = max(MP),
            FG_min = FG / MP,
            FGA_min = FGA / MP,
            Three_Pt_min = Three_Pt / MP,
            TRB_min = TRB / MP,
            AST_min = AST / MP,
            BLK_min = BLK / MP) %>% 
  filter(MP >= median(MP)) %>%
  select(-MP)

nba_main %>% head()

### Normalize all columns -------------------------------------------------

# z-score function - Explain what a z-score is. This now opens the potential to maybe move away from
# just tidy tuesday specific and maybe more general explaining R scripts we find!
z_score <- function(x){
  z = (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
  return(z)
}

# calculate z-scores
nba_main <- nba_main %>%
  mutate_at(vars(FG_min:BLK_min), .funs = z_score)

# t-score function
t_score <- function(x){
  t = (x * 10) + 50
  t = ifelse(t > 100, 100, 
             ifelse(t < 0, 0, t))
  return(t)
}

# calculate t-scores
nba_main <- nba_main %>%
  mutate_at(vars(FG_min:BLK_min), .funs = t_score)

# Now everything is on a scale of 1 to 100
nba_main %>%
  melt(., id = "Player", measure.vars = c(2:7)) %>%
  #pivot_longer(cols = 2:7, names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() +
  coord_flip() +
  theme_light()

### Put data into long format for plotting ----------------------------------

nba_long <- nba_main %>%
  rename('FG\nPer min' = FG_min,
         'FGA\nPer min' = FGA_min,
         '3PT\nPer min' = Three_Pt_min,
         'Reb\nPer min' = TRB_min,
         'AST\nPer min' = AST_min,
         'BLK\nPer min' = BLK_min) %>%
  melt(., id = "Player", measure.vars = c(2:7))

### Create Polar Plots ------------------------------------------------------
# Single Polar Plot Examples

nba_long %>%
  filter(Player == "LeBron James") %>%
  ggplot(aes(x = variable, y = value, fill = variable)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "x") +
  geom_hline(yintercept = seq(50, 50, by = 1), size = 1.2) +
  theme(
    axis.text.x = element_text(face = "bold", size = 12),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  labs(x = "", y = "")

nba_long %>%
  filter(Player %in% c("LeBron James", "Steven Adams")) %>%
  ggplot(aes(x = variable, y = value, fill = Player)) +
  geom_col(aes(color = variable), alpha = 0.75, size = 1., width = 0.85, position = position_dodge(width = 1)) +
  coord_polar(theta = "x") +
  geom_hline(yintercept = seq(50, 50, by = 1), size = 1.2) +
  theme(axis.text.x = element_text(face = "bold", size = 12),
        legend.title = element_blank(),
        legend.position = "bottom") +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("blue", "green")) +
  scale_color_manual(values = c("red", "black", "orange", "brown", "purple", "yellow"))

# Option 1 looks clean
# Option 2 looks busy
# For Shiny app create two plots, one for each player, to allow for comparisons

##################################
######### Shiny App ##############
##################################

# Get raw data for a table to accompany the visual
nba_main <- nba %>%
  select(Player,
         G,
         MP,
         FG,
         FGA,
         Three_Pt = '3P',
         TRB,
         AST,
         BLK) %>%
  group_by(Player) %>%
  summarize_at(vars(G:BLK), .funs = sum) %>% 
  filter(MP >= median(MP))

# Create a vector of unique player names
Player <- unique(nba_long$Player)


### Create Shiny User Interface ---------------------------------------------------

ui <- fluidPage(
  titlePanel("2019 NBA Performance"),
  mainPanel(width = 12,
            splitLayout(
                    cellWidths = c("50%", "50%"),
                    div(
                      selectInput(
                        input = "player1",
                        label = "Player 1",
                        choices = Player,
                        selected = "LeBron James",
                        width = "100%"
                      ),
                      plotOutput(outputId = "plot1")
                    ),
                    div(
                      selectInput(
                        input = "player2",
                        label = "Player 2",
                        choices = Player,
                        width = "100%"
                      )
                      ,
                      plotOutput(outputId = "plot2")
                    )
                  ),
                  tableOutput(outputId = "player.table")
                ))


### Create Shiny Server ----------------------------------------------------------

server <- function(input, output){
  
  # create data sets for plotting
  dat1 <- reactive({
    dataset1 <- subset(nba_long, Player == input$player1)
    dataset1
  })
  
  dat2 <- reactive({
    dataset2 <- subset(nba_long, Player == input$player2)
    dataset2
  })
  
  dat3 <- reactive({
    dataset3 <- subset(nba_main, Player %in% c(input$player1, input$player2))
    dataset3
  })
  
  
  ## create plots
  output$plot1 <- renderPlot({
    d1 <- dat1()
    
    plot1 <- ggplot(d1, aes(x = variable, y = value, fill = variable)) +
      geom_col(color = "white", width = 0.9) +
      coord_polar(theta = "x") +
      geom_hline(yintercept = seq(50, 50, by = 1), size = 1.2) +
      theme(axis.text.x = element_text(face = "bold", size = 10),
            legend.title = element_blank(),
            legend.position = "none",
            strip.background = element_rect(fill = "black"),
            strip.text = element_text(face = "bold", color = "white", size = 13)) +
      labs(x = "", y = "") +
      facet_wrap(~Player) +
      ylim(0, 100) +
      scale_fill_brewer(palette="Spectral")
    
    print(plot1)
  })
  
  output$plot2 <- renderPlot({
    d2 <- dat2()
    
    plot2 <- ggplot(d2, aes(x = variable, y = value, fill = variable)) +
      geom_col(color = "white", width = 0.9) +
      coord_polar(theta = "x") +
      geom_hline(yintercept = seq(50, 50, by = 1), size = 1.2) +
      theme(axis.text.x = element_text(face = "bold", size = 10),
            legend.title = element_blank(),
            legend.position = "none",
            strip.background = element_rect(fill = "black"),
            strip.text = element_text(face = "bold", color = "white", size = 13)) +
      labs(x = "", y = "") +
      facet_wrap(~Player) +
      ylim(0, 100) +
      scale_fill_brewer(palette="Spectral")
    
    print(plot2)
  })
  
  ## create player stats tables
  output$player.table <- renderTable(dat3(), align = "l")
}


### Run the shiny app ------------------------------------------------------------
shinyApp(ui = ui, server = server)

