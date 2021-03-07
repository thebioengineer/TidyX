
library(tidyverse)
library(randomForest)
library(shiny)



#### Linear Regression #####

head(iris)
str(iris$Species)
table(iris$Species)

summary(iris)

fit_lm <- lm(Sepal.Length ~ ., data = iris)
summary(fit_lm)

new_df <- data.frame(
  Species = as.factor(c("setosa", "versicolor", "virginica")),
  Sepal.Width = 3.3,
  Petal.Length = 2.1,
  Petal.Width = 0.1
)

new_df

new_df <- bind_cols(new_df, pred_sepal_length = predict(fit_lm, newdata = new_df))
new_df

ui <- fluidPage(
  
  sidebarPanel(
    
    sliderInput(inputId = "Sepal.Width",
                label = "Select Sepal Width:",
                min = min(iris$Sepal.Width),
                max = max(iris$Sepal.Width),
                value = median(iris$Sepal.Width)),
    
    sliderInput(inputId = "Petal.Length",
                label = "Select Petal Length:",
                min = min(iris$Petal.Length),
                max = max(iris$Petal.Length),
                value = median(iris$Petal.Length)),
    
    sliderInput(inputId = "Petal.Width",
                label = "Select Petal Width:",
                min = min(iris$Petal.Width),
                max = max(iris$Petal.Width),
                value = median(iris$Petal.Width))
  ),
  
  plotOutput(outputId = "bar_plot")
  
)


server <- function(input, output){
  
  dat <- reactive({
    
    pred_df <- data.frame(
      Species = as.factor(c("setosa", "versicolor", "virginica")),
      Sepal.Width = input$Sepal.Width,
      Petal.Length = input$Petal.Length,
      Petal.Width = input$Petal.Width
    )
    
    pred_df <- bind_cols(pred_df, pred_sepal_length = round(predict(fit_lm, newdata = pred_df), 1))
    pred_df
  })
  
  output$bar_plot <- renderPlot({
    
    dat() %>%
      ggplot(aes(x = Species, y = pred_sepal_length)) +
      geom_col() +
      geom_label(aes(label = pred_sepal_length)) +
      theme_classic()
    
  })
}


shinyApp(ui, server)



#### Classifcation ######

library(palmerpenguins)

head(penguins)

penguins <- penguins %>%
  drop_na()


fit_rf <- randomForest(
  species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
  data = penguins)

fit_rf

new_df <- data.frame(
  bill_length_mm = 40.2,
  bill_depth_mm = 16.4,
  flipper_length_mm = 202,
  body_mass_g = 3301
)

new_df

pred_species <- predict(fit_rf, newdata = new_df, type = "prob")

pred_species %>%
  data.frame() %>%
  pivot_longer(cols = everything(),
               names_to = "penguin_type",
               values_to = "probability")

## Shiny time!

ui <- fluidPage(
  div( style = "margin:auto;text-align: center; ",
  headerPanel("Penguin Probability")),
  
  fluidRow(
    
    column(3,
           sliderInput(inputId = "bill_length_mm",
                       label = "Select Bill Length (mm):",
                       min = min(penguins$bill_length_mm),
                       max = max(penguins$bill_length_mm),
                       value = median(penguins$bill_length_mm))),
    
    column(3,
           sliderInput(inputId = "bill_depth_mm",
                       label = "Select Bill Depth (mm):",
                       min = min(penguins$bill_depth_mm),
                       max = max(penguins$bill_depth_mm),
                       value = median(penguins$bill_depth_mm))),
    
    column(3,
           sliderInput(inputId = "flipper_length_mm",
                       label = "Select Flipper Length (mm):",
                       min = min(penguins$flipper_length_mm),
                       max = max(penguins$flipper_length_mm),
                       value = median(penguins$flipper_length_mm))),
    
    column(3,
           sliderInput(inputId = "body_mass_g",
                       label = "Select Body Mass (g):",
                       min = min(penguins$body_mass_g),
                       max = max(penguins$body_mass_g),
                       value = median(penguins$body_mass_g)))
  ),
  
  mainPanel( width = 12,
        div( style = "margin:auto;text-align: center; ",
             plotOutput(outputId = "bar_plot", inline = TRUE)
        )
  )
)


server <- function(input, output){
  
  dat <- reactive({
    
    new_df <- data.frame(
      bill_length_mm = input$bill_length_mm,
      bill_depth_mm = input$bill_depth_mm,
      flipper_length_mm = input$flipper_length_mm,
      body_mass_g = input$body_mass_g
    )
    
    pred_species <- predict(fit_rf, newdata = new_df, type = "prob")
    
    pred_species <- pred_species %>%
      data.frame() %>%
      pivot_longer(cols = everything(),
                   names_to = "penguin_type",
                   values_to = "probability")
    
    pred_species
    
  })
  
  output$bar_plot <- renderPlot({
    
    dat() %>%
      ggplot(aes(x = probability, y = penguin_type)) +
      geom_col(fill = "blue",
               alpha = 0.6,
               color = "black") +
      geom_label(aes(label = scales::percent(probability)),
                 size = 5) +
      scale_x_continuous(labels = scales::percent,limits = c(0,1)) +
      theme_bw() +
      labs(
        x = "Probability",
        y = "Penguin Species"
      ) +
      theme(axis.text = element_text(size = 14, face = "bold"),
            axis.title = element_text(size = 17, face = "bold"))
  },
  height = 600,
  width = 800)
}


shinyApp(ui, server)
