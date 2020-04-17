# Carregar bibliotecas
library(tidyverse)

# Functions
ggplot_tbi_age <- function(data, title){
  ggplot(
    data = data,
    aes(x = injury_mechanism, y = number_est, fill=injury_mechanism)
  ) +
    facet_wrap(~age_group_f) +
    geom_bar(stat="identity") +
    coord_polar() +
    ggtitle(title) +
    xlab("") +
    ylab("Estimated observed cases in 2014") +
    scale_fill_discrete(name = "Injury mechanism") +
    theme(
      axis.text.x=element_blank(),
      axis.ticks = element_blank()
    )
}

# Carregar dados
tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')

# Processar os dados
dados <- tbi_age %>%
  filter(type == "Deaths") %>%
  filter(age_group %in% c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"))
dados$age_group_f <- factor(
  dados$age_group,
  levels=c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
)

# Criar a imagem
ggplot_tbi_age(dados, "Death by Traumatic Brain Injury")

# Salvar a image
ggsave(
  'raniere-silva0.jpeg'
)

# Processar os dados
dados <- dados %>%
  filter(injury_mechanism != "Unintentional Falls")

# Criar a imagem
ggplot_tbi_age(dados, "Death by Traumatic Brain Injury (excluded Unintentional Falls)")

# Salvar a image
ggsave(
  'raniere-silva1.jpeg'
)
