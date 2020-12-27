library(tidyverse)
library(directlabels)

tusdata <- tidytuesdayR::tt_load("2020-12-22")

mac <- tusdata[[1]]

mac %>%
  filter(date == "2020-07-01") %>%
  select(date, name, dollar_price) %>%
  top_n(5)

mac %>%
  filter(date == "2020-07-01") %>%
  select(date, name, dollar_price) %>%
  top_n(-5)

'%!in%' <- function(x,y)!('%in%'(x,y))

p <- mac %>%
  select(date, name, dollar_price) %>%
  mutate(top_bottom = case_when(
    name %in% c("Switzerland", "Lebanon",
                "Norway", "Sweden", 
                "United States") ~ "top5",
    name %in% c("Mexico", "Russia", 
                "Turkey", "Ukraine", 
                "South Africa") ~ "bottom5",
    name %!in% c("Switzerland", "Lebanon",
                 "Norway", "Sweden", 
                 "United States",
                 "Mexico", "Russia", 
                 "Turkey", "Ukraine", 
                 "South Africa") ~ "other")
  ) %>%
  ggplot() +
  geom_line(aes(x = date, y = dollar_price,
                group = name, color = top_bottom,
                alpha = top_bottom)) +
  scale_color_manual(values = c("#FFC72C", "darkgrey", "black")) +
  scale_alpha_manual(values = c(1, .6, 1)) +
  scale_x_date(limits = as.Date(c("2000-01-01", "2023-01-01"))) + 
  labs(title = "Big Mac Index, 2000-2020",
       subtitle = "Top 5 & Bottom 5 Countries with Most Expensive Big Mac Prices, 2020",
       x = "Year",
       y = "Dollar Price (USD)") +
  annotate("text",
           x = as.Date(c("2020-09-01", "2020-09-01", "2020-09-01", "2020-09-01", "2020-09-01")),
           y = c(6.91, 6.25, 6, 5.75, 5.5),
           hjust = 0,
           label = c("Switzerland", "Lebanon", "Sweden", "United States", "Norway")) +
  annotate("text",
           x = as.Date(c("2020-09-01", "2020-09-01", "2020-09-01", "2020-09-01", "2020-09-01")),
           y = c(2.5, 2.27, 2.04, 1.85, 1.63),
           hjust = 0,
           label = c("Mexico", "Ukraine", "Turkey", "Russia", "South Africa"),
           color = "#FFC72C") +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(family = "Helvetica", face = "bold", color = "#FFC72C"),
    plot.background = element_rect(fill = "#DA291C"),
    panel.background = element_rect(fill = "#DA291C"),
    plot.title = element_text(size = rel(1.8)),
    plot.subtitle = element_text(size = rel(1.3)),
    axis.line = element_line(color = "#FFC72C"),
    panel.grid = element_line(color = "#FFC72C"),
    panel.grid.major = element_line(linetype = "dashed"),
    axis.text = element_text(color = "#FFC72C", size = rel(1.05)),
    axis.title = element_text(size = rel(1.2))
  )

p

ggsave("big_mac.pdf",
       plot = p,
       width = 7.49,
       height = 4.91,
       units = "in")