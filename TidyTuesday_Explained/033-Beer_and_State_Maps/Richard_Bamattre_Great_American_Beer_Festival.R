# Tidy Tuesday 2020-10-20
# Great American Beer Festival
# Richard Bamattre @rbamattre

# load
library(tidytuesdayR) # Get tidy tuesday data

# wrangle
library(tidyverse) # of course
library(janitor) # clean column names and nice tables

# Import fonts 
library(extrafont) # use fonts

# viz
#install.packages("statebins", repos = c("https://cinc.rud.is", "https://cloud.r-project.org/"))
library(statebins) # square arranged bins for the the U.S. states
library(ggtext) # use Markdown in ggplot2

# load data
tuesdata <- tidytuesdayR::tt_load('2020-10-20')

# Most awarded style by State?

beer <- tuesdata$beer_awards %>%
  mutate(state = toupper(state),
         medal = factor(medal, levels = c("Gold", "Silver", "Bronze"),
                        ordered = TRUE)) %>% # make sure these get arranged
  mutate(style = case_when(str_detect(category, "Lager|Pilsener|Pilsners|Bock") ~ "Lager/Pilsner/Bock",
                           str_detect(category, "India Pale Ale") ~ "IPA",
                           str_detect(category, "Pale Ale|Amber Ale") ~ "Pale/Amber Ale",
                           str_detect(category, "Porter|Stout|Rye|Scottish-Style Ale|Schwarzbier|Barrel-Aged") 
                           ~ "Porter/Stout/Rye/Barrel Aged",
                           str_detect(category, "Belgian-|Belgo") ~ "Belgian Style",
                           str_detect(category, "Brown Ale") ~ "Brown Ale",
                           str_detect(category, "Red Ale") ~ "Red Ale",
                           str_detect(category, "Wheat Beer|Wheat Ale|Witbier") ~ "Wheat Beer",
                           str_detect(category, "Sour") ~ "Sour",
                           TRUE ~ "Something Else") %>%
           # lots of sub-styles - group into major beer styles
           factor())

# Count these major styles
beer %>%
  count(style)

# Most common unclaimed beers
# Using this view to determine what beers are being missed
beer %>%
  filter(style == "Something Else") %>%
  count(category, sort = TRUE) %>%
  print(n = 50)

# Which states are just going to have a top beer
# that's "Something else"?
# If these are very obscure beer styles, maybe that's okay
# not to group them
beer %>%
  count(state, category, style) %>%
  group_by(state) %>%
  arrange(state, -n) %>% # arrange by n descending
  slice(1) %>%
  arrange(category) %>%
  filter(style == "Something Else") %>%
  print(n = 50)

# Top awarded beer styles by state 
top_styles <- beer %>%
  count(state, category, style) %>%
  #filter(style != "Something Else") %>%
  group_by(state) %>%
  arrange(state, -n) %>% # get the most awarded styles by state
  slice(1) %>% # only the top one
  ungroup() %>%
  bind_rows(tribble(~state, ~style,
                    "WV", "None")) %>% # West Virginia isn't included, add manually
  mutate(style = fct_relevel(style, "Lager/Pilsner/Bock", "IPA", "Pale/Amber Ale", 
                             "Belgian Style", "Wheat Beer", "Sour", "Red Ale", 
                             "Brown Ale", "Porter/Stout/Rye/Barrel Aged", "Something Else", 
                             "None"))
# Ensure styles are arrange roughly by color (looks nice)

# Turns out, no Sour or Red Ales as the top awarded beer

# To create more accurate beer color scheme, borrowed colors from
# this analysis (malt colors as defined by SRM Units):
# https://tech.webinterpret.com/calculating-beer-color-in-js/

top_styles %>%
  ggplot(aes(state = state, fill = style)) + # aesthetics passed on to statebins
  geom_statebins(radius = grid::unit(5, "pt"), # more rounded bins
                 na.rm = FALSE, # keep states with no beers
                 border_size = 2, 
                 size = 1, 
                 border_col = "#242F40") +
  theme_statebins(base_family = "Bahnschrift") +
  scale_fill_manual(values = c("#F6D281", # lager
                               "#EBAA31", # IPA
                               "#CF7200", # pale ale
                               "#88522B", # Belgian Style
                               "#A63C00", # wheat beer
                               #"#8AA000", # Sour
                               #"#5B0000", # red ale
                               "#4B0002", # Brown Ale
                               "#0E0001", # porter
                               "#4D5E31",# something else
                               "darkgray" # none?
  ),
  name = "Overall Beer Styles") +
  labs(title = "Great American Beers",
       subtitle = str_wrap("<br><span style = 'font-weight: 200'>The Professional Judge Panel awards gold, silver or bronze medals 
  that are recognized around the world as symbols of brewing excellence. These awards are 
  among the most coveted in the industry.</span><br><br>
                      <span style = 'color:#EBAA31'> This figure looks at the most awarded beer style by state, from 1987-2020.</span>",
                           width = 100),
       caption = "Tidy Tuesday | @rbamattre | Data Source: Great American Beer Festival") +
  theme(plot.background = element_rect(fill = "#242F40"), # tweaking theme to make it look nice
        text = element_text(color = "white"),
        legend.title = element_blank(),
        legend.position = "right",
        legend.background = element_rect(fill = NA, color = NA),
        legend.key = element_rect(fill = NA),
        plot.title = element_text(hjust = 0.5, size = 30, color = "#EBAA31"),
        plot.subtitle = element_textbox_simple(hjust = 0.5))

# ggsave("/Tidy Tuesday/2020-10-20 beer.png", device = "png", dpi = 150)