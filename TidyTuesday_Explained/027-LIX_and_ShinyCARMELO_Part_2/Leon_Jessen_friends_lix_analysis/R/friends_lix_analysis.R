# Code by Leon Eyrich Jessen, twitter: @jessenleon

# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("cowplot")
library("magick")


# Define functions --------------------------------------------------------
get_data <- function(){
  base_url <- "https://raw.githubusercontent.com/rfordatascience/"
  data_set <- "tidytuesday/master/data/2020/2020-09-08/friends.csv"
  friends_data <- read_csv(file = str_c(base_url, data_set))
  write_tsv(x = friends_data, path = "data/friends_data.tsv")
}
count_long_words <- function(x, len_long){
  x_split <- strsplit(x, split = "\\W+")
  x_split_nchars <- lapply(x_split, nchar)
  n_long_words <- sapply(x_split_nchars, function(x_i){ sum(x_i >= len_long) })
  return( n_long_words )
}
# LIX definition:
# >=55  Very difficult, academic literature at academic level, legal texts.
# 45-54 Difficult, e.g. non-fiction books, popular science works, academic
#       publications.
# 35-44 Medium, e.g. dailies and magazines.
# 25-34 Easy for experienced readers, e.g. weekly literature and fiction for
#       adults.
# <=24  Light text for all readers, e.g. children's literature.
get_lix <- function(x, n_min = 2, len_long = 6){
  o <- str_count(x, "\\w+")
  p <- str_count(x, "[.!?]")
  l <- count_long_words(x, len_long = len_long)
  lix <- o / p + l*100 / o
  lix[o < n_min] = NA
  return(lix)
}


# Load data ---------------------------------------------------------------
# Run only once:
# get_data()
friends_data <- read_tsv(file = "data/friends_data.tsv")


# Wrangle data ------------------------------------------------------------

# Define who are friends
the_friends <- c("Rachel Green", "Ross Geller", "Chandler Bing",
                 "Monica Geller", "Joey Tribbiani", "Phoebe Buffay")

# Find the friends and clean their dialogue
friends_data_clean <- friends_data %>% 
  filter(speaker %in% the_friends) %>% 
  mutate(speaker = speaker %>% str_split(" ") %>% map(1) %>% unlist,
         text = str_replace(text, "[-,;]+$", "\\."),
         text = case_when(str_detect(text, "[.!?]$") ~ text,
                          TRUE ~ str_c(text, ".")),
         text = str_replace_all(text, "[.!?]+", "\\."))

# Augment the data
friends_data_aug <- friends_data_clean %>%
  group_by(speaker, season) %>%
  summarise(all_lines = str_c(text, collapse = " ")) %>%
  ungroup %>%
  mutate(lix = get_lix(all_lines))

# Visualise data ----------------------------------------------------------

# Set plot parameters
xlab <- "Season"
ylab <- "Complexicity of Read Lines (LIX)"
title <- "Friends: Who Uses More Fancy Words?"
subtitle <- "Seasons 1-10"
caption <- "Twitter: @jessenleon"
text_colour <- "grey80"
friends_col <- c("#02B1e6", "#E81D22", "#F9BC15",
                 "#8015f9", "#20e81d", "#e64602")
plot_file <- "visualisations/friends_lix.png"
plot_width <- 16
plot_height <- 9
plot_dpi <- 72

# Create plot
main_plot <- friends_data_aug %>%
  ggplot(aes(x = season, y = lix, colour = speaker, label = speaker)) +
  geom_text(data = filter(friends_data_aug, season %in% c(1, 10)),
            nudge_x = rep(c(-0.5, 0.5), 6),
            nudge_y = c(rep(0, 7), -0.2, rep(0, 4)),
            size = 6) +
  geom_line(lwd = 2,
            alpha = 0.7) +
  scale_x_continuous(breaks = seq(1, 10)) +
  scale_colour_manual(values = friends_col) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none",
        # Plot
        plot.background = element_rect(fill = "black"),
        plot.title =  element_text(colour = text_colour, size = 30),
        plot.subtitle = element_text(colour = text_colour),
        plot.caption = element_text(colour = text_colour, hjust = 0,
                                    face = "italic", size = 12),
        plot.margin = unit(c(20, 100, 100, 20), "points"),
        # Panel
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey20"),
        panel.grid.major.x = element_blank(),
        # Axes
        axis.text.x = element_text(colour = text_colour),
        axis.text.y = element_text(colour = text_colour),
        
        axis.title.x = element_text(colour = text_colour,
                                    margin = unit(c(20, 20, 0, 20),
                                                  "points")),
        axis.title.y = element_text(colour = text_colour,
                                    margin = unit(c(20, 20, 20, 10),
                                                  "points"))) +
  labs(x = xlab,
       y = ylab,
       title = title,
       subtitle = subtitle,
       caption = caption)

# Add picture of friends in lower right corner
main_plot_w_friends <- ggdraw() +
  draw_plot(main_plot) +
  draw_image(image = "images/all_the_friends.png",
             scale = 0.3,
             halign = 1,
             valign = 0)

# Save plot to file -------------------------------------------------------
ggsave(plot = main_plot_w_friends,
       filename = plot_file,
       width = plot_width,
       height = plot_height, dpi = plot_dpi)
