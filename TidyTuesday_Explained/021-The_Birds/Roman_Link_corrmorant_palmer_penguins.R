# If necessary, install corrmorant from Github:
remotes::install_github("r-link/corrmorant")

# Load packages 
pacman::p_load(corrmorant, palmerpenguins, dplyr)

# Get some nice colors
ice     <- "#d7fffe"
beak    <- "#fac205"
sky     <- "#448ee4"
feather <- "#ffffe4"

# Plot with corrmorant::ggcorrm()
penguins %>% 
  select(-year) %>%    # a former version of this gist was based on an old version of palmerpenguins that did not contain year
  ggcorrm(
    aes(col = species, fill = species),                     # color specifications
    labels = c("Culmen length (mm)", "Culmen depth (mm)",   # text labels
               "Flipper length (mm)", "Body mass (g)"),
    bg_dia   = "#444444",                                   # panel background (diagonal, lower and upper triangle)
    bg_lotri = "#333333",
    bg_utri  = "#333333") +
  
  lotri(geom_smooth(alpha = 0.3, method = "lm", size = 0.3)) +  # add geom_smooth to lower triangle
  lotri(geom_point(alpha = 0.3, size = 1)) +                    # add geom_point to lower triangle
  
  utri_corrtext() +                                             # add Pearson correlations to upper triangle
  utri(geom_reltext(
    aes(relx = relx, rely = rely, label = label),  # add penguin emojis (might not work w/ windows)
    data = data.frame(relx = 0.77, rely = 0.25, label = "\U0001F427"),
                    size = 10, inherit.aes = FALSE)) +
  
  dia_names(color = "white") +                                 # add variable names to plot diagonal
  dia_density(alpha = 0.4, size = 0.3) +                       # add density plots to plot diagonal
  
  scale_color_manual(
    values = c(beak, sky, feather),           # add manual color scale
    aesthetics = c("colour", "fill")
  ) +
  
  theme(plot.background = element_rect(fill = "#222222"),      # modify corrmorant standard theme
        legend.background = element_rect(fill = "#222222"),
        axis.text = element_text(colour = ice),
        strip.text = element_text(colour = ice),
        legend.title = element_blank(),
        legend.text = element_text(colour = ice),
        panel.border = element_rect(fill = NULL, size = .6, colour = ice))
