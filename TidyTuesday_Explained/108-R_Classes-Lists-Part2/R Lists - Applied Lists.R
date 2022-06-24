
library(tidyverse)
library(palmerpenguins)

### Turn data frame into a list of elements by group

dat <- penguins

dat %>%
  split(.$species)

dat %>%
  split(.$island)

dat %>%
  group_split(island, species)

# data into list with summary stats
dat %>%
  group_by(species) %>%
  group_map(~quantile(.x$bill_length_mm, na.rm = TRUE)) %>%
  setNames(unique(sort(dat$species)))

dat %>%
  group_by(species) %>%
  group_map(~quantile(.x$bill_length_mm, na.rm = TRUE)) %>%
  setNames(unique(sort(dat$species))) %>% 
  data.frame()

## make a list-col
df_listcol <- dat %>%
  select(species, bill_length_mm) %>%
  group_by(species) %>%
  summarize(
    species_bl_quantiles = list(quantile(bill_length_mm, na.rm = TRUE)),
    dataset = list(bill_length_mm)
  )

df_listcol %>% 
  filter(species == "Adelie") %>% 
  pull(species_bl_quantiles)

df_listcol %>% 
  filter(species == "Adelie") %>% 
  pull(dataset)

## Regression Models


# Regression Model (tidy() for nicer tables)
dat %>%
  select(species, bill_length_mm, bill_depth_mm) %>%
  group_by(species) %>%
  group_map(~broom::tidy(lm(bill_depth_mm ~ bill_length_mm, data = .x))) 

# convert the list back to a data frame
dat %>%
  select(species, bill_length_mm, bill_depth_mm) %>%
  group_by(species) %>%
  group_split() %>% 
  map_dfr(function(x){
    data.frame(
      species = unique(x$species),
      broom::tidy(lm(bill_depth_mm ~ bill_length_mm, data = x))
    )
  })

###### base R ############

# by()
by(
  data = dat$bill_length_mm,
  INDICES = dat$island,
  FUN = quantile,
  na.rm = TRUE
)

by(
  data = dat$bill_length_mm,
  INDICES = dat$island,
  FUN = quantile,
  na.rm = TRUE
) %>%
  .[1]

by(
  data = dat$bill_length_mm,
  INDICES = dat$island,
  FUN = quantile,
  na.rm = TRUE
) %>%
  pluck(3)

head(penguins)

## Split The data into a list by each species
species_split <- penguins %>%
  select(species, bill_length_mm, bill_depth_mm) %>%
  group_split(species)

species_split %>% 
  .[1]

species_split %>% 
  `[`(1) 

species_split %>% 
  pluck(1)

species_split %>% 
  .[[1]] 

species_split %>% 
  `[[`(1) 

## Remove column 1 from each subset in the list since it is just the species name and is not numeric
species_split2 <- lapply(species_split, "[", TRUE, -c(1)) 

species_split3 <- list()
for(i in seq_along(species_split)){
  species_split3[[i]] <- species_split[[i]][, -c(1)]
}

## within species correlation
lapply(species_split2, function(x){
  round(cor(x, use = "pairwise.complete.obs"), 2)
})



### Looping 



### remove NA
penguins <- na.omit(penguins)

head(penguins)

theme_set(theme_classic())

### create a plot example
penguins %>%
  ggplot(aes(
    x = sex, 
    y = flipper_length_mm
    )) +
  geom_violin(aes(fill = sex),
              alpha = 0.6,
              draw_quantiles = 0.5) +
  geom_jitter(color = "black") +
  facet_wrap(~species)

## write a function that takes the penguin species and create plot
plot_func <- function(penguin){
  
  penguins %>%
    filter(species == penguin) %>%
    ggplot(aes(x = sex, y = flipper_length_mm)) +
    geom_violin(aes(fill = sex),
                alpha = 0.6,
                draw_quantiles = 0.5) +
    geom_jitter(color = "black") +
    ggtitle(penguin)
  
}

## test out the function
plot_func("Gentoo")

## Use lapply() to loop over each species and create a PDF of one species per page
pdf(width = 12, height = 8, "TidyTuesday_Explained/108-R_Classes-Lists-Part2/penguin.pdf")
lapply(X = unique(penguins$species), FUN = plot_func)
dev.off()

file.show("TidyTuesday_Explained/108-R_Classes-Lists-Part2/penguin.pdf")



