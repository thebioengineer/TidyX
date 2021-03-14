## Artificial data setup

library(palmerpenguins)
library(tidyverse)
library(here)

add_noise <- function(x){
  x + rnorm(length(x), sd = sd(x))
}

add_species_noise <- function(x, prob_accurate = 1){
  
  species <- as.character(unique(x))
  output <- rep("",length(x))
  
  for(i in seq_len(length(x))){
    
    original <- x[i]
    probs <- rep(1, length(species))
    probs[species == x[i]] <- prob_accurate
    
    output[i] <- sample(species, size = 1, prob = probs)
  }
  
  output
  
}

penguins2 <- penguins %>% 
  mutate(
    ## add noise to the measurements
    bill_length_mm = add_noise(bill_length_mm),
    bill_depth_mm = add_noise(bill_depth_mm),
    flipper_length_mm = add_noise(flipper_length_mm),
    body_mass_g = add_noise(body_mass_g)
  ) %>% 
  mutate(
    ## make the species incorrect
    species = sample(c("Adelie","Chinstrap","Gentoo"),size = n(),replace = TRUE,prob = c(1,1,5))
  ) 

penguins3 <- penguins %>% 
  mutate(
    ## add noise to the measurements
    bill_length_mm = add_noise(bill_length_mm),
    bill_depth_mm = add_noise(bill_depth_mm),
    flipper_length_mm = add_noise(flipper_length_mm),
    body_mass_g = add_noise(body_mass_g)
  ) %>% 
  mutate(
    ## make the species incorrect
    species = add_species_noise(species, 6)
  ) 

write_csv(
  penguins2%>% 
  sample_n(100),
  here("TidyTuesday_Explained/052-Xaringan-Presentations/palmer_penguins_2_100.csv")
)

write_csv(
  penguins2%>% 
    sample_n(200),
  here("TidyTuesday_Explained/052-Xaringan-Presentations/palmer_penguins_2_200.csv")
)

write_csv(
  penguins3,
  here("TidyTuesday_Explained/052-Xaringan-Presentations/palmer_penguins_3.csv")
)



