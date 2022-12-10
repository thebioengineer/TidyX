# TidyX Episode 127 - Fuzzy Wuzzy Joiny Tools

library(tidyverse)
library(fuzzyjoin)

name_bank <- tribble(
  ~name, ~team,
  "Bob Jones", "bulls",
  "Cam Hanson JR", "bisons",
  "BJ Franklin", "bigfoots",
  "BJ Franklin", "bisons"
)

name_bank

df_perf <- tribble(
  ~name, ~team, ~performance,
  'Bob Jones', "bulls", 5,
  "Cam Hanson Jr.", "bisons", 10,
  "B.J. Franklin", "bigfoots", 3,
  "B J Franklin", "bisons", 6,
  "Cam Hanson JR", "bisons", 8,
  "Cma Hnsn J", "bisons", 8
)

df_perf


## Fuzzy String matches in Base R

agrep(
  pattern = "Cam Hanson Jr.",
  x = df_perf$name,
  max.distance = .5,## the smaller the number, the less "fuzzy"  
  fixed = TRUE
)

## lets make a function, assuming every name is unique :grimace:
get_preferred_name <- function(db_names, preferred_names, max_dist = .2){
  
  db_names_out <- vector("character",length(db_names))
  for(name in preferred_names){
    preferred_name_loc <- agrepl(
      pattern = name,
      x = db_names,
      max.distance = max_dist, ## the smaller the number, the less "fuzzy" 
      fixed = TRUE
    )
    db_names_out[preferred_name_loc] <- name
  }
  db_names_out %>% 
    na_if("")
}

df_perf %>% 
  mutate(
    preferred_names = get_preferred_name(name, unique(name_bank$name),max_dist = .5)
  ) %>% 
  relocate(
    preferred_names, .before = name
  ) %>% 
  rename(
    original_name = name
  )
  


## Fuzzy Name Match (fuzzyjoin package)

df_perf %>%
  mutate(
    name_id = tolower(name),
    name_id = gsub("\\.", "", name_id),
    name_id = gsub(" ", "", name_id),
    id = paste(name_id, team, sep = "_")
  ) %>%
  fuzzy_left_join(
    name_bank %>% 
      mutate(
        name = tolower(name),
        name_id = gsub(" ", "", name),
        id = paste(name_id, team, sep = "_")
      ),
    by = c("id" = "id"),
    match_fun = str_detect
  ) %>%
  mutate(
    name.y = str_to_title(name.y)
  ) %>%
  select(
    preferred_names = name.y,
    original_name = name.x,
    team = team.x,
    performance
  )
            