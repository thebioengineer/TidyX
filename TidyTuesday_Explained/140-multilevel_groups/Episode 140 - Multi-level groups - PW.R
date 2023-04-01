
library(tidyverse)
library(gt)

d <- tibble(
  venue = c("competition A","competition A","competition A","competition A","competition A","competition A",
            "competition B", "competition C", "competition C"),
  category = c("red", "red", "blue", "green", "red", "yellow", "blue", "orange", "green"),
  gender = c("female", "female", "female", "female", "male", "male", "female", "male", "male"),
  participant = c("Jane", "Ana", "Hillary", "Ruth", "Antony", "Oscar", "Eva", "Rupert", "John")
)


d %>%
  knitr::kable()

### v1
d %>%
  group_by(category, gender) %>%
  group_by(venue) %>%
  gt()


### v2

d %>%
  split(.$venue) %>%
  map( ~ (.x %>% select(-venue))) 


### v3 -- return lists in tables
# create list of the competions
comp_lists <- d %>%
  split(.$venue) %>%
  map( ~ (.x %>% select(-venue))) 

# create list of unique competitions
comps <- unique(d$venue)

# pluck 1 table and make it a kable table
comp_lists %>%
  pluck(1) %>%
  knitr::kable()

## Run the loop for every table
for(i in comps){
  
  comp_lists %>%
    pluck(i) %>%
    knitr::kable() %>%
    print()
  
}