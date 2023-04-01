
library(tidyverse)

## created via the datapasta package & rstudio addin
## install.packages("datapasta")
dat <- tibble::tribble(
           ~venue, ~category,  ~gender, ~participant,
  "competition A",     "red", "female",       "Jane",
  "competition A",     "red", "female",        "Ana",
  "competition A",    "blue", "female",    "Hillary",
  "competition A",   "green", "female",       "Ruth",
  "competition A",     "red",   "male",     "Antony",
  "competition A",  "yellow",   "male",      "Oscar",
  "competition B",    "blue", "female",        "Eva",
  "competition C",  "orange",   "male",     "Rupert",
  "competition C",   "green",   "male",       "John"
  )



dat_group_list_vec <- dat %>% 
  ## to preserve order. not strictly necessary
  mutate(across( c(venue, category, gender), ~forcats::fct_inorder(.x))) %>% 
  ## split dat based on the unique combinations of columns
  group_split(venue,category,gender) %>% 
  ## apply vectoring function
  map(
    function(x){
      
      venue <- stringr::str_to_title(unique(x$venue))
      category <- unique(x$category)
      gender <- unique(x$gender)
      
      c(venue, paste0(category,", ", gender), stringr::str_to_title(x$participant))
    }
  )

## do.call calls function listed in "what", args is a list where every value is 
## an arg entry for function listed in what. Named list means named arguments 
dat_group_list_vec <- do.call(what = "c",args = dat_group_list_vec)

cat(dat_group_list_vec, sep = "\n")


## now if we wanted a table directlyu
dat %>%
  mutate(across( c(venue, category, gender), ~forcats::fct_inorder(.x))) %>% 
  group_split(venue,category,gender) %>% 
  map_dfr(
    function(x){
      
      venue <- stringr::str_to_title(unique(x$venue))
      category <- unique(x$category)
      gender <- unique(x$gender)
      
      tibble(my_col = c(venue, paste0(category,", ", gender), stringr::str_to_title(x$participant)))
    }
  )


