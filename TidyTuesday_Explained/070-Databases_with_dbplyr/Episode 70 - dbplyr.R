
## TidyX Episode 70: dbplyr
# https://dbplyr.tidyverse.org/

### Packages ---------------------------------------------
library(tidyverse)
library(rvest)

library(dbplyr)

library(RSQLite)
library(DBI)

### Get Data ---------------------------------------------
combine2019_raw <- read_html(
  "https://www.pro-football-reference.com/draft/2019-combine.htm"
  ) %>% 
  html_table(fill = T) %>%
  as.data.frame() %>% 
  filter(Player != "Player") #remove repeated headers

## Create position groupings
combine2019_pos <- combine2019_raw %>% 
  mutate(Position = case_when(
    Pos %in% c("OT","OG","C") ~ "OL",
    Pos %in% c("ILB", "OLB") ~ "LB",
    Pos %in% c("S","CB") ~ "DB",
    Pos %in% c("EDGE") ~ "DE",
    Pos %in% c("DT") ~ "DL",
    TRUE ~ as.character(Pos) ## default case
    )
  )

combine2019_clean <- combine2019_pos %>% 
  separate(Drafted..tm.rnd.yr., into = c("Tm", "Rd", "Pick", "Yr"), sep = "/") %>%
  mutate(
    Rd = case_when(
      is.na(Rd) ~ "FA",
      TRUE ~ as.character(readr::parse_number(Rd))
    )
  ) %>%
  mutate(across(.cols = c(Wt:Shuttle), ~as.numeric(.x)))

combine2019_clean %>% head()

### Write to database ---------------------------------------------

# create connection
db_con <- dbConnect(
  drv = RSQLite::SQLite(),
  here::here("TidyTuesday_Explained/070-Databases_with_dbplyr/combine.db")
  )

## write table
dbWriteTable(db_con, name = "combine2019", combine2019_clean)
dbListTables(db_con)

##disconnect
dbDisconnect(db_con)

### Interact with database ---------------------------------------------


## create connection
db_con <- dbConnect(
  RSQLite::SQLite(), 
  here::here("TidyTuesday_Explained/070-Databases_with_dbplyr/combine.db")
)

## Manually executing a SQL query

Position_counts <- dbGetQuery(db_con, 
                  paste(
                  "SELECT `Position`, COUNT (*) As `N`",
                  "FROM `combine2019`",
                  "WHERE (`School` = 'Alabama')",
                  "GROUP BY `Position`",
                  "ORDER BY `N` desc"))

### Write a query

alabama_draft_picks <- tbl(db_con, "combine2019") %>%
  filter(School == "Alabama") %>%
  count(Position) %>%
  arrange(desc(n))

## see results
alabama_draft_picks %>% 
  collect()

### see the actual SQL query
alabama_draft_picks %>% show_query()

##disconnect
dbDisconnect(db_con)
