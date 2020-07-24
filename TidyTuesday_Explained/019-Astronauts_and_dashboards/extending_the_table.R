#Extending Laurens Table
# 1. Adding flags
# 2. conditional formatting
# 3. Export a full version

library(rvest)

cf_io <- read_html("https://www.countryflags.io/")

country_ids <- cf_io %>% 
  html_nodes(".item_country") %>% 
  lapply(function(el){
    
    key <- el %>% 
      html_text %>% 
      str_split("\\n") %>% 
      `[[`(1) %>% 
      trimws() %>% 
      {
        .[nchar(.)>0]
      }
    
    data.frame(
      code = key[1],
      country = key[2]
    )
  }) %>% 
  bind_rows


get_country_flag <- function(x){
  
  urls <- sapply(x, function(x){
    code <- country_ids$code[which(country_ids$country == x)]
    file.path("https://www.countryflags.io", code, "flat/64.png")
  })
  
  paste0("<img src='",urls,"'/>")
  
}

astro %>%
  mutate(
    Nationality = case_when(
      Nationality == "U.S." ~ "United States",
      Nationality == "U.S.S.R/Russia" ~ "Russian Federation",
      TRUE ~ Nationality,
    ),
    Flag = get_country_flag(Nationality)
  ) %>%
  formattable(
    # R justify first column, center all others
    align = c('l', 'c', 'l', 'l','c'),
    # column list for custom formatting
    list(
      # create bar for % of astronauts
      `Percent of Astronauts` = color_bar('cadetblue')))


get_country_flag_v2 <- function(x){
  urls <- sapply(x, function(x){
    code <- country_ids$code[which(country_ids$country == x)]
    file.path("https://www.countryflags.io", code, "flat/64.png")
  })
  
  paste0("<div style = 'margins:5px;background-color:lightgrey;'><img src='",urls,"'/></div>")
}

best_table_ever <- astro %>%
  mutate(
    Nationality = case_when(
      Nationality == "U.S." ~ "United States",
      Nationality == "U.S.S.R/Russia" ~ "Russian Federation",
      TRUE ~ Nationality,
    ),
    Flag = get_country_flag_v2(Nationality)
  ) %>%
  select(
    Nationality, Flag, everything()
  ) %>% 
  formattable(
    # R justify first column, center all others
    align = c('l','c', 'c', 'l', 'l'),
    # column list for custom formatting
    list(
      # create bar for % of astronauts
      `Percent of Astronauts` = color_bar('cadetblue')))

best_table_ever 



best_table_ever2 <- astro %>%
  mutate(
    `North America` = ifelse(Nationality %in% c("U.S.", "Canada"), TRUE, FALSE),
    Nationality = case_when(
      Nationality == "U.S." ~ "United States",
      Nationality == "U.S.S.R/Russia" ~ "Russian Federation",
      TRUE ~ Nationality,
    ),
    Flag = get_country_flag_v2(Nationality)
  ) %>% 
  select(
    Nationality, Flag,`North America`, everything()
  ) %>% 
  formattable(
    # R justify first column, center all others
    align = c('l','c', 'c', 'l', 'l'),
    # column list for custom formatting
    list(
      # create bar for % of astronauts
      `Percent of Astronauts` = color_bar('cadetblue'),
      `Most Common Occupation` = formatter(
        "span",
        style = x ~ style(color = ifelse(x == "MSP", "red", 
                                         ifelse(x == "Commander", "blue", 
                                                "green")))),
      `North America` = formatter("span",
                                  style = x ~ style(color = ifelse(x, "green", "red")),
                                  x ~ icontext(ifelse(x, "ok", "remove"), 
                                               ifelse(x, "Yes", "No"))))
  )


best_table_ever2


htmlwidgets::saveWidget(
  widget = formattable::as.htmlwidget(best_table_ever2),
  file = here::here(
    "TidyTuesday_Explained/019-Astronauts_and_dashboards/best_table_ever2.html"
    )
)
