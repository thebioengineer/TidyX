library(rvest)
library(tidyverse)
library(here)
library(ggplot2)
library(plotly)


hike_data_rds <- here("TidyTuesday_Explained/035-Rectangles/hike_data.rds")

if(!file.exists(hike_data_rds)){
  
hike_pages <- 1:129

hike_data <- lapply( hike_pages - 1 , function(page){
  
  start_int <- 30 * page
  
  page_url <- paste0(
    "https://www.wta.org/go-outside/hikes?b_start:int=",
    start_int
    )
  
  page_html <- read_html(page_url)
  
  page_html %>% 
    
    html_nodes(".search-result-item") %>% 
    
    map(
      function(hike){
        
        hike_name <- hike %>% html_nodes(".listitem-title") %>% html_nodes("span") %>%  html_text()
        hike_location <- hike %>% html_node("h3") %>% html_text()
        
        hike_stats <- hike %>% html_node(".hike-stats")
        
        hike_length <- hike_stats %>% html_nodes(".hike-length") %>%html_nodes("span") %>%  html_text()
        hike_gain <- hike_stats %>% html_nodes(".hike-gain") %>%html_nodes("span") %>%  html_text()
        hike_highpoint <- hike_stats %>% html_nodes(".hike-highpoint") %>%html_nodes("span") %>%  html_text()
        hike_rating <- hike_stats %>% html_nodes(".hike-rating") %>%html_nodes(".current-rating") %>%  html_text()
        
        hike_desc <- hike %>% html_nodes(".listing-summary") %>% html_text()
        
        hike_features <- hike %>% html_nodes(".trip-features") %>% html_nodes("img") %>% html_attr("title") %>% list()
        
        tibble(
          name = hike_name,
          location = hike_location,
          length = hike_length,
          gain = hike_gain,
          highpoint = hike_highpoint,
          rating = hike_rating,
          features = hike_desc,
          description = hike_desc
        )
    }) %>% 
    bind_rows()
  
}) %>% bind_rows()

saveRDS(hike_data,file = hike_data_rds)

}else{
  
  hike_data <- readRDS(hike_data_rds)
  
}


clean_hike_data <- hike_data %>% 
  mutate(
    trip = case_when(
      grepl("roundtrip",length) ~ "roundtrip",
      grepl("one-way",length) ~ "one-way",
      grepl("of trails",length) ~ "trails"),
    
    length_total = as.numeric(gsub("(\\d+[.]\\d+).*","\\1", length)) * ((trip == "one-way") + 1),
    
    gain = as.numeric(gain),
    highpoint = as.numeric(highpoint),
    
    location_general = gsub("(.*)\\s[-][-].*","\\1",location)
  )




hike_plot <- ggplot(clean_hike_data) + 
  geom_rect(aes(
    xmin = 0,
    xmax = length_total,
    ymin = 0,
    ymax = gain,
    label = name
  ),
  alpha = .4,
  fill = "#228B22",
  color = "#765C48"
  ) + 
  facet_wrap(
    ~ location_general,
    scales = "free_x"
  ) +
  labs(
    title = "Washington State Hikes",
    x = "Hike Length (miles)",
    y = "Hike Elevation Gain (ft)",
    caption = "Data from Washingon Trails Association (wta.org) | Viz by @ellis_hughes"
  )


ggplotly(hike_plot)
