#API time!

library(httr)
library(jsonlite)
library(tidyverse)
library(plotly)

### API interaction

get_batter_json <- function(uid, year = 2019){
  url <- glue::glue("https://baseballsavant.mlb.com/player-viz/lookup?playerId={uid}&season={year}")
  res <- GET(url)
  jsonlite::parse_json(rawToChar(res$content),simplifyVector = TRUE)
}

### Nice 
player_query <- function(name){
  
  name_split <- rev(strsplit(tolower(name), " ")[[1]])
  
  html_name_search <- paste(name_split,collapse = ",%20")
  html_name_query <- URLencode(paste(html_name_search,collapse = ", "))
  url <- glue::glue("https://baseballsavant.mlb.com/player/search?search={html_name_search}&query={html_name_query}")
  res <- GET(url)
  players_list <- jsonlite::parse_json(rawToChar(res$content),simplifyVector = TRUE)
  
  if(is.list(players_list) & !is.data.frame(players_list) & length(players_list) == 0){
    stop("Player Not Found")
    
  }else if(is.list(players_list) & !is.data.frame(players_list)){
    stop("Unexpected return. Aborting!")
    
  }else if(is.data.frame(players_list) & nrow(players_list) > 1){
    player_idx <- seq_along(players_list$name)
    prompt <- paste0(
      "Multiple players are returned with that name. Select player:\n",
      paste0("\t",player_idx,": ", players_list$name,", ",players_list$id, collapse = "\n")
    )
    cat(prompt)
    idx <- 0
    while(!idx %in% player_idx){
      idx <- readline("Enter number corresponding with Player:")
    }
    players_list <- players_list[idx, ]
    
  }
  
  return(players_list[["id"]])

}

is_uid <- function(x){
  is.numeric(x) | suppressWarnings(!is.na(as.numeric(x)))
}

pull_batter_data <- function(uid, year){
  
  if(!is_uid(uid)){
    uid <- player_query(uid)
  }
  
  batter_json <- get_batter_json(uid = uid, year = year)
  
  batter_demog <- data.frame(batter_json[setdiff(names(batter_json), "data")])
  
  batter_pbp <- batter_json$data
  
  list(
    batter = batter_demog,
    pbp = batter_pbp
  )
  
}

pitch_classifier <- function(x, classifier = c("group","desc")){
  
  classifier <- match.arg(classifier)
  
  pitch_key <- tribble(
  ~ pitch_type,                 ~pitch_desc,  ~pitch_group,
  "FA",                          "fastball",    "fastball",
  "FF",                "four-seam fastball",    "fastball",
  "FT",                 "two-seam fastball",    "fastball",
  "FC",                            "cutter",    "fastball",
  "FS",                    "split-fingered",   "off-speed",
  "SI",                            "sinker",    "fastball",
  "SF", "fastball (sinker, split-fingered)",    "fastball",
  "SL",                            "slider",    "breaking",
  "CH",                          "changeup",   "off-speed",
  "CB",                         "curveball",    "breaking",
  "CU",                         "curveball",    "breaking",
  "KC",                     "knuckle-curve",    "breaking",
  "KN",                       "knuckleball",   "",
  "EP",                            "eephus",   "")

  
  class_vec <- setNames(
    pitch_key[[paste0("pitch_",classifier)]],
    pitch_key[["pitch_type"]])
    
  unname(class_vec[x])
}

## select the one with uid == 669257
batter_data <- pull_batter_data(
  "Will Smith",
  2019
)

pbp_extra <- batter_data %>% 
  pluck("pbp") %>% 
  mutate(
    pitch_group = pitch_classifier(pitch_type, "group"),
    pitch_desc = pitch_classifier(pitch_type, "desc")
  )


## create plot using plotly, not _le gasp_ ggplotly!
plot_ly() %>% 
  add_trace(
    mode = "markers",
    data = pbp_extra,
    x = ~ as.numeric(plate_x),
    y = ~ as.numeric(plate_z)
    )


## add something like we would do with ggplot!


plot_ly() %>% 
  
  ## add strikebox
  layout(
    shapes = list(
      list(type = "rect",
           fillcolor = "#eeeeee",
           line = list(color = "#d4d4d4"),
           x0 = -.8, x1 = .8, xref = "x",
           y0 = 1.4, y0 = 3.5, yref = "y",
           opacity = 0.7
    ))    
  ) %>% 
  
  ## add my points
  add_trace(
    mode = "marker",
    data = pbp_extra,
    x = ~ as.numeric(plate_x),
    y = ~ as.numeric(plate_z),
    color = ~ pitch_type,
    size = ~ as.numeric(release_speed),
    symbol = ~ pitch_group, 
    symbols = c("o","x"),
    text = ~paste0(
      "Pitcher: ",pitcher_name,"<br>",
      "Pitch: ",pitch_desc,"<br>",
      "Count: ",pre_ball_count,"/",pre_strike_count,"<br>",
      "Speed: ",release_speed," MPH","<br>",
      "Result: ",description),
    hoverinfo = "text"
  ) %>% 
  layout(
    title = "Will Smith(uid 669257) 2019"
  )



