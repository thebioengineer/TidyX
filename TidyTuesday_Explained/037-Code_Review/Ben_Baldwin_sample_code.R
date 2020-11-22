library(tidyverse)

# json file of player participation from week 1 SEA-ATL
# courtesy of sport radar
pbp <- readRDS(url("https://github.com/guga31bb/sport_radar/blob/master/data/participation/c2b24b1a-98c5-465c-8f83-b82e746b4fcf.rds?raw=true"))

print(paste(pbp$summary$home$alias, pbp$summary$away$alias, "week", pbp$summary$week$sequence))


# the goal: make a flat df with each play one row
# and the players on the field as columns
# (or do it in a better way)
# ben's bad code is below

bigdat <- NULL
for (i in 2:length(pbp$plays$description)) {
  
  # the home players on the field for play i
  h <- c(paste(unlist(pbp$plays$home.players[i])[12]),
         paste(unlist(pbp$plays$home.players[i])[13]),
         paste(unlist(pbp$plays$home.players[i])[14]),
         paste(unlist(pbp$plays$home.players[i])[15]),
         paste(unlist(pbp$plays$home.players[i])[16]),
         paste(unlist(pbp$plays$home.players[i])[17]),
         paste(unlist(pbp$plays$home.players[i])[18]),
         paste(unlist(pbp$plays$home.players[i])[19]),
         paste(unlist(pbp$plays$home.players[i])[20]),
         paste(unlist(pbp$plays$home.players[i])[21]),
         paste(unlist(pbp$plays$home.players[i])[22])
  )
  
  # the away players on the field for play i
  a <- c(paste(unlist(pbp$plays$away.players[i])[12]),
         paste(unlist(pbp$plays$away.players[i])[13]),
         paste(unlist(pbp$plays$away.players[i])[14]),
         paste(unlist(pbp$plays$away.players[i])[15]),
         paste(unlist(pbp$plays$away.players[i])[16]),
         paste(unlist(pbp$plays$away.players[i])[17]),
         paste(unlist(pbp$plays$away.players[i])[18]),
         paste(unlist(pbp$plays$away.players[i])[19]),
         paste(unlist(pbp$plays$away.players[i])[20]),
         paste(unlist(pbp$plays$away.players[i])[21]),
         paste(unlist(pbp$plays$away.players[i])[22])
  )
  
  # get the play id, time, description, type, and all the players
  toadd <- c(pbp$plays$sequence[i], pbp$plays$clock[i], pbp$plays$description[i],  pbp$plays$type[i],  h, a)
  
  # bind it all together
  bigdat <- rbind(bigdat,toadd)
  
}

bigdat <- bigdat %>% as_tibble()

colnames(bigdat) <- c("play_id","time", "desc", "play_type", "h1", "h2", "h3", "h4", "h5", "h6", "h7", "h8", "h9", "h10", "h11", 
                      "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11")

bigdat$home_team <- pbp$summary$home$alias
bigdat$away_team <- pbp$summary$away$alias


bigdat$home_team <- pbp$summary$home$alias
bigdat$away_team <- pbp$summary$away$alias
bigdat$week <- pbp$summary$week$sequence
bigdat$season <- pbp$summary$season$year

data <- bigdat %>% 
  mutate(
    # create quarter to join with external data
    quarter_seconds_remaining = lubridate::period_to_seconds(lubridate::ms(time)),
    increment = ifelse(lag(quarter_seconds_remaining) < quarter_seconds_remaining, 1, 0),
    increment = ifelse(is.na(increment), 0, increment),
    qtr = 1 + cumsum(increment)
  )


data

# 
# 
# > glimpse(data)
# Rows: 175
# Columns: 33
# $ play_id                   <chr> "55", "76", "97", "118", "149", "177", "198", "...
# $ time                      <chr> "15:00", "14:26", "13:50", "13:09", "13:05", "1...
# $ desc                      <chr> "(15:00) 21-T.Gurley left end to ATL 30 for 5 y...
# $ play_type                 <chr> "rush", "rush", "rush", "pass", "pass", "rush",...
# $ h1                        <chr> "Chris Lindstrom", "Chris Lindstrom", "Chris Li...
# $ h2                        <chr> "Alex Mack", "Alex Mack", "Alex Mack", "Alex Ma...
# $ h3                        <chr> "Jake Matthews", "Hayden Hurst", "Hayden Hurst"...
# $ h4                        <chr> "James Carpenter", "Jake Matthews", "Jake Matth...
# $ h5                        <chr> "Todd Gurley II", "James Carpenter", "Luke Stoc...
# $ h6                        <chr> "Calvin Ridley", "Todd Gurley II", "James Carpe...
# $ h7                        <chr> "Keith Smith", "Calvin Ridley", "Todd Gurley II...
# $ h8                        <chr> "Julio Jones", "Keith Smith", "Calvin Ridley", ...
# $ h9                        <chr> "Russell Gage", "Julio Jones", "Julio Jones", "...
# $ h10                       <chr> "Kaleb McGary", "Kaleb McGary", "Kaleb McGary",...
# $ h11                       <chr> "Matt Ryan", "Matt Ryan", "Matt Ryan", "Matt Ry...
# $ a1                        <chr> "L.J. Collier", "L.J. Collier", "L.J. Collier",...
# $ a2                        <chr> "Benson Mayowa", "Benson Mayowa", "Benson Mayow...
# $ a3                        <chr> "K.J. Wright", "K.J. Wright", "K.J. Wright", "J...
# $ a4                        <chr> "Jarran Reed", "Jarran Reed", "Jarran Reed", "B...
# $ a5                        <chr> "Bruce Irvin", "Bruce Irvin", "Bruce Irvin", "Q...
# $ a6                        <chr> "Quandre Diggs", "Quandre Diggs", "Quandre Digg...
# $ a7                        <chr> "Quinton Dunbar", "Quinton Dunbar", "Quinton Du...
# $ a8                        <chr> "Jamal Adams", "Jamal Adams", "Jamal Adams", "J...
# $ a9                        <chr> "Poona Ford", "Poona Ford", "Poona Ford", "Poon...
# $ a10                       <chr> "Bobby Wagner", "Bobby Wagner", "Bobby Wagner",...
# $ a11                       <chr> "Shaquill Griffin", "Shaquill Griffin", "Shaqui...
# $ home_team                 <chr> "ATL", "ATL", "ATL", "ATL", "ATL", "ATL", "ATL"...
# $ away_team                 <chr> "SEA", "SEA", "SEA", "SEA", "SEA", "SEA", "SEA"...
# $ week                      <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
# $ season                    <int> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020,...
# $ quarter_seconds_remaining <dbl> 900, 866, 830, 789, 785, 756, 719, 714, 677, 66...
# $ increment                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
# $ qtr                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...