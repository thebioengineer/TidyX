
# Episode 136: Fuzzy Joining on Dates

## Load Libraries --------------------------------------------------------------

library(tidyverse)
library(fuzzyjoin)

## Recap -----------------------------------------------------------------------
## In episode 132, we covered fuzzy joining on strings. Here is another method:

dat1 <- tribble(
   ~Name, ~Score, ~Rep,
   "Cody",     38,     1,
   "Jack",     35,     1,
   "jaKc",     43,     2,
   "anna",     45,     1,
)

dat2 <- tribble(
  ~FirstName,  ~LastName,
      "Cody",  "Wilders",
      "Jack",    "Frost",
      "Anna",    "Sharp",
)


fuzzyjoin::stringdist_left_join(
  dat1,
  dat2,
  by = c("Name" = "FirstName"),
  max_dist = 3
)
## But sometimes we have other things to match on that aren't so simple, like dates.

## Function for simple date comparison and find the closest date

date1 <- as.Date("2023-01-03")
date2 <- as.Date(c("2022-12-30","2022-12-31","2023-01-02","2023-01-05","2023-01-06","2023-01-07"))

## time difference, always put reference date second
time_dist <- difftime(date2,date1, units = "days")

## find closest date, absolute

smallest_dist <- min(abs(time_dist))

date2[which(abs(time_dist) == smallest_dist)]

## find date, at date is not negative (looking forward in time only)

smallest_dist <- min(time_dist[time_dist >= 0])
which(time_dist == smallest_dist)

date2[which(time_dist == min(time_dist[time_dist >= 0]))]

## find date, at date is not positive (looking back in time only)
## slightly more complicated

smallest_dist <- min(abs(time_dist)[time_dist <= 0])
which(abs(time_dist) == smallest_dist & time_dist <= 0)

date2[which(abs(time_dist) ==  min(abs(time_dist)[time_dist <= 0]) & time_dist <= 0)]


## what if we want to add a tolerance for max time away, but don't care about which direction

tolerance <- as.difftime(2, units = "days")
smallest_dist <- min(abs(time_dist)[abs(time_dist) <= tolerance])
which(abs(time_dist) == smallest_dist & abs(time_dist) <= tolerance)

date2[which(abs(time_dist) == smallest_dist & abs(time_dist) <= tolerance)]


## Lets combine all of this into a single function!

#' @title find closest date given constraints
#' 
#' @param x reference date, as a posixct, date, or character that can be unambiguously converted into a date
#' @param y vector of date options as a posixct, date, or characters that can be unambiguously converted into a date
#' @param direction choice of any, older, or newer - restricts the options of dates in y. "older" uses only dates before
#'         x, "newer" only uses dates after x. "any" is the default and allows any date to be used
#' @param tol date tolerance to allow, a difftime object constructed from as.difftime, indicating the 
#'         date tolerance to use when finding the closes dates. values in y that are greater that the 
#'         time difference away from x as defined here get removed. 

which_closest_date <- function(x, y, direction = c("any","older","newer"), tol = as.difftime(Inf, units = "days")){
  
  time_dist <- difftime(y,x, units = "days")
  
  # helper from R to allow automatic error messages on what values for "Direction" are allowed
  direction <- match.arg(direction)
  
  # helper from R to disallow continuing if values here returns false
  stopifnot(
    "`tol` must be a difftime object" = inherits(tol, "difftime")
  )
  
  ## use >= or <= because we could have exact date matches
  if(direction == "older"){
    time_dist_dir <- time_dist <= 0
  }else if(direction == "newer"){
    time_dist_dir <- time_dist >= 0 
  }else{
    time_dist_dir <- rep(TRUE, length(time_dist))
  }
  
  ## apply tolerance now
  time_dist_tol <- abs(time_dist) <= tol
  
  if(all(!(time_dist_dir & time_dist_tol))){
    return(rep(FALSE, length(time_dist)))
  }else{
    min_dist <- min(abs(time_dist[time_dist_dir & time_dist_tol]))
    ## which matches smallest distance, but also matches the direction and tolerance
    return((abs(time_dist) == min_dist) & time_dist_dir & time_dist_tol)
  }
}


date1
date2
## absolutely closest date
date2[which_closest_date(date1, date2)]

## closest date, after date1
date2[which_closest_date(date1, date2, direction = "newer")]

## closest date, before date1
date2[which_closest_date(date1, date2, direction = "older")]

## closest date, within 2 days
date2[which_closest_date(date1, date2, tol = as.difftime(2, units = "days"))]

## closest date after date1 that is within 1 day
date2[which_closest_date(date1, date2, direction = "newer", tol = as.difftime(1, units = "days"))]


## Now do it for fuzzy matching!

## Create sample datasets

weekly_data <- tribble(
                ~Date,  ~weekly_metric, ~Player,
         "2023-01-01",       81,    "Player1",
         "2023-01-01",       65,    "Player2",
         "2023-01-01",       94,    "Player3",
         "2023-01-08",       79,    "Player1",   
         "2023-01-08",       64,    "Player2",
         "2023-01-08",       96,    "Player3",
         "2023-01-15",       NA,    "Player1",
         "2023-01-15",       65,    "Player2",
         "2023-01-15",       93,    "Player3",
         "2023-01-22",       83,    "Player1",
         "2023-01-22",       65,    "Player2",
         "2023-01-22",       94,    "Player3",
         "2023-01-29",       81,    "Player1",
         "2023-01-29",       64,    "Player2",
         "2023-01-29",       93,    "Player3"
)

daily_data <- tibble(
    Player = rep(c("Player1","Player2","Player3"), each = 31),
    daily_date = rep(paste0("2023-01-",1:31),3),
    daily_metric = c(rnorm(31, mean = 3000), rnorm(31, 2500), rnorm(31, 4000))
  )


daily_data %>% 
  mutate(
    weekly_dat = map2(Player, daily_date , function(player, ref_date){
      weekly_data %>% 
        filter(
          Player == player
        ) %>% 
        filter(
          which_closest_date(ref_date, Date, direction = "older", tol = as.difftime(7, units = "days"))  
        ) %>% 
        select(-Player)
    })
  ) %>% 
  unnest(
    weekly_dat
  )



