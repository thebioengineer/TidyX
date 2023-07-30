#' @title F1 Championship Data
#'
#' @description Dataset containing 1950-2022 F1 Championship results. Data is by racing
#' driver.
#'
#' @format
#' \describe{
#'   \item{number}{Rank of driver in the championship}
#'   \item{driver}{first and last name of the driver}
#'   \item{constructor}{name of the constructor (team) that designed the F1 car}
#'   \item{points}{Number of points the driver earned that season}
#'   \item{year}{Racing season}
#' }
#' @source <https://www.racing-statistics.com/>
"f1_championships"

#' F1 Championship Data - Events
#'
#' Dataset containing 1950-2022 F1 Championship Event results.
#'
#' @format
#' \describe{
#'   \item{number}{Event number of the season}
#'   \item{date}{date of race}
#'   \item{event}{name of the event}
#'   \item{circuit}{name of the racing circuit}
#'   \item{winning_driver}{Name of the driver that won the race}
#'   \item{winning_constructor}{Name of the constructor that won the race}
#'   \item{laps}{number of laps}
#'   \item{time}{Duration of race}
#'   \item{year}{Racing season}
#' }
#' @source <https://www.racing-statistics.com/>
#'
#' @examples
#'
#' library(tidyverse)
#' data(f1_event_results)
#'
#' f1_event_results %>%
#'   group_by(year) %>%
#'   summarize(n_events = n())
#'
#'
"f1_event_results"
