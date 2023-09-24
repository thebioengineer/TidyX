#' @title Player Batting
#'
#' @description Subset of batting data from the {lahman} package to include only
#'   batters post 2010 and derive batting average
#'
#' @format
#' \describe{
#'   \item{playerID}{Player Number}
#'   \item{yearID}{Year}
#'   \item{AB}{At Bats}
#'   \item{H}{Hits}
#'   \item{batting_avg}{batting average}
#' }
"lahman_batting_2010"

#' @title Team Runs
#'
#' @description Subset of team level runs data from the {lahman} package to
#'   include only post 2010 and derive runs per game
#'
#' @format
#' \describe{
#'   \item{yearID}{Year}
#'   \item{teamID}{Team}
#'   \item{G}{Games}
#'   \item{R}{Runs}
#'   \item{runs_per_game}{Runs per game}
#' }
#'
#'
"lahman_team_runs_2010"
