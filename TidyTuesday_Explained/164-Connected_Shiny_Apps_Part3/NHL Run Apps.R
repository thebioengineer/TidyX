
## TidyX Episode 164 - Connected shiny apps through queries!
library(callr)

## start up players app on local host at port 3333
player_app_process <- callr::r_session$new()

player_app_process$call(
  function() {
    shiny::runApp(
      appDir = 'TidyTuesday_Explained/164-Connected_Shiny_Apps_Part3/NHL Players App.R',
      port = 3333
    )
  }
)

## Start up Game App on local host at port 2222
game_app_process <- callr::r_session$new()
game_app_process$call(
  function() {
    ## evaluates shiny app in this environment
    shiny::runApp(
      appDir = 'TidyTuesday_Explained/164-Connected_Shiny_Apps_Part3/NHL Games App.R',
      port = 2222
    )
  }
)

## open to see skaters app
utils::browseURL("http://127.0.0.1:3333")

## Open to see game app
utils::browseURL("http://127.0.0.1:2222")

## using the game app, look at the rosters of the teams that played, and then click to go to the skater app using the link in the app to 
## see the season long stats, click then on the link in the skater app to see the career of the player


# game_app_process$kill()
# player_app_process$kill()
