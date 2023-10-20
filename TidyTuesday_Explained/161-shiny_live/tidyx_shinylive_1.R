

## the cars folder contains last weeks shiny app (bit.ly/TidyX_Ep160)
## Convert your shiny app into all the assets for running the app 
## in the browser
shinylive::export(
  appdir = "TidyTuesday_Explained/161-shiny_live/cars",
  destdir = "TidyTuesday_Explained/161-shiny_live/tidyx_apps"
  )


## with development version of httpuv, run shinylive app locally
## remotes::install_github("rstudio/httpuv")
httpuv::runStaticServer(dir = "TidyTuesday_Explained/161-shiny_live/tidyx_apps", port = 8888)



