library(sidescroller)
library(here)


pres <- sidescroller(
  list(
    htmltools::htmlDependency(
      name = "darker-grotesque",
      version = "1.0",
      src = c(href = "https://fonts.googleapis.com/css?family=Darker+Grotesque&display=swap"),
      stylesheet = ""),
    htmltools::htmlDependency(
      name = "VT323",
      version = "1.0",
      src = c(href = "https://fonts.googleapis.com/css?family=VT323&display=swap"),
      stylesheet = ""),
    htmltools::htmlDependency(
      name = "font-awesome",
      version = "4.7.0",
      src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/"),
      stylesheet = "font-awesome.min.css")))

## Title ----
pres_title <- pres %>% 
  title_slide(
    title = NULL,
    subtitle = NULL,
    style = "font-family: 'Darker Grotesque',Arial;
    font-size: 40px;
    font-weight: 300;
    background-image: url(https://raw.githubusercontent.com/thebioengineer/TidyX/master/logo/TidyX_full.png);
    background-repeat: no-repeat;
    background-size: stretch;
    background-position: center;
    background-color: black;"
  )

## Intro to the Pain

pres_slides <- pres_title %>% 

  slide_multipanel(title = NULL,
                   
    panel(h1("TidyX Episode 32"), style = "margin-top:10%"),
    panel(h2("Special Guest Explainer Today!"), style = "margin-top:20%"),
                   
    panel_markdown("   
    
    <div style = 'margin:auto'>
    ![](https://avatars2.githubusercontent.com/u/1043111?s=400&u=bb3a363381b39a5172b817ae513c6d10ab1d239a&v=4)
    </div>
    Eric Nantz
    
      - Statistician/Developer
      - Host of the R-Podcast (https://r-podcast.org/)
      - rweekly curator/podcast host
      - Shiny Dev Series (https://www.youtube.com/c/shinydeveloperseries)

    ",style = "margin-top:0%; fon"),
    
    panel_markdown("   
    
    Picked TidyTuesday 2020, Week 42 - Datasaurus Dozen!
    
    <img src = 'https://camo.githubusercontent.com/3641fe3e096a9e0948683dc991d7ed4f324a1a16/68747470733a2f2f332e62702e626c6f6773706f742e636f6d2f2d64595763624b56736947592f563852466d4d466e4c6a492f41414141414141414739592f51725f50476d523056384d68535862382d72426441736463696e792d6f716c3241434c63422f733430302f31646174617361757275732e706e67' height = '400px'/>
    
    The data this week comes from Alberto Cairo courtesy of Steph Locke + Lucy McGowan.
    
    H/t to: Jesus M. Castagnetto for sharing it with the TidyTuesday crew.
    
    "),
    style = "background-color:black; color: white;text-size:50px; font-size: 30px; width: 1200px;"
  ) %>% 
  slide_wide_markdown(title = NULL,"
    
    - Code here: 
      - https://github.com/rpodcast/shinysaurus
      
    - Shiny App here: 
      - https://rpodcast.shinyapps.io/shinysaurus
    
    ",
  style = "margin-top:10%, font-size: 80px; background-color:black; color: white; font-size:50px")
                   


## Save Presentation ----

save_sidescroller(pres_slides,
                  here("TidyTuesday_Explained/032-Shiny_with_Eric_Nantz/index.html")
                  )

