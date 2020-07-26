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
                   
    panel(h1("TidyX Episode 20"), style = "margin-top:10%"),
    panel(h2("Special Guest Explainer Today!"), style = "margin-top:20%"),
                   
    panel_markdown("   
    
    <div style = 'margin:auto'>
    ![](http://varianceexplained.org/images/david_robinson_picture2.jpg)
    </div>
    - David Robinson
    
      - Principal Data Scientist at Heap
      - VarianceExplained.com
      - Creator of {broom}, {tidytext}, {fuzzyjoin}, {widyr}
      
    ",style = "margin-top:10%"),
    
    panel_markdown("   
    
    - weekly tidytuesday screencast!
    
    - [here](https://www.youtube.com/channel/UCeiiqmVK07qhY-wvg3IZiZQ)

    ",style = "margin-top:30%"),
    
    panel_markdown("   
    
    Picked TidyTuesday 2020, Week 22 - Cocktails!
    
    <img src = 'https://camo.githubusercontent.com/a35dd69041e94d26e2a8dc59140126fa5e612407/68747470733a2f2f696d616765732e756e73706c6173682e636f6d2f70686f746f2d313535313032343730392d3866323362656663366638373f69786c69623d72622d312e322e3126697869643d65794a6863484266615751694f6a45794d446439266175746f3d666f726d6174266669743d63726f7026773d3134323526713d3830' height = '400px'/>
    
    <p style='font-size: 20px'>photocredit: Kobby Mendez | https://unsplash.com/@kobbyfotos</p>
    
    "),
    style = "background-color:black; color: white;text-size:50px; font-size: 50px"
  ) %>% 
  slide_wide_markdown(title = NULL,"
    
    - Code here: 
      - https://github.com/dgrtwo/data-screencasts/blob/master/cocktails.Rmd
      
    - Video here: 
      - https://youtube.com/watch?v=EC0SVkFB2OU
    
    ",
  style = "margin-top:10%, font-size: 50px; background-color:black; color: white; text-size:50px")
                   


## Save Presentation ----

save_sidescroller(pres_slides,
                  here("TidyTuesday_Explained/020-Cocktails_with_David_Robinson/intro.html")
                  )

