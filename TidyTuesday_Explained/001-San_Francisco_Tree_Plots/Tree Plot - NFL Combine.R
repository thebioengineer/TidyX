library(tidyverse)
library(rvest)
library(treemap)

# Get 2019 combine data
combine2019 <- read_html("https://www.pro-football-reference.com/draft/2019-combine.htm") %>% 
  html_table(fill = T) %>%
  as.data.frame()

combine2019[1:4,]

## Create position groupings
combine2019 <- combine2019 %>% 
  mutate(Position = ifelse(Pos == "OT" | Pos == "OG" | Pos == "C", "OL",
                           ifelse(Pos == "ILB" | Pos == "OLB", "LB",
                                  ifelse(Pos == "S" | Pos == "CB", "DB",
                                         ifelse(Pos == "EDGE", "DE",
                                                ifelse(Pos == "DT", "DL", as.character(Pos)))))))

combine2019 <- combine2019 %>% 
  separate(Drafted..tm.rnd.yr., into = c("Tm", "Rd", "Pick", "Yr"), sep = "/") %>%
  mutate(Round = substring(Rd, 1, 2),
         Round = ifelse(is.na(Round), "FA", as.character(Round)))

combine2019$Round <- as.factor(combine2019$Round)

draft_count <- combine2019 %>%
  count(Position, Round) %>%
  filter(Position != "Pos")

draft_count

## plot a tree map
png(filename="draft_tree.png",width=800, height=500)
treemap(draft_count,
        index=c("Round","Position"),
        vSize="n",
        type="index",
        title="Draft Number by Round: 2019 NFL Draft",
        fontsize.title = 17,        # change the title size
        fontsize.labels=c(15,11),   # changes the label size
        fontcolor.labels="black",   # changes the colors of the plot labels    
        fontface.labels=c(2,2),     # change the face labels (e.g., bold, italic, normal, etc)          
        overlap.labels=1,           
        palette="Spectral",         # set color palette of interest 
        align.labels=list(          
          c("right", "bottom"),     # define label for the box title
          c("center", "center")))   # define label for the values within boxes
dev.off()


# Looks like Round 1 was pretty stacked with DL and DE -- Look at who they were
combine2019 %>% filter(Round == 1, Position %in% c("DL", "DE"))

