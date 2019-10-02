#################################
#### ---- Save Plotlies ---- ####
#################################

# date: 26.08.19

pacman::p_load(plotly, dplyr, magick)

setwd(here::here())

source("plots/Community plots/R Skripte/04_UU_Plotly.R")
htmlwidgets::saveWidget(as_widget(p1), file = "UU_widget.html")
source("plots/Community plots/R Skripte/05_LB_Plotly.R")
htmlwidgets::saveWidget(as_widget(p2), file = "LB_widget.html")
source("plots/Community plots/R Skripte/06_BB_Plotly.R")
htmlwidgets::saveWidget(as_widget(p3), file = "BB_widget.html")

# now you have to go into the folder and manually save the html objects as pngs. 


communitiyplots <- image_read(
      c(
            "plots/Community plots/01_Originals/UU.png",
            "plots/Community plots/01_Originals/LB.png",
            "plots/Community plots/01_Originals/BB.png"
            
      )
)

image_trim(communitiyplots) %>% 
      image_append() %>% 
      image_


      image_write("plots/Community plots/Communities_trimmed_appended.png")
