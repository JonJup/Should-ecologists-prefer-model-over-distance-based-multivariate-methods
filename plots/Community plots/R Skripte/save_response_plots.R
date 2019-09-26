#################################
#### ---- Save Plotlies ---- ####
#################################

# date: 26.08.19

pacman::p_load(plotly, dplyr, magick)

setwd("~/01_Uni/05_MTPnew/05_Plots/Community plots/R Skripte/")

source("04_UU_Plotly.R")
htmlwidgets::saveWidget(as_widget(p1), file = "UU_widget.html")
source("05_LB_Plotly.R")
htmlwidgets::saveWidget(as_widget(p2), file = "LB_widget.html")
source("06_BB_Plotly.R")
htmlwidgets::saveWidget(as_widget(p3), file = "BB_widget.html")

# now you have to go into the folder and manually save the html objects as pngs. 

communitiyplots <- image_read(c("Community plots/01_Originals/UU.png",
                   "Community plots/01_Originals/LB.png",
                   "Community plots/01_Originals/BB.png"))

image_trim(communitiyplots) %>% 
      image_append() %>% 
      image_write("../Communities_trimmed_appended.png")