## are there empty sites? 

pacman::p_load()
setwd("")
mod.names <- fs::dir_ls()
mod.names <- mod.names[1:40]

empty = 0
for (i in 1:40){
      
      source(mod.names[i])
      x <- sum(apply(out.samp,1,sum) < 0)
      if (x != 0) empty <- empty +1 
      
}


# There are no empty sites 