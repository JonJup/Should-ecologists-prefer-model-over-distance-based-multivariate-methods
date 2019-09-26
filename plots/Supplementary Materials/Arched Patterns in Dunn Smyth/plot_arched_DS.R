setwd(here::here("01_simulation/"))
source("22_LL_20.R")
library(mvabund)

            com_uni <- mvabund(Simulation.output[["data"]][,1:9])
            mvglm  <- manyglm(com_uni ~ ., 
                                      data = Simulation.output[["data"]][,10:13] 
                                      )
            plot(mvglm, which = 1)
            