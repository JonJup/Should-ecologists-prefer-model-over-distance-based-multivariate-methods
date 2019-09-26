mvglm_wrapper <- function(model, SEEDS, warn = F){

      if(warn) options(warn = 0)
      if(!warn) options(warn=-1)
      model.list <- list()
      
      for (i in SEEDS){
      set.seed(i)
      source(model)
            com_uni <- mvabund(Simulation.output[["data"]][,1:9])
            model.list[[i]]  <- manyglm(com_uni ~ ., 
                                      data = Simulation.output[["data"]][,10:13] 
                                      )
      }
      model.list[["Response Type"]] <- Simulation.output[["Response.Type"]]
      model.list[["Samples"]] <- Simulation.output[["Samples"]]
      
return(model.list)
}