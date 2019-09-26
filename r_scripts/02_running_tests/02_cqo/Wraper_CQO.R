### ------------------------------------------------------ ###
# --------------------  CQO Wrapper ------------------------ #
### ------------------------------------------------------ ###

#Jonathan Jupke
#10.12.18
#MvGLM Paper
#FUNCTION. Run a CQO analysis on communities as specified in the script which
#calls this wrapper.


cqo2r.wrapper <- function(model, comwd, SEEDS) {
      for (seed.i in SEEDS) {
            set.seed(seed.i)
            
            # set seed to folder of community simulation scripts.
            setwd(comwd)
            
            # load the model
            source(model)
            
            # Unpack the ouput of the simulation
            data <- Simulation.output$data
            
            # scale environmental variables
            data[, 10:13]   <- scale(data[, 10:13])
            
            
            # Especially with low sample sizes cqo can returen errors. To ensure
            # smooth sailing with loops the functions are conduceted "safely".
            error = TRUE
            n.runs <- 50
            Failed <- 0
            while (error) {
                  # the safe_cqo function is simply safely(cqo), with safely being a
                  # function from the purrr package.  It is defined in the CQO_run_V1.R
                  # file.
                  run.time <- system.time(
                        cqo.obj  <- safe_cqo(
                              cbind(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9) ~
                                    env1 + env2 + rand1 + rand2,
                              data = data,
                              family =  poissonff,
                              Rank = 2,
                              df1.nl = 1.5,
                              Bestof = n.runs,
                              Crow1positive = F,
                              eq.tolerance = T
                        )
                  )
                  
                  # Check if an error was returned. If not error = NULL and the
                  # while loop ends. f yes error != NULL. The while loop
                  # continues and "Original model fit resultet in error" is
                  # returned.
                  if (is.null(cqo.obj$error)) {
                        error = FALSE
                        cqo.obj <- cqo.obj$result
                  } else {
                        print(paste
                              (
                                    "Fitting the original model resultet in error",
                                    cqo.obj$error
                              )
                        )
                  } #END IF CLAUSE
                  
            } #END WHILE LOOP
            
            # Check the deviance of the models. To ensure that the minimum is not
            # local but global, at least the first 5 models should have very similar
            # deviance (here defined as difference < 1).
            History <- sort(deviance(cqo.obj, history = TRUE))
            timer <- 1
            while ((History[5] - History[1]) > 3) {
                  n.runs  <- 200
                  print("SOLUTION MIGHT BE LOCAL")
                  print(paste("RUN", timer))
                  if (timer <= 10) {
                        run.time <- system.time(
                              cqo.obj   <- safe_cqo(
                                    cbind(
                                          sp1,
                                          sp2,
                                          sp3,
                                          sp4,
                                          sp5,
                                          sp6,
                                          sp7,
                                          sp8,
                                          sp9
                                    ) ~
                                          env1 + env2 + rand1 + rand2,
                                    data = data,
                                    family =  poissonff,
                                    Rank = 2,
                                    df1.nl = 1.5,
                                    Bestof = n.runs,
                                    Crow1positive = F,
                                    eq.tolerance = T
                              )
                        )
                        timer <- timer + 1
                  } else if (timer > 10) {
                        break
                        print("No Good solution after 10 Runs.")
                        Failed <- 1
                  }# END ELSE IF CLAUSE
                  
                  if (is.null(cqo.obj$error)) {
                        cqo.obj <- cqo.obj$result
                        History <-
                              sort(deviance(cqo.obj, history = TRUE))
                  }
                  
                  paste("DIFFERENCE:", History[5] - History[1]) %>% print()
                  
            } # End While Loop
            
            # CALCULATE PSEUDO P-VALUES #
            # initialize cluster
            
            #p - vector
            p_vec <- c()
            # loop over all four columns of interest
            print(paste("CALCULATING P-VALUES FOR SEED", seed.i))
            for (column in 10:13) {
                  p_vec[column - 9] <- cqo_pseudo_p(
                        data = data,
                        n.permute = 100,
                        col = column,
                        cqo_result = cqo.obj
                  )
                  print(paste("P Value for Varaible", column, "calculated"))
            }
            # Summaries
            Summary.obj   <- summary.qrrvglm(cqo.obj)
            cqo.list[[seed.i]] <- list(
                  "Summary" = Summary.obj,
                  "Samples" = Simulation.output$Samples,
                  "Response" = Simulation.output$Response.Type,
                  "Number of Runs" = n.runs,
                  "Time" = run.time,
                  "Failed" = Failed,
                  "p Values" = p_vec
            )
            
            
      } # End for-loop
      
      return(cqo.list)
}# End function
