#----------------------------#
### CQO PSEDUO P Version.2 ### 
#----------------------------#

cqo_pseudo_p <- function(data, n.permute, col,cqo_result){
      
        
      # define safe cqo
      safe_cqo <- safely(cqo)
      # dataset to be permuted 
      data.permute <- data
      # matrix of permutation orders; each row is one order
      data.f <- matrix(nrow = n.permute,
                       ncol = nrow(data.permute)
                        )
      # fill matrix
      
      for (i in 1:n.permute){
                  
            data.f[i,] <- sample(
                                    1:nrow(data.permute),
                                    nrow(data.permute)
                              )
                  
            }

      # looper for foreach loop 
      cqo.1.i = numeric()
      
      # prepare cluster
      cluster <- makeCluster(detectCores() - 4)
      registerDoParallel(cluster)
      clusterExport(cluster, 
                    c("safe_cqo","n.permute","data","data.permute",
                      "data.f", "col"),
                    envir = environment())
      clusterEvalQ(cluster, library(VGAM))
      x <- foreach(cqo.1.i = 1:n.permute) %dopar% {
                  data.permute[,col] <- data[,col][data.f[cqo.1.i,]]
                  timer <- 0
                  while (timer == 0) {
                        result1 <- safe_cqo(
                              
                              cbind(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9, sp10, 
                                    sp11, sp12, sp13, sp14, sp15, sp16, sp17, sp18, sp19, sp20, 
                                    sp21, sp22, sp23, sp24, sp25, sp26, sp27, sp28, sp29, sp30, 
                                    sp31, sp32, sp33, sp34, sp35, sp36, sp37, sp38, sp39, sp40, 
                                    sp41, sp42, sp43, sp44, sp45, sp46, sp47, sp48, sp49, sp50) ~
                                env1 + env2 + rand1 + rand2,
                              data = data.permute,
                              family =  "poissonff",
                              Rank = 2,
                              df1.nl = 1.5,
                              Bestof = 5,
                              Crow1positive = F,
                              eq.tolerance = T
                        )
                        
                        if (is.null(result1$error)) timer = 1
                  }
                  result1
            }
            
      stopCluster(cluster)
      coef <- matrix(ncol = 2, nrow = length(x))
      for (i in 1:length(x)){
                  coef[i,] <- summary(x[[i]][[1]])@post$Coef@C[col-9,]
            }
      
      coef2 <- apply(abs(coef),1,sum)
  
      ## p-values
      #lv1 
      p <-  length(which(coef2 > sum(abs(summary(cqo_result)@post$Coef@C[col-9,])))) / (length(coef2)+1)

      p
      
      
}
