### ---------------------------- ###
# --- FIT CQO DRSG COMMUNITY 5 --- #
### ---------------------------- ###

pacman::p_load(
      VGAM,      ## CQO
      gtools, 
      foreach,   ## parallel processing
      parallel,  ## parallel processing
      doParallel,## parallel processing 
      beepr,     ## sound when computation is finished
      here,       ## Setting wds 
      purrr
)

setwd("~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/r_scripts/reviewer_comments/")

# load data 
data = readRDS("01_different_response_same_gradient/drsg_community5.RDS")

# scale environmental variables
data[, 4:7]   <- scale(data[, 4:7])
x = 5
while (x > 3) {
      time.before = Sys.time()
      cqo.pr2  <- cqo(
                  cbind(sp1, sp2, sp3) ~
                        env1 + env2 + rand1 + rand2,
                  data = data,
                  family =  poissonff,
                  Rank = 2,
                  df1.nl = 1.5,
                  Bestof = 100,
                  Crow1positive = F,
                  eq.tolerance = T
            )
      time.after = Sys.time()
      time.run = time.after - time.before
      History <- sort(deviance(cqo.pr2, history = TRUE))
      x = History[5] - History[1]
}

n.permute = 100
safe_cqo = safely(cqo)
p.values = c()
for (column in 4:7) {
      
      data.permute <- data
      # matrix of permutation orders; each row is one order
      data.f <- matrix(nrow = n.permute,
                       ncol = nrow(data.permute)
      )
      for (i in 1:n.permute) {
            
            data.f[i,] <- sample(
                  1:nrow(data.permute),
                  nrow(data.permute)
            )
            
      }
      cqo.1.i = numeric()
      
      # prepare cluster
      cluster <- makeCluster(2)
      registerDoParallel(cluster)
      clusterExport(cluster, 
                    c("safe_cqo","n.permute","data","data.permute",
                      "data.f", "column"),envir = environment())
      clusterEvalQ(cluster, library(VGAM))
      x <- foreach(cqo.1.i = 1:n.permute) %dopar% {
            data.permute[,column] <- data[,column][data.f[cqo.1.i,]]
            timer <- 0
            while (timer == 0) {
                  result1 <- safe_cqo(
                        
                        cbind(sp1, sp2, sp3) ~
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
      for (i in 1:length(x)) {
            coef[i,] <- summary(x[[i]][[1]])@post$Coef@C[column - 3,]
      }
      
      coef2 <- apply(abs(coef),1,sum)
      p.values[column - 3] <-  length(which(coef2 > sum(abs(summary(cqo.pr2)@post$Coef@C[column - 3,])))) / (length(coef2) + 1)
      
      print(paste("P Value for Varaible", column, "calculated"))
      
}     
time.after2 = Sys.time()
time.run2 = time.after2 - time.before

output = list("p.values" = p.values,
              "runtime.cqo" = time.run,
              "runtime.cqo.p.value" = time.run2,
              "community" = "drsg1")

saveRDS(object = output, 
        file = "~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/r_scripts/reviewer_comments/01_different_response_same_gradient/drsg_cqo/cqo_results/drsg5_cqo_resuts.RDS")
