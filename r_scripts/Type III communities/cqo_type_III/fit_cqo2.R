### --------------------------- ###
# --- FIT CQO VAC COMMUNITY 2 --- #
### --------------------------- ###

pacman::p_load(
      VGAM,      ## CQO
      gtools, 
      foreach,   ## parallel processing
      parallel,  ## parallel processing
      doParallel,## parallel processing 
      purrr
)

setwd("~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/r_scripts/reviewer_comments/")

# load data 
data = readRDS("02_variable_abundance_communities/vac2.RDS")

# scale environmental variables
data2 = data$data
data2[, 31:34]   <- scale(data2[, 31:34])
x = 5
while (x > 3) {
      time.before = Sys.time()
      cqo.pr2  <- cqo(
                  cbind(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9, sp10, sp11, sp12, sp13,
                        sp14, sp15, sp16, sp17, sp18, sp19, sp20, sp21, sp22, sp23, sp24, sp25,
                        sp26, sp27, sp28, sp29, sp30) ~
                        env1 + env2 + rand1 + rand2,
                  data = data2,
                  family =  poissonff,
                  Rank = 2,
                  df1.nl = 1.5,
                  Bestof = 50,
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
for (column in 31:34) {
      
      data.permute <- data2
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
                    c("safe_cqo","n.permute","data2","data.permute",
                      "data.f", "column"),envir = environment())
      clusterEvalQ(cluster, library(VGAM))
      x <- foreach(cqo.1.i = 1:n.permute) %dopar% {
            data.permute[,column] <- data2[,column][data.f[cqo.1.i,]]
            timer <- 0
            while (timer == 0) {
                  result1 <- safe_cqo(
                        
                        cbind(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9, sp10, sp11, sp12, sp13,
                              sp14, sp15, sp16, sp17, sp18, sp19, sp20, sp21, sp22, sp23, sp24, sp25,
                              sp26, sp27, sp28, sp29, sp30) ~
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
            coef[i,] <- summary(x[[i]][[1]])@post$Coef@C[column - 29,]
      }
      
      coef2 <- apply(abs(coef),1,sum)
      p.values[column - 29] <-  length(which(coef2 > sum(abs(summary(cqo.pr2)@post$Coef@C[column - 29,])))) / (length(coef2) + 1)
      
      print(paste("P Value for Varaible", column, "calculated"))
      
}     
time.after2 = Sys.time()
time.run2 = time.after2 - time.before

output = list("p.values" = p.values,
              "runtime.cqo" = time.run,
              "runtime.cqo.p.value" = time.run2,
              "community" = "vac2")

saveRDS(object = output, 
        file = "~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/r_scripts/reviewer_comments/02_variable_abundance_communities/vac_cqo/cqo_results/vac2_cqo_resuts.RDS")
