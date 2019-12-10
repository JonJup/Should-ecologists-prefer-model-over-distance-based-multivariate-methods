# drsg_cca

# 13.11.19

pacman::p_load(vegan, dplyr)
setwd("~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/r_scripts/reviewer_comments/")
for (cca.i in 1:5) {
  
  data = readRDS(paste0("01_different_response_same_gradient/drsg_community",cca.i,".RDS"))
  sp.data <- data[, 1:3]
  #remove empty sites
  var.data <- scale(data[, 4:7])[, 1:4] %>% as.data.frame

# remove emtpy rows, i.e. sites
index <- apply(sp.data, 1, sum) > 0
sp.data <- sp.data[index, ]
var.data <- var.data[index, ]

# run model
cca.drsg.time.before = Sys.time()
mod <- cca(sp.data ~ ., data = var.data, )
anv1 <- anova.cca(mod, step  = 1000, by = "margin", parallel = 2)
cca.drsg.time.after = Sys.time()
cca.drsg.time.run = cca.drsg.time.after - cca.drsg.time.before
mod.sum <- summary(mod)
#statements
p1 <- c(
      round(mod$CCA$tot.chi / mod$tot.chi * 100),
      round(sum(mod$CCA$eig[1:2]) / mod$CCA$tot.chi * 100),
      round(sum(mod$CCA$eig[1:2]) / mod$tot.chi * 100)
)
output <- list(
      "By Terms" = as.matrix(anv1),
      "Biplot Scores" = mod.sum$biplot,
      "Importance of components" = mod.sum$cont,
      "Variance Expplained" = p1,
      "Response" = "drsg1",
      "Times" = cca.drsg.time.run
)
saveRDS(object = output, 
        file = paste0("01_different_response_same_gradient/drsg_cca/drsg",cca.i,"_cca_result.RDS"))
}
