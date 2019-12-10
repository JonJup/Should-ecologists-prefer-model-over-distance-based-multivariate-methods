# drsg_cca

pacman::p_load(vegan, dplyr)
setwd("~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/r_scripts/reviewer_comments/")
for (rda.i in 1:5) {
  
  data = readRDS(paste0("01_different_response_same_gradient/drsg_community",rda.i,".RDS"))
  sp.data <- data[, 1:3]
  #remove empty sites
  var.data <- scale(data[, 4:7])[, 1:4] %>% as.data.frame

# remove emtpy rows, i.e. sites
index <- apply(sp.data, 1, sum) > 0
sp.data <- sp.data[index, ]
var.data <- var.data[index, ]

# run model
drsg.time.before = Sys.time()
mod <- dbrda(sp.data ~ .,data = var.data, distance = "bray")
anv1 <- anova.cca(mod, step  = 1000, by = "margin", parallel = 4)
drsg.time.after = Sys.time()
drsg.time.run = drsg.time.after - drsg.time.before
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
      "Times" = drsg.time.run
)
saveRDS(object = output, 
        file = paste0("01_different_response_same_gradient/drsg_dbrda/drsg",rda.i,"_dbrda_result.RDS"))
print(rda.i)
}
