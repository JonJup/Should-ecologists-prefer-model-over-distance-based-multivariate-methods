## -------------------------------- ###
# --- CCA on type III communities --- # 
# -- with Hellinger Transformation -- #
### ------------------------------- ###

# date 03.12.19
# Should ecologist prefer model- over algorithm-based methods? 
# Calculating CCA on type III communities with Hellinger transformation

### OVERVIEW ###
# 01. Setup
# 02. Run CCA
### -------- ###


# 01. Setup ---------------------------------------------------------------

pacman::p_load(vegan, dplyr)


setwd(
      "~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/r_scripts/Type III communities/abundance_data/"
)

for (cca.i in 1:5) {
      data = readRDS(paste0("vac", cca.i, ".RDS"))
      sp.data <- data$data[, 1:30]
      #remove empty sites
      var.data <- scale(data$data[, 31:34])[, 1:4] %>% as.data.frame
      
      # remove emtpy rows, i.e. sites
      index <- apply(sp.data, 1, sum) > 0
      sp.data <- sp.data[index,]
      sp.data <- decostand(sp.data, method = "hellinger")
      var.data <- var.data[index,]
      
      # run model
      cca.drsg.time.before = Sys.time()
      mod <- cca(sp.data ~ ., data = var.data,)
      anv1 <- anova.cca(mod,
                        step  = 1000,
                        by = "margin",
                        parallel = 2)
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
            "Response" = paste0("vac", cca.i),
            "Times" = cca.drsg.time.run,
            "Transform" = "hellinger"
      )
      saveRDS(
            object = output,
            file = paste0(
                  "../../../result_data/08_type_III_communities/type_III_cca/",
                  cca.i,
                  "_cca_hellinger_result.RDS"
            )
      )
      print(cca.i)
}
beepr::beep()
