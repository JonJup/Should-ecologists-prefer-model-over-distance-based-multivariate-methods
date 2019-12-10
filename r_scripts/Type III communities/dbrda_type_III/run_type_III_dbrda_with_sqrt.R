## ---------------------------------- ###
# --- dbRDA on type III communities --- # 
# ----- with sqrt Transformation ------ #
### --------------------------------- ###

# date 03.12.19
# Should ecologist prefer model- over algorithm-based methods? 
# Calculating dbRDA on type III communities with sqrt transformation

### OVERVIEW ###
# 01. Setup
# 02. Run dbRDA
### -------- ###


# 01. Setup ---------------------------------------------------------------
pacman::p_load(vegan, dplyr)
setwd(
      "~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/r_scripts/Type III communities/abundance_data/"
)
for (rda.i in 1:5) {
      data = readRDS(paste0("vac", rda.i, ".RDS"))
      sp.data <- data$data[, 1:30]
      #remove empty sites
      var.data <- scale(data$data[, 31:34])[, 1:4] %>% as.data.frame
      
      # remove emtpy rows, i.e. sites
      index <- apply(sp.data, 1, sum) > 0
      sp.data <- sp.data[index,]
      sp.data <- sqrt(sp.data)
      var.data <- var.data[index,]
      
      # run model
      drsg.time.before = Sys.time()
      mod <- dbrda(sp.data ~ ., data = var.data, distance = "bray")
      anv1 <- anova.cca(mod,
                        step  = 1000,
                        by = "margin",
                        parallel = 10)
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
            "Response" = paste0("vac", rda.i),
            "transformation" = "sqrt",
            "Times" = drsg.time.run
      )
      saveRDS(
            object = output,
            file = paste0(
                  "../../../result_data/08_type_III_communities/type_III_dbRDA/vac_",
                  rda.i,
                  "_dbrda_sqrt_result.RDS"
            )
      )
      print(rda.i)
}
beepr::beep()