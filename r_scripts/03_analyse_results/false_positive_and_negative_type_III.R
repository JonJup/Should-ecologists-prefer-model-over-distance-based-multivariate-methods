### ------------------------------------------------ ###
# --- Create False positive and negative summaries --- #
# ------------- of communities type III -------------- #
### ------------------------------------------------ ###

#date: 03.12.19
# Should ecologist prefer model- over algorithm-based methods?
# calculate false positive and negative rates for type I communities.

## -- OVERVIEW -- ## 
# 01. Setup
# 02. Calculate FPR and FNR
# 03. Save to file
# ---------------- #

# 01. Setup -------------------------------------------------------------------

pacman::p_load(
        dplyr,
        data.table
)

# load result table
setwd("~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/result_data/08_type_III_communities//")
dat <- readRDS("type_III_all_results.RDS")


# 02. Calculate FPR and FNR  --------------------------------------------------

dat[, method2 := ifelse(transformation == "NA", method, paste0(method,"_",transformation))]

# 02. Calculate FPR and FNR  --------------------------------------------------

FPR = data.table(FPR = rep(0, ),
                 method = rep("empty", 50),
                 sig.lvl = rep(0, 50))
FNR = data.table(FNR = rep(0, 50),
                 method = rep("empty", 50),
                 sig.lvl = rep(0, 50))


method.vec = unique(dat$method2)
siglevels = c(0.01, 0.03, 0.05, 0.07, 0.1)
for (i in (seq_along(method.vec)-1)) {
      
      methods = method.vec[i + 1] 
      dat2 = dat[method2 == methods]
      
      for (k in 1:5) {
            
            alpha = siglevels[k]
            FP = ifelse(
                  alpha == 0.01, 
                  dat2[,sum(false.positive.01)],
                  ifelse(
                        alpha == 0.03,
                        dat2[,sum(false.positive.03)],
                        ifelse(
                              alpha == 0.05,
                              dat2[,sum(false.positive.05)],
                              ifelse(
                                    alpha == 0.07,
                                    dat2[,sum(false.positive.07)],
                                    dat2[,sum(false.positive.1)]
                              )
                        )
                  )
                  
            )
            FN = ifelse(
                  alpha == 0.01, 
                  dat2[,sum(false.negative.01)],
                  ifelse(
                        alpha == 0.03,
                        dat2[,sum(false.negative.03)],
                        ifelse(
                              alpha == 0.05,
                              dat2[,sum(false.negative.05)],
                              ifelse(
                                    alpha == 0.07,
                                    dat2[,sum(false.negative.07)],
                                    dat2[,sum(false.negative.1)]
                              )
                        )
                  )
                  
            )
            TN = dat2[variable == "Noise" & p.value > alpha] %>% nrow()
            TP = dat2[variable %like% "env1" & p.value <= alpha] %>% nrow()
            FPR.var =  FP/(TN + FP)
            FNR.var = FN/(TP + FN)
            FPR[i * 5 + k, c("FPR", "method", "sig.lvl") := .(FPR.var, methods, alpha)]
            FNR[i * 5 + k, c("FNR", "method", "sig.lvl") := .(FNR.var, methods, alpha)]
            
      }
      
      
}

FPR %>% setDT
FNR %>% setDT

FPR[method == "mvglm", method := "MvGLM"]
FNR[method == "mvglm", method := "MvGLM"]
FNR[method == "dbRDA_no transformation", method := "dbRDA"]
FPR[method == "dbRDA_no transformation", method := "dbRDA"]
FPR[method == "CCA_SQRT", method := "CCA_sqrt"]
FNR[method == "CCA_SQRT", method := "CCA_sqrt"]
FNR[method == "CCA_no_transformation", method := "CCA"]
FPR[method == "CCA_no_transformation", method := "CCA"]
FNR[method == "CCA_LOG", method := "CCA_log"]
FPR[method == "CCA_LOG", method := "CCA_log"]

unique(FPR$method)


# 03. Save to file --------------------------------------------------------

 saveRDS(FNR, "../../result_data/06_false_negative_and_positive_rates/FNR_type_III.RDS")
 saveRDS(FPR, "../../result_data/06_false_negative_and_positive_rates/FPR_type_III.RDS")

