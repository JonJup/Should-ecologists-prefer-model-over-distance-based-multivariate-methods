#################################
### --- FPR and FNR plots --- ###
#################################



# Setup -------------------------------------------------------------------

pacman::p_load(here,
               dplyr,
               ggplot2,
               ggthemes, # for base theme
               cowplot, # ggdraw for annotations
               ggstance, # vertical dodging
               wesanderson, # color palette,
               data.table
)


result.wd = here::here("04_Analysis", "02_Summary Tables")
result.file = "all_results.csv"

# load result table
setwd(result.wd)
dat <- fread(result.file)

# color palette
mycol = wes_palette("Zissou1")[c(1,4,5)]


# Calculate FPR and FNR  --------------------------------------------------

FPR = data.table(FPR = rep(0, 20),
                 method = rep("empty", 20),
                 sig.lvl = rep(0, 20))
FNR = data.table(FNR = rep(0, 20),
                 method = rep("empty", 20),
                 sig.lvl = rep(0, 20))


method.vec = c("mvglm", "CQO", "CCA", "dbrda")
siglevels = c(0.01, 0.03, 0.05, 0.07, 0.1)
for (i in 0:3) {
      
      methods = method.vec[i + 1] 
      dat2 = dat[method == methods]
      
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
FPR[method == "dbrda", method := "dbRDA"]
FNR[method == "dbrda", method := "dbRDA"]

fpr_plot = ggplot(data = FPR, aes(x = sig.lvl, y = FPR)) + 
      geom_line(aes(col = method), size = 1, alpha = 0.9) + 
      geom_point(aes(fill = method), shape = 21, size = 3) + 
      ylab(label = "False Positive Rate") +
      xlab(label = "Significance level") + 
      ylim(0, 0.6) +
      #theme_economist() + 
      theme(
            axis.title.x = element_text(size = 15),
            legend.title = element_blank(),
            legend.position = "none",
            axis.title.y = element_blank()
      )

fnr_plot = ggplot(data = FNR, aes(x = sig.lvl, y = FNR)) + 
      geom_line(aes(col = method), size = 1, alpha = 0.9, show.legend = F) + 
      geom_point(aes(fill = method), shape = 21, size = 3) + 
      ylab(label = "False Negative Rate") +
      xlab(label = "Significance level") + 
      ylim(0, 0.6) + 
      #theme_minimal() + 
      theme(
            axis.title.x = element_text(size = 15),
            legend.title = element_blank(),
            axis.title.y = element_blank()
            
      )

both = plot_grid(fpr_plot, fnr_plot, rel_widths = c(1,1.3), labels = c("False Positive Rate", "False Negative Rate"))
# Save Plots --------------------------------------------------------------

ggplot2::ggsave(plot = both,
                filename = "../../05_Plots/FPNR.png",
                height = 10,
                width = 20,
                units = "cm")
# ggplot2::ggsave(plot = fnr_plot,
#                 filename = "../../06_Talk/Figures/FPR_FRN_plots/FNR.png",
#                 height = 13,
#                 width = 20,
#                 units = "cm")
