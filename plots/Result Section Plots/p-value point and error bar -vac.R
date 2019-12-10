# 01. Setup ---------------------------------------------------------------
pacman::p_load(here, 
               dplyr, 
               ggplot2, 
               ggthemes, # for base theme 
               cowplot, # ggdraw for annotations 
               ggstance, # vertical dodging 
               wesanderson, # color palette,
               data.table
               )


setwd("~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/r_scripts/reviewer_comments/03_analyse_results/")
result.file = readRDS("vac_all_results.RDS")

# color palette 
mycol = wes_palette("Zissou1")[c(1,4,5)]
result.file[, method2 := ifelse(transformation == "NA", method, paste0(method,"_",transformation))]



# 02. Handle Data ---------------------------------------------------------

# From here on the aggregation happens. For each response X method combination I will calculate the mean and sd. 
mean_tbl = aggregate(result.file$p.value,
                     by = list(result.file$variable,
                               result.file$method2),
                     mean)
colnames(mean_tbl) = c("variable", "method", "Mean")

sd_tbl = aggregate(result.file$p.value,
                   by = list(result.file$variable,
                             result.file$method2),
                   sd)
colnames(sd_tbl) = c("variable",  "method" , "SD")

# now both aggregated tables will be joined 
stat_tbl <-
   merge(mean_tbl, sd_tbl, by.x = c(1, 2), by.y = c(1, 2))


# Plot  -----------------------------------------------------------------

error_bar =  ggplot() +
   
 # vertical line indicating a p-value of 0.05 
      geom_vline(xintercept = 0.05, linetype = "dashed", size = 1.5) +
      geom_point(
            data = result.file,
            aes(
                  x = p.value,
                  y = method2,
                  fill = variable 
            ),
            size = 6,
            position = position_dodgev(height = 1),
            shape = 21, alpha = 0.9
      )  +
   # theme related stuff 
      # color palette for points 
      scale_fill_manual(values = mycol) +
      # color palette for error bars (same as points but without guide)
      scale_colour_manual(values = mycol, guide = F) +
      # remove y axis label because its obvious to someone who is reading the paper 
      ylab(label = "") +
      # x-axis label 
      xlab(expression(paste(italic("p"), "- value"))) +
      # here I change the labeling of the y axis back to the response withouth method
      scale_y_discrete(labels = rep(c("MvGLM", "dbRDA", "dbRDA_hellinger", "CQO", "CCA_sqrt",
                                      "CCA", "CCA_log"), times = 4)) +
      # choosing the theme 
       #theme_classic(base_size = 15) +
      # altering it 
      theme(
         legend.position = "top",
         legend.background = element_rect(fill = "lightgrey"),
         plot.margin = unit(c(1, 5, 1, 1), "lines")  # extend plot margin for annotations
        # axis.text = element_text(size = 15)
      ) +
   # alter legend 
      guides(fill =
                guide_legend(
                   ncol = 3,
                   override.aes = list(size = 8) # size of points in guide
                   )
             ) +
   # remove "variable" from legend 
   labs(fill = "") + 
   #expand y axis 
   expand_limits(y = c(0:4))
   
error_bar

ggplot2::ggsave(plot = error_bar,
                filename = "../../../plots/Result Section Plots/191202_error_bar_vac.pdf",
                height = 20,
                width = 13,
                units = "cm")
