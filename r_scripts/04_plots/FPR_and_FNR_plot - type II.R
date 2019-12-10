### ------------------------------------------------------------ ###
# --- False negative and positive plot for Type II communities --- #
### ------------------------------------------------------------ ### 

# 03.12.19
# Should ecologist prefer model- over algorithm-based methods?
# False positive and negative plot for type II communities.

## -- OVERVIEW -- ## 
# 01. Setup
# 02. Calculate FPR and FNR
# 03. Create Plot
# ---------------- #

# 01. Setup -------------------------------------------------------------------
pacman::p_load(ggplot2, 
               dplyr, 
               data.table, 
               #ggthemes,
               cowplot  # theme_minimal_hgrid
               )

setwd("~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/result_data/06_false_negative_and_positive_rates/")
FPR = readRDS("FPR_type_II.RDS")
FNR = readRDS("FNR_type_II.RDS")

fpr_plot = ggplot(data = FPR, aes(x = sig.lvl, y = FPR)) + 
      scale_fill_brewer( type = "qual", palette = 6, direction = 1,
                         aesthetics = "fill") + 
      scale_colour_brewer( type = "qual", palette = 6, direction = 1,
                           aesthetics = "colour") + 
      geom_line(aes(col = method), size = 1, alpha = 1) + 
      geom_point(aes(fill = method), shape = 21, size = 3) + 
      ylab(label = "False Positive Rate") +
      xlab(label = "Significance level") + 
      ylim(0, 0.58) +
      theme_minimal_hgrid() + 
      theme(
            axis.title.x = element_text(size = 15),
            legend.title = element_blank(),
            legend.position = "none",
            axis.title.y = element_blank()
      )
      

fnr_plot = ggplot(data = FNR, aes(x = sig.lvl, y = FNR)) + 
      scale_fill_brewer( type = "qual", palette = 6, direction = 1,
                         aesthetics = "fill") + 
      scale_colour_brewer( type = "qual", palette = 6, direction = 1,
                           aesthetics = "colour") + 
      geom_line(aes(col = method), size = 1, alpha = 1, show.legend = F) + 
      geom_point(aes(fill = method), shape = 21, size = 3) + 
      ylab(label = "False Negative Rate") +
      xlab(label = "Significance level") + 
      ylim(0, 0.58) + 
      theme_minimal_hgrid() + 
      theme(
            axis.title.x = element_text(size = 15),
            legend.title = element_blank(),
            axis.title.y = element_blank()
            
      )

(both = plot_grid(fpr_plot, 
                  fnr_plot))



# 04. Save Plot --------------------------------------------------------------

ggplot2::ggsave(plot = both,
                filename = "../../plots/191203_FPNR_type_II.pdf",
                height = 10,
                width = 20,
                units = "cm")


