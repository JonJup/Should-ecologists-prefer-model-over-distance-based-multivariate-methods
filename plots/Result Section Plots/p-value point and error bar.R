# 01. Setup ---------------------------------------------------------------
pacman::p_load(here, 
               dplyr, 
               ggplot2, 
               ggthemes, # for base theme 
               cowplot, # ggdraw for annotations 
               ggstance, # vertical dodging 
               wesanderson # color palette  
               )


result.wd = here::here("04_Analysis", "02_Summary Tables")
result.file = "all_results.csv"

# load result table 
setwd(result.wd)
dat <- readr::read_csv(result.file)

# color palette 
mycol = wes_palette("Zissou1")[c(1,4,5)]


# 02. Handle Data ---------------------------------------------------------

# create resoponse X method variable. This is the combination of response (e.g.
# UU) and method (e.g. CCA). This enables me to plot all methods on one Y axis.
# Later within the ggplot call I will change the labels again.
dat = mutate(dat, resonseXmethod  = paste0(response, method))

# order response X method variable 
responseXmethod_levels =  rev(
   c("UUmvglm","ULmvglm","UBmvglm","LLmvglm",
      "LBmvglm","BBmvglm","UUCQO",
      "ULCQO","UBCQO","LLCQO",
      "LBCQO","BBCQO","UUCCA",
      "ULCCA","UBCCA","LLCCA",
      "LBCCA","BBCCA","UUdbrda",
      "ULdbrda","UBdbrda","LLdbrda",
      "LBdbrda","BBdbrda"
   )
)

# assign the order from before to responseXmethod and turn into a factor.
dat$resonseXmethod <- factor(dat$resonseXmethod,
                             levels = responseXmethod_levels)


# From here on the aggregation happens. For each response X method combination I will calculate the mean and sd. 
mean_tbl = aggregate(dat$p.value,
                     by = list(dat$variable,
                               dat$resonseXmethod),
                     mean)
colnames(mean_tbl) = c("variable", "responseXmethod", "Mean")

sd_tbl = aggregate(dat$p.value,
                   by = list(dat$variable,
                             dat$resonseXmethod),
                   sd)
colnames(sd_tbl) = c("variable",  "responseXmethod" , "SD")

# now both aggregated tables will be joined 
stat_tbl <-
   merge(mean_tbl, sd_tbl, by.x = c(1, 2), by.y = c(1, 2))


# Plot  -----------------------------------------------------------------

error_bar = 
   ggplot() +
   
   # Background elements 
      
      # the lower grey rectangle 
      geom_rect(
         data = dat,
         aes(y = resonseXmethod, x = p.value),
         xmin = -0.3,
         xmax = 1.3,
         ymin =   0,
         ymax = 6.5,
         alpha = 0.1,
         fill = "lightgrey"
      )     +
      # the upper grey rectangle 
      geom_rect(
         data = dat,
         aes(y = resonseXmethod, x = p.value),
         xmin = -0.3,
         xmax = 1.3,
         ymin =   12.5,
         ymax = 18.5,
         alpha = 0.1,
         fill = "lightgrey"
      )     +
   # vertical line indicating a p-value of 0.05 
   geom_vline(xintercept = 0.05, linetype = "dashed", size = 1.5) +
   # main geoms 
      #error bars 
      ggplot2::geom_errorbarh(
         data = stat_tbl,
         aes(
            y = responseXmethod,
            x = Mean,
            xmin = Mean - SD,
            xmax = Mean + SD,
            col = variable,
         ),
         height = 0, # this removes the vertical lines at the end of the error bars 
         position = position_dodgev(height = 1),
         size = 1,
         alpha = 0.9
      )     +
      # means as points
      geom_point(
         data = stat_tbl,
         aes(y = responseXmethod,
             x = Mean,
             fill = variable),
         size = 6,
         position = position_dodgev(height = 1),
         shape = 21,
         alpha = 0.9
      ) +

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
      scale_y_discrete(labels = rep(c("BB", "LB", "LL", "UB", "UL", "UU"), times = 4)) +
      # choosing the theme 
       theme_classic(base_size = 15) +
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
   expand_limits(y = c(0:25))
   

# Finalize Plot  ----------------------------------------------------------

error_bar2 = ggdraw(error_bar) +
   draw_label(
      label = "MvGLM",
      size = 15,
      x = .9,
      y = .80)  +
   draw_label(
      label = "CQO",
      size = 15,
      x = .875,
      y = .6)  +
   draw_label(
      label = "CCA",
      size = 15,
      x = .875,
      y = .38)  +
   draw_label(
      label = "dbRDA",
      size = 15,
      x = .9,
      y = .18)  
error_bar2

ggplot2::ggsave(plot = error_bar2,
                filename = "../../plots/Result Section Plots/191202_error_bar.pdf",
                height = 20,
                width = 13,
                units = "cm")
