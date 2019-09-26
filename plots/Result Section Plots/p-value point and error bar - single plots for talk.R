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

# assign the order from before to responseXmethod and turn into a factor.
dat$response <- factor(dat$response,
                             levels = rev(c("UU", "UL", "UB", "LL", "LB", "BB")))

dat$variable = factor(dat$variable,
                      levels = c("Noise", "env2", "env1"))
## Aggregate mean table for each method 
glm_stat_tbl = dat %>%
   filter(method == "mvglm") %>% 
   group_by(variable, response) %>% 
   summarize(mean = mean(p.value),
             sd = sd(p.value))
cqo_stat_tbl = dat %>%
   filter(method == "CQO") %>% 
   group_by(variable, response) %>% 
   summarize(mean = mean(p.value),
             sd = sd(p.value))
cca_stat_tbl = dat %>%
   filter(method == "CCA")  %>% 
   group_by(variable, response) %>% 
   summarize(mean = mean(p.value),
             sd = sd(p.value))
             
dbrda_stat_tbl = dat %>%
   filter(method == "dbrda") %>% 
   group_by(variable, response) %>% 
   summarize(mean = mean(p.value),
             sd = sd(p.value))


## Aggregate sd table for each method 
# GLM Plot  -----------------------------------------------------------------

glm_error_bar = ggplot() +
   
   
   # background elements 
      geom_vline(xintercept = 0.05, linetype = "dashed", size = 1.5) +
   # main geoms
      #error bars
         ggplot2::geom_errorbarh(
            data = glm_stat_tbl,
            aes(
               y = response,
               xmin = mean - sd,
               xmax = mean + sd,
               col = variable,
            ),
            height = 0, # this removes the vertical lines at the end of the error bars
            position = position_dodgev(height = 1),
            size = 2,
            alpha = 0.6
         ) +
      # means as points
         geom_point(
            data = glm_stat_tbl,
            aes(y = response,
                x = mean,
                fill = variable),
            size = 6,
            position = position_dodgev(height = 1),
            shape = 21,
            alpha = 0.9
         ) + 
      # color palette for points
         scale_fill_manual(values = rev(mycol)) +
      # color palette for error bars (same as points but without guide)
         scale_colour_manual(values = rev(mycol), guide = F) +
      # remove y axis label because its obvious to someone who is reading the paper
         ylab(label = "") +
      # x-axis label
         xlab(expression(paste(italic("p"), "- value"))) +
      # setting a theme ...  
         theme_gray(base_size = 15) +
      # ... and altering it
         theme(
            legend.position = "top",
            legend.background = element_rect(fill = "lightgrey"),
            # extend plot margin for annotations
            plot.margin = unit(c(1, 5, 1, 1), "lines")  
            ) +
      # alter legend
         guides(fill =
                   guide_legend(
                      ncol = 3,
                      # size of points in guide
                      override.aes = list(size = 8),
                      reverse = TRUE
                      
                   )
         )
glm_error_bar   

# CQO Plot ----------------------------------------------------------------

cqo_error_bar = ggplot() +
   
   
   # background elements 
   geom_vline(xintercept = 0.05, linetype = "dashed", size = 1.5) +
   # main geoms
   #error bars
   ggplot2::geom_errorbarh(
      data = cqo_stat_tbl,
      aes(
         y = response,
         xmin = mean - sd,
         xmax = mean + sd,
         col = variable,
      ),
      height = 0, # this removes the vertical lines at the end of the error bars
      position = position_dodgev(height = 1),
      size = 2,
      alpha = 0.6
   ) +
   # means as points
   geom_point(
      data = cqo_stat_tbl,
      aes(y = response,
          x = mean,
          fill = variable),
      size = 6,
      position = position_dodgev(height = 1),
      shape = 21,
      alpha = 0.9
   ) + 
   # color palette for points
   scale_fill_manual(values = rev(mycol)) +
   # color palette for error bars (same as points but without guide)
   scale_colour_manual(values = rev(mycol), guide = F) +
   # remove y axis label because its obvious to someone who is reading the paper
   ylab(label = "") +
   # x-axis label
   xlab(expression(paste(italic("p"), "- value"))) +
   # setting a theme ...  
   theme_gray(base_size = 15) +
   # ... and altering it
   theme(
      legend.position = "top",
      legend.background = element_rect(fill = "lightgrey"),
      # extend plot margin for annotations
      plot.margin = unit(c(1, 5, 1, 1), "lines")  
   ) +
   # alter legend
   guides(fill =
             guide_legend(
                ncol = 3,
                # size of points in guide
                override.aes = list(size = 8),
                reverse = TRUE
                
             )
   )
cqo_error_bar

# CCA plot  ---------------------------------------------------------------
cca_error_bar = ggplot() +
   
   
   # background elements 
   geom_vline(xintercept = 0.05, linetype = "dashed", size = 1.5) +
   # main geoms
   #error bars
   ggplot2::geom_errorbarh(
      data = cca_stat_tbl,
      aes(
         y = response,
         xmin = mean - sd,
         xmax = mean + sd,
         col = variable,
      ),
      height = 0, # this removes the vertical lines at the end of the error bars
      position = position_dodgev(height = 1),
      size = 2,
      alpha = 0.6
   ) +
   # means as points
   geom_point(
      data = cca_stat_tbl,
      aes(y = response,
          x = mean,
          fill = variable),
      size = 6,
      position = position_dodgev(height = 1),
      shape = 21,
      alpha = 0.9
   ) + 
   # color palette for points
   scale_fill_manual(values = rev(mycol)) +
   # color palette for error bars (same as points but without guide)
   scale_colour_manual(values = rev(mycol), guide = F) +
   # remove y axis label because its obvious to someone who is reading the paper
   ylab(label = "") +
   # x-axis label
   xlab(expression(paste(italic("p"), "- value"))) +
   # setting a theme ...  
   theme_gray(base_size = 15) +
   # ... and altering it
   theme(
      legend.position = "top",
      legend.background = element_rect(fill = "lightgrey"),
      # extend plot margin for annotations
      plot.margin = unit(c(1, 5, 1, 1), "lines")  
   ) +
   # alter legend
   guides(fill =
             guide_legend(
                ncol = 3,
                # size of points in guide
                override.aes = list(size = 8),
                reverse = TRUE
                
             )
   )
cca_error_bar

# dbrda plot --------------------------------------------------------------
dbrda_error_bar = ggplot() +
   
   
   # background elements 
   geom_vline(xintercept = 0.05, linetype = "dashed", size = 1.5) +
   # main geoms
   #error bars
   ggplot2::geom_errorbarh(
      data = dbrda_stat_tbl,
      aes(
         y = response,
         xmin = mean - sd,
         xmax = mean + sd,
         col = variable,
      ),
      height = 0, # this removes the vertical lines at the end of the error bars
      position = position_dodgev(height = 1),
      size = 2,
      alpha = 0.6
   ) +
   # means as points
   geom_point(
      data = dbrda_stat_tbl,
      aes(y = response,
          x = mean,
          fill = variable),
      size = 6,
      position = position_dodgev(height = 1),
      shape = 21,
      alpha = 0.9
   ) + 
   # color palette for points
   scale_fill_manual(values = rev(mycol)) +
   # color palette for error bars (same as points but without guide)
   scale_colour_manual(values = rev(mycol), guide = F) +
   # remove y axis label because its obvious to someone who is reading the paper
   ylab(label = "") +
   # x-axis label
   xlab(expression(paste(italic("p"), "- value"))) +
   # setting a theme ...  
   theme_gray(base_size = 15) +
   # ... and altering it
   theme(
      legend.position = "top",
      legend.background = element_rect(fill = "lightgrey"),
      # extend plot margin for annotations
      plot.margin = unit(c(1, 5, 1, 1), "lines")  
   ) +
   # alter legend
   guides(fill =
             guide_legend(
                ncol = 3,
                # size of points in guide
                override.aes = list(size = 8),
                reverse = TRUE
                
             )
   )
dbrda_error_bar


# Save plots  -------------------------------------------------------------


ggplot2::ggsave(plot = glm_error_bar,
                filename = "../../06_Talk/Figures/Errorbar plots/glm_error_bar.png",
                height = 13,
                width = 20,
                units = "cm")
ggplot2::ggsave(plot = cqo_error_bar,
                filename = "../../06_Talk/Figures/Errorbar plots/cqo_error_bar.png",
                height = 13,
                width = 20,
                units = "cm")
ggplot2::ggsave(plot = cca_error_bar,
                filename = "../../06_Talk/Figures/Errorbar plots/cca_error_bar.png",
                height = 13,
                width = 20,
                units = "cm")
ggplot2::ggsave(plot = dbrda_error_bar,
                filename = "../../06_Talk/Figures/Errorbar plots/dbrda_error_bar.png",
                height = 13,
                width = 20,
                units = "cm")
