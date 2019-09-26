### --- p-value Boxplots 



# 01. Setup ---------------------------------------------------------------
pacman::p_load(here, dplyr, ggplot2, magrittr)

# define variables 
colorscheme.wd = here::here("05_Plots/Color Schemes/")
colorscheme.file = "mycol.R"
result.wd = here::here("04_Analysis", "02_Summary Tables")
result.file = "all_results.csv"
# load color scheme 
setwd(colorscheme.wd)
source(colorscheme.file)

# load result table 
setwd(result.wd)
dat <- readr::read_csv(result.file)


# 02. Prepare for plot ----------------------------------------------------

# subset 
sub.response = filter(dat, response %in% c("UU", "UL", "UB", "LL", "LB", "BB"))

# order response type 
sub.response$response <- factor(sub.response$response, levels = c("UU", "UL", "UB", "LL", "LB", "BB"))


# subset to RDA -- new dataset 
rda.data = filter(sub.response, method == "dbrda") %>% select(p.value, variable, response, samples)
cca.data = filter(sub.response, method == "CCA") %>%  select(p.value, variable, response, samples)

# add ID to join later 
rda.data$seed = rep(1:5, each = 4)
rda.data$variable = rep(c("env1", "env2", "rand1", "rand2"))
rda.data %<>% mutate(ID = paste0(response,"_",
                                 samples, "_",
                                 variable, "_",
                                 seed))
# test ID 
length(unique(rda.data$ID)) == nrow(rda.data)

# now add ID to CCA 
cca.data$seed = rep(1:5, each = 4)
cca.data$variable = rep(c("env1", "env2", "rand1", "rand2"))
cca.data %<>% mutate(ID = paste0(response,"_",
                                 samples, "_",
                                 variable, "_",
                                 seed))

## rename p.value columns 
rda.data = rename(rda.data, rda = p.value)
cca.data = rename(cca.data, cca = p.value)

## join tables 
plot.data = left_join(rda.data, cca.data, by = "ID") %>% select(rda,cca, variable = variable.x, response = response.x)
plot.data$variable =  rep(c("env1", "env2", "non-causal", "non-causal"))


# 03. Plotting ------------------------------------------------------------

(rcl <-
    plot.data %>%
    ggplot(aes(y = log10(rda), x = log10(cca))) +
    geom_jitter( aes(fill = response), size = 3, alpha = 1, pch = 21, height = 0.5, width = 0.5) + 
    # color scheme 
    scale_color_manual(values = mycol, guide = F) +     
    scale_fill_manual(values = mycol) + 
    scale_shape_manual(values = c(21, 22, 23)) +
    labs(fill = "Response") +
    theme(legend.background = element_rect(fill = "lightgrey"),
          legend.key.size = unit(1,"cm"),
          legend.title=element_text(size = 20),
          legend.text = element_text(size = 14)
          ) + 
    guides(fill = guide_legend(ncol = 2,
                               override.aes = list(size = 5))))
     
    

rc1 <- 
    plot.data %>% 
    filter(variable == "env1") %>% 
    ggplot(aes(y = log10(rda), x = log10(cca))) + 
    #rectangles 
    annotate("rect",xmin = -1.3, xmax = 1, ymin = -4, ymax = -1.3,
             alpha = 0.2, fill = "orange") + 
    annotate("rect",xmin = -4, xmax = -1.3, ymin = -4, ymax = -1.3,
             alpha = 0.2, fill = "green") + 
    annotate("rect",xmin = -4, xmax = -1.3, ymin = -1.3, ymax = 1,
             alpha = 0.2, fill = "orange") +
    annotate("rect",xmin = -1.3, xmax = 1, ymin = -1.3, ymax = 1,
             alpha = 0.2, fill = "red") +
    # points 
    geom_jitter( aes(fill = response), size = 3, alpha = 1, pch = 21, height = 0.5, width = 0.5) + 
    
    # horizontal lines indicating 0.05 
    geom_segment(aes(x = log10(0.05), xend = log10(0.05), y = -4, yend = 1), linetype = "dashed") + 
    geom_segment(aes(y = log10(0.05), yend = log10(0.05), x = -4, xend = 1), linetype = "dashed") +  
    
    # axis labels    
    geom_text(label = "dbRDA", y = -2, x = -4.2, hjust = 0, size = 8, angle = 90) +
    geom_text(label = "CCA", y = -4.2, x = -1.5, hjust = 0, size = 8) +
    geom_text(label = "env1", y = 1.5, x = -4, hjust = 0, size = 8) +
    
    # color scheme 
    scale_color_manual(values = mycol, guide = F ) +     
    scale_fill_manual(values = mycol, guide = F ) + 
    scale_shape_manual(values = c(21, 22, 23)) +
    
    # theme 
    theme_void() + 
    theme(text = element_text(size=15),
          strip.text.x = element_text(size = 15),
          legend.background = element_rect(fill = "gray90"),
          legend.position = "top",
          panel.grid.major = element_blank(),   # the next two remove the background grid
          panel.grid.minor = element_blank()) +
    xlab(expression(paste("CCA: ", italic("env1 "), "p-value", sep = " ")))+ 
    ylab(expression(paste("db-RDA: ", italic("env1 "), "p-value", sep = " ")))+ 
    ylim(-4,1) + 
    xlim(-4,1) + 
    coord_cartesian(clip = "off", xlim = c(-4.2,1), ylim = c(-4.2,1.5))

 
rc2 <- 
    plot.data %>% 
    filter(variable == "env2") %>% 
    ggplot(aes(y = log10(rda), x = log10(cca))) +
    
    #rectangles 
    annotate("rect",xmin = -1.3, xmax = 1, ymin = -4, ymax = -1.3,
             alpha = 0.2, fill = "orange") + 
    annotate("rect",xmin = -4, xmax = -1.3, ymin = -4, ymax = -1.3,
             alpha = 0.2, fill = "green") + 
    annotate("rect",xmin = -4, xmax = -1.3, ymin = -1.3, ymax = 1,
             alpha = 0.2, fill = "orange") +
    annotate("rect",xmin = -1.3, xmax = 1, ymin = -1.3, ymax = 1,
             alpha = 0.2, fill = "red") +
    # points 
    geom_jitter( aes(fill = response), size = 3, alpha = 1, pch = 21, height = 0.5, width = 0.5) + 
    
    # horizontal lines indicating 0.05 
    geom_segment(aes(x = log10(0.05), xend = log10(0.05), y = -4, yend = 1), linetype = "dashed") + 
    geom_segment(aes(y = log10(0.05), yend = log10(0.05), x = -4, xend = 1), linetype = "dashed") + 

    # axis labels
    geom_text(label = "dbRDA", y = -2, x = -4.2, hjust = 0, size = 8, angle = 90) +
    geom_text(label = "CCA", y = -4.2, x = -1.5, hjust = 0, size = 8) +
    geom_text(label = "env2", y = 1.5, x = -4, hjust = 0, size = 8) +
    
    # color scheme 
    scale_color_manual(values = mycol, guide = F ) +     
    scale_fill_manual(values = mycol, guide = F ) + 
    scale_shape_manual(values = c(21, 22, 23)) +
    
    # theme 
    theme_void() + 
    theme(text = element_text(size = 15),
          strip.text.x = element_text(size = 15),
          legend.background = element_rect(fill = "gray90"),
          legend.position = "top",
          panel.grid.major = element_blank(),   # the next two remove the background grid
          panel.grid.minor = element_blank()) +
    # xlab(expression(paste("CCA: ", italic("env2"), "p-value", sep = " ")))+ 
    # ylab(expression(paste("db-RDA: ", italic("env2"), "p-value", sep = " ")))+ 
    ylim(-4,1) + 
    xlim(-4,1) + 
    coord_cartesian(clip = "off", xlim = c(-4.2,1), ylim = c(-4.2,1.5))
rcn <- 
    plot.data %>% 
    filter(variable == "non-causal") %>% 
    ggplot() + 
    #rectangles 
    annotate("rect",xmin = -1.3, xmax = 1, ymin = -4, ymax = -1.3,
              alpha = 0.2, fill = "orange") + 
    annotate("rect",xmin = -4, xmax = -1.3, ymin = -4, ymax = -1.3,
                 alpha = 0.2, fill = "red") + 
    annotate("rect",xmin = -4, xmax = -1.3, ymin = -1.3, ymax = 1,
                 alpha = 0.2, fill = "orange") +
    annotate("rect",xmin = -1.3, xmax = 1, ymin = -1.3, ymax = 1,
                 alpha = 0.2, fill = "green") +
        
    # points 
    geom_jitter(aes(y = log10(rda), x = log10(cca), fill = response),
                size = 3, alpha = 1, pch = 21, height = 0.5, width = 0.5) + 
    
    # horizontal lines indicating 0.05 
    geom_segment(aes(x = log10(0.05), xend = log10(0.05), y = -4, yend = 1), linetype = "dashed") + 
    geom_segment(aes(y = log10(0.05), yend = log10(0.05), x = -4, xend = 1), linetype = "dashed") +   
    
    # axis labels    
    geom_text(label = "dbRDA", y = -2, x = -4.2, hjust = 0, size = 8, angle = 90) +
    geom_text(label = "CCA", y = -4.2, x = -1.5, hjust = 0, size = 8) +
    geom_text(label = "Non Causal", y = 1.5, x = -4, hjust = 0, size = 8) +
    
    # color scheme 
    scale_color_manual(values = mycol, guide = F ) +     
    scale_fill_manual(values = mycol, guide = F ) + 
    scale_shape_manual(values = c(21, 22, 23)) +
    
    # theme 
    theme_void() + 
    theme(text = element_text(size =  15),
          strip.text.x = element_text(size = 15),
          legend.background = element_rect(fill = "gray90"),
          legend.position = "top",
          panel.grid.major = element_blank(),   # the next two remove the background grid
          panel.grid.minor = element_blank()) +
    xlab(expression(paste("CCA: ", italic("env1 "), "p-value", sep = " "))) + 
    ylab(expression(paste("db-RDA: ", italic("env1 "), "p-value", sep = " "))) + 
    ylim(-4,1) + 
    xlim(-4,1) + 
    coord_cartesian(clip = "off", xlim = c(-4.2,1), ylim = c(-4.2,1.5))

## plot all togehter     
cowplot::ggdraw(cowplot::plot_grid(rc1, rc2, rcn, cowplot::get_legend(rcl)))# , labels = c("Env1", "Env1","Non-causal")))
               
