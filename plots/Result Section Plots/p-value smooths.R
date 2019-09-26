# 01. Setup ---------------------------------------------------------------
pacman::p_load(here, dplyr, ggplot2, magrittr, ggthemes, cowplot)

result.wd = here::here("04_Analysis", "02_Summary Tables")
result.file = "all_results.csv"

# load result table 
setwd(result.wd)
dat <- readr::read_csv(result.file)



dat %>% 
      filter(response == "UU") %>% 
      ggplot(aes(y = p.value, x = method)) + 
      geom_point(aes(fill  = variable), shape = 21)





for (i in unique(dat$method)) {
      
      for (k in unique(dat$variable)) {
      
            assign(paste0(i,"_",k),
                   filter(dat, method == i) %>% 
                         filter(variable == k) %>% 
                         ggplot(aes(x = samples, y = p.value)) + 
                         geom_smooth(aes(col = response), se = F, size = 0.1, alpha = 0.3) +
                         scale_colour_jco() +
                         stat_summary(
                               geom = "point",
                               fun.y = "mean" ,
                               aes(col = response),
                               size = 4,
                               shape = 17,
                               alpha = 0.9,
                               position = "jitter"
                               
                         ) + 
                         ylim(0,1) + 
                         xlim(20, 905) + 
                         theme_few() + 
                         theme(legend.position = "none",
                               axis.text.x=element_blank())+
                               #axis.ticks.x = element_line(colour = "black", size = .5),
                              # axis.ticks.length = unit(.5, "line")) +
                         xlab("") +
                         ylab("") + 
                         scale_x_continuous(breaks = c(25,100,225,400,625,900))      
                   )      
            
      }
      
      
}

legend = dat %>% 
      ggplot(aes(x = samples, y = p.value)) + 
      #geom_smooth(aes(col = response), se =F) + 
      geom_point(aes(col = response), shape = 17, size = 5)  + 
      scale_colour_jco() +
      labs(colour = "Response") + 
      theme(legend.background = element_rect(fill = "lightgrey"),
            legend.key.size = unit(1,"cm"),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 14)
      ) + 
      guides(col = guide_legend(ncol = 6, override.aes = list(size = 5)))

leg = get_legend(legend)

ggdraw(
      plot_grid(
            NULL, NULL, NULL,
            mvglm_env1,mvglm_env2,mvglm_Noise,
            CQO_env1,CQO_env2,CQO_Noise,
            CCA_env1,CCA_env2,CCA_Noise,
            dbrda_env1,dbrda_env2,dbrda_Noise,
            NULL, leg, NULL,
            cols = 3
            )) + 
            draw_label(label = "env1", angle = 15, size = 40, alpha = 1,
                       x = .15, y = .9) + 
            draw_label(label = "env2", angle = 15, size = 40, alpha = 1,
                 x = .45, y = .9) +  
            draw_label(label = "noise", angle = 15, size = 40, alpha = 1,
                 x = .75, y = .9) 

                