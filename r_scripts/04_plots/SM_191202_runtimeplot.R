### ------------------------------------------------------------ ###
# -------------------- Runtime plots ------------------------------#
### ------------------------------------------------------------ ###

#date 02.12.19
#Should ecologist prefer model- over algorithm-based methods?
#Plotting the runtimes of CCA, CQO, dbRDA abd MvGLM. 

pacman::p_load(ggplot2, dplyr, data.table)
setwd("~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/result_data/05_collected_results/")
data = fread("all_results.csv")

data[method == "dbrda", method := "dbRDA"]
data[method == "mvglm", method := "MvGLM"]

runtimeplot = ggplot(data = data, aes(x = runtime, y = method)) +
      
      geom_point(aes(col = samples)) + 
      # coord_trans(x = "log") +
      xlab(label = "runtime [seconds]") + 
      ylab(label = NULL)

ggplot2::ggsave(plot = runtimeplot,
                filename = "../../plots/191202_runtimeplot.pdf",
                height = 10,
                width = 20,
                units = "cm")
