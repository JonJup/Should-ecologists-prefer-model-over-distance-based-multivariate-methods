### -------------------------------------------------------------- ###
# -------------------- Supplementary Materials: -------------------- # 
# --------------------        Plot runtime        ------------------ #
### -------------------------------------------------------------- ###


# Jonathan Jupke
# date unknown 
# Paper: Should ecologists prefer model- over algorithm-based multivariate methods?

# This script shows how the runtime plot for the supplementary marterials section was created.

## -- OVERVIEW -- ## 
# 01.Setup
# 02.Work on Table
# 03.Create Plot
# 04.Save Plot to PDF 
## -------------- ##


# 01. Setup ---------------------------------------------------------------

pacman::p_load(
      ggplot2, 
      data.table,
      dplyr,
      tidyr
)
#other required packages: here

dat_wd = here::here("result_data/05_collected_results/")
setwd(dat_wd)


# 02. Work on Table -------------------------------------------------------

data = fread("all_results.csv")
data$response = factor(data$response, levels = c("UU", "UL", "UB", "LL", "LB", "BB"))
data[method == "mvglm", method := "MvGLM"]
data[method == "dbrda", method := "dbRDA"]


# 03. Create Plot ---------------------------------------------------------


runtimeplot = data %>% 
  ggplot(aes(x = runtime, y = method)) + 
  geom_point(aes(color = samples), size = 2, alpha = 0.8) + 
  scale_x_log10() + 
  xlab("log10 run time [s]") +
  ylab("Method")


# 04. Save Plot to PDF ----------------------------------------------------

ggplot2::ggsave(filename = "runtimeplot.pdf",
                plot = runtimeplot,
                path = here::here("plots/")
       )
