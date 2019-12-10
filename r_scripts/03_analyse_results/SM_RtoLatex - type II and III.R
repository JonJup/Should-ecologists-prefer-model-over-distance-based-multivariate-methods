### -------------------------------------------------------------- ###
# -------------------- Supplementary Materials: -------------------- # 
# --------------------    Create Latex Tables   -------------------- #
# ---------------- of type II and III communities   ---------------- #
### -------------------------------------------------------------- ###

# 04.12.19
# Should ecologists prefer model- over algorithm-based multivariate methods?

# In this script I derive the tables for the supplementary materials from the
# data. Note that the layout was slightly changed within the latex code, which
# you can find in the github repository: 
# https://github.com/JonJup/Should-ecologists-prefer-model--over-algorithm-based-multivariate-methods.

## -- OVERVIEW -- ## 
# 1.Setup
# 2.type II
# 3.type III
## -------------- ##

# 01. Setup ---------------------------------------------------------------

pacman::p_load(
      stargazer, # convert R tables to latex code
      data.table,
      dplyr,
      tidyr
)

# other required packages: here

dat_wd = ("~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/result_data/")
setwd(dat_wd)


#data$response = factor(data$response, levels = c("UU", "UL", "UB", "LL", "LB", "BB"))

# 02.  type II ----------------------------------------------------

data = readRDS("07_type_II_communities/type_II_all_results.RDS") %>% 
      setDT
  
mean = data[, .(mean = mean(p.value)), by = .(method, variable)] %>% 
   spread(variable, mean) 

sd = data[, .(sd = mean(p.value)), by = .(method, variable)] %>% 
      spread(variable, sd) 

left_join(mean,
          sd,
          by = c("method")) %>%
      select(
            method,
            mean = "env1.x",
            sd = "env1.y",
            mean1 = "env2.x",
            sd1 = "env2.y",
            mean2 = "Noise.x",
            sd2 = "Noise.y"
      ) %>%
      stargazer(summary = FALSE, rownames = F)

# 03. type III   ----------------------------------------------------

data = readRDS("08_type_III_communities/type_III_all_results.RDS") %>% 
      setDT

data[, method2 := ifelse(transformation == "NA", method, paste0(method,"_",transformation))]

mean = data[, .(mean = mean(p.value)), by = .(method2, variable)] %>% 
      spread(variable, mean) 

sd = data[, .(sd = mean(p.value)), by = .(method2, variable)] %>% 
      spread(variable, sd) 

left_join(mean,
          sd,
          by = c("method2")) %>%
      select(
            method2,
            mean = "env1.x",
            sd = "env1.y",
            mean1 = "env2.x",
            sd1 = "env2.y",
            mean2 = "Noise.x",
            sd2 = "Noise.y"
      ) %>%
      stargazer(summary = FALSE, rownames = F)



