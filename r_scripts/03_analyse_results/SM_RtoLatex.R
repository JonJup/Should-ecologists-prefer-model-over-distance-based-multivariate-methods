### -------------------------------------------------------------- ###
# -------------------- Supplementary Materials: -------------------- # 
# --------------------    Create Latex Tables   -------------------- #
### -------------------------------------------------------------- ###

# Jonathan Jupke
# date unknown 
# Paper: Should ecologists prefer model- over algorithm-based multivariate methods?

# In this script I derive the tables for the supplementary materials from the
# data. Note that the layout was slightly changed within the latex code, which
# you can find in the github repository: 
# https://github.com/JonJup/Should-ecologists-prefer-model--over-algorithm-based-multivariate-methods.

## -- OVERVIEW -- ## 
# 1.Setup
# 2.mvglm
# 3.cqo
# 4.cca
# 5.dbrda
## -------------- ##

# 01. Setup ---------------------------------------------------------------

pacman::p_load(
      stargazer, # convert R tables to latex code
      data.table,
      dplyr,
      tidyr
)

# other required packages: here

dat_wd = here::here("result_data/05_collected_results/")
setwd(dat_wd)

data = fread("all_results.csv")
data$response = factor(data$response, levels = c("UU", "UL", "UB", "LL", "LB", "BB"))
# 02.  mvglm ----------------------------------------------------
  
mm = data[method == "mvglm", .(mean = mean(p.value)), by = .(response, samples, variable)] %>% 
   spread(variable, mean) 

data[method == "mvglm", .(sd = sd(p.value)), 
     by = .(response, samples, variable)] %>% 
  spread(variable, sd) %>% 
  left_join(mm,., 
            by = c("response","samples")
            ) %>%
  select(
         response,
         samples,
         mean = "env1.x", 
         sd = "env1.y",
         mean1 = "env2.x",
         sd1 = "env2.y",
         mean2 = "Noise.x",
         sd2 = "Noise.y") %>%
  stargazer(summary = FALSE, rownames = F)

# 03. cqo   ----------------------------------------------------

mm = data[method == "CQO", .(mean = mean(p.value)), by = .(response, samples, variable)] %>% 
  spread(variable, mean) 

data[method == "CQO", .(sd = sd(p.value)), by = .(response, samples, variable)] %>% 
  spread(variable, sd) %>% 
  left_join(mm,., by = c("response","samples")) %>%
  select(
         response,
         samples,
         mean = "env1.x", 
         sd = "env1.y",
         mean1 = "env2.x",
         sd1 = "env2.y",
         mean2 = "Noise.x",
         sd2 = "Noise.y") %>%
  stargazer(summary = FALSE, rownames = F)


# 04. cca  ----------------------------------------------------

mm = data[method == "CCA", .(mean = mean(p.value)), by = .(response, samples, variable)] %>% 
  spread(variable, mean) 

data[method == "CCA", .(sd = sd(p.value)), by = .(response, samples, variable)] %>% 
  spread(variable, sd) %>% 
  left_join(mm,., by = c("response", "samples")) %>%
  select(
         response,
         samples,
         mean = "env1.x", 
         sd = "env1.y",
         mean1 = "env2.x",
         sd1 = "env2.y",
         mean2 = "Noise.x",
         sd2 = "Noise.y") %>%
  stargazer(summary = FALSE, rownames = F)


# 05. dbrda ----------------------------------------------------

mm = data[method == "dbrda", .(mean = mean(p.value)), by = .(response,samples, variable)] %>% 
  spread(variable, mean) 

data[method == "dbrda", .(sd = sd(p.value)), by = .(response, samples, variable)] %>% 
  spread(variable, sd) %>% 
  left_join(mm,., by = c("response","samples")) %>%
  select(
         response,      
         samples,
         mean = "env1.x", 
         sd = "env1.y",
         mean1 = "env2.x",
         sd1 = "env2.y",
         mean2 = "Noise.x",
         sd2 = "Noise.y") %>%
  stargazer(summary = FALSE, rownames = F)




