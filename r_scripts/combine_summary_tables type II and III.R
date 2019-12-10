### ----------------------------------------------------------- ###
# -------------------- Combine Summary Tables --------------------#
### ----------------------------------------------------------- ###

# Jonathan Jupke 
# 02.12.19
# Paper: Should ecologists prefer model- over algorithm-based multivariate methods?

# Combine summary tables from all methods into one homogenized table 

## -- OVERVIEW -- ## 
# 01.Setup
# 02.Build Table
# 03.Work on Table
# 04.Save to File 
## -------------- ##

# 01. Setup -------------------------------------------------------------------

pacman::p_load(data.table, dplyr)
# other required packages: fs, here, stringr, tidyr, readr 

# set wd 
setwd("~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/result_data/")

# 02. drsg-------------------------------------------------------------

#  Read all tables from with lapply

setwd("07_type_II_communities/")

drsg_cca = readRDS("CCA/cca_drsg_results.RDS")
drsg_cqo = readRDS("cqo/cqo_drsg_results.RDS")
drsg_dbrda = readRDS("dbRDA//dbRDA_drsg_results.RDS")
drsg_mvglm = readRDS("mvglm/mvglm_drsg_results.RDS")

names(drsg_cca)[names(drsg_cca) == "runtime"] <- "runtime1"
drsg_cca[, c("runtime2") := NA]
drsg_cca = drsg_cca[,.SD,.SDcols = c(1,2,3,4,5,17,6:16)]
names(drsg_cca)
names(drsg_cqo)
names(drsg_dbrda)
names(drsg_mvglm)

drsg_all = rbindlist(list(drsg_cca, drsg_cqo, drsg_dbrda, drsg_mvglm))


# vac ---------------------------------------------------------------------

rm(list = ls())
setwd("../08_type_III_communities/")
cca_vac = readRDS("type_III_cca/cca_type_III_all_results.RDS")
cqo_vac = readRDS("type_III_cqo/cqo_vac_results.RDS")
rda_vac = readRDS("type_III_dbRDA/dbrda_type_III_all_results.RDS")
glm_vac = readRDS("type_III_mvglm//mvglm_vac_results.RDS")

names(cca_vac)
names(cqo_vac)
names(rda_vac)
names(glm_vac)

names(cca_vac)[names(cca_vac) == "runtime"] <- "runtime1"
cca_vac[, c("runtime2") := NA]
cca_vac = cca_vac[,.SD,.SDcols = c(1,2,3,4,5,18,6:17)]
names(cca_vac)[names(cca_vac) == "tranformation"] <- "transformation"


cqo_vac[, transformation := "NA"]
cqo_vac = cqo_vac[,.SD,.SDcols = c(1:7,18,8:17)]

glm_vac[, transformation := "NA"]
glm_vac = glm_vac[,.SD,.SDcols = c(1:7,18,8:17)]

vac_all = rbindlist( list(cca_vac, cqo_vac, rda_vac, glm_vac))


# 04. Save to File ---------------------------------------------------------------

saveRDS(drsg_all, "../07_type_II_communities/type_II_all_results.RDS")
saveRDS(vac_all, "../08_type_III_communities/type_III_all_results.RDS")

# -------------------------------------------------------------------- #