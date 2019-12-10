### ---------------------------------------------------------- ###
# -------------------- Combine dbRDA Results --------------------#
# ---- for different response same gradient communities ---------# 
### ---------------------------------------------------------- ###

#Jonathan Jupke
# 02.12.19
#Paper: Should ecologists prefer model- over algorithm-based multivariate methods?

# Combine the results of the single CCAs into one table.

## -- OVERVIEW -- ## 
# 1.Setup
# 2.Build Table
# 3.Work on Table
# 4.Save to File 
## -------------- ##

# 01. Setup ----------------------------------------------------------------

pacman::p_load(dplyr, data.table)
# other required packages: here, fs, readr, stringr

setwd(dir = "~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/r_scripts/reviewer_comments/01_different_response_same_gradient/drsg_dbrda/")
output.files = fs::dir_ls(regexp = ".RDS$")

# create empty list to hold results
list.of.analysis.data = vector(mode = "list")


# 02. Build Table ---------------------------------------------------------

## FOR LOOP: READ RESULT FILES AND FORMAT INTO TABLE.
for (i in seq_along(output.files)) {
   
   save.obj = readRDS(output.files[i])

   
   analysis.data <- data.frame(matrix(ncol = 7, nrow = 4))
   names(analysis.data) <- c("variable","test.statistic","p.value","model","runtime1", "runtime2", "method")
   analysis.data[,1] = c("env1", "env2", "Noise", "Noise")
   analysis.data[,2] = unname(save.obj[["By Terms"]][1:4,3])
   analysis.data[,3] = unname(save.obj[["By Terms"]][1:4,4])
   analysis.data[,4] = paste0("drsg", i)
   analysis.data[,5] = rep(save.obj$`Times`, times = 4)
   analysis.data[,6] = NA
   analysis.data[,7] = rep("dbRDA", n = 4)
   
   setDT(analysis.data)
   
   list.of.analysis.data[[i]] <- analysis.data
}
   
cca_combine = rbindlist(list.of.analysis.data) 

# 03. Work on Table -------------------------------------------------------
cca_combine[variable %like% "rand", variable := "Noise"]
cca_combine = cca_combine[variable != "Model"]
cca_combine[, c(
   "false.negative.01",
   "false.negative.03",
   "false.negative.05",
   "false.negative.07",
   "false.negative.1",
   "false.positive.01",
   "false.positive.03",
   "false.positive.05",
   "false.positive.07",
   "false.positive.1"
) := 0]

# FPR and FNR
signivalue = c(0.01, 0.03, 0.05, 0.07, 0.1)

for (sv in 1:5) {
   sigv = signivalue[sv]
   n.variable = paste0("false.negative", 
                       stringr::str_extract(as.character(sigv), "\\.+.*")
   )
   p.variable = paste0("false.positive", 
                       stringr::str_extract(as.character(sigv), "\\.+.*")
   )
   
   for (i in 1:nrow(cca_combine)) {
      
      if (cca_combine[i, variable %like% "env" & p.value > sigv]) 
         cca_combine[i, (n.variable) := 1]
      if (cca_combine[i, variable == "Noise" & p.value < sigv])
         cca_combine[i, (p.variable) := 1]
   }
   
} 


# 04. Save to File --------------------------------------------------------

saveRDS(cca_combine, "../../03_analyse_results/dbRDA/dbRDA_drsg_results.RDS")

