### -------------------------------------------------------- ###
# -------------------- Combine CQO Results --------------------#
### -------------------------------------------------------- ###

#Jonathan Jupke
#22.01.19
#Paper: Should ecologists prefer model- over algorithm-based multivariate methods?

#Combine the results of the single CQOs into one table.

## -- OVERVIEW -- ## 
# 1.Setup
# 2.Build Table
# 3.Work on Table
# 4.Save to File 
## -------------- ##

# 01 Setup ----------------------------------------------------------------

pacman::p_load(dplyr, data.table)
# other required packages: here, fs, readr, stringr

setwd(here::here("result_data/02_cqo/"))
output.files = fs::dir_ls()

# create empty list to hold results
list.of.analysis.data <- vector(mode = "list")

# start points to fill analysis data tables. Start and endpoints for different seeds 
fill.ends <- c(1, 5, 9, 13, 17)



# 02. Build Table ---------------------------------------------------------

## FOR LOOP: READ RESULT FILES AND FORMAT INTO TABLE.
for (i in 1:length(output.files)) {
      # BEGIN FOR LOOP 1
      load(output.files[i])
      # Number of rows: 4 per seed times 5 seeds
      
   analysis.data = data.table()
   analysis.data[,
                    c("variable",
                      "samples",
                      "response",
                      "method"
                      ) := 
                       list(
                          rep(c("env1", "env2", "rand1", "rand2"), 5),
                          rep(CQO_result[[1]]$Samples, 20),
                          rep(CQO_result[[1]]$Response, 20),
                          rep("CQO", 20)
                          )
                    ]   
      
      # FOR 5 SEEDS
      for (k in 1:5) {
            # BEGIN FOR LOOP 2
            
            analysis.data[fill.ends[k]:(k * 4),
                          c("test statistic", 
                            "runtime") := 
                             list(
                                as.numeric(apply(abs(CQO_result[[k]]$Summary@post$Coef@C), 1, sum)),

                                rep(as.numeric(CQO_result[[k]]$Time[3]), 4)
                                )
                          ]
            analysis.data[fill.ends[k]:(k * 4), "p.value" := CQO_result[[k]][7]]
      } # END FOR LOOP 2
      
      list.of.analysis.data[[i]] <- analysis.data
} # END FOR LOOP 1

cqo_combine = rbindlist(list.of.analysis.data)


# 03. Work on table  --------------------------------------------------------

# replace rand1 and rand2 with noise 
cqo_combine[variable %like% "rand", variable := "Noise"]
cqo_combine[, c("false.negative.01", 
                  "false.negative.03",
                  "false.negative.05",
                  "false.negative.07",
                  "false.negative.1",
                  "false.positive.01",
                  "false.positive.03",
                  "false.positive.05",
                  "false.positive.07",
                  "false.positive.1") := 0]
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
   
   for (i in 1:nrow(cqo_combine)) {
      
      if (cqo_combine[i, variable %like% "env" & p.value > sigv]) 
         cqo_combine[i, (n.variable) := 1]
      if (cqo_combine[i, variable == "Noise" & p.value < sigv])
         cqo_combine[i, (p.variable) := 1]
   }
   
} 


# 04. Save to File --------------------------------------------------------

readr::write_csv(
   x = cqo_combine,
   path = here::here("result_data/05_collected_results/cqo_results.csv")
)     

                   