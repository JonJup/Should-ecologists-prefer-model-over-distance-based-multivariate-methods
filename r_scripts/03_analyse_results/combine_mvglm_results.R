### ---------------------------------------------------------- ###
# -------------------- Combine MvGLM Results --------------------#
### ---------------------------------------------------------- ###

#Jonathan Jupke
#22.01.19
#Paper: Should ecologists prefer model- over algorithm-based multivariate methods?

#Combine the results of the single MvGLMs into one table.

## -- OVERVIEW -- ## 
# 1.Setup
# 2.Build Table
# 3.Work on Table
# 4.Save to File 
## -------------- ##

# 01. Setup ----------------------------------------------------------------

pacman::p_load(dplyr, data.table)
# other required packages: here, fs, readr, stringr

setwd(here::here("result_data/01_mvglm/"))
output.files = fs::dir_ls()

# create empty list to hold results
list.of.analysis.data <- vector(mode = "list")


# 02. Build table -----------------------------------------------------------

## FOR LOOP: READ RESULT FILES AND FORMAT INTO TABLE.
for (i in 1:length(output.files)) {
  load(output.files[i])
  analysis.data = data.table()
  n_rowname = length(row.names(MvGLM_result$table))
  
  analysis.data[, c("variable",
                    "test statistic",
                    "p.value",
                    "response",
                    "samples",
                    "runtime",
                    "method") :=
                  list(
                    row.names(MvGLM_result$table)[-1],
                    as.numeric(MvGLM_result$table[["Dev"]][-1]),
                    as.numeric(MvGLM_result$table[["Pr(>Dev)"]][-1]),
                    rep(MvGLM_result$`Response Type`, n = n_rowname[-1]),
                    rep(MvGLM_result$`Samples`, n = n_rowname[-1]),
                    rep(MvGLM_result$`Time`, n = n_rowname[-1]),
                    rep("mvglm", n = n_rowname)
                  )]
  
  list.of.analysis.data[[i]]  = analysis.data
}

MvGLM.combine = rbindlist(list.of.analysis.data)


# 03. Work on table ---------------------------------------------------------

# replace rand1 and rand2 with noise 
MvGLM.combine[variable %like% "rand", variable := "Noise"]
MvGLM.combine[, c("false.negative.01", 
                  "false.negative.03",
                  "false.negative.05",
                  "false.negative.07",
                  "false.negative.1",
                  "false.positive.01",
                  "false.positive.03",
                  "false.positive.05",
                  "false.positive.07",
                  "false.positive.1") := 0]

# False Positive and negative ratios 
signivalue = c(0.01, 0.03, 0.05, 0.07, 0.1)

for (sv in 1:5) {
  sigv = signivalue[sv]
  n.variable = paste0("false.negative", 
                      stringr::str_extract(as.character(sigv), "\\.+.*")
  )
  p.variable = paste0("false.positive", 
                      stringr::str_extract(as.character(sigv), "\\.+.*")
  )

  for (i in 1:nrow(MvGLM.combine)) {
    
    if (MvGLM.combine[i, variable %like% "env" & p.value > sigv]) 
      MvGLM.combine[i, (n.variable) := 1]
    if (MvGLM.combine[i, variable == "Noise" & p.value < sigv])
      MvGLM.combine[i, (p.variable) := 1]
  }
    
} 


# 04. Save to File  -----------------------------------------------------------

readr::write_csv(
  x = MvGLM.combine,
  path = here::here("result_data/05_collected_results/mvglm_results.csv")
)

