### ---------------------------------------------------------- ###
# -------------------- Combine dbRDA Results --------------------#
### ---------------------------------------------------------- ###

#Jonathan Jupke
#07.12 
#Paper: Should ecologists prefer model- over algorithm-based multivariate methods?

#Combine the results of the single dbRDAs into one table.

## -- OVERVIEW -- ## 
# 1.Setup
# 2.Build Table
# 3.Work on Table
# 4.Save to File 
## -------------- ##

# 01. Setup ----------------------------------------------------------------

pacman::p_load(dplyr, data.table)
# other required packages: here, fs, readr, stringr

setwd(here::here("result_data/04_dbrda/"))
output.files = fs::dir_ls()

# create empty list to hold results
list.of.analysis.data <- vector(mode = "list")



# 02. Buid table  -----------------------------------------------------------

## FOR LOOP: READ RESULT FILES AND FORMAT INTO TABLE.
for (i in 1:length(output.files)) {
      load(output.files[i])
      
      analysis.data <- data.table()
      analysis.data[,
                    c(
                          "variable",
                          "test statistic",
                          "p.value",
                          "response",
                          "samples",
                          "runtime",
                          "method"
                    ) :=
                          list(
                                row.names(save.obj[["By Terms"]])[-nrow(save.obj$`By Terms`)],
                                (save.obj[["By Terms"]][-nrow(save.obj$`By Terms`), 3]),
                                as.numeric(save.obj[["By Terms"]][-nrow(save.obj$`By Terms`), 4]),
                                rep(save.obj$`Response`, times = nrow(save.obj$`By Terms`) -
                                          1),
                                rep(save.obj$`Samples`,    times = nrow(save.obj$`By Terms`) -
                                          1),
                                rep(save.obj$`Times`[1], n = nrow(save.obj$`By Terms`) -
                                          1),
                                rep("dbrda", n = nrow(save.obj$`By Terms`) - 1)
                          )]
      
      
      list.of.analysis.data[[i]] <- analysis.data
      
      
}

dbrda_combine = rbindlist(list.of.analysis.data)


# 03. Work on Table -------------------------------------------------------

dbrda_combine[variable %like% "rand", variable := "Noise"]
dbrda_combine[, c("false.negative.01", 
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
      
      for (i in 1:nrow(dbrda_combine)) {
            
            if (dbrda_combine[i, variable %like% "env" & p.value > sigv]) 
                  dbrda_combine[i, (n.variable) := 1]
            if (dbrda_combine[i, variable == "Noise" & p.value < sigv])
                  dbrda_combine[i, (p.variable) := 1]
      }
      
} 


# 04. Save to File --------------------------------------------------------


readr::write_csv(
      x = dbrda_combine,
      path = "../../04_Analysis/02_Summary Tables/dbrda_results.csv",
)


