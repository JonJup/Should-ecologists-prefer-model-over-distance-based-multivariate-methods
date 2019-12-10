### ----------------------------------------------- ###
# --------------------  Run dbRDA ------------------- #
### ----------------------------------------------- ###

#Run dbRDAs on simulated communities.


# Setup -------------------------------------------------------------------

pacman::p_load(vegan, tidyverse, here)

# other required packages: fs, beepr
# Directory with community simulation R-scripts.
com.wd <- "~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/r_scripts/01_simulating_communities/"
function.wd <- here("r_scripts/02_running_tests/04_dbRDA/")
save.wd     <- here("result_data/04_dbrda/")
setwd(function.wd)

# Source dbRDA Wrapper
source("wrapper_dbrda_withoutLingoes.R")
setwd(com.wd)
mod.names <- fs::dir_ls()

# Remove Collected_Functions.R and README.txt files.
mod.names <- mod.names[1:(length(mod.names) - 2)]


# Run Models --------------------------------------------------------------

SEEDS = 1:5
for (i in 19:36) {
      dbRDA_result <-
            dbrda.wrapper(Model = mod.names[i],
                          comwd = com.wd,
                          SEEDS = SEEDS)
      for (seed.k in SEEDS) {
            setwd(save.wd)
            save.obj <- dbRDA_result[[seed.k]]
            save(
                  save.obj,
                  file = mod.names[i] %>% stringr::str_replace_all(".R", "") %>% paste0("_Seed_", seed.k, ".Rdata")
            )
      }
      print(i)
};beepr::beep("complete")
      \