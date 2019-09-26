### ----------------------------------------------- ###
# --------------------  Run CCA --------------------- #
### ----------------------------------------------- ###

#Run CCAs on simulated communities.

# Setup -------------------------------------------------------------------

pacman::p_load(vegan,
               here, 
               dplyr)
# other required packages: fs, 
# Directory with community simulation R-scripts.

com.wd <- here("r_scripts/01_simulating_communities/")
function.wd <- here("r_scripts/02_running_tests/03_cca/")
save.wd     <- here("result_data/03_cca/")

setwd(function.wd)
source("wrapper_cca.R")

setwd(com.wd)
mod.names <- fs::dir_ls()
# remove readme.md and collected_functions.r from list 
mod.names <- mod.names[1:(length(mod.names) - 2)]

save.names <-
    paste0(paste0("CCA_0", 1:9, sep = "") %>% append(paste0("CCA_", 10:70)), "_")



# Run models --------------------------------------------------------------


SEEDS = 1:5
for (i in 1:36) {
    CCA_result <-
        CCA.wrapper(Model = mod.names[i],
                    comwd = com.wd,
                    SEEDS = SEEDS)
    for (seed.k in SEEDS) {
        setwd(save.wd)
        save.obj <- CCA_result[[seed.k]]
        save(
            save.obj,
            file = mod.names[i] %>% stringr::str_replace_all(".R", "") %>% paste0("_Seed_", seed.k, ".Rdata")
        )
    }
    print(i)
};beepr::beep("complete")

