### ------------------------------------------------ ###
# -------------------- CQO run------------------------ #
### ------------------------------------------------ ###

#Run CQO Wrapper on communities and save outputs.


# Setup -------------------------------------------------------------------

pacman::p_load(
                VGAM,      ## CQO
                gtools, 
                foreach,   ## parallel processing
                parallel,  ## parallel processing
                doParallel,## parallel processing 
                beepr,     ## sound when computation is finished
                here       ## Setting wds 
)

# other required packages: purrr, fs

# Establish a "safe" version of CQO. Especially for models with low samples
# sizes cqo might return an error. The safe version prevents the whole loop to
# break down just because one function returns an error. Instead if an error, a
# list is returned which indacetes that an error has occured. The function is
# run agian until no error is returned anymore. If this is not the case after
# ten runs the model is skipped and the element in the model list is "error".

safe_cqo <- purrr::safely(cqo)

wrapperwd <- here("r_scripts/02_running_tests/02_cqo/")
comwd     <- here("r_scripts/01_simulating_communities/")
savewd    <- here("result_data/02_cqo/")

# load the wrapper that runs the cqo as well as the function to calculate pseudo p-values for cqo.
setwd(wrapperwd)
source("wraper_cqo.R")
source("pseudo-p_cqo.R")


setwd(comwd)
model.files <- fs::dir_ls()
cqo.list <- list()


# Loop for computation ----------------------------------------------------

for (cqo.i in 1:36) {
      CQO_result <-
            cqo2r.wrapper(model = model.files[cqo.i],
                          comwd = comwd,
                          SEEDS = 1:5)
      if (cqo.i < 10)
            save.name <-
                  paste0(0, cqo.i, "_", CQO_result[[1]][["Response"]], "_", CQO_result[[1]][["Samples"]], ".Rdata")
      if (cqo.i >= 10)
            save.name <-
                  paste0(cqo.i, "_", CQO_result[[1]][["Response"]], "_", CQO_result[[1]][["Samples"]], ".Rdata")
      setwd(savewd)
      save(CQO_result, file = save.name)
      beep()
}# END FOR-LOOP



