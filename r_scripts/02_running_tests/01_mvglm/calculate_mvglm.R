### ----------------------- ### 
# ----- Calculate MvGLM ----- #
### ----------------------- ### 

# call the warper function to calculate mvglm models


# Setup -------------------------------------------------------------------
pacman::p_load(
                mvabund,
                dplyr
)

# also required: stringr, fs, here  

# source the wrapper which runs the mvglm on each community 
setwd(here::here("r_scripts/02_running_tests/01_mvglm/"))
source("wraper_mvglm.R")

# define directories of simulated communities and the designated folder to save
# results.
com.wd <- here::here("r_scripts/01_simulating_communities/")
save.wd <- here::here("result_data/01_mvglm/")

setwd(com.wd)
# create a vector with the names of all simulated communities 
model.files <- fs::dir_ls()

# Select the models to run the test on. Models go from 1 to 36. 37 is the
# collected_functions.r script and 38 is a readme.
k.obj = 1:36

# Run Models --------------------------------------------------------------

# k = Models, i = Seeds
for (k in k.obj) {
        setwd(com.wd)
        test.out <- mvglm_wrapper(model.files[k], SEEDS = 1:5)
        
        for (i in 1:5) {
                print(paste("Model", model.files[k], "Seed", i))
                save.name <-
                        model.files[k] %>% stringr::str_replace_all(".R", "") %>% paste0("_Seed_", i, ".Rdata")
                
                setwd(com.wd)
                MvGLM.time <- system.time(
                        MvGLM_result <-
                                anova.manyglm(
                                        test.out[[i]],
                                        p.uni = "adjusted",
                                        test = "LR",
                                        resamp = "perm.resid",
                                        nBoot = 1000,
                                        show.time = "all"
                                )
                )
                MvGLM_result[["Samples"]] <- test.out[["Samples"]]
                MvGLM_result[["Response Type"]] <-
                        test.out[["Response Type"]]
                MvGLM_result[["Time"]] <- MvGLM.time[[1]]
                setwd(save.wd)
                save(MvGLM_result, file = save.name)
        }
        # sound signal when run is finished
        #beepr::beep()
}
 

