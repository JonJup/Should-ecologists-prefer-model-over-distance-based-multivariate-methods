### ----------------------------------------------------------- ###
# -------------------- Combine Summary Tables --------------------#
### ----------------------------------------------------------- ###

# Jonathan Jupke 
# 06.06.19
# Paper: Should ecologists prefer model- over algorithm-based multivariate methods?

# Combine summary tables from all methods into one homogenized table 

## -- OVERVIEW -- ## 
# 01.Setup
# 02.Build Table
# 03.Work on Table
# 04.Save to File 
## -------------- ##

# 01. Setup -------------------------------------------------------------------

pacman::p_load(data.table, dplyr, magrittr)
# other required packages: fs, here, stringr, tidyr, readr 

# set wd 
setwd(here::here("result_data/05_collected_results/"))

# 02. Combine Tables -------------------------------------------------------------

#  Read all tables from with lapply

result_files = 
      fs::dir_ls() %>% 
      as.character %>%  
      .[stringr::str_detect(. ,"_results")] 

result_files = result_files[!(stringr::str_detect(result_files, "old"))]

all_tables <- lapply(result_files, fread)

# Assign each list element to own object so I can modify them
cca <- all_tables[[1]]
cqo <- all_tables[[2]] 
dbrda <- all_tables[[3]]
mvglm <- all_tables[[4]]

# Row bind all tables 
all = rbind(cca,cqo,dbrda, mvglm)


# 03. Modify Table --------------------------------------------------------

# For the Response combination LB env1 and env2 have to be reversed 
all$variable[with(all, which(response == "LB" &
                                   variable == "env1"))] <- "Placeholder"
all$variable[with(all, which(response == "LB" &
                                   variable == "env2"))] <- "env1"
all$variable[with(all, which(response == "LB" &
                                   variable == "Placeholder"))] <- "env2"

# Split Response column 
all <-
      tidyr::separate(
            all,
            col = response,
            into = c("response1", "response2"),
            sep = 1,
            remove = F
      )


# 04. Save to File ---------------------------------------------------------------
save.path = "all_results.csv"
readr::write_csv(x = all, path = save.path)




# -------------------------------------------------------------------- #