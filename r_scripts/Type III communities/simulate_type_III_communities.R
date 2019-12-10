### -------------------------------------------------- ###
# --- Variing abundance communities, uniform SAD --- #
### -------------------------------------------------- ### 

### --- OVERVIEW --- ### 

# 01. SETUP
# 02. PARAMETER
# 03. SIMULATION

### ---------------- ###



# 01. SETUP ---------------------------------------------------------------
pacman::p_load(gambin)
# sets wd to location of sourced script. Only works when sourced!
setwd(here::here("r_scripts/reviewer_comments/"))

# The file collected_functions.R contains all the functions necessary to run this script.
source("Collected_Functions.R")


# 02. PARAMETER -----------------------------------------------------------

for (i in 1:5) {
  # run with 1:5 
  seed.var = i
  set.seed(seed.var)
  
  
  # number of species
  n <- 30
  
  # number of sampling sites per gradient
  sites <- 25
  
  # number of noise variables
  nerr <- 2
  
  # env is the grid of x and y gradient along which species abundances will be simulated.
  env <- expand.grid(1:100,1:100)
  
  # simulate SAD with a gambin function. Arguments:  number of species, alpha
  # parameter. Value five ist taken from vignette. Number of octaves.
  SAD = rgambin(n, 5,10)
  SAD[which(SAD == 0)] = 1
  table(SAD)
  SAD2 = c()
  for (k in 1:10) {
    SAD2[k] = sum(SAD == k)
  }
  
  # Now the species' response pararmeters are set:
  # maximum abundance
  
  c = list("G" = list(# 1. Grad
    sqrt(unlist(
      list(
        runif(SAD2[1], 1, 1),
        runif(SAD2[2], 2, 3),
        runif(SAD2[3], 4, 7),
        runif(SAD2[4], 8, 15),
        runif(SAD2[5], 16, 31),
        runif(SAD2[6], 32, 63),
        runif(SAD2[7], 64, 127),
        runif(SAD2[8], 128, 255),
        runif(SAD2[9], 256, 511),
        runif(SAD2[10], 512, 1023)
      )
    )),
    # 2. Grad
    sqrt(unlist(
      list(
        runif(SAD2[1], 1, 1),
        runif(SAD2[2], 2, 3),
        runif(SAD2[3], 4, 7),
        runif(SAD2[4], 8, 15),
        runif(SAD2[5], 16, 31),
        runif(SAD2[6], 32, 63),
        runif(SAD2[7], 64, 127),
        runif(SAD2[8], 128, 255),
        runif(SAD2[9], 256, 511),
        runif(SAD2[10], 512, 1023)
      )
    ))))
  # tolerance

  tol = c 
             
             
             
  # optima
  opt = list("G" = list(sample(1:100,n), sample(1:100,n)))
  
  # combine all parameters into one list
  para <- list(c = c, tol = tol,opt = opt)
  
  
  # 03. SIMULATION ----------------------------------------------------------
  
  # This function simultes the abundances. For more details see the collected_functions script.
  out <- sim.re.md(n.grad = 2, re.type = list(rep(2,n),rep(2,n)), parameter = para, grad = env)
  out <- as.data.frame(out)
  # Assign names sp1, sp2 ... to species
  colnames(out) <- paste("sp", 1:ncol(out), sep = "")
  
  # Sample the 100x100 dataset
  sample <- take.sample(out, n.x = sites, n.y = sites, grid = env)
  
  # Extract new abundance data
  out.samp <- sample[["Abundance"]]
  
  # Extract new sample location
  out.grid <- sample[["Sample grid"]]
  
  # Add noise variables
  envn <- make.noise(n.grad = nerr, n.cor = .0, grid = out.grid, h = 100)
  
  Simulation.output <- list("data" = cbind(out.samp,envn), "SAD" = "uniform")
  
  saveRDS(object = Simulation.output, 
          file = paste0("02_variable_abundance_communities/vac",
    seed.var,
    ".RDS"))
  
  rm(out.grid, out.samp, sample, out, para, opt, tol, c,sites, nerr, env, envn, n )
}

