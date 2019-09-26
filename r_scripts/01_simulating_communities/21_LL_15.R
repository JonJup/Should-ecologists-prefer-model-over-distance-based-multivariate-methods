### --- Li-Li 15 Samples --- ###

#------------#
#1. SETUP ----
#------------#

# sets wd to location of sourced script. Only works when sourced!
setwd(dirname(parent.frame(2)$ofile))
# The file collected_functions.R contains all the functions necessary to run this script.
source("Collected_Functions.R")

#----------------#
#2. PARAMETER ----
#----------------#

# number of species
n <- 9

# number of sampling sites per gradient
sites <- 15

# number of noise variables
nerr <- 2

# env is the grid of x and y gradient along which species abundances will be simulated.
env <- expand.grid(1:100, 1:100)

# Now the species' response pararmeters are set:
# linear parameter
lc = list(seq(from = 0.1,
              to = 1, 
              length.out = 9),
          seq(from = 0.1,
              to = 1, 
              length.out = 9)
)

# combine all parameters into one list
para <- list(lc = lc)

#------------------#
# 3. Simulation ----
#------------------#

# This function simultes the abundances. For more details see the collected_functions script
out <-
      sim.re.md(
            n.grad = 2,
            re.type = list(rep(1,9), rep(1,9)),
            parameter = para,
            grad = env
      )
out <- as.data.frame(out)
colnames(out) <- paste("sp", 1:ncol(out), sep = "")
# Sample the big 100x100 dataset
sample <- take.sample(out,
                      n.x = sites,
                      n.y = sites,
                      grid = env)

# Extract new abundance data
out.samp <- sample[["Abundance"]]

# Extract new sample location
out.grid <- sample[["Sample grid"]]

# Add noise variables
envn <-
      make.noise(
            n.grad = nerr,
            n.cor = .0,
            grid = out.grid,
            h = 100
      )


Simulation.output <-
      list(
            "data" = cbind(out.samp, envn),
            "Response.Type" = "LL",
            "Samples" = sites ^ 2
      )
rm(
      out.grid,
      out.samp,
      sample,
      out,
      para,
      sites,
      nerr,
      env,
      envn,
      n,
      complement,
      make.noise,
      sim.re.md,
      take.sample
)
