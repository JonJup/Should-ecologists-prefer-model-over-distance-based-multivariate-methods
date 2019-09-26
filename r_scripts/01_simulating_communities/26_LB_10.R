### --- LB 10 Samples --- ###

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
sites <- 10

# number of noise variables
nerr <- 2

# env is the grid of x and y gradient along which species abundances will be simulated.
env <- expand.grid(1:100, 1:100)

# Now the species' response pararmeters are set:
# linear parameter
lc = list(rep (0.1, times = n),
          rep (0.1, times = n))

# maximal abundance
c <-
      list("B" = list(
            list(
                  c(100, 100),
                  c(100, 100),
                  c(100, 100),
                  c(100, 100),
                  c(100, 100),
                  c(100, 100),
                  c(100, 100),
                  c(100, 100),
                  c(100, 100)
            ),
            list(
                  c(100, 100),
                  c(100, 100),
                  c(100, 100),
                  c(100, 100),
                  c(100, 100),
                  c(100, 100),
                  c(100, 100),
                  c(100, 100),
                  c(100, 100)
            )
      ))

# optima
ose <- seq(from = 5,
           to = 95,
           length.out = n + 1)
opt <-  list("B" = list(
      list(
            c(ose[1], ose[2]),
            c(ose[2], ose[3]),
            c(ose[3], ose[4]),
            c(ose[4], ose[5]),
            c(ose[5], ose[6]),
            c(ose[6], ose[7]),
            c(ose[7], ose[8]),
            c(ose[8], ose[9]),
            c(ose[9], ose[10])
      ),
      list(
            c(ose[1], ose[2]),
            c(ose[2], ose[3]),
            c(ose[3], ose[4]),
            c(ose[4], ose[5]),
            c(ose[5], ose[6]),
            c(ose[6], ose[7]),
            c(ose[7], ose[8]),
            c(ose[8], ose[9]),
            c(ose[9], ose[10])
      )
))

# tolerances
tol <-
      list ("B" = list (
            list(
                  c(6, 6),
                  c(6, 6),
                  c(6, 6),
                  c(6, 6),
                  c(6, 6),
                  c(6, 6),
                  c(6, 6),
                  c(6, 6),
                  c(6, 6)
            ),
            list(
                  c(6, 6),
                  c(6, 6),
                  c(6, 6),
                  c(6, 6),
                  c(6, 6),
                  c(6, 6),
                  c(6, 6),
                  c(6, 6),
                  c(6, 6)
            )
      ))

# combine parameters
para <- list(lc = lc,
             tol = tol ,
             c = c,
             opt = opt)

#------------------#
# 3. Simulation ----
#------------------#

# This function simultes the abundances. For more details see the collected_functions script
out <-
      sim.re.md(
            n.grad = 2,
            re.type = list(rep(4, n), rep(1, n)),
            parameter = para,
            grad = env
      )
out <- as.data.frame(out)
colnames(out) <- paste("sp", 1:ncol(out), sep = "")
# Sample the big 100x100 dataset
names(out) <- paste("sp", 1:ncol(out), sep = "")

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
            "Response.Type" = "LB",
            "Samples" = sites ^ 2
      )
rm(
      out.grid,
      out.samp,
      sample,
      out,
      para,
      opt,
      tol,
      c,
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
