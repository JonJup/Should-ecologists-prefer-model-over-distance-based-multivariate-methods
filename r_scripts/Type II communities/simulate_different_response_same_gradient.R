### ------------------------------------ ###
# --- Different Response same Gradient --- #
### ------------------------------------ ###

#12.11.19 

#Here I simulate a community of nine species with the most basic response
#combination where response are not the same towards one gradient.

### --- OVERVIEW --- ###
#01. SETUP 
#02. Simulate Communities
#03. Prepare data for further analysis
### ---------------- ###


# 01.Setup ----------------------------------------------------------------

setwd(here::here())

source("r_scripts/reviewer_comments/Collected_Functions.R")

## --  set parameters
# unimodal 
c.u = 10
u.u = 50
tol.u = 7.5

# bimodal 
c.b = 5
u.b1 = 25
u.b2 = 75

# linear 
beta = .037


# 02. Simulate Communities  -----------------------------------------------


env <- expand.grid(1:100,1:100)

unimodspecies.grad1 =  round(c.u * exp(-(env$Var1 - u.u)^2 / (2*tol.u^2)))
unimodspecies.grad2 =  round(c.u * exp(-(env$Var2 - u.u)^2 / (2*tol.u^2)))
unimodol.combined = unimodspecies.grad1 * unimodspecies.grad2
sum(unimodol.combined)

bimodspecies.grad1 =  round(
    c.b * exp(-(env$Var1 - u.b1)^2 / (2*tol.u^2)) + 
    c.b * exp(-(env$Var1 - u.b2)^2 / (2*tol.u^2))    
)
bimodspecies.grad2 =  round(
    c.b * exp(-(env$Var2 - u.b1)^2 / (2*tol.u^2)) + 
        c.b * exp(-(env$Var2 - u.b2)^2 / (2*tol.u^2))    
)

bimod.combined = bimodspecies.grad1 * bimodspecies.grad2
sum(bimod.combined)/sum(unimodol.combined)

linearspecies.grad1 = round(env$Var1 * beta)
linearspecies.grad2 = round(env$Var2 * beta)
linearspecies.combined = linearspecies.grad1 * linearspecies.grad2
sum(linearspecies.combined)/sum(unimodol.combined)

allspecies = as.data.frame(cbind(unimodol.combined, 
                   bimod.combined,
                   linearspecies.combined))



# 03. Prepare data for further analysis -----------------------------------

out = allspecies
sites = 25
colnames(out) <- paste("sp", 1:ncol(out), sep = "")

sample <- take.sample(out, n.x = sites, n.y = sites, grid = env)
out.samp <- sample[["Abundance"]]
out.grid <- sample[["Sample grid"]]

for (i in 1:5) {
set.seed(i)
envn <- make.noise(n.grad = 2, n.cor = .0, grid = out.grid, h = 100)
out = cbind(out.samp, envn)
saveRDS(out, 
        paste0("r_scripts/reviewer_comments/01_different_response_same_gradient/drsg_community", 
               i,".RDS"))

}



