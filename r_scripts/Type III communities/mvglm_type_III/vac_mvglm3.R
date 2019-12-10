### ---------------- ###
# ---  Vac MvGLM 3 --- #
### ---------------- ###

pacman::p_load(mvabund)
setwd("~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/r_scripts/reviewer_comments/")
data = readRDS("02_variable_abundance_communities/vac3.RDS")

com <- mvabund(data$data[,1:30])

nb  <- manyglm(com ~ ., data = data$data[,31:34])
pois <- manyglm(com ~., data = data$data[,31:34], family = "poisson")
gaus <- manylm(com ~., data = data$data[,31:34])

plot(nb, which = 2)
plot(pois, which = 2)
plot(gaus, which = 2)

AIC(nb, pois, gaus)

# nb 
start.time = Sys.time()
anova = anova.manyglm(
      nb,
      p.uni = "adjusted",
      test = "LR",
      resamp = "perm.resid",
      nBoot = 1000,
      show.time = "all"
)
end.time = Sys.time()
run.time = end.time - start.time

output = list(anova, run.time, "vac3")
saveRDS(object = output, 
        file = "02_variable_abundance_communities/vac_mvglm/vac3_mvglm_result.RDS")
beepr::beep()