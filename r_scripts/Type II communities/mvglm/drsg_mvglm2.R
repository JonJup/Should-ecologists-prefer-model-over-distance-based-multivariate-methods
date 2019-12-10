### ---------------- ###
# --- DRSG MvGLM 2 --- #
### ---------------- ###

pacman::p_load(mvabund)

data = readRDS("01_different_response_same_gradient/drsg_community2.RDS")

com <- mvabund(data[,1:3])

nb  <- manyglm(com ~ ., data = data[,4:7])
pois <- manyglm(com ~., data = data[,4:7], family = "poisson")
gaus <- manylm(com ~., data = data[,4:7])

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

output = list(anova, run.time, "drsg2")
saveRDS(object = output, 
        file = "01_different_response_same_gradient/drs_mvglm/drsg2_mvglm_result.RDS")
