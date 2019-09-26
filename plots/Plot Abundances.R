# --------------------- #
#     Plot Abundances   # 
# --------------------- #

out <- Simulation.output$data
out.samp <- out[,1:9]
out.grid <- out[,10:13]

library(lattice)
names <- paste("sp",1:ncol(out.samp), sep = "")
names.env <- append(names, c("env1","env2"))
wire <- cbind(out.samp,out.grid)
colnames(wire) <- names.env
trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1))
trellis.par.set(name = "fontsize", list(text =14, points = 500))
plot.wire <- wireframe(as.formula(paste(paste(names,collapse="+"), "~ env1 * env2",sep = "")),
                       data=out, xlab="env1", ylab="env2", zlab=list("",rot = 0), aspect = c(100/100,1),
                       screen = list(z = 300, x = -80, y = 20)
                       #par.box = list(col=NA), colorkey = F
)
plot.wire





