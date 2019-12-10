out <- Simulation.output$data
out.samp <- out[,1:n]
out.grid <- out[,(n + 1):(n + 4)]

library(lattice)
names <- paste("sp", 1:ncol(out.samp), sep = "")
names.env <- append(names, c("env1", "env2"))
wire <- cbind(out.samp, out.grid)
colnames(wire) <- names.env
trellis.par.set("axis.line", list(col = NA, lty = 1, lwd = 1))
trellis.par.set(name = "fontsize", list(text = 14, points = 500))
plot.wire <-
      wireframe(
            as.formula(paste(
                  paste(names, collapse = "+"), "~ env1 * env2", sep = ""
            )),
            data = out,
            xlab = "env1",
            ylab = "env2",
            zlab = list("", rot = 0),
            aspect = c(100 / 100, 1),
            screen = list(z = 300, x = -80, y = 20)
            #par.box = list(col=NA), colorkey = F
      )
plot.wire


# recreate plots from McGill et al 2007

site.x = as.numeric(out[sample(1:100,1),1:50])
hist(site.x, ylab = "# species", xlab = "Abundance (N)")

hist(site.x, breaks = c(0,1,3,4,8,16,32,64,128,256,512,1024), freq = T)

plot(x = dplyr::dense_rank(dplyr::desc(site.x)),
     y = log10(site.x))