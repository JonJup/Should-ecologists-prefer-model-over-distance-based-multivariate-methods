###  ------------------------------------------------------------ ###
# -------------------------- Plot UU_100 -------------------------- # 
###  -------------------------------------------------------------###

 ## create plotly plot of example UU community for methods section


# Setup -------------------------------------------------------------------
pacman::p_load(plotly, magrittr, tidyverse)


# Simulate Community ------------------------------------------------------

# The file collected_functions.R contains all the functions necessary to run this script.
source("plots/Collected_Functions.R")


n <- 9
sites <- 100
nerr <- 2
env <- expand.grid(1:100,1:100)
c = list("G" = list(rep(100,n),  # 1. Grad
                    rep(100, n))) # 2. Grad

tol = list("G" = list(rep(7.5, n),
                      rep(7.5, n)))
opt = list("G" = list(rep(c(20, 50, 80), times = 3),
                      rep(c(20, 50, 80), each = 3)))



para <- list(c = c, tol = tol, opt = opt)
out <-
        sim.re.md(
                n.grad = 2,
                re.type = list(rep(2, n), rep(2, n)),
                parameter = para,
                grad = env
        )
out <- as.data.frame(out)
colnames(out) <- paste("sp", 1:ncol(out), sep = "")
sample <- take.sample(out,
                      n.x = sites,
                      n.y = sites,
                      grid = env)
out.samp <- sample[["Abundance"]]
out.grid <- sample[["Sample grid"]]
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
                "Response.Type" = "UU",
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

# Reformat data -----------------------------------------------------------


out <- Simulation.output$data
out.samp <- out[, 1:9]
out.grid <- out[, 10:13]
env1 <- env2 <- 1:100

# Create one matrix per species. The matrix has one column per sampling location on a gradient
for (i in 1:9) {
        assign(
                x     = paste0("plot_sp", i),
                value = matrix(out.samp[, i],
                               ncol = out.grid$env1 %>% unique %>% length)
        )
        
}


# Plot --------------------------------------------------------------------

## color scale see https://stackoverflow.com/questions/46150158/plotly-different-colours-for-different-surfaces?noredirect=1&lq=1

for (i in 1:9) {
        assign(paste0("color", i),
               matrix(rep(
                       as.numeric(paste0("0.", i - 1)),
                       length(plot_sp1)
               ),
               ncol = length(unique(
                       out.grid$env1
               ))))
}

a <- list(
        showticklabels = FALSE, title = ""
)
p1 <-
        plot_ly(
                showscale = F,
                colors = c(
                        "#e6194B",
                        "#3cb44b", #2
                         "#ffe119",
                        "#4363d8",
                        "#f58231", #1 
                        "#911eb4", #4
                        "#42d4f4",#6
                        "#f032e6",
                        "#a9a9a9" #3
                        
                )
        ) %>%
        add_surface(
                x = ~ env1,
                y = ~ env2,
                z = ~ plot_sp1,
                surfacecolor = color9,
                cauto = F,
                cmax = 1,
                cmin = 0
         ) %>%
        add_surface(
                x = ~ env1,
                y = ~ env2,
                z = ~ plot_sp2,
                surfacecolor = color2,
                cauto = F,
                cmax = 1,
                cmin = 0
        ) %>%
        add_surface(
                x = ~ env1,
                y = ~ env2,
                z = ~ plot_sp3,
                surfacecolor = color4,
                cauto = F,
                cmax = 1,
                cmin = 0
        ) %>%
        add_surface(
                x = ~ env1,
                y = ~ env2,
                z = ~ plot_sp4,
                surfacecolor = color3,
                cauto = F,
                cmax = 1,
                cmin = 0
        ) %>%
        add_surface(
                x = ~ env1,
                y = ~ env2,
                z = ~ plot_sp5,
                surfacecolor = color6,
                cauto = F,
                cmax = 1,
                cmin = 0
        ) %>%
        add_surface(
                x = ~ env1,
                y = ~ env2,
                z = ~ plot_sp6,
                surfacecolor = color5,
                cauto = F,
                cmax = 1,
                cmin = 0
        ) %>%
        add_surface(
                x = ~ env1,
                y = ~ env2,
                z = ~ plot_sp7,
                surfacecolor = color7,
                cauto = F,
                cmax = 1,
                cmin = 0
        ) %>%
        add_surface(
                x = ~ env1,
                y = ~ env2,
                z = ~ plot_sp8,
                surfacecolor = color8,
                cauto = F,
                cmax = 1,
                cmin = 0
        ) %>%
        add_surface(
                x = ~ env1,
                y = ~ env2,
                z = ~ plot_sp9,
                surfacecolor = color1,
                cauto = F,
                cmax = 1,
                cmin = 0
        ) %>%
        layout(
               scene = list(
                zaxis = list(title.text = "Abundance",
                             showticklabels = FALSE),
                xaxis = list(showticklabels = FALSE),
                yaxis = list(showticklabels = FALSE),
                camera = list(eye = list(
                        x = 1.97, y = 0.98, z = .6
                ))
        ))


p1
