###  ------------------------------------------------------------ ###
# -------------------------- Plot LL_100 -------------------------- # 
###  -------------------------------------------------------------###

## create plotly plot of example UU community for methods section


pacman::p_load(plotly, magrittr)


## set wd and load community data 
setwd("~/01_Uni/05_MTPnew/05_Plots/Community plots/R Skripte/")
model_list <- fs::dir_ls()
model_list["02_PLOT_LB_100.R"] %>% source()
out <- Simulation.output$data
out.samp <- out[,1:9]
out.grid <- out[,10:13]
env2 <- 1:100
env1 <- 100:1
# create one matrix per species. The matrix has one column per sampling location on a gradient
for (i in 1:9 ) {
        assign(
                x     = paste0("plot_sp",i),
                value = matrix(out.samp[,i], 
                               ncol = out.grid$env1 %>% unique %>% length)
                )

        }


## color scale see https://stackoverflow.com/questions/46150158/plotly-different-colours-for-different-surfaces?noredirect=1&lq=1

for(i in 1:9) {
assign(
        paste0("color",i),
        matrix(
                rep(
                        as.numeric(
                                paste0("0.",i - 1)
                                ),
                        length(plot_sp1)),
                ncol = length(unique(out.grid$env1))))
}


p2 <- plot_ly(showscale = F, colors = c(
        "#e6194B",
        "#3cb44b", #2
        "#ffe119",
        "#4363d8",
        "#f58231", #1 
        "#911eb4", #4
        "#42d4f4",#6
        "#f032e6",
        "#a9a9a9" #3
                                  
                                  )) %>% 
        add_surface(
                x = ~ env1,
                y = ~ env2,
                z = ~ plot_sp1,
                surfacecolor = color4,
                cauto = F,
                cmax = 1,
                cmin = 0
        ) %>%
        add_surface(
                x = ~ env1,
                y = ~ env2,
                z = ~ plot_sp2,
                surfacecolor = color5,
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
                surfacecolor = color6,
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
                surfacecolor = color7,
                cauto = F,
                cmax = 1,
                cmin = 0
        ) %>%
        add_surface(
                x = ~ env1,
                y = ~ env2,
                z = ~ plot_sp6,
                surfacecolor = color2,
                cauto = F,
                cmax = 1,
                cmin = 0
        ) %>%
        add_surface(
                x = ~ env1,
                y = ~ env2,
                z = ~ plot_sp7,
                surfacecolor = color8,
                cauto = F,
                cmax = 1,
                cmin = 0
        ) %>%
        add_surface(
                x = ~ env1,
                y = ~ env2,
                z = ~ plot_sp8,
                surfacecolor = color1,
                cauto = F,
                cmax = 1,
                cmin = 0
        ) %>%
        add_surface(
                x = ~ env1,
                y = ~ env2,
                z = ~ plot_sp9,
                surfacecolor = color9,
                cauto = F,
                cmax = 1,
                cmin = 0
        ) %>% 
        layout(
                scene = list(
                        zaxis = list(
                                title.text = "Abundance",
                                showticklabels = FALSE
                        ),
                        xaxis = list(showticklabels = FALSE),
                        yaxis = list(showticklabels = FALSE),
                        camera = list(
                                eye = list(
                                        x = 1.97, y = 0.98, z = .6    
                                )
                        )
                )
        )


p2
