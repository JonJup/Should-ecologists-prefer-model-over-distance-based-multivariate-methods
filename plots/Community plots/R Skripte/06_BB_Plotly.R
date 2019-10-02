# ----------------------- #
#     Plotly Abundances   # 
# ----------------------- #

## load packages 
pacman::p_load(plotly, magrittr)


## set wd and load community data 

model_list <- fs::dir_ls("plots/Community plots/R Skripte/")
model_list["plots/Community plots/R Skripte/03_PLOT_UB_100.R"] %>% source()
out <- Simulation.output$data
out.samp <- out[,1:9]
out.grid <- out[,10:13]
env1 <- env2 <- 1:100

# create one matrix per species. The matrix has one column per sampling location on a gradient
for (i in 1:9){
        assign(
                x     = paste0("plot_sp",i),
                value = matrix(out.samp[,i], 
                               ncol = out.grid$env1 %>% unique %>% length)
                )

        }


## color scale see https://stackoverflow.com/questions/46150158/plotly-different-colours-for-different-surfaces?noredirect=1&lq=1

for(i in 1:9){
assign(
        paste0("color",i),
        matrix(
                rep(
                        as.numeric(
                                paste0("0.",i-1)
                                ),
                        length(plot_sp1)),
                ncol = length(unique(out.grid$env1))))
}


p3 <- plot_ly(showscale = F, colors = c("#FAD510", "#F98400", "#3B9AB2",
                                   "#4DAC26",
                                   "#D69C4E", # Darjeeling2 Braun [3]
                                   "#E41A1C",
                                   "#273046", # dark Navy,
                                   "#972D15", # Wesanderson Cavalcanti1 dunkel Rot [6] 
                                   "#E6A0C4" # Wesanderson GrandBudapest2 Lila [1]
                                  
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

p3
