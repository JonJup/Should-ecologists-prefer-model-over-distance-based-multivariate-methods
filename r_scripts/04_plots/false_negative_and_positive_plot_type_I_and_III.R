### ------------------------------------------------------------------- ###
# --- False negative and positive plot for Type I and III communities --- #
### ------------------------------------------------------------------- ### 

#date 03.12.19
#Should ecologist prefer model- over algorithm-based methods? 
# Plotting the false negative and positive rates of communties type I and III in a single plot

# 01. Setup ---------------------------------------------------------------

pacman::p_load(data.table, ggplot2, dplyr)

setwd("~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/result_data/06_false_negative_and_positive_rates/")
FNRI = readRDS("FNR_type_I.RDS")
FPRI = readRDS("FPR_type_I.RDS")
FNRIII = readRDS("FNR_type_III.RDS")
FPRIII = readRDS("FPR_type_III.RDS")

# Turn method into factor
FNRI$method = factor(FNRI$method, levels = unique(FNRIII$method))
FPRI$method = factor(FPRI$method, levels = unique(FNRIII$method))
FNRIII$method = factor(FNRIII$method, levels = unique(FNRIII$method))
FPRIII$method = factor(FPRIII$method, levels = unique(FNRIII$method))

# join all together
FNRI$data = "False Negative Rate - type I"
FPRI$data = "False Positive Rate - type I"
FNRIII$data = "False Negative Rate - type III"
FPRIII$data = "False Positive Rate - type III"
names(FNRI)[1] <- "rate"
names(FNRIII)[1] <- "rate"
names(FPRI)[1] <- "rate"
names(FPRIII)[1] <- "rate"

test = rbindlist(list(FNRI, FNRIII, FPRI, FPRIII))

# 02. Create single plots -------------------------------------------------

# False positive - community type I
FnFpP = ggplot(data = test, aes(x = sig.lvl, y = rate)) + 
        scale_fill_brewer( type = "qual", palette = 3, direction = 1,
                           aesthetics = "fill") + 
        scale_colour_brewer( type = "qual", palette = 3, direction = 1,
                             aesthetics = "colour") + 
        geom_line(aes(col = method), size = 1, alpha = 1) + 
        geom_point(aes(fill = method), shape = 21, size = 3) + 
        ylab(label = "False Positive Rate") +
        xlab(label = "Significance level") + 
        ylim(0, 0.58) +
        theme_minimal_hgrid() + 
        theme(
                axis.title.x = element_text(size = 15),
                legend.title = element_blank(),
                #legend.position = "none",
                axis.title.y = element_blank()
        ) + 
        facet_wrap(.~data)


# 03. Save to file --------------------------------------------------------

ggplot2::ggsave(plot = FnFpP,
                filename = "../../plots/191203_FPFNR_TypeIandIII.pdf",
                height = 10,
                width = 20,
                units = "cm")
