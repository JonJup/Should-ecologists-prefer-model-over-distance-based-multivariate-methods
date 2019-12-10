### ------------------------------------------------------------------- ###
# --- False negative and positive plot for Type I and III communities --- #
### ------------------------------------------------------------------- ### 

#date 03.12.19
#Should ecologist prefer model- over algorithm-based methods? 
# Plotting the false negative and positive rates of communties type I and III in a single plot

# 01. Setup ---------------------------------------------------------------

pacman::p_load(data.table, ggplot2, dplyr, cowplot)

setwd("~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/result_data/06_false_negative_and_positive_rates/")
FNRI = readRDS("FNR_type_I.RDS")
FPRI = readRDS("FPR_type_I.RDS")
FNRII = readRDS("FNR_type_II.RDS")
FPRII = readRDS("FPR_type_II.RDS")
FNRIII = readRDS("FNR_type_III.RDS")
FPRIII = readRDS("FPR_type_III.RDS")

# Turn method into factor
FNRI$method = factor(FNRI$method, levels = unique(FNRIII$method))
FPRI$method = factor(FPRI$method, levels = unique(FNRIII$method))
FPRII$method = factor(FPRII$method, levels = unique(FNRIII$method))
FNRII$method = factor(FNRII$method, levels = unique(FNRIII$method))
FNRIII$method = factor(FNRIII$method, levels = unique(FNRIII$method))
FPRIII$method = factor(FPRIII$method, levels = unique(FNRIII$method))

# join all together
FNRI$data = "False Negative Rate - type I"
FPRI$data = "False Positive Rate - type I"
FNRII$data = "False Negative Rate - type II"
FPRII$data = "False Positive Rate - type II"
FNRIII$data = "False Negative Rate - type III"
FPRIII$data = "False Positive Rate - type III"

names(FNRI)[1] <- "rate"
names(FNRII)[1] <- "rate"
names(FNRIII)[1] <- "rate"
names(FPRI)[1] <- "rate"
names(FPRII)[1] <- "rate"
names(FPRIII)[1] <- "rate"

all.types = rbindlist(list(FNRI, FNRII, FNRIII, FPRI, FPRII, FPRIII))
all.types2 = all.types
all.types2$method = factor(all.types2$method, levels = levels(all.types2$method)[c(3,2,5,8,7,10,1,4,6,9)])


levels(all.types2$method) = levels(all.types2$method)[c(3,2,5,8,7,10,1,4,6,9)]
all.types2 =  all.types[!(as.character(method) %in% c("CCA_hellinger", "CCA_sqrt","dbRDA_hellinger", "dbRDA_sqrt"))]
all.types2$method = gdata::drop.levels(all.types2$method)
table(all.types2$method)

# 02. Create single plots -------------------------------------------------

# False positive - community type I
FnFpP = ggplot(data = all.types2, aes(x = sig.lvl, y = rate)) + 
        scale_fill_brewer( type = "qual", palette = 3, direction = 1,
                           aesthetics = "fill") + 
        scale_colour_brewer( type = "qual", palette = 3, direction = 1,
                             aesthetics = "colour") + 
        geom_line(aes(col = method), size = 1, alpha = 1) + 
        geom_jitter(aes(fill = method), shape = 21, size = 3, width = 0.003, height = 0) + 
        ylab(label = "False Positive Rate") +
        xlab(label = "Significance level") + 
        ylim(0, 0.58) +
        theme_minimal_hgrid() + 
        theme(
                axis.title.x = element_text(size = 15),
                legend.title = element_blank(),
                #legend.position = "none",
                axis.title.y = element_blank(),
                axis.text.x = element_text(size = 8),
                axis.text.y = element_text(size = 12),
                strip.text.x = element_text(size = 8)
        ) + 
        facet_wrap(.~data)

# x11();FnFpP
# 
# 
# # 03. Save to file --------------------------------------------------------
# 
# ggplot2::ggsave(plot = FnFpP,
#                 filename = "../../plots/191204_FPFNR_TypeI,II,III.pdf",
#                 height = 10,
#                 width = 20,
#                 units = "cm")
