l_info("Computing indicator of fishing capacity")

# FISHING CAPACITY ####

## Numbers and capacity of large-scale purse seiners using drifting FADs ####

GT_BREAKS = c(300, 1000, 2000, 3000, 4000, 5000)
GT_LABELS = c("<1,000", "1,000-2,000", "2,000-3,000", "3,000-4,000", ">4,000")
GT_COLORS = data.table(GT = GT_LABELS, FILL = rev(brewer.pal(length(GT_LABELS), "Reds")), OUTLINE = darken(rev(brewer.pal(length(GT_LABELS), "Reds")), 0.3))
ACTIVE_PS[, GT_CLASS := cut(GT, breaks = GT_BREAKS, labels = GT_LABELS)]
ACTIVE_PS[, GT_CLASS := factor(GT_CLASS, levels = rev(GT_LABELS))]

ANNUAL_PS_GT_CLASS = ACTIVE_PS[, .(N = length(unique(VESSEL_NUMBER))), keyby = .(YEAR, GT_CLASS)]

ANNUAL_PS_GT_CLASS_BARPLOT =  value_bar(ANNUAL_PS_GT_CLASS, value = "N", time = C_YEAR, fill_by = "GT_CLASS", colors = GT_COLORS, y_axis_label = "Number of large-scale purse seiners") + theme(legend.position = "bottom") + guides (fill = guide_legend(reverse = TRUE)) + theme(axis.text.x = element_text(size = 8))

ANNUAL_PS_GT_CLASS_BARPLOT_PERC = value_bar_rel(ANNUAL_PS_GT_CLASS, value = "N", time = C_YEAR, fill_by = "GT_CLASS", colors = GT_COLORS, y_axis_label = "Gross tonnage category composition (%)") + theme(legend.position = "bottom") + guides (fill = guide_legend(reverse = TRUE)) + theme(axis.text.x = element_text(size = 8))

ANNUAL_PS_GT_CLASS_BARPLOT_WITH_PERC = ggarrange(ANNUAL_PS_GT_CLASS_BARPLOT, ANNUAL_PS_GT_CLASS_BARPLOT_PERC, 
                                                 common.legend = TRUE, 
                                                 legend = "bottom", 
                                                 labels = list("a", "b"))

save_plot("../outputs/charts/AVL/ANNUAL_PS_GT_CLASS_COMBINED.png", ANNUAL_PS_GT_CLASS_BARPLOT_WITH_PERC, 12, 9/16*12)

ANNUAL_PS_LOA_GT = ACTIVE_PS[, .(N = length(unique(VESSEL_NUMBER)), LOA = mean(LOA), GT = mean(GT)), keyby = .(YEAR)]

ANNUAL_PS_MEAN_GT_LINEPLOT = 
  ggplot(data = ANNUAL_PS_LOA_GT, aes(x = YEAR, y = GT)) +
  ggh4x::geom_pointpath(col = "red", size = 2) +
  labs(x = "", y = "Mean gross tonnage") + 
  scale_y_continuous(labels = function(x) prettyNum(x, big.mark = ",")) + 
  theme_bw() + 
  theme(panel.grid.minor = element_line(colour = "darkgrey", linetype = "dashed", size = 0.1))

save_plot("../outputs/charts/AVL/ANNUAL_PS_MEAN_GT_LINEPLOT.png", ANNUAL_PS_MEAN_GT_LINEPLOT, 8, 4.5)

## Numbers of purse seiners by length overall
LOA_BREAKS = c(50, 65, 80, 95, 110, 125)
LOA_LABELS = c("<65", "65-80", "80-95", "95-110", ">110")
LOA_COLORS = data.table(LOA = LOA_LABELS, FILL = rev(brewer.pal(length(LOA_LABELS), "Blues")), OUTLINE = darken(rev(brewer.pal(length(LOA_LABELS), "Blues")), 0.3))
ACTIVE_PS[, LOA_CLASS := cut(LOA, breaks = LOA_BREAKS, labels = LOA_LABELS)]
ACTIVE_PS[, LOA_CLASS := factor(LOA_CLASS, levels = rev(LOA_LABELS))]

ANNUAL_PS_LOA_CLASS = ACTIVE_PS[, .(N = length(unique(VESSEL_NUMBER))), keyby = .(YEAR, LOA_CLASS)]

ANNUAL_PS_LOA_CLASS_BARPLOT =  value_bar(ANNUAL_PS_LOA_CLASS, value = "N", time = C_YEAR, fill_by = "LOA_CLASS", colors = LOA_COLORS, y_axis_label = "Number of large-scale purse seiners") + theme(legend.position = "bottom") + guides (fill = guide_legend(reverse = TRUE)) + theme(axis.text.x = element_text(size = 8)) 

ANNUAL_PS_LOA_CLASS_BARPLOT_PERC = value_bar_rel(ANNUAL_PS_LOA_CLASS, value = "N", time = C_YEAR, fill_by = "LOA_CLASS", colors = LOA_COLORS, y_axis_label = "Length category composition (%)") + theme(legend.position = "bottom") + guides (fill = guide_legend(reverse = TRUE)) + theme(axis.text.x = element_text(size = 8))

ANNUAL_PS_LOA_CLASS_BARPLOT_WITH_PERC = ggarrange(ANNUAL_PS_LOA_CLASS_BARPLOT, ANNUAL_PS_LOA_CLASS_BARPLOT_PERC, 
                                                  common.legend = TRUE, 
                                                  legend = "bottom", 
                                                  labels = list("a", "b"))

save_plot("../outputs/charts/AVL/ANNUAL_PS_LOA_CLASS_COMBINED.png", ANNUAL_PS_LOA_CLASS_BARPLOT_WITH_PERC, 12, 9/16*12)

ANNUAL_PS_MEAN_LOA_LINEPLOT = 
  ggplot(data = ANNUAL_PS_LOA_GT, aes(x = YEAR, y = LOA)) +
  ggh4x::geom_pointpath(col = "blue", size = 2) +
  labs(x = "", y = "Length overall (m)") + 
  scale_y_continuous(labels = function(x) prettyNum(x, big.mark = ",")) + 
  theme_bw() + 
  theme(panel.grid.minor = element_line(colour = "darkgrey", linetype = "dashed", size = 0.1))

save_plot("../outputs/charts/AVL/ANNUAL_PS_MEAN_LOA_LINEPLOT.png", ANNUAL_PS_MEAN_LOA_LINEPLOT, 8, 4.5)

## Numbers and capacity of support vessels managing the drifting FADs ####

GT_BREAKS = c(100, 200, 300, 400, 500, 600)
GT_LABELS = c("100-200", "200-300", "300-400", "400-500", "500-600")
GT_COLORS = data.table(GT = GT_LABELS, FILL = rev(brewer.pal(length(GT_LABELS), "Reds")), OUTLINE = darken(rev(brewer.pal(length(GT_LABELS), "Reds")), 0.3))
ACTIVE_SP[, GT_CLASS := cut(GT, breaks = GT_BREAKS, labels = GT_LABELS)]
ACTIVE_SP[, GT_CLASS := factor(GT_CLASS, levels = rev(GT_LABELS))]

ANNUAL_SP_GT_CLASS = ACTIVE_SP[, .(N = length(unique(VESSEL_NUMBER))), keyby = .(YEAR, GT_CLASS)]

ANNUAL_SP_GT_CLASS_BARPLOT =  value_bar(ANNUAL_SP_GT_CLASS, value = "N", time = C_YEAR, fill_by = "GT_CLASS", colors = GT_COLORS, y_axis_label = "Number of support vessels") + theme(legend.position = "bottom") + guides (fill = guide_legend(reverse = TRUE)) 

ANNUAL_SP_GT_CLASS_BARPLOT_PERC = value_bar_rel(ANNUAL_SP_GT_CLASS, value = "N", time = C_YEAR, fill_by = "GT_CLASS", colors = GT_COLORS, y_axis_label = "Gross tonnage category composition (%)") + theme(legend.position = "bottom") + guides (fill = guide_legend(reverse = TRUE)) 

ANNUAL_SP_GT_CLASS_BARPLOT_WITH_PERC = ggarrange(ANNUAL_SP_GT_CLASS_BARPLOT, ANNUAL_SP_GT_CLASS_BARPLOT_PERC, 
                                                 common.legend = TRUE, legend = "bottom")

save_plot("../outputs/charts/AVL/ANNUAL_SP_GT_CLASS_COMBINED.png", ANNUAL_SP_GT_CLASS_BARPLOT_WITH_PERC, 12, 9/16*12)

ANNUAL_SP_LOA_GT = ACTIVE_SP[, .(N = length(unique(VESSEL_NUMBER)), LOA = mean(LOA), GT = mean(GT)), keyby = .(YEAR)]

ANNUAL_SP_MEAN_GT_LINEPLOT = 
  ggplot(data = ANNUAL_SP_LOA_GT, aes(x = YEAR, y = GT)) +
  ggh4x::geom_pointpath(col = "red", size = 2) +
  labs(x = "", y = "Mean gross tonnage") + 
  scale_y_continuous(labels = function(x) prettyNum(x, big.mark = ",")) + 
  theme_bw() + 
  theme(panel.grid.minor = element_line(colour = "darkgrey", linetype = "dashed", size = 0.1))

save_plot("../outputs/charts/AVL/ANNUAL_SP_MEAN_GT_LINEPLOT.png", ANNUAL_SP_MEAN_GT_LINEPLOT, 8, 4.5)

## Numbers of support vessels by length overall
LOA_BREAKS = c(25, 30, 35, 40, 45, 50)
LOA_LABELS = c("25-30", "30-35", "35-40", "40-45", "45-50")
LOA_COLORS = data.table(LOA = LOA_LABELS, FILL = rev(brewer.pal(length(LOA_LABELS), "Blues")), OUTLINE = darken(rev(brewer.pal(length(LOA_LABELS), "Blues")), 0.3))
ACTIVE_SP[, LOA_CLASS := cut(LOA, breaks = LOA_BREAKS, labels = LOA_LABELS)]
ACTIVE_SP[, LOA_CLASS := factor(LOA_CLASS, levels = rev(LOA_LABELS))]

ANNUAL_SP_LOA_CLASS = ACTIVE_SP[, .(N = length(unique(VESSEL_NUMBER))), keyby = .(YEAR, LOA_CLASS)]

ANNUAL_SP_LOA_CLASS_BARPLOT =  value_bar(ANNUAL_SP_LOA_CLASS, value = "N", time = C_YEAR, fill_by = "LOA_CLASS", colors = LOA_COLORS, y_axis_label = "Number of support vessels") + theme(legend.position = "bottom") + guides (fill = guide_legend(reverse = TRUE)) 

ANNUAL_SP_LOA_CLASS_BARPLOT_PERC = value_bar_rel(ANNUAL_SP_LOA_CLASS, value = "N", time = C_YEAR, fill_by = "LOA_CLASS", colors = LOA_COLORS, y_axis_label = "Length category composition (%)") + theme(legend.position = "bottom") + guides (fill = guide_legend(reverse = TRUE)) 

ANNUAL_SP_LOA_CLASS_BARPLOT_WITH_PERC = ggarrange(ANNUAL_SP_LOA_CLASS_BARPLOT, ANNUAL_SP_LOA_CLASS_BARPLOT_PERC, 
                                                  common.legend = TRUE, legend = "bottom")

save_plot("../outputs/charts/AVL/ANNUAL_SP_LOA_CLASS_COMBINED.png", ANNUAL_SP_LOA_CLASS_BARPLOT_WITH_PERC, 12, 9/16*12)

ANNUAL_SP_MEAN_LOA_LINEPLOT = 
  ggplot(data = ANNUAL_SP_LOA_GT, aes(x = YEAR, y = LOA)) +
  ggh4x::geom_pointpath(col = "blue", size = 2) +
  labs(x = "", y = "Length overall (m)") + 
  scale_y_continuous(labels = function(x) prettyNum(x, big.mark = ",")) + 
  theme_bw() + 
  theme(panel.grid.minor = element_line(colour = "darkgrey", linetype = "dashed", size = 0.1))

save_plot("../outputs/charts/AVL/ANNUAL_SP_MEAN_LOA_LINEPLOT.png", ANNUAL_SP_MEAN_LOA_LINEPLOT, 8, 4.5)

l_info("Fishing capacity indicator computed!")