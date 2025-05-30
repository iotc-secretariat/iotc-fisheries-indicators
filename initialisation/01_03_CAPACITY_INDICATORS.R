l_info("Computing fishing capacity indicators")

# ALL VESSELS ####

# Consider years after 2010 for consistency

AVL_ANNUAL_NUMBER = ACTIVE_VESSELS[YEAR>2009, .(N = length(unique(IOTC_NUMBER))), keyby = .(YEAR, FISHERY_GROUP_CODE)]

# Add missing Maldivian baitboats
# Y:\04 - Meetings\05 - Scientific Committee\SC18 - 2015 - Bali\02 - SC18 - Documents
BB_ANNUAL_NUMBER_2010_2014 = data.table(YEAR = 2010:2014, FISHERY_GROUP_CODE = "BB", N = c(199, 294, 319, 303, 383))

AVL_ANNUAL_NUMBER = rbindlist(list(AVL_ANNUAL_NUMBER, BB_ANNUAL_NUMBER_2010_2014))

AVL_ANNUAL_NUMBER[, FISHERY_GROUP_CODE := factor(FISHERY_GROUP_CODE, levels = c("PS", "LL", "BB", "SP"))]
AVL_ANNUAL_NUMBER[FISHERY_GROUP_CODE == "PS", FISHERY_GROUP := "Purse seine"]
AVL_ANNUAL_NUMBER[FISHERY_GROUP_CODE == "LL", FISHERY_GROUP := "Longline"]
AVL_ANNUAL_NUMBER[FISHERY_GROUP_CODE == "BB", FISHERY_GROUP := "Baitboat"]
AVL_ANNUAL_NUMBER[FISHERY_GROUP_CODE == "SP", FISHERY_GROUP := "Support vessel"]

AVL_ANNUAL_NUMBER[, FISHERY_GROUP := factor(FISHERY_GROUP, levels = c("Purse seine", "Longline", "Baitboat", "Support vessel"))]

FG_COLORS = rbindlist(list(
  fishery_group_colors_for(factorize_fishery_groups(AVL_ANNUAL_NUMBER)), 
  data.table(FISHERY_GROUP_CODE = "SP", FILL = darken("#FB4A6AFF", 0.3), OUTLINE = darken("#D81C4AFF", 0.3))
))

VESSELS_YEAR_FISHERY_GROUP_LINEPLOT_FACETED =
  ggplot(data = AVL_ANNUAL_NUMBER, aes(x = YEAR, y = N, color = FISHERY_GROUP_CODE)) + 
  geom_point(size = 2) + 
  geom_line() +
  scale_color_manual(values = FG_COLORS$FILL) +
  labs(x = "", y = "Number of vessels") + 
  scale_y_continuous(labels = function(x) prettyNum(x, big.mark = ",")) + 
  theme_bw() + 
  facet_wrap(~FISHERY_GROUP, scales = "free_y") +
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text.x = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed"))

ggsave("../outputs/charts/CAPACITY/VESSELS_YEAR_FISHERY_GROUP_LINEPLOT_FACETED.png", VESSELS_YEAR_FISHERY_GROUP_LINEPLOT_FACETED, width = 8, height = 5)

# LARGE-SCALE PURSE SEINERS ####

## Total number of purse seiners ####
PS_ANNUAL_NUMBER = ACTIVE_PS[, .(N = length(unique(IOTC_CODE))), keyby = .(YEAR)]

PS_COLOR = colors_for_fishery_group("PS")

### PNG ####
ggplot(data = PS_ANNUAL_NUMBER, aes(x = YEAR, y = N)) + 
  geom_point(col = PS_COLOR$FILL, size = 2) + 
  geom_line(col = PS_COLOR$FILL) +
  labs(x = "", y = "Number of vessels") + 
  scale_y_continuous(labels = function(x) prettyNum(x, big.mark = ",")) + 
  theme_bw() + 
  theme(panel.grid.minor = element_line(colour = "darkgrey", linetype = "dashed", size = 0.1))

### HTML ####
#PS_ANNUAL_NUMBER_LINEPLOT_HTML = 
#  plot_ly(data = PS_ANNUAL_NUMBER, x = ~YEAR, y = ~N, type = 'scatter', mode = 'lines+markers', line = list(color = "red", width = 2), marker = list(color = "red", size = 10)) %>% 
#  layout(title = "Annual changes in number of purse seiners", yaxis = list(title = "Number of vessels"), xaxis = list(title = ""), plot_bgcolor = "lighgrey")

#htmlwidgets::saveWidget(PS_ANNUAL_NUMBER_LINEPLOT_HTML, "../outputs/charts/CAPACITY/PS/PS_ANNUAL_NUMBER_LINEPLOT_HTML.html")

## Number of purse seiners by capacity category ####
GT_BREAKS = c(300, 1000, 2000, 3000, 4000, 5000)
GT_LABELS = c("<1,000", "1,000-2,000", "2,000-3,000", "3,000-4,000", ">4,000")
GT_COLORS = data.table(GT = GT_LABELS, FILL = rev(brewer.pal(length(GT_LABELS), "Reds")), OUTLINE = darken(rev(brewer.pal(length(GT_LABELS), "Reds")), 0.3))
ACTIVE_PS[, GT_CLASS := cut(GT, breaks = GT_BREAKS, labels = GT_LABELS)]
ACTIVE_PS[, GT_CLASS := factor(GT_CLASS, levels = rev(GT_LABELS))]

PS_NUMBER_YEAR_GT_CLASS = ACTIVE_PS[, .(N = length(unique(IOTC_NUMBER))), keyby = .(YEAR, GT_CLASS)]

PS_ANNUAL_NUMBER_GT_CLASS_BARPLOT = value_bar(PS_NUMBER_YEAR_GT_CLASS, value = "N", time = C_YEAR, fill_by = "GT_CLASS", colors = GT_COLORS, y_axis_label = "Number of large-scale purse seiners") + theme(legend.position = "bottom") + guides (fill = guide_legend(reverse = TRUE)) + theme(axis.text.x = element_text(size = 8))

PS_ANNUAL_NUMBER_GT_CLASS_BARPLOT_PERC = value_bar_rel(PS_NUMBER_YEAR_GT_CLASS, value = "N", time = C_YEAR, fill_by = "GT_CLASS", colors = GT_COLORS, y_axis_label = "Gross tonnage category composition (%)") + theme(legend.position = "bottom") + guides (fill = guide_legend(reverse = TRUE)) + theme(axis.text.x = element_text(size = 8))

PS_ANNUAL_NUMBER_GT_CLASS_BARPLOT_WITH_PERC = ggarrange(PS_ANNUAL_NUMBER_GT_CLASS_BARPLOT, PS_ANNUAL_NUMBER_GT_CLASS_BARPLOT_PERC, 
                                                 common.legend = TRUE, 
                                                 legend = "bottom", 
                                                 labels = list("a", "b"))

save_plot("../outputs/charts/CAPACITY/PS/PS_ANNUAL_NUMBER_GT_CLASS_BARPLOT_COMBINED.png", PS_ANNUAL_NUMBER_GT_CLASS_BARPLOT_PERC, 10, 4.5)

## Annual changes in mean length overall ####
PS_MEAN_LOA_GT_YEAR = ACTIVE_PS[, .(N = length(unique(IOTC_NUMBER)), LOA = mean(LOA), GT = mean(GT)), keyby = .(YEAR)]

### PNG ####
ANNUAL_PS_MEAN_GT_LINEPLOT = 
  ggplot(data = PS_MEAN_LOA_GT_YEAR, aes(x = YEAR, y = GT)) +
  ggh4x::geom_pointpath(col = "red", size = 2) +
  labs(x = "", y = "Mean gross tonnage") + 
  scale_y_continuous(labels = function(x) prettyNum(x, big.mark = ",")) + 
  theme_bw() + 
  theme(panel.grid.minor = element_line(colour = "darkgrey", linetype = "dashed", size = 0.1))

save_plot("../outputs/charts/CAPACITY/PS/ANNUAL_PS_MEAN_GT_LINEPLOT.png", ANNUAL_PS_MEAN_GT_LINEPLOT, 8, 4.5)

### HTML ####

#ANNUAL_PS_MEAN_GT_LINEPLOT_HTML = 
#  plot_ly(data = PS_MEAN_LOA_GT_YEAR, x = ~YEAR, y = ~GT, type = 'scatter', mode = 'lines+markers', line = list(color = "red", width = 2), marker = list(color = "red", size = 10)) %>% 
#  layout(title = "Annual changes in mean gross tonnage", yaxis = list(title = "Mean gross tonnage"), xaxis = list(title = ""), plot_bgcolor = "lighgrey")

#htmlwidgets::saveWidget(ANNUAL_PS_MEAN_GT_LINEPLOT_HTML, "../outputs/charts/CAPACITY/PS/ANNUAL_PS_MEAN_GT_LINEPLOT.html")

## Numbers of purse seiners by length overall
LOA_BREAKS = c(50, 65, 80, 95, 110, 125)
LOA_LABELS = c("<65", "65-80", "80-95", "95-110", ">110")
LOA_COLORS = data.table(LOA = LOA_LABELS, FILL = rev(brewer.pal(length(LOA_LABELS), "Blues")), OUTLINE = darken(rev(brewer.pal(length(LOA_LABELS), "Blues")), 0.3))
ACTIVE_PS[, LOA_CLASS := cut(LOA, breaks = LOA_BREAKS, labels = LOA_LABELS)]
ACTIVE_PS[, LOA_CLASS := factor(LOA_CLASS, levels = rev(LOA_LABELS))]

ANNUAL_PS_LOA_CLASS = ACTIVE_PS[, .(N = length(unique(IOTC_NUMBER))), keyby = .(YEAR, LOA_CLASS)]

ANNUAL_PS_LOA_CLASS_BARPLOT =  value_bar(ANNUAL_PS_LOA_CLASS, value = "N", time = C_YEAR, fill_by = "LOA_CLASS", colors = LOA_COLORS, y_axis_label = "Number of large-scale purse seiners") + theme(legend.position = "bottom") + guides (fill = guide_legend(reverse = TRUE)) + theme(axis.text.x = element_text(size = 8)) 

ANNUAL_PS_LOA_CLASS_BARPLOT_PERC = value_bar_rel(ANNUAL_PS_LOA_CLASS, value = "N", time = C_YEAR, fill_by = "LOA_CLASS", colors = LOA_COLORS, y_axis_label = "Length category composition (%)") + theme(legend.position = "bottom") + guides (fill = guide_legend(reverse = TRUE)) + theme(axis.text.x = element_text(size = 8))

ANNUAL_PS_LOA_CLASS_BARPLOT_WITH_PERC = ggarrange(ANNUAL_PS_LOA_CLASS_BARPLOT, ANNUAL_PS_LOA_CLASS_BARPLOT_PERC, 
                                                  common.legend = TRUE, 
                                                  legend = "bottom", 
                                                  labels = list("a", "b"))

save_plot("../outputs/charts/CAPACITY/PS/ANNUAL_PS_LOA_CLASS_COMBINED.png", ANNUAL_PS_LOA_CLASS_BARPLOT_WITH_PERC, 10, 4.5)

ANNUAL_PS_MEAN_LOA_LINEPLOT = 
  ggplot(data = PS_MEAN_LOA_GT_YEAR, aes(x = YEAR, y = LOA)) +
  ggh4x::geom_pointpath(col = "blue", size = 2) +
  labs(x = "", y = "Length overall (m)") + 
  scale_y_continuous(labels = function(x) prettyNum(x, big.mark = ",")) + 
  theme_bw() + 
  theme(panel.grid.minor = element_line(colour = "darkgrey", linetype = "dashed", size = 0.1))

save_plot("../outputs/charts/CAPACITY/PS/ANNUAL_PS_MEAN_LOA_LINEPLOT.png", ANNUAL_PS_MEAN_LOA_LINEPLOT, 8, 4.5)

## Numbers and capacity of support vessels managing the drifting FADs ####

GT_BREAKS = c(100, 200, 300, 400, 500, 600)
GT_LABELS = c("100-200", "200-300", "300-400", "400-500", "500-600")
GT_COLORS = data.table(GT = GT_LABELS, FILL = rev(brewer.pal(length(GT_LABELS), "Reds")), OUTLINE = darken(rev(brewer.pal(length(GT_LABELS), "Reds")), 0.3))
ACTIVE_SP[, GT_CLASS := cut(GT, breaks = GT_BREAKS, labels = GT_LABELS)]
ACTIVE_SP[, GT_CLASS := factor(GT_CLASS, levels = rev(GT_LABELS))]

ANNUAL_SP_GT_CLASS = ACTIVE_SP[, .(N = length(unique(IOTC_NUMBER))), keyby = .(YEAR, GT_CLASS)]

ANNUAL_SP_GT_CLASS_BARPLOT =  value_bar(ANNUAL_SP_GT_CLASS, value = "N", time = C_YEAR, fill_by = "GT_CLASS", colors = GT_COLORS, y_axis_label = "Number of support vessels") + theme(legend.position = "bottom") + guides (fill = guide_legend(reverse = TRUE)) 

ANNUAL_SP_GT_CLASS_BARPLOT_PERC = value_bar_rel(ANNUAL_SP_GT_CLASS, value = "N", time = C_YEAR, fill_by = "GT_CLASS", colors = GT_COLORS, y_axis_label = "Gross tonnage category composition (%)") + theme(legend.position = "bottom") + guides (fill = guide_legend(reverse = TRUE)) 

ANNUAL_SP_GT_CLASS_BARPLOT_WITH_PERC = ggarrange(ANNUAL_SP_GT_CLASS_BARPLOT, ANNUAL_SP_GT_CLASS_BARPLOT_PERC, 
                                                 common.legend = TRUE, legend = "bottom")

save_plot("../outputs/charts/CAPACITY/PS/ANNUAL_SP_GT_CLASS_COMBINED.png", ANNUAL_SP_GT_CLASS_BARPLOT_WITH_PERC, 12, 9/16*12)

ANNUAL_SP_LOA_GT = ACTIVE_SP[, .(N = length(unique(IOTC_NUMBER)), LOA = mean(LOA), GT = mean(GT)), keyby = .(YEAR)]

ANNUAL_SP_MEAN_GT_LINEPLOT = 
  ggplot(data = ANNUAL_SP_LOA_GT, aes(x = YEAR, y = GT)) +
  ggh4x::geom_pointpath(col = "red", size = 2) +
  labs(x = "", y = "Mean gross tonnage") + 
  scale_y_continuous(labels = function(x) prettyNum(x, big.mark = ",")) + 
  theme_bw() + 
  theme(panel.grid.minor = element_line(colour = "darkgrey", linetype = "dashed", size = 0.1))

save_plot("../outputs/charts/CAPACITY/PS/ANNUAL_SP_MEAN_GT_LINEPLOT.png", ANNUAL_SP_MEAN_GT_LINEPLOT, 8, 4.5)

## Numbers of support vessels by length overall
LOA_BREAKS = c(25, 30, 35, 40, 45, 50)
LOA_LABELS = c("25-30", "30-35", "35-40", "40-45", "45-50")
LOA_COLORS = data.table(LOA = LOA_LABELS, FILL = rev(brewer.pal(length(LOA_LABELS), "Blues")), OUTLINE = darken(rev(brewer.pal(length(LOA_LABELS), "Blues")), 0.3))
ACTIVE_SP[, LOA_CLASS := cut(LOA, breaks = LOA_BREAKS, labels = LOA_LABELS)]
ACTIVE_SP[, LOA_CLASS := factor(LOA_CLASS, levels = rev(LOA_LABELS))]

ANNUAL_SP_LOA_CLASS = ACTIVE_SP[, .(N = length(unique(IOTC_NUMBER))), keyby = .(YEAR, LOA_CLASS)]

ANNUAL_SP_LOA_CLASS_BARPLOT =  value_bar(ANNUAL_SP_LOA_CLASS, value = "N", time = C_YEAR, fill_by = "LOA_CLASS", colors = LOA_COLORS, y_axis_label = "Number of support vessels") + theme(legend.position = "bottom") + guides (fill = guide_legend(reverse = TRUE)) 

ANNUAL_SP_LOA_CLASS_BARPLOT_PERC = value_bar_rel(ANNUAL_SP_LOA_CLASS, value = "N", time = C_YEAR, fill_by = "LOA_CLASS", colors = LOA_COLORS, y_axis_label = "Length category composition (%)") + theme(legend.position = "bottom") + guides (fill = guide_legend(reverse = TRUE)) 

ANNUAL_SP_LOA_CLASS_BARPLOT_WITH_PERC = ggarrange(ANNUAL_SP_LOA_CLASS_BARPLOT, ANNUAL_SP_LOA_CLASS_BARPLOT_PERC, 
                                                  common.legend = TRUE, legend = "bottom")

save_plot("../outputs/charts/CAPACITY/PS/ANNUAL_SP_LOA_CLASS_COMBINED.png", ANNUAL_SP_LOA_CLASS_BARPLOT_WITH_PERC, 12, 9/16*12)

ANNUAL_SP_MEAN_LOA_LINEPLOT = 
  ggplot(data = ANNUAL_SP_LOA_GT, aes(x = YEAR, y = LOA)) +
  ggh4x::geom_pointpath(col = "blue", size = 2) +
  labs(x = "", y = "Length overall (m)") + 
  scale_y_continuous(labels = function(x) prettyNum(x, big.mark = ",")) + 
  theme_bw() + 
  theme(panel.grid.minor = element_line(colour = "darkgrey", linetype = "dashed", size = 0.1))

save_plot("../outputs/charts/CAPACITY/PS/ANNUAL_SP_MEAN_LOA_LINEPLOT.png", ANNUAL_SP_MEAN_LOA_LINEPLOT, 8, 4.5)

l_info("Fishing capacity indicators computed!")