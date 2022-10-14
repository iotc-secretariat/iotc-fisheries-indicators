l_info("Computing indicator of fishing time")

# PURSE SEINERS ####

EF_PS_DAYS_FLEET_BARPLOT = value_bar(EF_DAYS_PS_YEAR_FLEET, value = "FISHING DAYS", time = C_YEAR, fill_by = C_FLEET_CODE, y_axis_label = "Number of fishing days", num_legend_rows = 1) + theme(legend.position = "bottom") + theme(axis.text.x = element_text(size = 7))

save_plot("../outputs/charts/EFFORT/DAYS/EF_DAYS_PS_FLEET_BARPLOT.png", EF_PS_DAYS_FLEET_BARPLOT, 9, 4.5)

# Support vessels

EF_SP_DAYS_FLEET_BARPLOT = value_bar(EF_DAYS_SP_YEAR_FLEET, value = "FISHING DAYS", time = C_YEAR, fill_by = C_FLEET_CODE, y_axis_label = "Number of fishing days") + theme(legend.position = "bottom")  + theme(axis.text.x = element_text(size = 7))

save_plot("../outputs/charts/EFFORT/DAYS/EF_DAYS_SP_FLEET_BARPLOT.png", EF_SP_DAYS_FLEET_BARPLOT, 9, 4.5)

l_info("Indicator of fishing time computed!")




