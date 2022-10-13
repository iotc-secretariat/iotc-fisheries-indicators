print("Initializing NC charts...")

# BARPLOT BY FISHERY GROUP ####
NC_BARPLOT_FISHERY_GROUP  = catch_bar(NC_YEARS_FISHERY_GROUP, C_FISHERY_GROUP, num_legend_rows = 2) + theme(legend.position = "bottom", axis.text.x = element_text(size = 6), legend.text = element_text(size = 7))

NC_LINEPLOT_FISHERY_GROUP = catch_line(NC_YEARS_FISHERY_GROUP, C_FISHERY_GROUP, num_legend_rows = 2, plot_points = TRUE) + theme(legend.position = "bottom", axis.text.x = element_text(size = 6), legend.text = element_text(size = 7))

NC_PLOTS_FISHERY_GROUP = 
    ggarrange(NC_BARPLOT_FISHERY_GROUP,
          NC_LINEPLOT_FISHERY_GROUP,
          common.legend = TRUE,
          legend = "bottom",
          labels = list("a", "b"))
save_plot(paste0("../outputs/", ES_SPECIES_CODE, "/charts/NC_PLOTS_FISHERY_GROUP.png"), NC_PLOTS_FISHERY_GROUP, 9.5, 4)

# BARPLOT BY FISHERY ####
NC_BARPLOT_FISHERY  = catch_bar(NC_YEARS_FISHERY, C_FISHERY, num_legend_rows = 2) + theme(legend.position = "bottom", axis.text.x = element_text(size = 6), legend.text = element_text(size = 7))

NC_LINEPLOT_FISHERY = catch_line(NC_YEARS_FISHERY, C_FISHERY, num_legend_rows = 2, plot_points = TRUE) + theme(legend.position = "bottom", axis.text.x = element_text(size = 6), legend.text = element_text(size = 7))

NC_PLOTS_FISHERY = 
  ggarrange(NC_BARPLOT_FISHERY,
            NC_LINEPLOT_FISHERY,
            common.legend = TRUE,
            legend = "bottom",
            labels = list("a", "b"))

save_plot(paste0("../outputs/", ES_SPECIES_CODE, "/charts/NC_PLOTS_FISHERY.png"), NC_PLOTS_FISHERY, 9.5, 4)

# BARPLOT BY FISHERY and FISHERY GROUP ####
NC_BARPLOT_FISHERY_LINEPLOT_FISHERY_GROUP = 
  ggarrange(NC_BARPLOT_FISHERY + guides(fill = guide_legend(nrow = 3)),
            NC_LINEPLOT_FISHERY_GROUP + guides(color = guide_legend(nrow = 3)),
            common.legend = FALSE,
            legend = "bottom",
            labels = list("a", "b"))

save_plot(paste0("../outputs/", ES_SPECIES_CODE, "/charts/NC_BARPLOT_FISHERY_LINEPLOT_FISHERY_GROUP.png"), NC_BARPLOT_FISHERY_LINEPLOT_FISHERY_GROUP, 9.5, 4)

# PARETO PLOT BY FISHERY GROUP AND FLEET ####
NC_PARETO_RECENT_FISHERY_GROUP_FLEET = pareto.catch(NC_BSE[YEAR %in% LAST_5_YEARS], fill_by = C_FISHERY_GROUP,  categorize_by = C_FLEET_CODE, max_categories = 10, num_legend_rows = 2) + theme(legend.position = "bottom")

save_plot(paste0("../outputs/", ES_SPECIES_CODE, "/charts/NC_PARETO_RECENT_FISHERY_GROUP_FLEET.png"), NC_PARETO_RECENT_FISHERY_GROUP_FLEET, 12, 6.75)

# PARETO PLOT BY FISHERY AND FLEET ####
NC_PARETO_RECENT_FISHERY_FLEET = pareto.catch(NC_BSE[YEAR %in% LAST_5_YEARS], fill_by = C_FISHERY,  categorize_by = C_FLEET_CODE, max_categories = 10, num_legend_rows = 2) + theme(legend.position = "bottom")

save_plot(paste0("../outputs/", ES_SPECIES_CODE, "/charts/NC_PARETO_RECENT_FISHERY_FLEET.png"), NC_PARETO_RECENT_FISHERY_FLEET, 12, 6.75)

print("NC charts initialized!")
