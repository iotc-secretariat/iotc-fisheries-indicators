l_info("Computing indicators of fishing effort")

# LONGLINE FISHERIES ####

## Map of core fishing grounds ####

EF_LL_HOOKS_CWP55 = EF_LL_SPECIES[FISHING_GROUND_TYPE != "IRREGULAR", .(HOOKS = sum(EFFORT)), keyby = .(CWP55)][order(HOOKS, decreasing = T)]

EF_LL_HOOKS_CWP55[, HOOKS_MEAN := HOOKS/length(min(EF_LL_SPECIES$YEAR):max(EF_LL_SPECIES$YEAR))]
EF_LL_HOOKS_CWP55[, HOOKS_MEAN_CUM := cumsum(HOOKS_MEAN)]
EF_LL_HOOKS_CWP55[, HOOKS_MEAN_PERCENT := round(HOOKS_MEAN_CUM/sum(HOOKS_MEAN), 4)]

EF_LL_HOOKS_CWP55_MAP = geo.grid.heatmap(EF_LL_SPECIES, value = "EFFORT", yearly_average = TRUE, unit = "Hooks")

ggsave("../outputs/charts/EFFORT/LL/EF_LL_HOOKS_CWP55_MAP.png", EF_LL_HOOKS_CWP55_MAP, width = 8, height = 4.5)

# Keep grids where cumulative effort contributes to X% of total effort
CUM_EFFORT_THRESHOLD = 0.8

EF_LL_HOOKS_CWP55_CORE = EF_LL_HOOKS_CWP55[HOOKS_MEAN_PERCENT<=CUM_EFFORT_THRESHOLD, CWP55]

EF_LL_HOOKS_CWP55_MAP_CORE = geo.grid.heatmap(EF_LL_SPECIES[CWP55 %in% EF_LL_HOOKS_CWP55_CORE], value = "EFFORT", yearly_average = TRUE, unit = "Hooks", num_intervals = 1) + theme(legend.position = "none")


ggsave("../outputs/charts/EFFORT/LL/EF_LL_HOOKS_CWP55_MAP_CORE.png", EF_LL_HOOKS_CWP55_MAP_CORE, width = 8, height = 4.5)

## Annual time series ####

# Compute total annual effort
EF_LL_HOOKS_YEAR = EF_LL_SPECIES[, .(HOOKS = sum(EFFORT)), keyby = .(YEAR)]
EF_LL_HOOKS_YEAR[, HOOKS_STD := HOOKS/mean(HOOKS)]

EF_LL_HOOKS_YEAR_LINEPLOT = 
  ggplot(data = EF_LL_HOOKS_YEAR, aes(x  = YEAR, y = HOOKS_STD)) + 
  labs(x = "", y = "Standardized effort", title = "Longline (all reporting CPCs)") + 
  geom_pointpath(col = "black", size = 1) +
  geom_hline(yintercept = quantile(EF_LL_HOOKS_YEAR$HOOKS_STD, 0.05), linetype = "dashed") + 
  geom_hline(yintercept = quantile(EF_LL_HOOKS_YEAR$HOOKS_STD, 0.95), linetype = "dashed")

ggsave("../outputs/charts/EFFORT/LL/EF_LL_HOOKS_YEAR_LINEPLOT.png", EF_LL_HOOKS_YEAR_LINEPLOT, width = 8, height = 4.5)

EF_LL_HOOKS_RAW_FLEETS_YEAR = EF_LL_HOOKS_RAW[!FLEET_CODE %in% c("THA", "NEICE", "NEIFR"), .(HOOKS = sum(EFFORT)), keyby = .(FISHERY_CODE, FISHERY, FLEET_CODE, FLEET, YEAR)]

EF_LL_HOOKS_FLEETS_YEAR_BARPLOT = value_bar(EF_LL_HOOKS_RAW_FLEETS_YEAR, value = "HOOKS", fill_by = "FLEET", scale = 1e6, y_axis_label = "Effort (milion hooks)", trim_labels = TRUE) + theme(legend.position = "bottom")

ggsave("../outputs/charts/EFFORT/LL/EF_LL_HOOKS_FLEETS_YEAR_BARPLOT.png", EF_LL_HOOKS_FLEETS_YEAR_BARPLOT, width = 8, height = 4.5)


# PURSE SEINERS ####

EF_PS_DAYS_FLEET_BARPLOT = value_bar(EF_DAYS_PS_YEAR_FLEET, value = "FISHING DAYS", time = C_YEAR, fill_by = C_FLEET_CODE, y_axis_label = "Number of fishing days", num_legend_rows = 1) + theme(legend.position = "bottom") + theme(axis.text.x = element_text(size = 7))

save_plot("../outputs/charts/EFFORT/DAYS/EF_DAYS_PS_FLEET_BARPLOT.png", EF_PS_DAYS_FLEET_BARPLOT, 9, 4.5)

# Support vessels

EF_SP_DAYS_FLEET_BARPLOT = value_bar(EF_DAYS_SP_YEAR_FLEET, value = "FISHING DAYS", time = C_YEAR, fill_by = C_FLEET_CODE, y_axis_label = "Number of fishing days") + theme(legend.position = "bottom")  + theme(axis.text.x = element_text(size = 7))

save_plot("../outputs/charts/EFFORT/DAYS/EF_DAYS_SP_FLEET_BARPLOT.png", EF_SP_DAYS_FLEET_BARPLOT, 9, 4.5)

l_info("Indicator of fishing time computed!")




