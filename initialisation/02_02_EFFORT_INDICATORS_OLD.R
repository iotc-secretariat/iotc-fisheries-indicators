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

EF_LL_HOOKS_CWP55_MAP_CORE = geo.grid.heatmap(EF_LL_SPECIES[CWP55 %in% EF_LL_HOOKS_CWP55_CORE], value = "EFFORT", yearly_average = TRUE, unit = "Hooks", num_intervals = 1, palette = diverging_hsv) + theme(legend.position = "none")

ggsave("../outputs/charts/EFFORT/LL/EF_LL_HOOKS_CWP55_MAP_CORE.png", EF_LL_HOOKS_CWP55_MAP_CORE, width = 8, height = 4.5)

## Annual time series ####

# Compute total annual effort
EF_LL_HOOKS_YEAR = EF_LL_SPECIES[, .(HOOKS = sum(EFFORT)), keyby = .(YEAR)]
EF_LL_HOOKS_YEAR[, HOOKS_STD := HOOKS/mean(HOOKS)]

EF_LL_HOOKS_YEAR_LINEPLOT = 
  ggplot(data = EF_LL_HOOKS_YEAR, aes(x  = YEAR, y = HOOKS_STD)) + 
  labs(x = "", y = "Standardized effort", title = "Longline") + 
  geom_pointpath(col = "black", size = 1) +
  geom_hline(yintercept = quantile(EF_LL_HOOKS_YEAR$HOOKS_STD, 0.05), linetype = "dashed") + 
  geom_hline(yintercept = quantile(EF_LL_HOOKS_YEAR$HOOKS_STD, 0.95), linetype = "dashed")

ggsave("../outputs/charts/EFFORT/LL/EF_LL_HOOKS_YEAR_LINEPLOT.png", EF_LL_HOOKS_YEAR_LINEPLOT, width = 8, height = 4.5)

# Remove Thai and NEI longline fisheries
EF_LL_HOOKS_RAW_FLEETS_YEAR = EF_LL_HOOKS_RAW[!FLEET_CODE %in% c("THA", "NEICE", "NEIFR"), .(HOOKS = sum(EFFORT)), keyby = .(FISHERY_CODE, FISHERY, FLEET_CODE, FLEET, YEAR)]

EF_LL_HOOKS_FLEETS_YEAR_BARPLOT = value_bar(EF_LL_HOOKS_RAW_FLEETS_YEAR, value = "HOOKS", fill_by = "FLEET", scale = 1e6, y_axis_label = "Effort (milion hooks)", trim_labels = TRUE) + theme(legend.position = "bottom")

ggsave("../outputs/charts/EFFORT/LL/EF_LL_HOOKS_FLEETS_YEAR_BARPLOT.png", EF_LL_HOOKS_FLEETS_YEAR_BARPLOT, width = 8, height = 4.5)

# PURSE SEINERS ####

# Compute total annual effort
EF_PS_FDAYS_YEAR = EF_FDAYS_PS_YEAR_FLEET[, .(FDAYS = round(sum(FDAYS))), keyby = .(YEAR)]
EF_PS_FDAYS_YEAR[, FDAYS_STD := FDAYS/mean(FDAYS)]

EF_PS_FDAYS_YEAR_LINEPLOT = 
  ggplot(data = EF_PS_FDAYS_YEAR, aes(x  = YEAR, y = FDAYS_STD)) + 
  labs(x = "", y = "Standardized effort", title = "Purse seine") + 
  geom_pointpath(col = "black", size = 1) +
  geom_hline(yintercept = quantile(EF_PS_FDAYS_YEAR$FDAYS_STD, 0.05), linetype = "dashed") + 
  geom_hline(yintercept = quantile(EF_PS_FDAYS_YEAR$FDAYS_STD, 0.95), linetype = "dashed")

ggsave("../outputs/charts/EFFORT/PS/EF_PS_FDAYS_YEAR_LINEPLOT.png", EF_PS_FDAYS_YEAR_LINEPLOT, width = 8, height = 4.5)

EF_PS_DAYS_FLEET_BARPLOT = value_bar(EF_PS_FDAYS_YEAR_FLEET, value = "FDAYS", fill_by = "FLEET", scale = 1, y_axis_label = "Effort (Fishing days)", trim_labels = FALSE) + theme(legend.position = "bottom")

ggsave("../outputs/charts/EFFORT/PS/EF_PS_DAYS_FLEET_BARPLOT.png", EF_PS_DAYS_FLEET_BARPLOT, width = 9, height = 4.5)

# Support vessels

# Compute total annual effort
EF_SP_DAYS_YEAR = EF_SP_RAW[, .(DAYS = round(sum(DAYS))), keyby = .(YEAR)]
EF_SP_DAYS_YEAR[, DAYS_STD := DAYS/mean(DAYS)]

EF_SP_FDAYS_YEAR_LINEPLOT = 
  ggplot(data = EF_SP_DAYS_YEAR, aes(x  = YEAR, y = DAYS_STD)) + 
  labs(x = "", y = "Standardized effort", title = "Support vessels") + 
  geom_pointpath(col = "black", size = 1) +
  geom_hline(yintercept = quantile(EF_PS_FDAYS_YEAR$FDAYS_STD, 0.05), linetype = "dashed") + 
  geom_hline(yintercept = quantile(EF_PS_FDAYS_YEAR$FDAYS_STD, 0.95), linetype = "dashed")

ggsave("../outputs/charts/EFFORT/PS/EF_PS_FDAYS_YEAR_LINEPLOT.png", EF_PS_FDAYS_YEAR_LINEPLOT, width = 8, height = 4.5)

## Compute effort by fleet

EF_SP_DAYS_YEAR_FLEET = EF_SP_RAW[, .(DAYS = sum(DAYS, na.rm = TRUE)), keyby = .(YEAR, FLEET_CODE, FLEET)]

EF_SP_DAYS_FLEETS_YEAR_BARPLOT = value_bar(EF_SP_DAYS_YEAR_FLEET, value = "DAYS", fill_by = "FLEET", scale = 1, y_axis_label = "Effort (Days at sea)", trim_labels = FALSE) + theme(legend.position = "bottom")

#EF_SP_DAYS_FLEET_BARPLOT = value_bar(EF_DAYS_SP_YEAR_FLEET, value = "FISHING DAYS", time = C_YEAR, fill_by = C_FLEET_CODE, y_axis_label = "Number of fishing days") + theme(legend.position = "bottom")  + theme(axis.text.x = element_text(size = 7))

save_plot("../outputs/charts/EFFORT/DAYS/EF_DAYS_SP_FLEET_BARPLOT.png", EF_SP_DAYS_FLEET_BARPLOT, 9, 4.5)

## BAITBOATS

EF_BB_FDAYS_RAW_YEAR = EF_BB_RAW[, .(FDAYS = sum(EFFORT)), keyby = .(YEAR)]
EF_BB_FDAYS_RAW_YEAR[, FDAYS_STD := FDAYS/mean(FDAYS)]

EF_BB_FDAYS_YEAR_LINEPLOT = 
  ggplot(data = EF_BB_FDAYS_RAW_YEAR, aes(x  = YEAR, y = FDAYS_STD)) + 
  labs(x = "", y = "Standardized effort", title = "Longline") + 
  geom_pointpath(col = "black", size = 1) +
  geom_hline(yintercept = quantile(EF_BB_FDAYS_RAW_YEAR$FDAYS_STD, 0.05), linetype = "dashed") + 
  geom_hline(yintercept = quantile(EF_BB_FDAYS_RAW_YEAR$FDAYS_STD, 0.95), linetype = "dashed")

ggsave("../outputs/charts/EFFORT/BB/EF_BB_FDAYS_YEAR_LINEPLOT.png", EF_BB_FDAYS_YEAR_LINEPLOT, width = 8, height = 4.5)

EF_BB_FDAYS_YEAR = EF_BB_RAW[, .(FDAYS = sum(EFFORT)), keyby = .(YEAR, FLEET_CODE, FLEET)]

EF_BB_FDAYS_FLEETS_YEAR_BARPLOT = value_bar(EF_BB_FDAYS_YEAR, value = "FDAYS", fill_by = "FLEET", y_axis_label = "Effort (number of fishing days)", trim_labels = FALSE) + theme(legend.position = "bottom")

ggsave("../outputs/charts/EFFORT/BB/EF_BB_FDAYS_FLEETS_YEAR_BARPLOT.png", EF_BB_FDAYS_FLEETS_YEAR_BARPLOT, width = 8, height = 4.5)


# ### SUPPORT VESSELS ####
# 
# # Missing effort from Seychelles (2014, 2015, 2017)
# 
# EF_SP_RAW = as.data.table(read.xlsx("../inputs/data/3SU_DATA_collated.xlsx"))
# 
# # Re-express days at sea in "days" for consistency with purse seiners (operations only occur in daylight)
# EF_SP_RAW[, DAYS := DAYS_AT_SEA/24*13]
# 
# names(EF_SP_RAW)[names(EF_SP_RAW) == "FLAG_CODE"] = "FLEET_CODE"
# EF_SP_RAW[FLEET_CODE %in% c("ESP", "FRA", "ITA"), FLEET_CODE := paste0("EU", FLEET_CODE)]
# 
# EF_SP_RAW[FLEET_CODE == "EUESP", FLEET := "EU (Spain)"]
# EF_SP_RAW[FLEET_CODE == "EUFRA", FLEET := "EU (France)"]
# EF_SP_RAW[FLEET_CODE == "JPN", FLEET := "Japan"]
# EF_SP_RAW[FLEET_CODE == "KOR", FLEET := "Republic of Korea"]
# EF_SP_RAW[FLEET_CODE == "SYC", FLEET := "Seychelles"]
# EF_SP_RAW[FLEET_CODE == "MUS", FLEET := "Mauritius"]
# 
# ## NUMBER OF SETS #### 
# 
# EF_SETS_PS_RAW = raw.EF(connection = DB_CONNECT_TO(server = "IOTCS09", database = "IOTDB"), 
#                    years = 2013:LAST_YEAR, 
#                    fishery_group_codes = "PS",  
#                    fleet_codes = c("JPN", "KOR"), 
#                    effort_unit_codes = "SETS", 
# )
# 
# EF_PS_LS_SETS_YEAR_FLEET1 = EF_SETS_PS_RAW[SCHOOL_TYPE_CODE == "LS", .(NUM_SETS_ON_FOB = sum(EFFORT)), keyby = .(YEAR, SCHOOL_TYPE_CODE, FLEET_CODE, FLEET)]
# 
# EF_PS_LS_SETS_YEAR_FLEET2 = fread("../inputs/data/ANNUAL_NUM_SETS_LS_COMPILED.csv")
# 
# EF_PS_LS_SETS_YEAR_FLEET = rbindlist(list(EF_PS_LS_SETS_YEAR_FLEET1, EF_PS_LS_SETS_YEAR_FLEET2))
# 
# EF_PS_LS_SETS_YEAR_FLEET_BARPLOT = value_bar(EF_PS_LS_SETS_YEAR_FLEET, value = "NUM_SETS_ON_FOB", fill_by = "FLEET", scale = 1, y_axis_label = "Number of sets on FOBs", trim_labels = FALSE) + theme(legend.position = "bottom")

# ## NUMBER OF FADS DEPLOYED ####
# # Lacks Seychelles
# 
# FA_RAW = as.data.table(read.xlsx("../inputs/data/3FA_DATA_collated.xlsx"))
# FA_RAW[NUM_FOBS == " " | NUM_FOBS == "_", NUM_FOBS := NA]
# FA_RAW[, NUM_FOBS := as.numeric(NUM_FOBS)]
# FA_RAW[, FISHERY_TYPE := trimws(FISHERY_TYPE)]
# FA_RAW[FLAG_CODE %in% c("ESP", "FRA"), FLAG_CODE := paste0("EU", FLAG_CODE)]
# 
# PS_DEPLOYMENTS_YEAR_FLEET = FA_RAW[YEAR<2022 & ACTIVITY_TYPE_CODE == "DD" & FISHERY_TYPE %in% c("PSLS", "PS"), .(DEPLOYED = sum(NUM_FOBS, na.rm = TRUE)), keyby = .(YEAR, FLEET_CODE = FLAG_CODE)]
# 
# SP_DEPLOYMENTS_YEAR_FLEET = FA_RAW[YEAR<2022 & FISHERY_TYPE == "PSSP", .(DEPLOYED = sum(NUM_FOBS, na.rm = TRUE)), keyby = .(YEAR, FLEET_CODE = FLAG_CODE)]
# 
# ALL_DEPLOYMENTS_YEAR_FLEET = rbindlist(list(PS_DEPLOYMENTS_YEAR_FLEET, SP_DEPLOYMENTS_YEAR_FLEET))
# 
# ALL_DEPLOYMENTS_YEAR_FLEET[FLEET_CODE == "EUESP", FLEET := "EU (Spain)"]
# ALL_DEPLOYMENTS_YEAR_FLEET[FLEET_CODE == "EUFRA", FLEET := "EU (France)"]
# ALL_DEPLOYMENTS_YEAR_FLEET[FLEET_CODE == "JPN", FLEET := "Japan"]
# ALL_DEPLOYMENTS_YEAR_FLEET[FLEET_CODE == "KOR", FLEET := "Republic of Korea"]
# ALL_DEPLOYMENTS_YEAR_FLEET[FLEET_CODE == "MUS", FLEET := "Mauritius"]
# ALL_DEPLOYMENTS_YEAR_FLEET[FLEET_CODE == "SYC", FLEET := "Seychelles"]
# 
# EF_FAD_DEPLOYMENTS_FLEETS_YEAR_BARPLOT = value_bar(ALL_DEPLOYMENTS_YEAR_FLEET[YEAR>2014], value = "DEPLOYED", fill_by = "FLEET", scale = 1, y_axis_label = "Number of FADs deployed", trim_labels = FALSE) + theme(legend.position = "bottom")


l_info("Indicator of fishing time computed!")