d_info("Initializing effort data extaction...")

# LONGLINE FISHERIES ####

CE_LL_SPECIES = CE_raw(species_codes = SPECIES_WPTT, fishery_group_codes = "LL", effort_unit_codes = "HOOKS")

CE_LL_SPECIES_STRATA = unique(CE_LL_SPECIES[, .(YEAR, MONTH_START, MONTH_END, FISHING_GROUND_CODE, FLEET_CODE, GEAR_CODE)])

EF_LL_SPECIES = merge(EF_raw(), CE_LL_SPECIES_STRATA, by = c("YEAR", "MONTH_START", "MONTH_END", "FISHING_GROUND_CODE", "FLEET_CODE", "GEAR_CODE"))

EF_LL_SPECIES = merge(EF_LL_SPECIES, filter_grids()[, .(FISHING_GROUND_CODE, FISHING_GROUND_TYPE)], by = "FISHING_GROUND_CODE", all.x = TRUE)

# Add type of grid
EF_LL_SPECIES[is.na(FISHING_GROUND_TYPE) , FISHING_GROUND_TYPE := "IRREGULAR"]

# Convert to 5x5 grid
EF_LL_SPECIES[FISHING_GROUND_TYPE != "IRREGULAR", CWP55 := convert_CWP_grid(FISHING_GROUND_CODE, target_grid_type_code = grid_5x5), by = .(FISHING_GROUND_CODE)]

# Rename UK fleet
EF_LL_SPECIES[FLEET_CODE == "GBR", FLEET := "United Kingdom"]

## BAITBOAT FISHERIES ####

# Focus on Maldives
EF_BB_RAW = EF_raw(fleet_codes = "MDV", fishery_codes = "BB")

# Assuming 1 day-trips
EF_BB_RAW[EFFORT_UNIT_CODE == "TRIPS", EFFORT_UNIT_CODE := "FDAYS"]

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

# PURSE SEINE FISHERIES ####

## FISHING TIME ####

### PURSE SEINERS ####

EF_PS_TIME_RAW = raw.EF(fishery_group_codes = "PS")[FISHERY_TYPE_CODE == "IND" & FLEET_CODE != "AUS"][EFFORT_UNIT_CODE %in% c("FHOURS", "FDAYS")]
EF_PS_TIME_RAW[EFFORT_UNIT_CODE == "FHOURS", `:=`(EFFORT = EFFORT / 13, EFFORT_UNIT_CODE = "FDAYS")] 

EF_PS_FDAYS_YEAR_FLEET = EF_PS_TIME_RAW[, .(FDAYS = sum(EFFORT, na.rm = TRUE)), keyby = .(YEAR, FLEET_CODE, FLEET)]

# Add days at sea for EUFRA in 2018-2021
EF_FDAYS_PS_YEAR_FRA_2018_2021 = data.table(YEAR = 2018:2021, FLEET_CODE = "EUFRA", FLEET = "EU,France", FDAYS = c(2885, 2501, 1805, 1834))

EF_FDAYS_PS_YEAR_FLEET = EF_PS_FDAYS_YEAR_FLEET[!(FLEET_CODE == "EUFRA" & YEAR == 2018)]

EF_FDAYS_PS_YEAR_FLEET = rbindlist(list(EF_FDAYS_PS_YEAR_FLEET, EF_FDAYS_PS_YEAR_FRA_2018_2021))

EF_PS_FDAYS_RAW_FLEETS_YEAR_BARPLOT = value_bar(EF_PS_FDAYS_YEAR_FLEET, value = "FDAYS", fill_by = "FLEET", scale = 1, y_axis_label = "Effort (Fishing days)", trim_labels = FALSE) + theme(legend.position = "bottom")

#plotly_build(EF_PS_FDAYS_RAW_FLEETS_YEAR_BARPLOT)

### SUPPORT VESSELS ####

EF_SP_RAW = as.data.table(read.xlsx("../inputs/data/3SU_DATA_collated.xlsx"))
names(EF_SP_RAW)[names(EF_SP_RAW) == "FLAG_CODE"] = "FLEET_CODE"
EF_SP_RAW[FLEET_CODE %in% c("ESP", "FRA", "ITA"), FLEET_CODE := paste0("EU", FLEET_CODE)]

EF_SP_DAYS_YEAR_FLEET = EF_SP_RAW[, .(DAYS = sum(DAYS_AT_SEA/24*13, na.rm = TRUE)), keyby = .(YEAR, FLEET_CODE)]

EF_SP_DAYS_RAW_FLEETS_YEAR_BARPLOT = value_bar(EF_SP_DAYS_YEAR_FLEET, value = "DAYS", fill_by = "FLEET", scale = 1, y_axis_label = "Effort (Days at sea)", trim_labels = FALSE) + theme(legend.position = "bottom")

## NUMBER OF SETS #### 

EF_SETS_PS_RAW = raw.EF(connection = DB_CONNECT_TO(server = "IOTCS09", database = "IOTDB"), 
                   years = 2013:last_year, 
                   fishery_group_codes = "PS",  
                   fleet_codes = c("JPN", "KOR"), 
                   effort_unit_codes = "SETS", 
)

EF_PS_LS_SETS_YEAR_FLEET1 = EF_SETS_PS_RAW[SCHOOL_TYPE_CODE == "LS", .(NUM_SETS_ON_FOB = sum(EFFORT)), keyby = .(YEAR, SCHOOL_TYPE_CODE, FLEET_CODE, FLEET)]

EF_PS_LS_SETS_YEAR_FLEET2 = fread("../inputs/data/ANNUAL_NUM_SETS_LS_COMPILED.csv")

EF_PS_LS_SETS_YEAR_FLEET = rbindlist(list(EF_PS_LS_SETS_YEAR_FLEET1, EF_PS_LS_SETS_YEAR_FLEET2))

EF_PS_LS_SETS_YEAR_FLEET_BARPLOT = value_bar(EF_PS_LS_SETS_YEAR_FLEET, value = "NUM_SETS_ON_FOB", fill_by = "FLEET", scale = 1, y_axis_label = "Number of sets on FOBs", trim_labels = FALSE) + theme(legend.position = "bottom")

## NUMBER OF FADS DEPLOYED ####
# Lacks Seychelles

FA_RAW = as.data.table(read.xlsx("../inputs/data/3FA_DATA_collated.xlsx"))
FA_RAW[NUM_FOBS == " " | NUM_FOBS == "_", NUM_FOBS := NA]
FA_RAW[, NUM_FOBS := as.numeric(NUM_FOBS)]
FA_RAW[, FISHERY_TYPE := trimws(FISHERY_TYPE)]
FA_RAW[FLAG_CODE %in% c("ESP", "FRA"), FLAG_CODE := paste0("EU", FLAG_CODE)]

PS_DEPLOYMENTS_YEAR_FLEET = FA_RAW[YEAR<2022 & ACTIVITY_TYPE_CODE == "DD" & FISHERY_TYPE %in% c("PSLS", "PS"), .(DEPLOYED = sum(NUM_FOBS, na.rm = TRUE)), keyby = .(YEAR, FLEET_CODE = FLAG_CODE)]

SP_DEPLOYMENTS_YEAR_FLEET = FA_RAW[YEAR<2022 & FISHERY_TYPE == "PSSP", .(DEPLOYED = sum(NUM_FOBS, na.rm = TRUE)), keyby = .(YEAR, FLEET_CODE = FLAG_CODE)]

ALL_DEPLOYMENTS_YEAR_FLEET = rbindlist(list(PS_DEPLOYMENTS_YEAR_FLEET, SP_DEPLOYMENTS_YEAR_FLEET))

ALL_DEPLOYMENTS_YEAR_FLEET[FLEET_CODE == "EUESP", FLEET := "EU (Spain)"]
ALL_DEPLOYMENTS_YEAR_FLEET[FLEET_CODE == "EUFRA", FLEET := "EU (France)"]
ALL_DEPLOYMENTS_YEAR_FLEET[FLEET_CODE == "JPN", FLEET := "Japan"]
ALL_DEPLOYMENTS_YEAR_FLEET[FLEET_CODE == "KOR", FLEET := "Republic of Korea"]
ALL_DEPLOYMENTS_YEAR_FLEET[FLEET_CODE == "MUS", FLEET := "Mauritius"]
ALL_DEPLOYMENTS_YEAR_FLEET[FLEET_CODE == "SYC", FLEET := "Seychelles"]

EF_FAD_DEPLOYMENTS_FLEETS_YEAR_BARPLOT = value_bar(ALL_DEPLOYMENTS_YEAR_FLEET[YEAR>2014], value = "DEPLOYED", fill_by = "FLEET", scale = 1, y_axis_label = "Number of FADs deployed", trim_labels = FALSE) + theme(legend.position = "bottom")


