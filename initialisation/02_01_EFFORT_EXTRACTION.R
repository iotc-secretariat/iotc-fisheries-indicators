l_info("Initializing effort data extraction...", "TROPICAL TUNAS")

## LONGLINE ####

## Select main fleets with consistent fishing effort
LL_MAIN_FLEETS = c("CHN", "TWN", "JPN", "KOR")

## Identify strata with catch of tropical tunas
CE_RAW_LL_HOOKS_TROP = CE_raw(species_codes = c("BET", "YFT"), fishery_group_codes = "LL", effort_unit_codes = "HOOKS", fleet_codes = LL_MAIN_FLEETS)

CE_RAW_LL_HOOKS_TROP_STRATA = unique(CE_RAW_LL_HOOKS_TROP[, .(YEAR, MONTH_START, MONTH_END, FISHING_GROUND_CODE, FLEET_CODE, GEAR_CODE)])

## Extract effort in strata with some catch of tropical tunas
## To avoid fishing grounds of sworfish and temperate tunas
EF_RAW_LL_HOOKS_TROP = merge(EF_raw(fishery_group_codes = "LL", effort_unit_codes = "HOOKS", fleet_codes = LL_MAIN_FLEETS), CE_RAW_LL_HOOKS_TROP_STRATA, by = c("YEAR", "MONTH_START", "MONTH_END", "FISHING_GROUND_CODE", "FLEET_CODE", "GEAR_CODE"))

# Add type of grid
EF_LL_HOOKS_TROP = merge(EF_RAW_LL_HOOKS_TROP, filter_grids()[, .(FISHING_GROUND_CODE, FISHING_GROUND_TYPE)], by = "FISHING_GROUND_CODE", all.x = TRUE)

EF_LL_HOOKS_TROP[is.na(FISHING_GROUND_TYPE) , FISHING_GROUND_TYPE := "IRREGULAR"]

# Convert to 5x5 grid
EF_LL_HOOKS_TROP[FISHING_GROUND_TYPE != "IRREGULAR", CWP55 := convert_CWP_grid(FISHING_GROUND_CODE, target_grid_type_code = grid_5x5), by = .(FISHING_GROUND_CODE)]

# Add stock assessment areas
SA_AREAS_MAPPING = iotc.core.gis.cwp.IO::grid_intersections_by_source_grid_type(target_grid_codes = SA_AREAS_CONFIG$IOTC_CODE, source_grid_type_code = grid_5x5)

EF_LL_HOOKS_TROP_SA = merge(EF_LL_HOOKS_TROP, SA_AREAS_MAPPING,
                            by.x = "FISHING_GROUND_CODE",
                            by.y = "SOURCE_FISHING_GROUND_CODE",
                            all.x = TRUE,
                            allow.cartesian = TRUE)

EF_LL_HOOKS_TROP_SA[!is.na(PROPORTION), EFFORT_WEIGHTED_AREA := EFFORT * PROPORTION]

EF_LL_HOOKS_TROP_SA_YEAR = EF_LL_HOOKS_TROP_SA[, .(EFFORT = sum(EFFORT_WEIGHTED_AREA, na.rm = TRUE)),
                                               keyby = .(YEAR, 
                                                         FLEET_CODE, FLEET, 
                                                         GEAR_CODE, GEAR,
                                                         FISHERY_TYPE_CODE, FISHERY_TYPE,
                                                         FISHERY_GROUP_CODE, FISHERY_GROUP,
                                                         FISHERY_CODE, FISHERY,
                                                         AREA_CODE = TARGET_FISHING_GROUND_CODE)]


EF_LL_HOOKS_TROP_SA_YEAR[, AREA_CODE := factor(AREA_CODE, levels = SA_AREAS_CONFIG$IOTC_CODE, labels = SA_AREAS_CONFIG$AREA_NAME, ordered = TRUE)]

# Compute annual effort by fleet and area
EF_LL_HOOKS_YEAR_FLEET_AREA = EF_LL_HOOKS_TROP_SA_YEAR[, .(UNIT = "HOOKS", EFFORT = sum(EFFORT, na.rm = TRUE)), keyby = .(YEAR, FISHERY_CODE, FISHERY, FLEET_CODE, FLEET, AREA_CODE)]

#  GILLNET ####

## Sri Lanka
EF_RAW_GN_TRIPS = rbindlist(list(
  EF_raw(fleet_codes = "LKA", fishery_group_codes = "GN", effort_unit_codes = "TRIPS", years = 2014:2021), 
  EF_raw(fleet_codes = "IRN", fishery_group_codes = "GN", effort_unit_codes = "TRIPS", years = 2007:2021)
))

## Attempt to raise the fishing effort
NC_GN_TROP = NC_est(fleet_codes = c("LKA", "IRN"), species_codes = c("SKJ", "YFT", "BET"), fishery_group_codes = "GN")[, .(CATCH = round(sum(CATCH))), keyby = .(YEAR, FLEET_CODE, FLEET)]

CA_GN_TROP = rbindlist(list(
  CA_raw(fishery_group_codes = "GN", fleet_codes = "LKA", catch_unit_codes = "MT", species_codes = c("YFT", "BET", "SKJ")), 
  CA_raw(fishery_group_codes = "GN", fleet_codes = "IRN", catch_unit_codes = "MT", species_codes = c("YFT", "BET", "SKJ"))))  [, .(CATCH = round(sum(CATCH))), keyby = .(YEAR, FLEET_CODE, FLEET)]

CATCH_GN_TROP = merge(NC_GN_TROP, CA_GN_TROP, by = c("YEAR", "FLEET_CODE", "FLEET"), suffixes = c("_NC", "_CA"))
CATCH_GN_TROP[, RF := CATCH_NC/CATCH_CA, by = .(YEAR, FLEET_CODE)]

EFFORT_GN_TROP_WITH_RAISING_FACTORS = merge(EF_RAW_GN_TRIPS, CATCH_GN_TROP[, .(YEAR, FLEET_CODE, FLEET, RF)], by = c("YEAR", "FLEET_CODE", "FLEET"))
EFFORT_GN_TROP_WITH_RAISING_FACTORS[, EFFORT_RAISED := EFFORT*RF]

# Compute annual effort by fleet
EF_GN_TRIPS_YEAR_FLEET = EFFORT_GN_TROP_WITH_RAISING_FACTORS[, .(UNIT = "TRIPS", EFFORT = sum(EFFORT_RAISED, na.rm = TRUE)), keyby = .(YEAR, FISHERY_CODE, FISHERY, FLEET_CODE, FLEET)]

EF_GN_TRIPS_YEAR_LKA = EF_GN_TRIPS_YEAR_FLEET[FLEET_CODE == "LKA"]    #Inconsistent effort for IRN in 2018-2020

## BAITBOAT ####

# Focus on Maldives
EF_BB_RAW_MDV = EF_raw(fleet_codes = "MDV", fishery_codes = "BB")

# Assuming 1 day-trips
EF_BB_RAW_MDV[EFFORT_UNIT_CODE == "TRIPS", EFFORT_UNIT_CODE := "FDAYS"]

# Add type of grid
EF_BB_MDV = merge(EF_BB_RAW_MDV, filter_grids()[, .(FISHING_GROUND_CODE, FISHING_GROUND_TYPE)], by = "FISHING_GROUND_CODE", all.x = TRUE)
EF_BB_MDV[is.na(FISHING_GROUND_TYPE) , FISHING_GROUND_TYPE := "IRREGULAR"]

# Convert to 5x5 grid
EF_BB_MDV[FISHING_GROUND_TYPE != "IRREGULAR", CWP55 := convert_CWP_grid(FISHING_GROUND_CODE, target_grid_type_code = grid_5x5), by = .(FISHING_GROUND_CODE)]

# Compute annual effort by fleet
EF_BB_FDAYS_YEAR_MDV = EF_BB_MDV[, .(UNIT = "FDAYS", EFFORT = sum(EFFORT, na.rm = TRUE)), keyby = .(YEAR, FISHERY_CODE, FISHERY, FLEET_CODE, FLEET)]

## Attempt to raise the fishing effort
## Geo-referenced catches are raised to the nominal catches
# NC_BB_MDV_TROP = NC_est(fleet_codes = c("MDV"), species_codes = c("SKJ", "YFT", "BET"), fishery_group_codes = "BB")[, .(CATCH = round(sum(CATCH))), keyby = .(YEAR, FLEET_CODE, FLEET)]
# 
# CA_BB_MDV_TROP = CA_raw(fishery_group_codes = "BB", fleet_codes = "MDV", catch_unit_codes = "MT", species_codes = c("YFT", "BET", "SKJ"))[, .(CATCH = round(sum(CATCH))), keyby = .(YEAR, FLEET_CODE, FLEET)]
# 
# CATCH_BB_MDV_TROP = merge(NC_BB_MDV_TROP, CA_BB_MDV_TROP, by = c("YEAR", "FLEET_CODE", "FLEET"), suffixes = c("_NC", "_CA"))
# CATCH_BB_MDV_TROP[, RF := CATCH_NC/CATCH_CA, by = .(YEAR, FLEET_CODE)]

# PURSE SEINE ####

# Selected fleets with consistent effort reported
EF_PS_TIME_RAW = raw.EF(fishery_group_codes = "PS")[FISHERY_TYPE_CODE == "IND" & FLEET_CODE %in% c("EUESP", "EUFRA", "EUMYT", "SYC")][EFFORT_UNIT_CODE %in% c("FHOURS", "FDAYS")]
EF_PS_TIME_RAW[EFFORT_UNIT_CODE == "FHOURS", `:=`(EFFORT = EFFORT / 13, EFFORT_UNIT_CODE = "FDAYS")] 

# Compute annual effort by fleet
EF_PS_FDAYS_RAW_YEAR_FLEET = EF_PS_TIME_RAW[, .(UNIT = "FDAYS", FISHERY_CODE = "PSOT", FISHERY = "Purse seine", EFFORT = sum(EFFORT, na.rm = TRUE)), keyby = .(YEAR, FLEET_CODE, FLEET)]

# Add days at sea for EUFRA in 2018-2021
EF_PS_FDAYS_YEAR_FRA_2018_2021 = data.table(YEAR = 2018:2021, FISHERY_CODE = "PSOT", FISHERY = "Purse seine", FLEET_CODE = "EUFRA", FLEET = "EU (France)", UNIT = "FDAYS", EFFORT = c(2885, 2501, 1805, 1834))

EF_PS_FDAYS_YEAR_FLEET = EF_PS_FDAYS_RAW_YEAR_FLEET[!(FLEET_CODE == "EUFRA" & YEAR %in% 2018:2021)]

EF_PS_FDAYS_YEAR_FLEET = rbindlist(list(EF_PS_FDAYS_YEAR_FLEET, EF_PS_FDAYS_YEAR_FRA_2018_2021), use.names = TRUE)

EF_PS_FDAYS_YEAR_MAIN = EF_PS_FDAYS_YEAR_FLEET[, .(FLEET_CODE = "EUAS", FLEET = "Main fleets", EFFORT = sum(EFFORT)), keyby = .(YEAR, UNIT, FISHERY_CODE, FISHERY)]

# Combine effort data sets ####

EF_FISHERIES_YEAR_FLEET = 
rbindlist(list(EF_GN_TRIPS_YEAR_LKA, 
               EF_BB_FDAYS_YEAR_MDV, 
               EF_PS_FDAYS_YEAR_MAIN), use.names = TRUE)
               
l_info("Effort data extracted", WPTT_SPECIES)




