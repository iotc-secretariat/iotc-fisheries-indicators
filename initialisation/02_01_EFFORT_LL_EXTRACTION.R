l_info("Initializing longline effort data extraction...", "TROPICAL TUNAS")

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
