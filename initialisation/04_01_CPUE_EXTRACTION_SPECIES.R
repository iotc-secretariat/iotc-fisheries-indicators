# CPUE ####

# Add assessment areas
SA_AREAS_MAPPING = iotc.core.gis.cwp.IO::grid_intersections_by_source_grid_type(target_grid_codes = SA_AREAS_CONFIG$IOTC_CODE, source_grid_type_code = grid_5x5)

## RAW CATCH AND EFFORT ####
CE_RAW = CE_raw(species_codes = WPTT_SPECIES, catch_unit_codes = "No")

CE_RAW_SA = merge(CE_RAW, SA_AREAS_MAPPING,
                  by.x = "FISHING_GROUND_CODE",
                  by.y = "SOURCE_FISHING_GROUND_CODE",
                  all.x = TRUE,
                  allow.cartesian = TRUE)

CE_RAW_SA[!is.na(PROPORTION), CATCH := CATCH * PROPORTION]
CE_RAW_SA[, CPUE := CATCH/(EFFORT/1000)]

CE_RAW_SA_YEAR = CE_RAW_SA[, .(CATCH = sum(CATCH, na.rm = TRUE), EFFORT = sum(EFFORT, na.rm = TRUE), CPUE_TOT = sum(CATCH, na.rm = TRUE)/sum(EFFORT/1000, na.rm = TRUE), CPUE_MEAN = mean(CPUE, na.rm = TRUE), CPUE_SD = sd(CPUE, na.rm = TRUE)),
                      keyby = .(YEAR, 
                                FLEET_CODE, FLEET, 
                                GEAR_CODE, GEAR,
                                FISHERY_TYPE_CODE, FISHERY_TYPE,
                                FISHERY_GROUP_CODE, FISHERY_GROUP,
                                FISHERY_CODE, FISHERY,
                                AREA_CODE = TARGET_FISHING_GROUND_CODE)]

CE_RAW_SA_YEAR$AREA_CODE = factor(
  CE_RAW_SA_YEAR$AREA_CODE,
  levels = SA_AREAS_CONFIG$IOTC_CODE,
  labels = SA_AREAS_CONFIG$AREA_NAME,
  ordered = TRUE
)
