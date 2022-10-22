l_info("Initialisation of catch data extraction...", WPTT_SPECIES)

# STOCK ASSESSMENT AREAS MAPPING ####
SA_AREAS_MAPPING = iotc.core.gis.cwp.IO::grid_intersections_by_source_grid_type(target_grid_codes = SA_AREAS_CONFIG$IOTC_CODE, source_grid_type_code = grid_5x5)

# NC DATA ####
NC_RAISED = NC_raised(species_codes = WPTT_SPECIES)

# CA DATA ####

## RAW CATCHES ####
CA_RAW = CA_raw(species_codes = WPTT_SPECIES)

CA_RAW_SA = merge(CA_RAW, SA_AREAS_MAPPING,
                  by.x = "FISHING_GROUND_CODE",
                  by.y = "SOURCE_FISHING_GROUND_CODE",
                  all.x = TRUE,
                  allow.cartesian = TRUE)

CA_RAW_SA[!is.na(PROPORTION), CATCH := CATCH * PROPORTION]

CA_RAW_SA = CA_RAW_SA[, .(CATCH = sum(CATCH, na.rm = TRUE)),
                      keyby = .(YEAR, 
                                FLEET_CODE, FLEET, 
                                GEAR_CODE, GEAR,
                                FISHERY_TYPE_CODE, FISHERY_TYPE,
                                FISHERY_GROUP_CODE, FISHERY_GROUP,
                                FISHERY_CODE, FISHERY,
                                AREA_CODE = TARGET_FISHING_GROUND_CODE)]

CA_RAW_SA$AREA_CODE = factor(
  CA_RAW_SA$AREA_CODE,
  levels = SA_AREAS_CONFIG$IOTC_CODE,
  labels = SA_AREAS_CONFIG$AREA_NAME,
  ordered = TRUE
)

## RAISED CATCHES ####
CA_RAISED = CA_raised(species_codes = WPTT_SPECIES)

# Add stock assessment areas
CA_RAISED_SA = merge(CA_RAISED, SA_AREAS_MAPPING,
                     by.x = "FISHING_GROUND_CODE",
                     by.y = "SOURCE_FISHING_GROUND_CODE",
                     all.x = TRUE,
                     allow.cartesian = TRUE)

CA_RAISED_SA[!is.na(PROPORTION), CATCH := CATCH * PROPORTION]

CA_RAISED_SA = CA_RAISED_SA[, .(CATCH = sum(CATCH, na.rm = TRUE)),
                            keyby = .(YEAR, 
                                      FLEET_CODE, FLEET, 
                                      GEAR_CODE, GEAR,
                                      FISHERY_TYPE_CODE, FISHERY_TYPE,
                                      FISHERY_GROUP_CODE, FISHERY_GROUP,
                                      FISHERY_CODE, FISHERY,
                                      AREA_CODE = TARGET_FISHING_GROUND_CODE)]

NC_NO_SA_AREA = CA_RAISED_SA[is.na(AREA_CODE)]

CA_RAISED_SA$AREA_CODE = factor(
  CA_RAISED_SA$AREA_CODE,
  levels = SA_AREAS_CONFIG$IOTC_CODE,
  labels = SA_AREAS_CONFIG$AREA_NAME,
  ordered = TRUE
)

l_info("Catch data extraction initialised!", WPTT_SPECIES)
