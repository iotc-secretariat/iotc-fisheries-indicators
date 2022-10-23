l_info("Initializing average weight data extraction...", WPTT_SPECIES)

### AVERAGE WEIGHTS (from raised CE)

### IMPORTANT!!! The approach below only works if the CEForSF table in the WP_CE_RAISED database is updated manually
### Considering the work recently done on the production of SA / raised catches summaries straight from the Access DBs
### and how the CA_RAISED table on WP_CE_RAISED is now automatically updated during the process, we need to ensure the
### same is done for the CEForSF table as well

#### Reads the metadata on the availability of SF for all the grids in the raised catches dataset
SF_STRATA_Q = paste0("
SELECT 
	Year AS YEAR,
	LTRIM(RTRIM(Fleet)) AS FLEET_CODE,
	LTRIM(RTRIM(Gear)) AS GEAR_CODE,
	G.LAggCESF AS FISHERY_GROUP,
	LTRIM(RTRIM(SchoolType)) AS SCHOOL_TYPE_CODE,
	LTRIM(RTRIM(Species)) AS SPECIES_CODE,
	CASE WHEN Substitution = 0 THEN 'ORIGINAL' ELSE 'SUBSTITUTION' END AS STATUS,
	SUM(CEmtFish) AS MT
FROM 
  CEForSF C
LEFT 
  JOIN [IOTDB].[dbo].cdeGears G
ON 
  C.Gear = G.ACode COLLATE SQL_Latin1_General_CP1_CI_AI
WHERE ", 
                     ifelse(is.na(WPTT_SPECIES), 
                            "1 = 1", 
                            paste0("Species = '", WPTT_SPECIES, "'")
                     ), " 
GROUP BY 
	Year, 
	LTRIM(RTRIM(Fleet)),
	LTRIM(RTRIM(Gear)), 
	G.LAggCESF,
	LTRIM(RTRIM(SchoolType)), 
	LTRIM(RTRIM(Species)),
	CASE WHEN Substitution = 0 THEN 'ORIGINAL' ELSE 'SUBSTITUTION' END
ORDER BY
	6, 1, 2, 3, 4, 5")

SF_STRATA = as.data.table(query(connection = DB_WP_CE_RAISED(), query = SF_STRATA_Q))

# Assigns standard fishery group codes to the records (as these are not originally available)
SF_STRATA[, FISHERY_GROUP_CODE := "OT"]
SF_STRATA[FISHERY_GROUP == "Purse Seine", FISHERY_GROUP_CODE := "PS"]
SF_STRATA[FISHERY_GROUP == "Longline",    FISHERY_GROUP_CODE := "LL"]
SF_STRATA[FISHERY_GROUP == "Baitboat",    FISHERY_GROUP_CODE := "BB"]
SF_STRATA[FISHERY_GROUP == "Gillnet",     FISHERY_GROUP_CODE := "GN"]
SF_STRATA[FISHERY_GROUP == "Handline",    FISHERY_GROUP_CODE := "HL"]
SF_STRATA[FISHERY_GROUP == "Trolling",    FISHERY_GROUP_CODE := "TL"]

SF_STRATA = 
  update_fisheries_for_SF(
    add_fishery_codes(
      SF_STRATA
    )
  )

## Aggregate strata
SF_STRATA = SF_STRATA[, .(MT = sum(MT)), keyby = .(YEAR, STATUS, AW_FISHERY)]

## Extract catch data
SPECIES_CA_RAISED = CA.raised(species_codes = WPTT_SPECIES)

## AVERAGE WEIGHTS IN SAMPLED FISHERIES FOR THE WHOLE INDIAN OCEAN ####
SF = SPECIES_CA_RAISED[, .(YEAR, FISHERY_CODE, FLEET_CODE, CATCH, CATCH_IN_NUMBERS)]

SFA = SF[, .(AVG_WEIGHT = sum(CATCH * 1000) / sum(CATCH_IN_NUMBERS)), keyby = .(YEAR)]
SFA[, AW_FISHERY := "ALL"]
SFA = update_fisheries_for_SF(SFA)

SFA = merge(SFA, SF_STRATA[STATUS == "ORIGINAL", .(MT = sum(MT)), keyby = .(YEAR)], by = c("YEAR"), all.x = TRUE)
SFA[, LOW_COVERAGE := ( is.na(MT) | MT < 50 )]
SFA[, MT := NULL]

# The 'update_fisheries_for_SF' function is defined in the species-specific "constants" script
SFF = update_fisheries_for_SF(SF)

# Aggregate by year
SFF = SFF[, .(CATCH = sum(CATCH), CATCH_IN_NUMBERS = sum(CATCH_IN_NUMBERS)), keyby = .(YEAR, AW_FISHERY)]

# Add strata with annual sample weight
SFF2 = merge(SFF, SF_STRATA[STATUS == "ORIGINAL"], by = c("YEAR", "AW_FISHERY"), all.x = TRUE)

# Computing average weight
SFF2[, AVG_WEIGHT := CATCH * 1000 / CATCH_IN_NUMBERS]

# Filters out all records for which SF was available, but only from strata covering less than 50 t of catches in total (the threshold is arbitrary)  
SFF2[, LOW_COVERAGE := ( is.na(MT) | MT < 50 )]

SFF2 = SFF2[, .(YEAR, AW_FISHERY, AVG_WEIGHT, LOW_COVERAGE)]

SFF2 = rbind(SFF2, SFA)

# Number of records
SFF_FISHERIES_WITH_DATA = SFF2[LOW_COVERAGE == FALSE, .(NUM_RECORDS = .N), keyby = .(AW_FISHERY)]

## AVERAGE WEIGHTS BY ASSESSMENT AREA ####

# Add stock assessment areas
SA_AREAS_MAPPING = iotc.core.gis.cwp.IO::grid_intersections_by_source_grid_type(target_grid_codes = SA_AREAS_CONFIG$IOTC_CODE, source_grid_type_code = grid_5x5)

SPECIES_CA_RAISED_SA = merge(SPECIES_CA_RAISED, SA_AREAS_MAPPING,
                             by.x = "FISHING_GROUND_CODE",
                             by.y = "SOURCE_FISHING_GROUND_CODE",
                             all.x = TRUE,
                             allow.cartesian = TRUE)

SPECIES_CA_RAISED_SA[!is.na(PROPORTION), `:=` (CATCH = CATCH * PROPORTION, CATCH_IN_NUMBERS = CATCH_IN_NUMBERS * PROPORTION)]

SPECIES_CA_RAISED_SA_YEAR = SPECIES_CA_RAISED_SA[, .(CATCH = sum(CATCH, na.rm = TRUE), CATCH_IN_NUMBERS = sum(CATCH_IN_NUMBERS, ma.rm = TRUE)),
                                                 keyby = .(YEAR, 
                                                           FLEET_CODE, FLEET, 
                                                           GEAR_CODE, GEAR,
                                                           FISHERY_TYPE_CODE, FISHERY_TYPE,
                                                           FISHERY_GROUP_CODE, FISHERY_GROUP,
                                                           FISHERY_CODE, FISHERY,
                                                           AREA_CODE = TARGET_FISHING_GROUND_CODE)]

SPECIES_CA_RAISED_SA_YEAR[, AREA_CODE := factor(AREA_CODE, levels = SA_AREAS_CONFIG$IOTC_CODE, labels = SA_AREAS_CONFIG$AREA_NAME, ordered = TRUE)]

# Add SF fisheries and filter on them
SFF3 = update_fisheries_for_SF(SPECIES_CA_RAISED_SA_YEAR)[!is.na(AW_FISHERY)]

# Compute annual average weight
SFF3[, AVG_WEIGHT := round(CATCH * 1000 / CATCH_IN_NUMBERS, 2)]
SFF3[, AVG_WEIGHT_STD := AVG_WEIGHT/mean(AVG_WEIGHT), by = .(AW_FISHERY, AREA_CODE)]

l_info("Average weight data extracted...", WPTT_SPECIES)
