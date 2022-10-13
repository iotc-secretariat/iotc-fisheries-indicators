print("Initializing NC data...")

# Data extraction
if (!ES_SPECIES_CODE %in% c("BET", "SKJ", "YFT")) NC_BSE = NC_est(species_codes = ES_SPECIES_CODE, years = START_YEAR:END_YEAR) else NC_BSE = NC_raised(species_codes = ES_SPECIES_CODE, years = START_YEAR:END_YEAR)

# Species information extraction
ES_SPECIES_INFORMATION = unique(NC_BSE[, .(SPECIES_CODE, SPECIES, SPECIES_SCIENTIFIC)])

FIRST_YEAR   = first_year(NC_BSE$YEAR)
LAST_YEAR    = last_year(NC_BSE$YEAR)
LAST_5_YEARS = last_5_years(NC_BSE$YEAR)

NC_LAST_YEAR    = round(NC_BSE[YEAR == LAST_YEAR, sum(CATCH)])
NC_LAST_5_YEARS = round(NC_BSE[YEAR %in% LAST_5_YEARS, sum(CATCH/5)])

# REPORTED vs. ESTIMATED IN LAST YEAR
NC_REPORTED_LAST_YEAR  = data_quality(species_code = ES_SPECIES_CODE, year_from = END_YEAR, year_to = END_YEAR)[NC == 0, sum(CATCH)]
NC_ESTIMATED_LAST_YEAR = NC_LAST_YEAR - NC_REPORTED_LAST_YEAR
PERCENTAGE_ESTIMATED_LAST_YEAR = round(NC_ESTIMATED_LAST_YEAR / (NC_ESTIMATED_LAST_YEAR + NC_REPORTED_LAST_YEAR) * 100, 1)

# Datasets for charts
NC_YEARS_FISHERY_GROUP = catches_by_year_and_fishery_group(NC_BSE)
NC_YEARS_FISHERY       = catches_by_year_and_fishery(NC_BSE)

# Recent years
NC_FISHERY_GROUP_RECENT_MEAN = NC_BSE[YEAR %in% LAST_5_YEARS, .(CATCH = round(sum(CATCH)/5)), keyby = .(FISHERY_GROUP_CODE, FISHERY_GROUP)][order(CATCH, decreasing = TRUE)]
NC_FISHERY_GROUP_RECENT_MEAN[, TOTAL := sum(CATCH)]
NC_FISHERY_GROUP_RECENT_MEAN[, PERCENT_CATCH := round(CATCH/TOTAL*100, 1)]

NC_FISHERY_RECENT_MEAN      = NC_BSE[YEAR %in% LAST_5_YEARS, .(CATCH = round(sum(CATCH)/5)), keyby = .(FISHERY_CODE, FISHERY)][order(CATCH, decreasing = TRUE)]
NC_FISHERY_RECENT_MEAN[, TOTAL := sum(CATCH)]
NC_FISHERY_RECENT_MEAN[, PERCENT_CATCH := round(CATCH/TOTAL*100, 1)]

NC_FLEET_RECENT_MEAN      = NC_BSE[YEAR %in% LAST_5_YEARS, .(CATCH = round(sum(CATCH)/5)), keyby = .(FLEET_CODE, FLEET)][order(CATCH, decreasing = TRUE)]
NC_FLEET_RECENT_MEAN[, TOTAL := sum(CATCH)]
NC_FLEET_RECENT_MEAN[, PERCENT_CATCH := round(CATCH/TOTAL*100, 1)]

print("NC data initialized!")