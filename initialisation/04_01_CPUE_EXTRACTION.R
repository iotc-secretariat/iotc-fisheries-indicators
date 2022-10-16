# CPUE ####

## LONGLINE FISHERIES ####

### BIGEYE TUNA

CA_LL_YEAR = CA_raw(fleet_codes = c("SYC", "TWN", "CHN", "JPN"), catch_unit_codes = "MT", species_code = c("BET"), fishery_codes = c("LLD", "LLF"))[, .(CATCH = round(sum(CATCH))), keyby = .(FLEET_CODE, YEAR)]

EF_LL_YEAR = CE_raw(fleet_codes = c("SYC", "TWN", "CHN"), effort_unit_codes = "HOOKS")[, .(HOOKS = sum(EFFORT)), keyby = .(FLEET_CODE, FLEET, YEAR)]

CATCH_EFFORT_LL_YEAR = merge(EF_LL_YEAR, CA_LL_YEAR, by = c("YEAR", "FLEET_CODE"), all = TRUE)
CATCH_EFFORT_LL_YEAR[, HOOKS_PER_MT := round(HOOKS/CATCH, 2)]

CATCH_LL_YEAR = merge(NC_LL_YEAR, CA_LL_YEAR, by = c("YEAR", "FLEET_CODE"), all.x = TRUE, suffixes = c("_NC", "_CA"))
CATCH_LL_YEAR[, RF := CATCH_NC/CATCH_CA]


