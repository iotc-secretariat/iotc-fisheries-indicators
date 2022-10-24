# Clears the environment
#rm(list = ls())

# Prevents formatting of numbers using scientific notation (e.g., in heatmap legends)
options(scipen = 99999)

# Includes defaults and helper functions
source("90_defaults.R")
source("91_libs_external.R")
source("92_libs_IOTC.R")
source("93_functions.R")

## Capacity
source("01_01_CAPACITY_EXTRACTION.R")
source("01_02_CAPACITY_CONSOLIDATION.R")
source("01_03_CAPACITY_INDICATORS.R")

## Effort
source("02_01_EFFORT_EXTRACTION.R")
source("02_02_EFFORT_INDICATORS.R")

## Loop for the 3 species
for(WPTT_SPECIES in c("BET", "SKJ", "YFT")) {

# source configuration scripts
source(paste0("00_constants_", WPTT_SPECIES, ".R"))
source("00_SA_AREAS.R")

  if(ANALYZE_SF_LL) { # This is configured in the species-specific constants
    l_info("Computing LL effort by assessment area...", WPTT_SPECIES)
    source("02_03_EFFORT_LL_INDICATORS_SPECIES_AREAS.R")
    l_info("Finished analyzing SF for LL fisheries!", WPTT_SPECIES)
  }

# Catch
source("03_01_CATCH_EXTRACTION_SPECIES.R")
source("03_02_CATCH_INDICATORS_SPECIES.R")

# CPUE
source("04_01_CPUE_EXTRACTION_SPECIES.R")
source("04_02_CPUE_INDICATORS_SPECIES.R")

# Average weight
source("05_01_AW_EXTRACTION_SPECIES.R")
source("05_02_AW_INDICATORS_SPECIES.R")
  if(ANALYZE_SF_LL) { # This is configured in the species-specific constants
  source("05_03_AW_ll_INDICATORS_SPECIES_AREAS.R")
  }
}