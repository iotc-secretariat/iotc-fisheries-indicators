# Clears the environment
#rm(list = ls())

# Prevents formatting of numbers using scientific notation (e.g., in heatmap legends)
options(scipen = 99999)

# Includes defaults and helper functions
source("90_defaults.R")
source("91_libs_external.R")
source("92_libs_IOTC.R")
source("93_functions.R")

# source configuration scripts
source(paste0("00_constants_", WPTT_SPECIES, ".R"))
source("00_SA_AREAS.R")

source("01_01_CAPACITY_EXTRACTION.R")
source("01_02_CAPACITY_CONSOLIDATION.R")
source("01_03_CAPACITY_INDICATOR.R")

source("02_01_EFFORT_EXTRACTION.R")
source("02_02_EFFORT_INDICATOR.R")

source("03_01_CATCH_EXTRACTION.R")
source("03_02_CATCH_INDICATOR.R")

#source("04_01_CPUE_EXTRACTION.R")

source("05_01_AW_EXTRACTION.R")
source("05_02_AW_INDICATOR.R")
