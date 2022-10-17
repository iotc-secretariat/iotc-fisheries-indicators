# Clears the environment
rm(list = ls())

# Prevents formatting of numbers using scientific notation (e.g., in heatmap legends)
options(scipen = 99999)

# Includes defaults and helper functions
source("90_defaults.R")
source("91_libs_external.R")
source("92_libs_IOTC.R")
source("93_functions.R")

# source the scripts
source("01_01_CAPACITY_EXTRACTION.R")
source("01_02_CAPACITY_CONSOLIDATION.R")