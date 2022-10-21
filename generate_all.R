## Libraries
pacman::p_load("knitr", "rmarkdown", "bookdown")

# Data parameters
WPTT_SPECIES  = "BET"
LAST_YEAR     = 2021
PREVIOUS_YEAR = LAST_YEAR - 4
FULL_PERIOD   = 1950:LAST_YEAR
RECENT_PERIOD = PREVIOUS_YEAR:LAST_YEAR

# Source the R codes
setwd("initialization")
source("00_CORE.R")
setwd("..")



# PPTX
# render("rmd/FAD_INDICATORS_DASHBOARD_PPTX.Rmd",
#         output_format = powerpoint_presentation(reference_doc = "../templates/ppt_template.potx", slide_level = 2),
#         output_dir    = "outputs/"
# )

