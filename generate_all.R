## Libraries
pacman::p_load("knitr", "rmarkdown", "bookdown")

# Data parameters
last_year     = 2021
previous_year = last_year - 4
full_period   = 1950:last_year
recent_period = previous_year:last_year

# Source the R codes
setwd("initialization")
source("01_CORE.R")
setwd("..")

# PPTX
render("rmd/FAD_INDICATORS_DASHBOARD_PPTX.Rmd",
        output_format = powerpoint_presentation(reference_doc = "../templates/ppt_template.potx", slide_level = 2),
        output_dir    = "outputs/"
)

