# Source the R codes
setwd("initialization")
source("00_CORE.R")
setwd("..")

# PPTX
render("rmd/00_PPTX.Rmd",
        output_format = powerpoint_presentation(reference_doc = "../templates/ppt_template.potx", slide_level = 2),
        output_dir    = "outputs/"
)

