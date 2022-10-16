# Install/load pacman
if(!require(pacman)){
  install.packages("pacman")
  suppressPackageStartupMessages(library(pacman,quietly = TRUE))
}

# Install/load libraries required for analysis
pacman::p_load(
  "tidyverse",
  "flextable",
  "scales",
  "openxlsx",
  "ggpubr",
  "gridExtra",
  "rmarkdown",
  "knitr",
  "bookdown",
  "officer",
  "ggsci",
  "patchwork",
  "magick", 
  "plotly",
  "ggh4x"
)

# Set theme_bw for all plots
theme_set(theme_bw())
