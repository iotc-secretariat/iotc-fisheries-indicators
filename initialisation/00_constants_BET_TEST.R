CA_REFERENCE_VALUE = 5000 # Reference value for average annual catches by grid (for CA piemaps) 

#Triggers the execution of the scripts that analyze the SF for TWN and JPN LL
ANALYZE_SF_LL = TRUE

#Species-dependent size class threshold to tell small / large fish apart
LARGE_FISH_SIZE_THRESHOLD = 75 #This is YFT-specific: to be changed to a more appropriate value for BET

# Max fork length to be used to plot L-W relationships 
MAX_FISH_SIZE = 250

FL_RW_RELATIONSHIPS = data.table(
  A = c(0.00002217, 0.0000159207),
  B = c(3.01211000, 3.0415414023),
  M = c(1, 1.13),
  min_FL = c(29.5, 70),
  max_FL = c(174, 187),
  gears = c("Purse seine, pole-and-line, gillnet", "Longline, line, other gears"),
  gear_codes = c("PSPLGN", "LLOT"),
  source = c("Length-weight relationships for tropical tunas caught with purse seine in the Indian Ocean: Update and lessons learned (Chassot, E. et al in
IOTC-2016-WPDSC12-INF05)",
             "Multilateral catch monitoring Benoa (2002-04), ICCAT Field Manual (Appendix 4: Population parameters for key ICCAT species. Product Conversion Factors)"),
  num_samples = c(2156, 12047)
)

TWN_LL_SF_YEAR_CUTOFF = 2002 # The year when TWN LL average weights from CE became < than those from SF

# BET SA areas
SA_AREAS_CONFIG = data.table(
  IOTC_CODE  = c("IRBET01", "IRBET02", "IRBET03", "IRBET00"),
  AREA_CODE  = c("A1", "A2", "A3", "A0"),
  NAME_SHORT = c("West",
                 "East",
                 "South",
                 "All other")
)

SA_AREAS_CONFIG[, AREA_NAME := paste0(AREA_CODE, " - ", NAME_SHORT)]

# Color constants for current BET SA areas
BET_SA_AREA_COLOR_1 = ALL_FI_COLORS[FISHERY_CODE == "GN"]
BET_SA_AREA_COLOR_2 = ALL_FI_COLORS[FISHERY_CODE == "LLF"]
BET_SA_AREA_COLOR_3 = ALL_FI_COLORS[FISHERY_CODE == "PSLS"]
BET_SA_AREA_COLOR_0 = ALL_FI_COLORS[FISHERY_CODE == "BB"]

SA_AREA_COLORS = rbind(BET_SA_AREA_COLOR_1,
                       BET_SA_AREA_COLOR_2,
                       BET_SA_AREA_COLOR_3,
                       BET_SA_AREA_COLOR_0)

SF_FISHERY_CONFIG = data.table(
  FISHERY_CODE = c("PSFS", "PSLS", "LLDJPN", "LLDTWN", "LLDKOR", "LLFCHN",  "LLFTWN", "BB", "GN", "OT", "ALL"),
  FISHERY_NAME = c("Purse seine | FS", 
                   "Purse seine | LS", 
                   "Longline deep-freezing | Japan", 
                   "Longline deep-freezing | Taiwan,China",
                   "Longline deep-freezing | Korea",
                   "Longline fresh | China",
                   "Longline fresh | Taiwan,China",
                   "Baitboat", 
                   "Gillnet", 
                   "Other", 
                   "All fisheries"),
  FISHERY_COLOR = c(colors_for_fishery("PSFS")$FILL,
                    darken(colors_for_fishery("PSLS")$FILL, amount = 0.2),
                    darken(colors_for_fishery("LLD")$FILL, amount = 0.2), 
                    darken(colors_for_fishery("LLD")$FILL, amount = 0.3), 
                    darken(colors_for_fishery("LLD")$FILL, amount = 0.4), 
                    colors_for_fishery("LLF")$FILL,
                    darken(colors_for_fishery("LLF")$FILL, amount = 0.2),
                    colors_for_fishery("BB")$FILL,
                    colors_for_fishery("GN")$FILL,
                    colors_for_fishery("OT")$FILL,
                    "black")
)

# Function to update the fishery code to the species-specific categories used to display / compare average weights
update_fisheries_for_SF = function(sf_data) {
  if(!"AW_FISHERY" %in% colnames(sf_data)) {
    sf_data[, AW_FISHERY := "OT"]
  } else {
    sf_data[is.na(AW_FISHERY), AW_FISHERY := "OT"]
  }
  
  if(C_FISHERY_CODE %in% colnames(sf_data)) {
    sf_data[FISHERY_CODE == "PSFS", AW_FISHERY := "PSFS"]
    sf_data[FISHERY_CODE == "PSLS", AW_FISHERY := "PSLS"]
    sf_data[FISHERY_CODE == "BB"  , AW_FISHERY := "BB"]
    sf_data[FISHERY_CODE == "GN"  , AW_FISHERY := "GN"]
    sf_data[FISHERY_CODE == "LLD" & FLEET_CODE == "JPN", AW_FISHERY := "LLDJPN"]
    sf_data[FISHERY_CODE == "LLD" & FLEET_CODE == "TWN", AW_FISHERY := "LLDTWN"]
    sf_data[FISHERY_CODE == "LLD" & FLEET_CODE == "KOR", AW_FISHERY := "LLDKOR"]
    sf_data[FISHERY_CODE == "LLF" & FLEET_CODE == "CHN", AW_FISHERY := "LLFCHN"]
    sf_data[FISHERY_CODE == "LLF" & FLEET_CODE == "TWN", AW_FISHERY := "LLFTWN"]
    
  }
  
  sf_data$AW_FISHERY = factor(
    sf_data$AW_FISHERY,
    levels = SF_FISHERY_CONFIG$FISHERY_CODE,
    labels = SF_FISHERY_CONFIG$FISHERY_NAME,
    ordered = TRUE
  )
  
  return(sf_data)
}
