CA_REFERENCE_VALUE = 20000 # Reference value for average annual catches by grid (for CA piemaps) 

#Triggers the execution of the scripts that analyze the SF for TWN and JPN LL
ANALYZE_SF_LL = TRUE

# Species-dependent size class threshold to tell small / large fish apart
LARGE_FISH_SIZE_THRESHOLD = 75 #YFT

# Max fork length to be used to plot L-W relationships 
MAX_FISH_SIZE = 250

FL_RW_RELATIONSHIPS = data.table(
  A = c(0.00002459, 0.0000094007),
  B = c(2.96670000, 3.1268439870),
  M = c(1, 1.13),
  min_FL = c(29, 72),
  max_FL = c(166, 177),
  gears = c("Purse seine, pole-and-line, gillnet", "Longline, line, other gears"),
  gear_codes = c("PSPLGN", "LLOT"),
  source = c("Length-weight relationships for tropical tunas caught with purse seine in the Indian Ocean: Update and lessons learned (Chassot, E. et al in IOTC-2016-WPDSC12-INF05)",
             "Multilateral catch monitoring Benoa (2002-04), ICCAT Field Manual (Appendix 4: Population parameters for key ICCAT species. Product Conversion Factors)"),
  num_samples = c(25386, 15133)
)

TWN_LL_SF_YEAR_CUTOFF = 2002 # The year when TWN LL average weights from CE became < than those from SF

# YFT SA areas
SA_AREAS_CONFIG = data.table(
  IOTC_CODE  = c("IRYFT1A", "IRYFT1B", "IRYFT02", "IRYFT03", "IRYFT04", "IRYFT00"),
  AREA_CODE  = c("R1a", "R1b", "R2", "R3", "R4", "R0"),
  NAME_SHORT = c("Arabian sea",
                 "Western Indian ocean (tropical)",
                 "Mozambique channel",
                 "Southern Indian ocean",
                 "Eastern Indian ocean (tropical)",
                 "All other areas")
)

# Color constants for current YFT SA areas
YFT_SA_AREA_COLOR_1 = ALL_FI_COLORS[FISHERY_CODE == "LLF"]  # IRYFT1A
YFT_SA_AREA_COLOR_2 = ALL_FI_COLORS[FISHERY_CODE == "PSLS"] # IRYFT1B
YFT_SA_AREA_COLOR_3 = ALL_FI_COLORS[FISHERY_CODE == "BB"]   # IRYFT02
YFT_SA_AREA_COLOR_4 = ALL_FI_COLORS[FISHERY_CODE == "GN"]   # IRYFT03
YFT_SA_AREA_COLOR_5 = ALL_FI_COLORS[FISHERY_CODE == "LIT"]  # IRYFT04
YFT_SA_AREA_COLOR_0 = ALL_FI_COLORS[FISHERY_CODE == "OT"]   # ...

SA_AREA_COLORS = rbind(YFT_SA_AREA_COLOR_1,
                       YFT_SA_AREA_COLOR_2,
                       YFT_SA_AREA_COLOR_3,
                       YFT_SA_AREA_COLOR_4,
                       YFT_SA_AREA_COLOR_5,
                       YFT_SA_AREA_COLOR_0)

SF_FISHERY_CONFIG = data.table(
  FISHERY_CODE = c("PSFS", "PSLS", "LLJPN", "LLTWN", "BB", "GN", "OT", "ALL"),
  FISHERY_NAME = c("Purse seine | FS", 
                   "Purse seine | LS", 
                   "Longline | Japan", 
                   "Longline | Taiwan,China",
                   "Baitboat", 
                   "Gillnet", 
                   "Other", 
                   "All fisheries"),
  FISHERY_COLOR = c(colors_for_fishery("PSFS")$FILL,
                    darken(colors_for_fishery("PSLS")$FILL, amount = 0.2),
                    darken(colors_for_fishery("LLD")$FILL, amount = 0.2),
                    colors_for_fishery("LLF")$FILL,
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
    sf_data[FISHERY_CODE == "LLD" &  FLEET_CODE %in% c("JPN", "KOR", "THA"), AW_FISHERY := "LLJPN"]
    sf_data[FISHERY_CODE == "LLD" & !FLEET_CODE %in% c("JPN", "KOR", "THA"), AW_FISHERY := "LLTWN"]
  }
  
  sf_data$AW_FISHERY = factor(
    sf_data$AW_FISHERY,
    levels = SF_FISHERY_CONFIG$FISHERY_CODE,
    labels = SF_FISHERY_CONFIG$FISHERY_NAME,
    ordered = TRUE
  )
  
  return(sf_data)
}