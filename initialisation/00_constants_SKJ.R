CA_REFERENCE_VALUE = 30000 # Reference value for average annual catches by grid (for CA piemaps) 

#Prevents triggering the execution of the scripts that analyze the SF for TWN and JPN LL
ANALYZE_SF_LL = FALSE

#Species-dependent size class threshold to tell small / large fish apart
LARGE_FISH_SIZE_THRESHOLD = 40 # Approximates the median value of sizes for fish taken by PS (both LS and FS)

# Max fork length to be used to plot L-W relationships 
MAX_FISH_SIZE = 80

FL_RW_RELATIONSHIPS = data.table(
  A = c(0.00000497),
  B = c(3.39292),
  M = c(1),
  min_FL = c(30),
  max_FL = c(73),
  gears = c("All gears"),
  gear_codes = c("ALL"),
  source = c("Length-weight relationships for tropical tunas caught with purse seine in the Indian Ocean: Update and lessons learned (Chassot, E. et al in
IOTC-2016-WPDSC12-INF05)"),
  num_samples = c(1762)
)

# SKJ SA areas
SA_AREAS_CONFIG = data.table(
  IOTC_CODE  = c("IRSKJWE", "IRSKJEA"),
  AREA_CODE  = c("A1", "A2"),
  NAME_SHORT = c("Western Indian ocean",
                 "Eastern Indian Ocean, Arabian sea and Maldives atolls")
)

SA_AREAS_CONFIG[, AREA_NAME := paste0(AREA_CODE, " - ", NAME_SHORT)]

# Color constants for current YFT SA areas
SKJ_SA_AREA_COLOR_1 = ALL_FI_COLORS[FISHERY_CODE == "PSLS"]  # IRYFT1A
SKJ_SA_AREA_COLOR_2 = ALL_FI_COLORS[FISHERY_CODE == "BB"] # IRYFT1B

SA_AREA_COLORS = rbind(SKJ_SA_AREA_COLOR_1,
                       SKJ_SA_AREA_COLOR_2)


SF_FISHERY_CONFIG = data.table(
  FISHERY_CODE = c("PSFS", "PSLS", "PSOT", "BB", "GN", "OT", "ALL"),
  FISHERY_NAME = c("Purse seine | FS", 
                   "Purse seine | LS", 
                   "Purse seine | OT",
                   "Baitboat", 
                   "Gillnet", 
                   "Other", 
                   "All fisheries"),
  FISHERY_COLOR = c(colors_for_fishery("PSFS")$FILL,
                    darken(colors_for_fishery("PSLS")$FILL, amount = 0.2),
                    darken(colors_for_fishery("PSOT")$FILL, amount = 0.2),
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
    sf_data[FISHERY_CODE == "PSOT", AW_FISHERY := "PSOT"]
    sf_data[FISHERY_CODE == "BB"  , AW_FISHERY := "BB"]
    sf_data[FISHERY_CODE == "GN"  , AW_FISHERY := "GN"]
  }
  
  sf_data$AW_FISHERY = factor(
    sf_data$AW_FISHERY,
    levels = SF_FISHERY_CONFIG$FISHERY_CODE,
    labels = SF_FISHERY_CONFIG$FISHERY_NAME,
    ordered = TRUE
  )
  
  return(sf_data)
}
