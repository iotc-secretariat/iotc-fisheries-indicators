d_info("Initializing effort data extaction...")

# LONGLINE FISHERIES ####

EF_LL_HOOKS_RAW = raw.EF(effort_unit_codes = "HOOKS", fishery_group_codes = "LL")

EF_LL_HOOKS_RAW[, LL_FLEET := "OTLL"]
EF_LL_HOOKS_RAW = EF_LL_HOOKS_RAW[FISHERY_CODE == "LLD" & FLEET_CODE == "JPN", LL_FLEET := "LLJP"]
EF_LL_HOOKS_RAW = EF_LL_HOOKS_RAW[FISHERY_CODE == "LLD" & FLEET_CODE == "TWN", LL_FLEET := "LLTW"]
EF_LL_HOOKS_RAW = EF_LL_HOOKS_RAW[FISHERY_CODE == "LLF", LL_FLEET := "FTLL"]
EF_LL_HOOKS_RAW = EF_LL_HOOKS_RAW[GEAR_CODE %in% c("ELL", "ELLOB"), LL_FLEET := "SWLL"]

EF_LL_HOOKS_RAW$LL_FLEET = 
  factor(
    EF_LL_HOOKS_RAW$LL_FLEET,
    levels = c("LLJP", "LLTW", "SWLL", "FTLL", "OTLL"),
    labels = c("Deep-freezing (JPN)",
               "Deep-freezing (TWN)",
               "Swordfish longliners",
               "Fresh-tuna longliners",
               "Other longliners"),
    ordered = TRUE
  )

EF_LL_HOOKS_YEAR = EF_LL_HOOKS_RAW[, .(EFFORT = sum())]

#EF_LL_HOOKS_COLORS = color_table(unique_colors(length(unique(EF_LL_HOOKS_RAW$LL_FLEET))))
#EF_LL_HOOKS_COLORS[, FILL_BY := sort(unique(EF_LL_HOOKS_RAW$LL_FLEET))]

# PURSE SEINE FISHERIES ####

## FISHING TIME ####

### PURSE SEINERS ####

EF_PS_TIME_RAW = raw.EF(fishery_group_codes = "PS")[FISHERY_TYPE_CODE == "IND" & FLEET_CODE != "AUS"][EFFORT_UNIT_CODE %in% c("FHOURS", "FDAYS")]
EF_PS_TIME_RAW[EFFORT_UNIT_CODE == "FHOURS", `:=`(EFFORT = EFFORT / 13, EFFORT_UNIT_CODE = "FDAYS")] 

EF_DAYS_PS_YEAR_FLEET = EF_TIME_PS_RAW[, .(`FISHING DAYS` = sum(EFFORT, na.rm = TRUE)), keyby = .(YEAR, FLEET_CODE)]

# Add days at sea for EUFRA in 2018-2021
EF_DAYS_PS_YEAR_FRA_2018_2021 = data.table(YEAR = 2018:2021, FLEET_CODE = "EUFRA", `FISHING DAYS` = c(2885, 2501, 1805, 1834))

EF_DAYS_PS_YEAR_FLEET = EF_DAYS_PS_YEAR_FLEET[!(FLEET_CODE == "EUFRA" & YEAR == 2018)]
EF_DAYS_PS_YEAR_FLEET = rbindlist(list(EF_DAYS_PS_YEAR_FLEET, EF_DAYS_PS_YEAR_FRA_2018_2021))

### SUPPORT VESSELS ####

EF_SP_RAW = as.data.table(read.xlsx("../inputs/data/3SU_DATA_collated.xlsx"))
names(EF_SP_RAW)[names(EF_SP_RAW) == "FLAG_CODE"] = "FLEET_CODE"
EF_SP_RAW[FLEET_CODE %in% c("ESP", "FRA", "ITA"), FLEET_CODE := paste0("EU", FLEET_CODE)]

EF_DAYS_SP_YEAR_FLEET = EF_SP_RAW[, .(`FISHING DAYS` = sum(DAYS_AT_SEA/24*13, na.rm = TRUE)), keyby = .(YEAR, FLEET_CODE)]

## NUMBER OF SETS #### 

EF_SETS_PS_RAW = raw.EF(connection = DB_CONNECT_TO(server = "IOTCS09", database = "IOTDB"), 
                   years = 2013:last_year, 
                   fishery_group_codes = "PS",  
                   fleet_codes = c("JPN", "KOR"), 
                   effort_unit_codes = "SETS", 
)

EF_CE_SETS_LS_YEAR_FLEET1 = EF_CE_RAW[SCHOOL_TYPE_CODE == "LS", .(NUM_SETS_ON_FOB = sum(EFFORT)), keyby = .(YEAR, SCHOOL_TYPE_CODE, FLEET_CODE)]
EF_CE_SETS_LS_YEAR_FLEET2 = fread("../inputs/data/ANNUAL_NUM_SETS_LS_COMPILED.csv")

EF_CE_SETS_LS_YEAR_FLEET = rbindlist(list(EF_CE_SETS_LS_YEAR_FLEET1, EF_CE_SETS_LS_YEAR_FLEET2))

## NUMBER OF FADS DEPLOYED ####

FA_RAW = as.data.table(read.xlsx("../inputs/data/3FA_DATA_collated.xlsx"))
FA_RAW[NUM_FOBS == " " | NUM_FOBS == "_", NUM_FOBS := NA]
FA_RAW[, NUM_FOBS := as.numeric(NUM_FOBS)]
FA_RAW[, FISHERY_TYPE := trimws(FISHERY_TYPE)]
FA_RAW[FLAG_CODE %in% c("ESP", "FRA"), FLAG_CODE := paste0("EU", FLAG_CODE)]

PS_DEPLOYMENTS_YEAR_FLEET = FA_RAW[YEAR<2022 & ACTIVITY_TYPE_CODE == "DD" & FISHERY_TYPE %in% c("PSLS", "PS"), .(DEPLOYED = sum(NUM_FOBS, na.rm = TRUE)), keyby = .(YEAR, FLEET_CODE = FLAG_CODE)]

SP_DEPLOYMENTS_YEAR_FLEET = FA_RAW[YEAR<2022 & FISHERY_TYPE == "PSSP", .(DEPLOYED = sum(NUM_FOBS, na.rm = TRUE)), keyby = .(YEAR, FLEET_CODE = FLAG_CODE)]

ALL_DEPLOYMENTS_YEAR_FLEET = FA_RAW[YEAR>2014 & YEAR<2022 & ACTIVITY_TYPE_CODE == "DD", .(DEPLOYED = sum(NUM_FOBS, na.rm = TRUE)), keyby = .(YEAR, FLEET_CODE = FLAG_CODE)]

## BAITBOAT FISHERIES ####

EF_BB_RAW = EF_raw(fleet_codes = "MDV", fishery_codes = "BB")

# Assuming 1 day-trips
EF_BB_RAW[EFFORT_UNIT_CODE == "TRIPS", EFFORT_UNIT_CODE := "FDAYS"]

EF_FDAYS_BB_YEAR = EF_BB_RAW[, .(EFFORT = sum(EFFORT)), keyby = .]

  
  

