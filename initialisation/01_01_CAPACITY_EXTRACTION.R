l_info("Initializing data extraction...")

# Active list of purse seiners
ACTIVE_PS = data.table(dbGetQuery(DB_RAV(), "
SELECT DISTINCT Year_Active AS YEAR, 
		IOTC_no AS VESSEL_NUMBER, Name_Ship AS VESSEL, 
		Type_Gear AS GEAR, Type_Ship AS VESSEL_TYPE, 
		Flag AS FLEET_CODE, LOA, GT
  FROM IOTCVessels.dbo.AVL_ListWeb
  WHERE Year_Active BETWEEN 2002 AND 2021
  AND Type_Ship NOT IN ('Longliners', 'Supply vessel (purse seiners)', 'Research-Training', 'Carrier Vessel')
  AND Type_Gear NOT IN ('Drifting longline', 'Fresh Longline')
  AND Name_Ship NOT LIKE 'MV.%'
  AND (LOA IS NULL OR LOA > 50)
  AND Flag NOT IN ('Iran', 'Philippines', 'China', 'Sri Lanka', 'Indonesia', 'Portugal (EU)', 'India', 'Tanzania', 'Oman', 'Kenya', 'South Africa', 'United Kingdom (EU)')
  AND Name_Ship NOT IN ('AUSTRAL', 'CHULABHORN', 'MAHIDOL');"))

# Active list of PS support vessels
ACTIVE_SP_AVL = data.table(dbGetQuery(DB_RAV(), "
SELECT DISTINCT Year_Active AS YEAR, 
		IOTC_no AS VESSEL_NUMBER, Name_Ship AS VESSEL, 
		Type_Gear AS GEAR, Type_Ship AS VESSEL_TYPE, 
		Flag AS FLEET, LOA, GT
  FROM IOTCVessels.dbo.AVL_ListWeb
  WHERE Year_Active BETWEEN 2002 AND 2021
  AND (Type_Ship LIKE 'Supply vessel (purse seiners)' 
  OR IOTC_no IN ('IOTC000813', 'IOTC000880', 'IOTC000893', 'IOTC000894', 'IOTC000913', 'IOTC003590', 'IOTC003599', 
  'IOTC003828', 'IOTC004976', 'IOTC008744', 'IOTC016139', 'IOTC016462'))
  AND IOTC_no NOT IN ('IOTC008465', 'IOTC008466', 'IOTC016324')
;
                             "))

# Add missing vessels (from collation of different sources)

ACTIVE_SP_COMPLEMENT = fread("../inputs/data/ANNUAL_SUPPORT_VESSELS_COMPLEMENT.csv")

ACTIVE_SP = rbindlist(list(ACTIVE_SP_AVL, ACTIVE_SP_COMPLEMENT))

l_info("Data extraction initialized")
