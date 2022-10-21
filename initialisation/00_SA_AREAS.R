d_info("Initializing SA areas...", WPTT_SPECIES)

retrieve_areas = function(sa_areas_configuration = SA_AREAS_CONFIG) {
  areas = iotc.core.gis.wkt::fishing_grounds_data(fishing_ground_codes = sa_areas_configuration$IOTC_CODE, connection = DB_IOTCSTATISTICS())
  areas = merge(areas, sa_areas_configuration,
                by.x = "CODE",
                by.y = "IOTC_CODE",
                all.x = TRUE)
  
  areas$CODE = factor(
    areas$CODE,
    levels = sa_areas_configuration$IOTC_CODE,
    labels = sa_areas_configuration$AREA_NAME,
    ordered = TRUE
  )
  
  areas = areas[order(CODE)]
  
  return(areas)
}

SA_AREAS = retrieve_areas(SA_AREAS_CONFIG) #SA_AREAS_CONFIG are defined in a species-specific file (00_SA_<species>_AREAS.R)
SA_SFs = WKT_to_simple_feature(SA_AREAS)

SA_SFs$AREA_NAME = SA_AREAS$AREA_NAME
SA_SFs$AREA_NAME = factor(
  SA_SFs$AREA_NAME,
  levels = SA_AREAS$AREA_NAME,
  ordered = TRUE
)

SA_MAP = iotc.core.gis.maps::IO_map(show_EEZs = TRUE, show_high_seas = FALSE, show_IO_areas = FALSE)
SA_MAP = 
  SA_MAP + 
  geom_sf(SA_SFs, mapping = aes(geometry = WKT, fill = AREA_NAME, color = AREA_NAME, alpha = AREA_NAME)) +
  scale_fill_manual(values = SA_AREA_COLORS$FILL) +
  scale_color_manual(values = SA_AREA_COLORS$OUTLINE) +
  scale_alpha_manual(values = rep(0.6, nrow(SA_AREAS))) + 
  theme(legend.position = "right") + 
  labs(fill = "Stock assessment area") + 
  guides(alpha = "none", color = "none")

save_plot(paste0("../outputs/charts/AREAS/SA_AREA_", WPTT_SPECIES, ".png"), SA_MAP, DEFAULT_PLOT_WIDTH_MEDIUM, DEFAULT_PLOT_HEIGHT_MEDIUM)

d_info("SA areas initialized!", WPTT_SPECIES)