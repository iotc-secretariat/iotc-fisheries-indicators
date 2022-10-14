### AVERAGE WEIGHTS (from raised CE)

### IMPORTANT!!! The approach below only works if the CEForSF table in the WP_CE_RAISED database is updated manually
### Considering the work recently done on the production of SA / raised catches summaries straight from the Access DBs
### and how the CA_RAISED table on WP_CE_RAISED is now automatically updated during the process, we need to ensure the
### same is done for the CEForSF table as well

#### Reads the metadata on the availability of SF for all the grids in the raised catches dataset
SF_STRATA_Q = paste0("
SELECT 
	Year AS YEAR,
	LTRIM(RTRIM(Fleet)) AS FLEET_CODE,
	LTRIM(RTRIM(Gear)) AS GEAR_CODE,
	G.LAggCESF AS FISHERY_GROUP,
	LTRIM(RTRIM(SchoolType)) AS SCHOOL_TYPE_CODE,
	LTRIM(RTRIM(Species)) AS SPECIES_CODE,
	CASE WHEN Substitution = 0 THEN 'ORIGINAL' ELSE 'SUBSTITUTION' END AS STATUS,
	SUM(CEmtFish) AS MT
FROM 
  CEForSF C
LEFT 
  JOIN [IOTDB].[dbo].cdeGears G
ON 
  C.Gear = G.ACode COLLATE SQL_Latin1_General_CP1_CI_AI
WHERE ", 
  ifelse(is.na(WPTT_SPECIES), 
    "1 = 1", 
    paste0("Species = '", WPTT_SPECIES, "'")
  ), " 
GROUP BY 
	Year, 
	LTRIM(RTRIM(Fleet)),
	LTRIM(RTRIM(Gear)), 
	G.LAggCESF,
	LTRIM(RTRIM(SchoolType)), 
	LTRIM(RTRIM(Species)),
	CASE WHEN Substitution = 0 THEN 'ORIGINAL' ELSE 'SUBSTITUTION' END
ORDER BY
	6, 1, 2, 3, 4, 5")

SF_STRATA = as.data.table(query(connection = DB_WP_CE_RAISED(), query = SF_STRATA_Q))

# Assigns standard fishery group codes to the records (as these are not originally available)
SF_STRATA[, FISHERY_GROUP_CODE := "OT"]
SF_STRATA[FISHERY_GROUP == "Purse Seine", FISHERY_GROUP_CODE := "PS"]
SF_STRATA[FISHERY_GROUP == "Longline",    FISHERY_GROUP_CODE := "LL"]
SF_STRATA[FISHERY_GROUP == "Baitboat",    FISHERY_GROUP_CODE := "BB"]
SF_STRATA[FISHERY_GROUP == "Gillnet",     FISHERY_GROUP_CODE := "GN"]
SF_STRATA[FISHERY_GROUP == "Handline",    FISHERY_GROUP_CODE := "HL"]
SF_STRATA[FISHERY_GROUP == "Trolling",    FISHERY_GROUP_CODE := "TL"]

SF_STRATA = 
  update_fisheries_for_SF(
    add_fishery_codes(
      SF_STRATA
    )
  )

SF_STRATA = SF_STRATA[, .(MT = sum(MT)), keyby = .(YEAR, STATUS, AW_FISHERY)]

#as.data.table(read.csv("../data/SF_YFT_TO_KEEP.csv", header = TRUE))

SPECIES_CA_RAISED = CA.raised(species_codes = WPTT_SPECIES)

SF = SPECIES_CA_RAISED[, .(YEAR, FISHERY_CODE, FLEET_CODE, CATCH, CATCH_IN_NUMBERS)]

SFA = SF[, .(AVG_WEIGHT = sum(CATCH * 1000) / sum(CATCH_IN_NUMBERS)), keyby = .(YEAR)]
SFA[, AW_FISHERY := "ALL"]
SFA = update_fisheries_for_SF(SFA)

SFA = merge(SFA, SF_STRATA[STATUS == "ORIGINAL", .(MT = sum(MT)), keyby = .(YEAR)], by = c("YEAR"), all.x = TRUE)
SFA[, LOW_COVERAGE := ( is.na(MT) | MT < 50 )]
SFA$MT = NULL

# The 'update_fisheries_for_SF' function is defined in the species-specific "constants" script
SFF = update_fisheries_for_SF(SF)

SFF = SFF[, .(CATCH = sum(CATCH), CATCH_IN_NUMBERS = sum(CATCH_IN_NUMBERS)), keyby = .(YEAR, AW_FISHERY)]

SFF2 = merge(SFF, SF_STRATA[STATUS == "ORIGINAL"], by = c("YEAR", "AW_FISHERY"), all.x = TRUE)

SFF2[, AVG_WEIGHT := CATCH * 1000 / CATCH_IN_NUMBERS]

# Filters out all records for which SF was available, but only from strata covering less than 50 t of catches in total (the threshold is arbitrary)  
#SFF2[is.na(MT) | MT < 50, AVG_WEIGHT := NA]

SFF2[, LOW_COVERAGE := ( is.na(MT) | MT < 50 )]

SFF2 = SFF2[, .(YEAR, AW_FISHERY, AVG_WEIGHT, LOW_COVERAGE)]

SFF2 = rbind(SFF2, SFA)

#SFF_FISHERIES_WITH_DATA = SFF2[!is.na(AVG_WEIGHT), .(NUM_RECORDS = .N), keyby = .(AW_FISHERY)]
SFF_FISHERIES_WITH_DATA = SFF2[LOW_COVERAGE == FALSE, .(NUM_RECORDS = .N), keyby = .(AW_FISHERY)]

SFF2_FILTERED = SFF2[AW_FISHERY %in% SFF_FISHERIES_WITH_DATA$AW_FISHERY]

# Faceted AW plot showing all fisheries with at least one data point
FACETED_PLOT = 
  ggplot(SFF2_FILTERED, 
         aes(x = YEAR, 
             y = AVG_WEIGHT, 
             group = AW_FISHERY,
             color = AW_FISHERY,
             alpha = LOW_COVERAGE
         )
  ) +
  scale_color_manual(values = fishery_colors_for_SF(SFF2_FILTERED)$FILL) + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_alpha_manual(values = c(1, 0.2), guide = "none") + 
  theme_bw() + 
  theme(legend.position  = "none",
        strip.background = element_rect(fill = "white")) + 
  geom_line(size = 1) + geom_point() +
  facet_wrap(ncol = 2, 
             scales = "free_y",
             facets = ~AW_FISHERY) +
  ylab("Average weight (kg/fish)") + 
  xlab("")

save_plot(chart_file("SF", WPTT_SPECIES, "AVG_WEIGHT", "LINEPLOT_FACETED_FISHERY.png"), 
          FACETED_PLOT, 
          DEFAULT_PLOT_WIDTH_HALF, 2.5 * ceiling(length(unique(SFF2_FILTERED$AW_FISHERY)) / 2))

FACETED_PLOT_WIDE = 
  ggplot(SFF2_FILTERED, 
         aes(x = YEAR, 
             y = AVG_WEIGHT, 
             group = AW_FISHERY,
             color = AW_FISHERY,
             alpha = LOW_COVERAGE
         )
  ) +
  scale_color_manual(values = fishery_colors_for_SF(SFF2_FILTERED)$FILL) + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_alpha_manual(values = c(1, 0.2), guide = "none") + 
  theme_bw() + 
  theme(legend.position  = "none",
        strip.background = element_rect(fill = "white")) + 
  geom_line(size = 1) + geom_point() +
  facet_wrap(ncol = 3, 
             scales = "free_y",
             facets = ~AW_FISHERY) +
  ylab("Average weight (kg/fish)") + 
  xlab("")

save_plot(chart_file("SF", WPTT_SPECIES, "AVG_WEIGHT", "LINEPLOT_FACETED_FISHERY.png"), 
          FACETED_PLOT, 
          DEFAULT_PLOT_WIDTH_HALF, 2.5 * ceiling(length(unique(SFF2_FILTERED$AW_FISHERY)) / 2))

save_plot(chart_file("SF", WPTT_SPECIES, "AVG_WEIGHT", "LINEPLOT_FACETED_FISHERY_WIDE.png"), 
          FACETED_PLOT_WIDE, 
          DEFAULT_PLOT_WIDTH_HALF, 2.5 * ceiling(length(unique(SFF2_FILTERED$AW_FISHERY)) / 2))

# Iterates through all fisheries with at least one data point and produces their corresponding AW plots, one per fishery
for(fishery in SFF_FISHERIES_WITH_DATA$AW_FISHERY) {
  SFF2_for_fishery = SFF2[AW_FISHERY == fishery]
  
  fishery_code = SF_FISHERY_CONFIG[FISHERY_NAME == fishery]$FISHERY_CODE
  
  avg_weight_plot = 
    ggplot(
      SFF2_for_fishery,
      aes(
        x = YEAR, 
        y = AVG_WEIGHT, 
        group = AW_FISHERY,
        color = AW_FISHERY, 
        alpha = LOW_COVERAGE)
    ) +
    scale_color_manual(values = fishery_colors_for_SF(SFF2_for_fishery)$FILL) +
    scale_y_continuous(limits = c(0, NA)) + 
    scale_alpha_manual(values = c(1, 0.2), guide = "none") + 
    theme_bw() + 
    theme(legend.position  = "none") +
    geom_line(size = 1) + geom_point() + 
    ylab("Average weight (kg/fish)") + 
    xlab("")
  
  save_plot(chart_file("SF", WPTT_SPECIES, "AVG_WEIGHT", paste0("LINEPLOT_", fishery_code, ".png")), avg_weight_plot, DEFAULT_PLOT_WIDTH_MEDIUM, DEFAULT_PLOT_HEIGHT_MEDIUM)
}

# Combined AW plot for all fisheries with at least one data point
COMBINED_PLOT = 
  ggplot(SFF2_FILTERED, aes(x = YEAR, 
                            y = AVG_WEIGHT, 
                            group = AW_FISHERY,
                            color = AW_FISHERY, 
                            alpha = LOW_COVERAGE)) +
  scale_color_manual(values = fishery_colors_for_SF(SFF2_FILTERED)$FILL) + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_alpha_manual(values = c(1, 0.2), guide = "none") + 
  theme_bw() + 
  theme(legend.position  = "bottom", 
        legend.title = element_blank(),
        legend.text =  element_text(size = 13),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13)) + 
  geom_line(size = 1) + geom_point() + 
  ylab("Average weight (kg/fish)") + 
  xlab("")

save_plot(chart_file("SF", WPTT_SPECIES, "AVG_WEIGHT", "LINEPLOT_COMBINED_FISHERY.png"), COMBINED_PLOT, DEFAULT_PLOT_WIDTH_MEDIUM, DEFAULT_PLOT_HEIGHT_MEDIUM)