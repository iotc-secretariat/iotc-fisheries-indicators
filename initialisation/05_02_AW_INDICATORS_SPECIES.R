l_info("Computing average weight indicator", WPTT_SPECIES)

# Computing average weight
SFF2[, AVG_WEIGHT := CATCH * 1000 / CATCH_IN_NUMBERS]

# Filters out all records for which SF was available, but only from strata covering less than 50 t of catches in total (the threshold is arbitrary)  
SFF2[, LOW_COVERAGE := ( is.na(MT) | MT < 50 )]

SFF2 = SFF2[, .(YEAR, AW_FISHERY, AVG_WEIGHT, LOW_COVERAGE)]

SFF2 = rbind(SFF2, SFA)

# Number of records
SFF_FISHERIES_WITH_DATA = SFF2[LOW_COVERAGE == FALSE, .(NUM_RECORDS = .N), keyby = .(AW_FISHERY)]

# Filter on fisheries with good coverage
SFF2_FILTERED = SFF2[AW_FISHERY %in% SFF_FISHERIES_WITH_DATA$AW_FISHERY]

# Faceted AW plot showing all fisheries with at least one data point
N_SELECTED_AW_FISHERIES = length(unique(SFF2_FILTERED$AW_FISHERY)) - 1    #Remove All fisheries
N_ROW_FACETS = ifelse(N_SELECTED_AW_FISHERIES<4, 1, ifelse(N_SELECTED_AW_FISHERIES<9, 2, 3))

AW_YEAR_FISHERY_LINEPLOT_FACETED = 
  ggplot(SFF2_FILTERED[AW_FISHERY != "All fisheries"], 
         aes(x = YEAR, 
             y = AVG_WEIGHT, 
             group = AW_FISHERY,
             color = AW_FISHERY,
             alpha = LOW_COVERAGE
         )
  ) +
  geom_line() + 
  geom_point(shape = 21, size = 0.7, stroke = 0.7, aes(color = AW_FISHERY)) + 
  scale_color_manual(values = fishery_colors_for_SF(SFF2_FILTERED)$FILL) + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_alpha_manual(values = c(1, 0.6), guide = "none") + 
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed")) +
  facet_wrap(nrow = N_ROW_FACETS, 
             scales = "free_y",
             facets = ~AW_FISHERY, dir = "h") +
  labs(x = "", y = "Average weight (kg/fish)")

ggsave(paste0("../outputs/charts/AW/AW_YEAR_FISHERY_LINEPLOT_FACETED_", WPTT_SPECIES, ".png"), AW_YEAR_FISHERY_LINEPLOT_FACETED, width = 6.7, height = 5)

SKJ 2x3: width = 10/3*2, height = 5
2x4: width = 10, height = 5
3x3: width = 10/3*2, height = 5/2*3



l_info("Average weight indicator computed", WPTT_SPECIES)