l_info("Computing LL average weight indicator by area", WPTT_SPECIES)

# Filter on longline fisheries with good coverage
SFF3_FILTERED = SFF3[AW_FISHERY %in% c("Longline deep-freezing | Japan", "Longline deep-freezing | Taiwan,China", "Longline deep-freezing | Korea", "Longline fresh | Taiwan,China") & !is.na(AREA_CODE) &  !AREA_CODE %in% c("A0 - All other areas", "R0 - All other areas")]

AVG_WEIGHT_YEAR_LL_FISHERIES_QUANTILES = SFF3_FILTERED[, .(AW_QLOW = quantile(AVG_WEIGHT, 0.05), AW_QHIGH = quantile(AVG_WEIGHT, 0.95), AW_STD_QLOW = quantile(AVG_WEIGHT_STD, 0.05), AW_STD_QHIGH = quantile(AVG_WEIGHT_STD, 0.95)), keyby = .(FISHERY_CODE, FISHERY, FLEET, AW_FISHERY, AREA_CODE)]

# Faceted AW plot showing all fisheries with at least one data point
N_FACETS = length(unique(SFF3_FILTERED$AW_FISHERY))
N_ROW_FACETS = ifelse(N_FACETS <=4, 1, ifelse(N_FACETS<9, 2, 3))

AW_FLEET_COLORS = fleet_colors_for(SFF3_FILTERED)

## Non-standardized weights ####
AW_YEAR_FISHERY_AREA_LINEPLOT_FACETED = 
  ggplot(SFF3_FILTERED, 
         aes(x = YEAR, 
             y = AVG_WEIGHT, 
             color = FLEET
         )
  ) +
  geom_line(aes(color = FLEET)) + 
  geom_point(shape = 21, size = 0.7, stroke = 0.7, aes(color = FLEET)) + 
  geom_hline(data = AVG_WEIGHT_YEAR_LL_FISHERIES_QUANTILES, aes(yintercept = AW_QLOW, color = FLEET), linetype = "dashed") + 
  geom_hline(data = AVG_WEIGHT_YEAR_LL_FISHERIES_QUANTILES, aes(yintercept = AW_QHIGH, color = FLEET), linetype = "dashed") + 
  scale_color_manual(values = AW_FLEET_COLORS$FILL) + 
  scale_y_continuous(limits = c(0, NA)) + 
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text.x = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed")) +
  facet_grid(vars(AREA_CODE), vars(AW_FISHERY), scales = "free_y") +
  labs(x = "", y = "Average weight (kg/fish)")

ggsave(paste0("../outputs/charts/AW/", WPTT_SPECIES, "/AW_YEAR_FISHERY_AREA_LINEPLOT_FACETED.png"), AW_YEAR_FISHERY_AREA_LINEPLOT_FACETED, width = 10, height = 6/8*10)

## Standardized weights ####
AW_STD_YEAR_FISHERY_AREA_LINEPLOT_FACETED = 
  ggplot(SFF3_FILTERED, 
         aes(x = YEAR, 
             y = AVG_WEIGHT_STD, 
             color = FLEET
         )
  ) +
  geom_line(aes(color = FLEET)) + 
  geom_point(shape = 21, size = 0.7, stroke = 0.7, aes(color = FLEET)) + 
  geom_hline(data = AVG_WEIGHT_YEAR_LL_FISHERIES_QUANTILES, aes(yintercept = AW_STD_QLOW, color = FLEET), linetype = "dashed") + 
  geom_hline(data = AVG_WEIGHT_YEAR_LL_FISHERIES_QUANTILES, aes(yintercept = AW_STD_QHIGH, color = FLEET), linetype = "dashed") + 
  scale_color_manual(values = AW_FLEET_COLORS$FILL) + 
  scale_y_continuous(limits = c(0, NA)) + 
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text.x = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed")) +
  facet_grid(vars(AREA_CODE), vars(AW_FISHERY), scales = "free_y") +
  labs(x = "", y = "Standardized average weight")

ggsave(paste0("../outputs/charts/AW/", WPTT_SPECIES, "/AW_STD_YEAR_FISHERY_AREA_LINEPLOT_FACETED.png"), AW_STD_YEAR_FISHERY_AREA_LINEPLOT_FACETED, width = 10, height = 6/8*10)
