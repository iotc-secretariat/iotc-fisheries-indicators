l_info("Computing average weight indicator", WPTT_SPECIES)

# Filter on fisheries with good coverage
# Remove all fisheries
SFF2_FILTERED = SFF2[AW_FISHERY %in% SFF_FISHERIES_WITH_DATA$AW_FISHERY][AW_FISHERY != "All fisheries"]
SFF2_FILTERED[, AVG_WEIGHT_STD := AVG_WEIGHT/mean(AVG_WEIGHT), by = .(AW_FISHERY)]

AVG_WEIGHT_YEAR_FISHERIES_QUANTILES = SFF2_FILTERED[, .(AW_QLOW = quantile(AVG_WEIGHT, 0.05), AW_QHIGH = quantile(AVG_WEIGHT, 0.95), AW_STD_QLOW = quantile(AVG_WEIGHT_STD, 0.05), AW_STD_QHIGH = quantile(AVG_WEIGHT_STD, 0.95)), keyby = .(AW_FISHERY)]

# Faceted AW plot showing all fisheries with at least one data point
N_FACETS = length(unique(SFF2_FILTERED$AW_FISHERY)) - 1    #Remove All fisheries
N_ROW_FACETS = ifelse(N_FACETS <=4, 1, ifelse(N_FACETS<9, 2, 3))

AW_FISHERY_COLORS = fishery_colors_for_SF(SFF2_FILTERED)

## Non-standardized weights ####
AW_YEAR_FISHERY_LINEPLOT_FACETED = 
  ggplot(SFF2_FILTERED, 
         aes(x = YEAR, 
             y = AVG_WEIGHT, 
             group = AW_FISHERY,
             color = AW_FISHERY,
             alpha = LOW_COVERAGE
         )
  ) +
  geom_line() + 
  geom_point(shape = 21, size = 0.7, stroke = 0.7, aes(color = AW_FISHERY)) + 
  geom_hline(data = AVG_WEIGHT_YEAR_FISHERIES_QUANTILES, aes(yintercept = AW_QLOW, color = AW_FISHERY), linetype = "dashed") + 
  geom_hline(data = AVG_WEIGHT_YEAR_FISHERIES_QUANTILES, aes(yintercept = AW_QHIGH, color = AW_FISHERY), linetype = "dashed") + 
  scale_color_manual(values = AW_FISHERY_COLORS$FILL) + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_alpha_manual(values = c(1, 0.6), guide = "none") + 
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed")) +
  facet_wrap(nrow = N_ROW_FACETS, 
             scales = "free_y",
             facets = ~AW_FISHERY, dir = "h") +
  labs(x = "", y = "Average weight (kg/fish)")

ggsave(paste0("../outputs/charts/AW/", WPTT_SPECIES, "/AW_YEAR_FISHERY_LINEPLOT_FACETED.png"), AW_YEAR_FISHERY_LINEPLOT_FACETED, width = 12, height = 4.5/8*10)

## Standardized weights ####
AW_STD_YEAR_FISHERY_LINEPLOT_FACETED = 
  ggplot(SFF2_FILTERED, 
         aes(x = YEAR, 
             y = AVG_WEIGHT_STD, 
             group = AW_FISHERY,
             color = AW_FISHERY,
             alpha = LOW_COVERAGE
         )
  ) +
  geom_line() + 
  geom_point(shape = 21, size = 0.7, stroke = 0.7, aes(color = AW_FISHERY)) + 
  scale_color_manual(values = AW_FISHERY_COLORS$FILL) + 
  scale_alpha_manual(values = c(1, 0.6), guide = "none") + 
  geom_hline(data = AVG_WEIGHT_YEAR_FISHERIES_QUANTILES, aes(yintercept = AW_STD_QLOW, color = AW_FISHERY), linetype = "dashed") + 
  geom_hline(data = AVG_WEIGHT_YEAR_FISHERIES_QUANTILES, aes(yintercept = AW_STD_QHIGH, color = AW_FISHERY), linetype = "dashed") + 
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed")) +
  facet_wrap(nrow = N_ROW_FACETS, 
             scales = "free_y",
             facets = ~AW_FISHERY, dir = "h") +
  labs(x = "", y = "Standardised average weight")

ggsave(paste0("../outputs/charts/AW/", WPTT_SPECIES, "/AW_STD_YEAR_FISHERY_LINEPLOT_FACETED.png"), AW_STD_YEAR_FISHERY_LINEPLOT_FACETED, width = 13, height = 4.5/8*10)

l_info("Average weight indicator computed", WPTT_SPECIES)
