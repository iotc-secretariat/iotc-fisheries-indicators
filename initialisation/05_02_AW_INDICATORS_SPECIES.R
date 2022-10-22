l_info("Computing average weight indicator", WPTT_SPECIES)

# Faceted AW plot showing all fisheries with at least one data point
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
  scale_alpha_manual(values = c(1, 0.4), guide = "none") + 
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed")) +
  facet_wrap(ncol = 3, 
             scales = "free_y",
             facets = ~AW_FISHERY, dir = "v") +
  labs(x = "", y = "Average weight (kg/fish)")

ggsave(paste0("../outputs/charts/AW/AW_YEAR_FISHERY_LINEPLOT_FACETED_", WPTT_SPECIES, ".png"), AW_YEAR_FISHERY_LINEPLOT_FACETED, width = 8, height = 4.5)

l_info("Average weight indicator computed", WPTT_SPECIES)