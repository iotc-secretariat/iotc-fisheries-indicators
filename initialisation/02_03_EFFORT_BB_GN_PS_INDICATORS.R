l_info("Computing effort indicators for baitboat, gillnet, and purse seine")

EF_FISHERIES_YEAR_FLEET[, EFFORT := EFFORT/1000]
EF_FISHERIES_YEAR_FLEET[, FISHERY_FLEET_UNIT := paste(FISHERY, FLEET_CODE, UNIT, sep = " | ")]
EF_FISHERIES_YEAR_FLEET[, EFFORT_STD := EFFORT/mean(EFFORT), by = .(FLEET_CODE, FISHERY_CODE, FISHERY)]

EF_FISHERIES_YEAR_FLEET_QUANTILES = EF_FISHERIES_YEAR_FLEET[, .(EFFORT_QLOW = quantile(EFFORT, 0.05), EFFORT_QHIGH = quantile(EFFORT, 0.95), EFFORT_STD_QLOW = quantile(EFFORT_STD, 0.05), EFFORT_STD_QHIGH = quantile(EFFORT_STD, 0.95)), keyby = .(FISHERY_CODE, FISHERY, FISHERY_FLEET_UNIT)]

FISHERY_COLORS = fishery_colors_for(EF_FISHERIES_YEAR_FLEET)

## Non-standardized effort ####
EF_BB_GN_PS_YEAR_FLEET_LINEPLOT_FACETED =
  ggplot(data = EF_FISHERIES_YEAR_FLEET, aes(x  = YEAR, y = EFFORT)) + 
  labs(x = "", y = "Fishing effort (x1,000)") + 
  geom_point(shape = 21, size = 0.7, stroke = 0.7, aes(color = FISHERY_CODE)) + 
  geom_line(aes(color = FISHERY_CODE)) + 
  geom_hline(data = EF_FISHERIES_YEAR_FLEET_QUANTILES, aes(yintercept = EFFORT_QLOW, color = FISHERY_CODE), linetype = "dashed") + 
  geom_hline(data = EF_FISHERIES_YEAR_FLEET_QUANTILES, aes(yintercept = EFFORT_QHIGH, color = FISHERY_CODE), linetype = "dashed") + 
  scale_color_manual(values = FISHERY_COLORS$OUTLINE) +
  scale_fill_manual(values = FISHERY_COLORS$FILL) + 
    facet_wrap(~FISHERY_FLEET_UNIT, scales = "free_y") +
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text.x = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed"))

ggsave("../outputs/charts/EFFORT/EF_BB_GN_PS_YEAR_FLEET_LINEPLOT_FACETED.png", EF_BB_GN_PS_YEAR_FLEET_LINEPLOT_FACETED, width = 8, height = 3)

## Standardized effort ####
EF_STD_BB_GN_PS_YEAR_FLEET_LINEPLOT_FACETED =
  ggplot(data = EF_FISHERIES_YEAR_FLEET, aes(x  = YEAR, y = EFFORT_STD)) + 
  labs(x = "", y = "Standardized fishing effort") + 
  geom_point(shape = 21, size = 0.7, stroke = 0.7, aes(color = FISHERY_CODE)) + 
  geom_line(aes(color = FISHERY_CODE)) + 
  geom_hline(data = EF_FISHERIES_YEAR_FLEET_QUANTILES, aes(yintercept = EFFORT_STD_QLOW, color = FISHERY_CODE), linetype = "dashed") + 
  geom_hline(data = EF_FISHERIES_YEAR_FLEET_QUANTILES, aes(yintercept = EFFORT_STD_QHIGH, color = FISHERY_CODE), linetype = "dashed") + 
  scale_color_manual(values = FISHERY_COLORS$OUTLINE) +
  scale_fill_manual(values = FISHERY_COLORS$FILL) + 
  facet_wrap(~FISHERY_FLEET_UNIT) +
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text.x = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed"))

ggsave("../outputs/charts/EFFORT/EF_STD_BB_GN_PS_YEAR_FLEET_LINEPLOT_FACETED.png", EF_STD_BB_GN_PS_YEAR_FLEET_LINEPLOT_FACETED, width = 8, height = 3)

l_info("Indicator of fishing time computed!")