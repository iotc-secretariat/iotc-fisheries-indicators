l_info("Computing CPUE indicators", WPTT_SPECIES)

# DEEP-FREEZING LONGLINE ####
# Focus on the 3 main areas
CE_LLD = CE_RAW_SA_YEAR[!is.na(AREA_CODE) & !AREA_CODE %in% c("A0 - All other areas", "R0 - All other areas") & FISHERY_CODE == "LLD" & FLEET_CODE %in% LL_MAIN_FLEETS]
CE_LLD[, CPUE_STD := CPUE_TOT/mean(CPUE_TOT), by = .(FLEET_CODE, AREA_CODE)]

CPUE_LLD_YEAR_FLEET_QUANTILES = CE_LLD[, .(CPUE_QLOW = quantile(CPUE_TOT, 0.05), CPUE_QHIGH = quantile(CPUE_TOT, 0.95), CPUE_STD_QLOW = quantile(CPUE_STD, 0.05), CPUE_STD_QHIGH = quantile(CPUE_STD, 0.95)), keyby = .(FLEET, AREA_CODE)]

FLEET_COLORS = fleet_colors_for(CE_LLD)

## Non-standardized effort ####
CPUE_LLD_YEAR_FLEET_AREA_LINEPLOT_FACETED =
  ggplot(data = CE_LLD, aes(x  = YEAR, y = CPUE_TOT)) + 
  labs(x = "", y = "CPUE (t/1,000 hooks)") + 
  geom_point(shape = 21, size = 0.7, stroke = 0.7, aes(color = FLEET)) + 
  geom_line(aes(color = FLEET)) + 
  geom_hline(data = CPUE_LLD_YEAR_FLEET_QUANTILES, aes(yintercept = CPUE_QLOW, color = FLEET), linetype = "dashed") + 
  geom_hline(data = CPUE_LLD_YEAR_FLEET_QUANTILES, aes(yintercept = CPUE_QHIGH, color = FLEET), linetype = "dashed") + 
  scale_color_manual(values = FLEET_COLORS$OUTLINE) +
  scale_fill_manual(values = FLEET_COLORS$FILL) + 
  facet_grid(vars(FLEET), vars(AREA_CODE), scales = "free_y") +
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text.x = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed"))

ggsave(paste0("../outputs/charts/CPUE/", WPTT_SPECIES, "/CPUE_LLD_YEAR_FLEET_LINEPLOT_FACETED.png"), CPUE_LLD_YEAR_FLEET_AREA_LINEPLOT_FACETED, width = 8, height = 6)

## Standardized effort ####
CPUE_STD_LLD_YEAR_FLEET_AREA_LINEPLOT_FACETED =
  ggplot(data = CE_LLD, aes(x  = YEAR, y = CPUE_STD)) + 
  labs(x = "", y = "Standardized CPUE") + 
  geom_point(shape = 21, size = 0.7, stroke = 0.7, aes(color = FLEET)) + 
  geom_line(aes(color = FLEET)) + 
  geom_hline(data = CPUE_LLD_YEAR_FLEET_QUANTILES, aes(yintercept = CPUE_STD_QLOW, color = FLEET), linetype = "dashed") + 
  geom_hline(data = CPUE_LLD_YEAR_FLEET_QUANTILES, aes(yintercept = CPUE_STD_QHIGH, color = FLEET), linetype = "dashed") + 
  scale_color_manual(values = FLEET_COLORS$OUTLINE) +
  scale_fill_manual(values = FLEET_COLORS$FILL) + 
  facet_grid(vars(FLEET), vars(AREA_CODE), scales = "free_y") +
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text.x = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed"))

ggsave(paste0("../outputs/charts/CPUE/", WPTT_SPECIES, "/CPUE_STD_LLD_YEAR_FLEET_LINEPLOT_FACETED.png"), CPUE_STD_LLD_YEAR_FLEET_AREA_LINEPLOT_FACETED, width = 8, height = 6)

# FRESH LONGLINE ####
# Focus on the 3 main areas
CE_LLF = CE_RAW_SA_YEAR[!is.na(AREA_CODE) & !AREA_CODE %in% c("A0 - All other areas", "R0 - All other areas") & FISHERY_CODE == "LLF" & FLEET_CODE %in% LL_MAIN_FLEETS]
CE_LLF[, CPUE_STD := CPUE_TOT/mean(CPUE_TOT), by = .(FLEET_CODE, AREA_CODE)]

CPUE_LLF_YEAR_FLEET_QUANTILES = CE_LLF[, .(CPUE_QLOW = quantile(CPUE_TOT, 0.05), CPUE_QHIGH = quantile(CPUE_TOT, 0.95), CPUE_STD_QLOW = quantile(CPUE_STD, 0.05), CPUE_STD_QHIGH = quantile(CPUE_STD, 0.95)), keyby = .(FLEET, AREA_CODE)]

FLEET_COLORS = fleet_colors_for(CE_LLF)

## Non-standardized effort ####
CPUE_LLF_YEAR_FLEET_AREA_LINEPLOT_FACETED =
  ggplot(data = CE_LLF, aes(x  = YEAR, y = CPUE_TOT)) + 
  labs(x = "", y = "CPUE (fish/1,000 hooks)") + 
  geom_point(shape = 21, size = 0.7, stroke = 0.7, aes(color = FLEET)) + 
  geom_line(aes(color = FLEET)) + 
  geom_hline(data = CPUE_LLF_YEAR_FLEET_QUANTILES, aes(yintercept = CPUE_QLOW, color = FLEET), linetype = "dashed") + 
  geom_hline(data = CPUE_LLF_YEAR_FLEET_QUANTILES, aes(yintercept = CPUE_QHIGH, color = FLEET), linetype = "dashed") + 
  scale_color_manual(values = FLEET_COLORS$OUTLINE) +
  scale_fill_manual(values = FLEET_COLORS$FILL) + 
  facet_grid(vars(FLEET), vars(AREA_CODE), scales = "free_y") +
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text.x = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed"))

ggsave(paste0("../outputs/charts/CPUE/", WPTT_SPECIES, "/CPUE_LLF_YEAR_FLEET_LINEPLOT_FACETED.png"), CPUE_LLF_YEAR_FLEET_AREA_LINEPLOT_FACETED, width = 8, height = 4)

## Standardized CPUE ####
CPUE_STD_LLF_YEAR_FLEET_AREA_LINEPLOT_FACETED =
  ggplot(data = CE_LLF, aes(x  = YEAR, y = CPUE_STD)) + 
  labs(x = "", y = "Standardised CPUE") + 
  geom_point(shape = 21, size = 0.7, stroke = 0.7, aes(color = FLEET)) + 
  geom_line(aes(color = FLEET)) + 
  geom_hline(data = CPUE_LLF_YEAR_FLEET_QUANTILES, aes(yintercept = CPUE_STD_QLOW, color = FLEET), linetype = "dashed") + 
  geom_hline(data = CPUE_LLF_YEAR_FLEET_QUANTILES, aes(yintercept = CPUE_STD_QHIGH, color = FLEET), linetype = "dashed") + 
  scale_color_manual(values = FLEET_COLORS$OUTLINE) +
  scale_fill_manual(values = FLEET_COLORS$FILL) + 
  facet_grid(vars(FLEET), vars(AREA_CODE), scales = "free_y") +
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text.x = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed"))

ggsave(paste0("../outputs/charts/CPUE/", WPTT_SPECIES, "/CPUE_STD_LLF_YEAR_FLEET_LINEPLOT_FACETED.png"), CPUE_STD_LLF_YEAR_FLEET_AREA_LINEPLOT_FACETED, width = 8, height = 4)
