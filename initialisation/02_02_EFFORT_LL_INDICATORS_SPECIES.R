l_info("Computing effort indicators for longline by assessment area", WPTT_SPECIES)

# MAPPING WITH STOCK ASSESSMENT AREAS ####

# Add stock assessment areas
SA_AREAS_MAPPING = iotc.core.gis.cwp.IO::grid_intersections_by_source_grid_type(target_grid_codes = SA_AREAS_CONFIG$IOTC_CODE, source_grid_type_code = grid_5x5)

EF_LL_HOOKS_TROP_SA = merge(EF_LL_HOOKS_TROP, SA_AREAS_MAPPING,
                            by.x = "FISHING_GROUND_CODE",
                            by.y = "SOURCE_FISHING_GROUND_CODE",
                            all.x = TRUE,
                            allow.cartesian = TRUE)

EF_LL_HOOKS_TROP_SA[!is.na(PROPORTION), EFFORT_WEIGHTED_AREA := EFFORT * PROPORTION]

EF_LL_HOOKS_TROP_SA_YEAR = EF_LL_HOOKS_TROP_SA[, .(EFFORT = sum(EFFORT_WEIGHTED_AREA, na.rm = TRUE)),
                                               keyby = .(YEAR, 
                                                         FLEET_CODE, FLEET, 
                                                         GEAR_CODE, GEAR,
                                                         FISHERY_TYPE_CODE, FISHERY_TYPE,
                                                         FISHERY_GROUP_CODE, FISHERY_GROUP,
                                                         FISHERY_CODE, FISHERY,
                                                         AREA_CODE = TARGET_FISHING_GROUND_CODE)]


EF_LL_HOOKS_TROP_SA_YEAR[, AREA_CODE := factor(AREA_CODE, levels = SA_AREAS_CONFIG$IOTC_CODE, labels = SA_AREAS_CONFIG$AREA_NAME, ordered = TRUE)]

# Compute annual effort by fleet and area
EF_LL_HOOKS_YEAR_FLEET_AREA = EF_LL_HOOKS_TROP_SA_YEAR[, .(UNIT = "HOOKS", EFFORT = sum(EFFORT, na.rm = TRUE)), keyby = .(YEAR, FISHERY_CODE, FISHERY, FLEET_CODE, FLEET, AREA_CODE)]

# DEEP-FREEZING LONGLINE ####
# Focus on the 3 main areas
EF_LLD = EF_LL_HOOKS_YEAR_FLEET_AREA[!is.na(AREA_CODE) & AREA_CODE != "A0 - All other" & FISHERY_CODE == "LLD"]
EF_LLD[, EFFORT := EFFORT/1e6]
EF_LLD[, EFFORT_STD := EFFORT/mean(EFFORT), by = .(FLEET_CODE, AREA_CODE)]

EF_LLD_YEAR_FLEET_QUANTILES = EF_LLD[, .(EFFORT_QLOW = quantile(EFFORT, 0.05), EFFORT_QHIGH = quantile(EFFORT, 0.95), EFFORT_STD_QLOW = quantile(EFFORT_STD, 0.05), EFFORT_STD_QHIGH = quantile(EFFORT_STD, 0.95)), keyby = .(FLEET_CODE, FLEET, AREA_CODE)]

FLEET_COLORS = fleet_colors_for(EF_LLD)

## Non-standardized effort ####
EF_LLD_YEAR_FLEET_AREA_LINEPLOT_FACETED =
ggplot(data = EF_LLD, aes(x  = YEAR, y = EFFORT)) + 
  labs(x = "", y = "Fishing effort (million hooks)") + 
  geom_point(shape = 21, size = 0.7, stroke = 0.7, aes(color = FLEET_CODE)) + 
  geom_line(aes(color = FLEET_CODE)) + 
  geom_hline(data = EF_LLD_YEAR_FLEET_QUANTILES, aes(yintercept = EFFORT_QLOW, color = FLEET_CODE), linetype = "dashed") + 
  geom_hline(data = EF_LLD_YEAR_FLEET_QUANTILES, aes(yintercept = EFFORT_QHIGH, color = FLEET_CODE), linetype = "dashed") + 
  scale_color_manual(values = FLEET_COLORS$OUTLINE) +
  scale_fill_manual(values = FLEET_COLORS$FILL) + 
  facet_grid(vars(FLEET), vars(AREA_CODE), scales = "free_y") +
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text.x = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed"))

ggsave(paste0("../outputs/charts/EFFORT/", WPTT_SPECIES, "/EF_LLD_YEAR_FLEET_LINEPLOT_FACETED.png"), EF_LLD_YEAR_FLEET_AREA_LINEPLOT_FACETED, width = 8, height = 6)

## Standardized effort ####
EF_STD_LLD_YEAR_FLEET_AREA_LINEPLOT_FACETED =
  ggplot(data = EF_LLD, aes(x  = YEAR, y = EFFORT_STD)) + 
  labs(x = "", y = "Standardized fishing effort") + 
  geom_point(shape = 21, size = 0.7, stroke = 0.7, aes(color = FLEET_CODE)) + 
  geom_line(aes(color = FLEET_CODE)) + 
  geom_hline(data = EF_LLD_YEAR_FLEET_QUANTILES, aes(yintercept = EFFORT_STD_QLOW, color = FLEET_CODE), linetype = "dashed") + 
  geom_hline(data = EF_LLD_YEAR_FLEET_QUANTILES, aes(yintercept = EFFORT_STD_QHIGH, color = FLEET_CODE), linetype = "dashed") + 
  scale_color_manual(values = FLEET_COLORS$OUTLINE) +
  scale_fill_manual(values = FLEET_COLORS$FILL) + 
  facet_grid(vars(FLEET), vars(AREA_CODE), scales = "free_y") +
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text.x = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed"))

ggsave(paste0("../outputs/charts/EFFORT/", WPTT_SPECIES, "/EF_STD_LLD_YEAR_FLEET_LINEPLOT_FACETED.png"), EF_STD_LLD_YEAR_FLEET_AREA_LINEPLOT_FACETED, width = 8, height = 6)

# FRESH LONGLINE ####

EF_LLF = EF_LL_HOOKS_YEAR_FLEET_AREA[!is.na(AREA_CODE) & AREA_CODE != "A0 - All other" & FISHERY_CODE == "LLF"]
EF_LLF[, EFFORT := EFFORT/1e6]
EF_LLF[, EFFORT_STD := EFFORT/mean(EFFORT), by = .(FLEET_CODE, AREA_CODE)]

EF_LLF_YEAR_FLEET_QUANTILES = EF_LLF[, .(EFFORT_QLOW = quantile(EFFORT, 0.05), EFFORT_QHIGH = quantile(EFFORT, 0.95), EFFORT_STD_QLOW = quantile(EFFORT_STD, 0.05), EFFORT_STD_QHIGH = quantile(EFFORT_STD, 0.95)), keyby = .(FLEET_CODE, FLEET, AREA_CODE)]

## Non-standardized effort ####
EF_LLF_YEAR_FLEET_AREA_LINEPLOT_FACETED =
  ggplot(data = EF_LLF, aes(x  = YEAR, y = EFFORT)) + 
  labs(x = "", y = "Fishing effort (million hooks)") + 
  geom_point(shape = 21, size = 0.7, stroke = 0.7, aes(color = FLEET_CODE)) + 
  geom_line(aes(color = FLEET_CODE)) + 
  geom_hline(data = EF_LLF_YEAR_FLEET_QUANTILES, aes(yintercept = EFFORT_QLOW, color = FLEET_CODE), linetype = "dashed") + 
  geom_hline(data = EF_LLF_YEAR_FLEET_QUANTILES, aes(yintercept = EFFORT_QHIGH, color = FLEET_CODE), linetype = "dashed") + 
  scale_color_manual(values = FLEET_COLORS$OUTLINE) +
  scale_fill_manual(values = FLEET_COLORS$FILL) + 
  facet_grid(vars(FLEET), vars(AREA_CODE), scales = "free_y") +
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text.x = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed"))

ggsave(paste0("../outputs/charts/EFFORT/", WPTT_SPECIES, "/EF_LLF_YEAR_FLEET_LINEPLOT_FACETED.png"), EF_LLF_YEAR_FLEET_AREA_LINEPLOT_FACETED, width = 8, height = 3)

## Standardized effort ####
EF_STD_LLF_YEAR_AREA_FLEET_LINEPLOT_FACETED =
  ggplot(data = EF_LLF, aes(x  = YEAR, y = EFFORT_STD)) + 
  labs(x = "", y = "Standardized fishing effort") + 
  geom_point(shape = 21, size = 0.7, stroke = 0.7, aes(color = FLEET_CODE)) + 
  geom_line(aes(color = FLEET_CODE)) + 
  geom_hline(data = EF_LLF_YEAR_FLEET_QUANTILES, aes(yintercept = EFFORT_STD_QLOW, color = FLEET_CODE), linetype = "dashed") + 
  geom_hline(data = EF_LLF_YEAR_FLEET_QUANTILES, aes(yintercept = EFFORT_STD_QHIGH, color = FLEET_CODE), linetype = "dashed") + 
  scale_color_manual(values = FLEET_COLORS$OUTLINE) +
  scale_fill_manual(values = FLEET_COLORS$FILL) + 
  facet_grid(vars(FLEET), vars(AREA_CODE), scales = "free_y") +
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text.x = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed"))

ggsave(paste0("../outputs/charts/EFFORT/", WPTT_SPECIES, "/EF_STD_LLF_YEAR_FLEET_LINEPLOT_FACETED.png"), EF_STD_LLF_YEAR_FLEET_AREA_LINEPLOT_FACETED, width = 8, height = 3)

l_info("Longline effort indicators by assessment area computed", WPTT_SPECIES)