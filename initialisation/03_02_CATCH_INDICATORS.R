l_info("Computing catch indicators", WPTT_SPECIES)

# TOTAL CATCHES BY FISHERY GROUP ####
NC_SP_YEAR = NC_SP_RAISED[, .(CATCH = sum(CATCH/1000)), keyby = .(FISHERY_CODE, FISHERY, YEAR)]
NC_SP_YEAR[, CATCH_STD := CATCH/mean(CATCH), by = .(FISHERY_CODE, FISHERY)]

NC_SP_YEAR_QUANTILES = NC_SP_YEAR[, .(CATCH_QLOW = quantile(CATCH, 0.05), CATCH_QHIGH = quantile(CATCH, 0.95), CATCH_STD_QLOW = quantile(CATCH_STD, 0.05), CATCH_STD_QHIGH = quantile(CATCH_STD, 0.95)), keyby = .(FISHERY_CODE, FISHERY)]

FISHERY_COLORS = fishery_colors_for(NC_SP_YEAR)

## Non-standardized catch ####
NC_YEAR_FISHERY_LINEPLOT_FACETED = 
  ggplot(data = NC_SP_YEAR, aes(x  = YEAR, y = CATCH)) + 
  labs(x = "", y = "Total catch (x1,000 t)") + 
  geom_point(shape = 21, size = 0.7, stroke = 0.7, aes(color = FISHERY_CODE)) + 
  geom_line(aes(color = FISHERY_CODE)) + 
  geom_hline(data = NC_SP_YEAR_QUANTILES, aes(yintercept = CATCH_QLOW, color = FISHERY_CODE), linetype = "dashed") + 
  geom_hline(data = NC_SP_YEAR_QUANTILES, aes(yintercept = CATCH_QHIGH, color = FISHERY_CODE), linetype = "dashed") +
  scale_color_manual(values = FISHERY_COLORS$OUTLINE) +
  scale_fill_manual(values = FISHERY_COLORS$FILL) + 
  facet_wrap(~FISHERY, scale = "free_y", dir = "v", ncol = 4) + 
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed"))

ggsave(paste0("../outputs/charts/CATCH/NC_YEAR_FISHERY_LINEPLOT_FACETED_", WPTT_SPECIES, ".png"), NC_YEAR_FISHERY_LINEPLOT_FACETED, width = 12, height = 6.75)

## Standardized catch ####
NC_STD_YEAR_FISHERY_LINEPLOT_FACETED = 
  ggplot(data = NC_SP_YEAR, aes(x  = YEAR, y = CATCH_STD)) + 
  labs(x = "", y = "Standardized catch") + 
  geom_point(shape = 21, size = 0.7, stroke = 0.7, aes(color = FISHERY_CODE)) + 
  geom_line(aes(color = FISHERY_CODE)) + 
  scale_color_manual(values = FISHERY_COLORS$OUTLINE) +
  scale_fill_manual(values = FISHERY_COLORS$FILL) + 
  geom_hline(data = NC_SP_YEAR_QUANTILES, aes(yintercept = CATCH_STD_QLOW, color = FISHERY_CODE), linetype = "dashed") + 
  geom_hline(data = NC_SP_YEAR_QUANTILES, aes(yintercept = CATCH_STD_QHIGH, color = FISHERY_CODE), linetype = "dashed") +
  facet_wrap(~FISHERY, scale = "free_y", dir = "v", ncol = 4) + 
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed"))

ggsave(paste0("../outputs/charts/CATCH/NC_STD_YEAR_FISHERY_LINEPLOT_FACETED_", WPTT_SPECIES, ".png"), NC_STD_YEAR_FISHERY_LINEPLOT_FACETED, width = 12, height = 6.75)

# TOTAL CATCHES BY FISHERY GROUP ####

NC_SP_YEAR = NC_SP_RAISED[, .(CATCH = sum(CATCH/1000)), keyby = .(FISHERY_GROUP_CODE, FISHERY_GROUP, YEAR)]
NC_SP_YEAR[, CATCH_STD := CATCH/mean(CATCH), by = .(FISHERY_GROUP_CODE, FISHERY_GROUP)]

NC_SP_YEAR_QUANTILES = NC_SP_YEAR[, .(CATCH_QLOW = quantile(CATCH, 0.05), CATCH_QHIGH = quantile(CATCH, 0.95), CATCH_STD_QLOW = quantile(CATCH_STD, 0.05), CATCH_STD_QHIGH = quantile(CATCH_STD, 0.95)), keyby = .(FISHERY_GROUP_CODE, FISHERY_GROUP)]

FG_COLORS = fishery_group_colors_for(NC_SP_YEAR)

## Non-standardized catches ####
NC_YEAR_FISHERY_GROUP_LINEPLOT_FACETED = 
  ggplot(data = NC_SP_YEAR, aes(x  = YEAR, y = CATCH)) + 
  labs(x = "", y = "Total catch (x1,000 t)") + 
  geom_point(shape = 21, fill = "white", size = 0.7, stroke = 0.7, aes(color = FISHERY_GROUP_CODE)) + 
  geom_line(aes(color = FISHERY_GROUP_CODE)) + 
  geom_hline(data = NC_SP_YEAR_QUANTILES, aes(yintercept = CATCH_QLOW, color = FISHERY_GROUP_CODE), linetype = "dashed") + 
  geom_hline(data = NC_SP_YEAR_QUANTILES, aes(yintercept = CATCH_QHIGH, color = FISHERY_GROUP_CODE), linetype = "dashed") +
  scale_color_manual(values = FG_COLORS$OUTLINE) +
  scale_fill_manual(values = FG_COLORS$FILL) + 
  facet_wrap(~FISHERY_GROUP, scale = "free_y") + 
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed"))

ggsave(paste0("../outputs/charts/CATCH/NC_YEAR_FISHERY_GROUP_LINEPLOT_FACETED_", WPTT_SPECIES, ".png"), NC_YEAR_FISHERY_GROUP_LINEPLOT_FACETED, width = 8, height = 4.5)

## Standardized catches ####
NC_STD_YEAR_FISHERY_GROUP_LINEPLOT_FACETED = 
  ggplot(data = NC_SP_YEAR, aes(x  = YEAR, y = CATCH_STD)) + 
  labs(x = "", y = "Standardized catch") + 
  geom_point(shape = 21, fill = "white", size = 0.7, stroke = 0.7, aes(color = FISHERY_GROUP_CODE)) + 
  geom_line(aes(color = FISHERY_GROUP_CODE)) + 
  scale_color_manual(values = FG_COLORS$OUTLINE) +
  scale_fill_manual(values = FG_COLORS$FILL) + 
  geom_hline(data = NC_SP_YEAR_QUANTILES, aes(yintercept = CATCH_STD_QLOW, color = FISHERY_GROUP_CODE), linetype = "dashed") + 
  geom_hline(data = NC_SP_YEAR_QUANTILES, aes(yintercept = CATCH_STD_QHIGH, color = FISHERY_GROUP_CODE), linetype = "dashed") +
  facet_wrap(~FISHERY_GROUP, scale = "free_y") + 
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text = element_text(hjust = 0), legend.position = "none", panel.grid.minor = element_line(linetype = "dashed"))

ggsave(paste0("../outputs/charts/CATCH/NC_STD_YEAR_FISHERY_GROUP_LINEPLOT_FACETED_", WPTT_SPECIES, ".png"), NC_STD_YEAR_FISHERY_GROUP_LINEPLOT_FACETED, width = 8, height = 4.5)

# CLASSICAL BARPLOT OF TOTAL CATCHES BY FISHERY ####

## PNG ####
NUM_FISHERIES = length(unique(NC_SP_RAISED$FISHERY_CODE))
NUM_LEGEND_ROWS = ifelse(NUM_FISHERIES <= 6, 1, ifelse(NUM_FISHERIES <= 8, 2, 3))

NC_RAISED_BARPLOT_FISHERY = bar.catch(NC_SP_RAISED, C_FISHERY) + theme(axis.text.x = element_text(size = 8), legend.position = "bottom") + guides(fill = guide_legend(nrow = NUM_LEGEND_ROWS))

save_plot(paste0("../outputs/charts/CATCH/NC_RAISED_BARPLOT_FISHERY_", WPTT_SPECIES, ".png"), NC_RAISED_BARPLOT_FISHERY, 8, 4.5)

l_info("Catch indicators computed!")