l_info("Computing indicator of catches", WPTT_SPECIES)

# TOTAL CATCHES ####

NC_SP_YEAR = NC_SP_RAISED[, .(CATCH = sum(CATCH/1000)), keyby = .(FISHERY_GROUP, YEAR)]
NC_SP_YEAR[, CATCH_STD := round(CATCH/sum(CATCH)*100, 2), by = .(FISHERY_GROUP)]

## Non-standardized catches ####
NC_YEAR_FISHERY_GROUP_LINEPLOT_FACETED = 
  ggplot(data = NC_SP_YEAR, aes(x  = YEAR, y = CATCH)) + 
  labs(x = "", y = "Total catch (x1,000 t)") + 
  #geom_pointpath(col = "black", size = .6) + 
  geom_point(shape = 21, fill = "white", size = 0.8, stroke = 0.8) + 
  geom_line() + 
  facet_wrap(~FISHERY_GROUP) + 
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text = element_text(hjust = 0))

ggsave(paste0("../outputs/charts/CATCH/NC_YEAR_FISHERY_GROUP_LINEPLOT_FACETED_", WPTT_SPECIES, ".png"), NC_YEAR_FISHERY_GROUP_LINEPLOT_FACETED, width = 8, height = 6)

## Standardized catches ####
NC_STD_YEAR_FISHERY_GROUP_LINEPLOT_FACETED = 
  ggplot(data = NC_SP_YEAR, aes(x  = YEAR, y = CATCH_STD)) + 
  labs(x = "", y = "Standardized catch") + 
  geom_pointpath(col = "black", size = .6) + 
  facet_wrap(~FISHERY_GROUP) + 
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), strip.text = element_text(hjust = 0))

ggsave(paste0("../outputs/charts/CATCH/NC_STD_YEAR_FISHERY_GROUP_LINEPLOT_FACETED_", WPTT_SPECIES, ".png"), NC_STD_YEAR_FISHERY_GROUP_LINEPLOT_FACETED, width = 8, height = 6)

# TOTAL CATCHES BY FISHERY ####

## PNG ####
NUM_FISHERIES = length(unique(NC_SP_RAISED$FISHERY_CODE))
NUM_LEGEND_ROWS = ifelse(NUM_FISHERIES <= 6, 1, ifelse(NUM_FISHERIES <= 8, 2, 3))

NC_RAISED_BARPLOT_FISHERY = bar.catch(NC_SP_RAISED, C_FISHERY) + theme(axis.text.x = element_text(size = 8), legend.position = "bottom") + guides(fill = guide_legend(nrow = NUM_LEGEND_ROWS))

save_plot(paste0("../outputs/charts/CATCH/NC_RAISED_BARPLOT_FISHERY_", WPTT_SPECIES, ".png"), NC_RAISED_BARPLOT_FISHERY, 8, 4.5)

l_info("Catch indicator computed!")