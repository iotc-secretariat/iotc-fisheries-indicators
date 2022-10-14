d_info("Initializing EF...")

DEFAULT_EFFORT_PLOTS_WIDE_WIDTH  = 13
DEFAULT_EFFORT_PLOTS_WIDE_HEIGHT = 16

### RAW LL EFFORT MAPS BY FISHERY

EF_LL_HOOKS_RAW = raw.EF(effort_unit_codes = "HOOKS", fishery_group_codes = "LL")

EF_LL_HOOKS_RAW[, LL_FLEET := "OTLL"]
EF_LL_HOOKS_RAW = EF_LL_HOOKS_RAW[FISHERY_CODE == "LLD" & FLEET_CODE == "JPN", LL_FLEET := "LLJP"]
EF_LL_HOOKS_RAW = EF_LL_HOOKS_RAW[FISHERY_CODE == "LLD" & FLEET_CODE == "TWN", LL_FLEET := "LLTW"]
EF_LL_HOOKS_RAW = EF_LL_HOOKS_RAW[FISHERY_CODE == "LLF", LL_FLEET := "FTLL"]
EF_LL_HOOKS_RAW = EF_LL_HOOKS_RAW[GEAR_CODE %in% c("ELL", "ELLOB"), LL_FLEET := "SWLL"]

EF_LL_HOOKS_RAW$LL_FLEET = 
  factor(
    EF_LL_HOOKS_RAW$LL_FLEET,
    levels = c("LLJP", "LLTW", "SWLL", "FTLL", "OTLL"),
    labels = c("Deep-freezing (JPN)",
               "Deep-freezing (TWN)",
               "Swordfish longliners",
               "Fresh-tuna longliners",
               "Other longliners"),
    ordered = TRUE
  )

EF_LL_HOOKS_COLORS = color_table(unique_colors(length(unique(EF_LL_HOOKS_RAW$LL_FLEET))))
EF_LL_HOOKS_COLORS$FILL_BY = sort(unique(EF_LL_HOOKS_RAW$LL_FLEET))

  # EF by decade and fishery
  
  ELD1 = produce_effort_map_decade(EF_LL_HOOKS_RAW, min(D1), EF_LL_HOOKS_COLORS) + labs(title = paste("a.", paste(D1, collapse = "-")))
  ELD2 = produce_effort_map_decade(EF_LL_HOOKS_RAW, min(D2), EF_LL_HOOKS_COLORS) + labs(title = paste("b.", paste(D2, collapse = "-")))
  ELD3 = produce_effort_map_decade(EF_LL_HOOKS_RAW, min(D3), EF_LL_HOOKS_COLORS) + labs(title = paste("c.", paste(D3, collapse = "-")))
  ELD4 = produce_effort_map_decade(EF_LL_HOOKS_RAW, min(D4), EF_LL_HOOKS_COLORS) + labs(title = paste("d.", paste(D4, collapse = "-")))
  ELD5 = produce_effort_map_decade(EF_LL_HOOKS_RAW, min(D5), EF_LL_HOOKS_COLORS) + labs(title = paste("e.", paste(D5, collapse = "-")))
  ELD6 = produce_effort_map_decade(EF_LL_HOOKS_RAW, min(D6), EF_LL_HOOKS_COLORS, show_scatterpie_legend = TRUE)
  
  ELD6 = ELD6 + 
    labs(title = paste("f.", paste(D6, collapse = "-"))) + 
    theme(legend.text = element_text(size = LEGEND_TEXT_SIZE_SMALLER),
          legend.spacing.x = unit(.7, 'cm')) + 
    guides(fill = guide_legend(title = NULL, nrow = 1))
  
  #ELD7 = produce_effort_map_decade(EF_LL_HOOKS_RAW, 2010, EF_LL_HOOKS_COLORS) + labs(title = "a. 2010-2019") 
  
  ELDG = ggarrange(no_legend(ELD1), no_legend(ELD2), 
                   no_legend(ELD3), no_legend(ELD4), 
                   no_legend(ELD5),  no_legend(ELD6), 
                   common.legend = FALSE,
                   legend.grob = get_legend(ELD6),
                   legend = "bottom",
                   nrow = 3, ncol = 2) 
  
  save_plot(chart_file("EF", NA, "LL", "PIEMAP_FLEET_DECADES.png"), ELDG, DEFAULT_EFFORT_PLOTS_WIDE_WIDTH, DEFAULT_EFFORT_PLOTS_WIDE_HEIGHT)
  
  ELDGPPT = ggarrange(no_legend(ELD1), no_legend(ELD2), no_legend(ELD3), 
                      no_legend(ELD4), no_legend(ELD5), no_legend(ELD6), 
                      common.legend = FALSE,
                      legend.grob = get_legend(ELD6),
                      legend = "bottom",
                      nrow = 2, ncol = 3) 
  
  save_plot(chart_file("EF", NA, "LL", "PIEMAP_FLEET_DECADES_wide.png"), ELDGPPT, DEFAULT_PLOT_WIDTH, DEFAULT_PLOT_HEIGHT)

  # EF by year and fishery
  
  ELY1 = produce_effort_map_year(EF_LL_HOOKS_RAW, Y1, EF_LL_HOOKS_COLORS) + labs(title = paste("a.", Y1))
  ELY2 = produce_effort_map_year(EF_LL_HOOKS_RAW, Y2, EF_LL_HOOKS_COLORS) + labs(title = paste("b.", Y2))
  ELY3 = produce_effort_map_year(EF_LL_HOOKS_RAW, Y3, EF_LL_HOOKS_COLORS) + labs(title = paste("c.", Y3))
  ELY4 = produce_effort_map_year(EF_LL_HOOKS_RAW, Y4, EF_LL_HOOKS_COLORS) + labs(title = paste("d.", Y4))
  ELY5 = produce_effort_map_year(EF_LL_HOOKS_RAW, YL, EF_LL_HOOKS_COLORS) + labs(title = paste("e.", YL))
  ELD0 = produce_effort_map_decade(EF_LL_HOOKS_RAW, min(DL), EF_LL_HOOKS_COLORS, show_scatterpie_legend = TRUE)

  ELD0 = ELD0 + 
    labs(title = paste("f.", paste(DL, collapse = "-"), "(decade)")) + 
    theme(legend.text = element_text(size = LEGEND_TEXT_SIZE_SMALLER),
          legend.spacing.x = unit(.7, 'cm')) + 
    guides(fill = guide_legend(title = NULL, nrow = 1))
  
  ELYG = ggarrange(no_legend(ELY1), no_legend(ELY2), 
                   no_legend(ELY3), no_legend(ELY4), 
                   no_legend(ELY5), no_legend(ELD0),
                   common.legend = FALSE,
                   legend.grob = get_legend(ELD0),
                   legend = "bottom",
                   nrow = 3, ncol = 2) 
  
  save_plot(chart_file("EF", NA, "LL", "PIEMAP_FLEET_YEARS.png"), ELYG, DEFAULT_EFFORT_PLOTS_WIDE_WIDTH, DEFAULT_EFFORT_PLOTS_WIDE_HEIGHT)
  
  ELYGPPT = ggarrange(no_legend(ELY1), no_legend(ELY2), no_legend(ELY3), 
                      no_legend(ELY4), no_legend(ELY5), no_legend(ELD0),
                      common.legend = FALSE,
                      legend.grob = get_legend(ELD0),
                      legend = "bottom",
                      nrow = 2, ncol = 3) 
  
  save_plot(chart_file("EF", NA, "LL", "PIEMAP_FLEET_YEARS_wide.png"), ELYGPPT, DEFAULT_PLOT_WIDTH, DEFAULT_PLOT_HEIGHT)

### RAW PS EFFORT HEATMAPS BY FLEET

EF = raw.EF(fishery_group_codes = "PS")[FISHERY_TYPE_CODE == "IND"][EFFORT_UNIT_CODE %in% c("SETS", "FHOURS", "FDAYS")]

### 13 FISHING HOURS = 1 FISHING DAY
EF[EFFORT_UNIT_CODE == "FHOURS", `:=`(EFFORT = EFFORT / 13, EFFORT_UNIT_CODE = "FDAYS")] 

### 1 SET = 1 FISHING DAY
EF[EFFORT_UNIT_CODE == "SETS", `:=`(EFFORT = EFFORT, EFFORT_UNIT_CODE = "FDAYS")]

EF_EU = EF[ FLEET_CODE %in% c("EUESP", "EUFRA", "EUITA", "SYC")]
EF_OT = EF[!FLEET_CODE %in% c("EUESP", "EUFRA", "EUITA", "SYC")]

  # EF by decade and EUPS
  
  EPD1 = produce_effort_heatmap_decade(EF_EU, min(D4)) + labs(title = paste("a.", paste(D4, collapse = "-"), "(EU)"))
  EPD3 = produce_effort_heatmap_decade(EF_EU, min(D5)) + labs(title = paste("c.", paste(D5, collapse = "-"), "(EU)"))
  EPD5 = produce_effort_heatmap_decade(EF_EU, min(D6)) + labs(title = paste("e.", paste(D6, collapse = "-"), "(EU)"))
  
  # EF by decade and EUOT
  
  EPD2 = produce_effort_heatmap_decade(EF_OT, min(D4)) + labs(title = paste("b.", paste(D4, collapse = "-"), "(OT)"))
  EPD4 = produce_effort_heatmap_decade(EF_OT, min(D5)) + labs(title = paste("d.", paste(D5, collapse = "-"), "(OT)"))
  EPD6 = produce_effort_heatmap_decade(EF_OT, min(D6)) + labs(title = paste("f.", paste(D6, collapse = "-"), "(OT)"))
  
  EPD5 = EPD5 + 
    theme(legend.title = element_blank(),
          legend.text = element_text(size = LEGEND_TEXT_SIZE_SMALLER)) + 
    guides(fill = guide_legend(nrow = 1))
  
  #EPD7 = produce_effort_heatmap_decade(EF_EU, 2010) + labs(title = "a. 2010-2019 (EU)") 
  
  EPDG = ggarrange(no_legend(EPD1), no_legend(EPD2), 
                   no_legend(EPD3), no_legend(EPD4), 
                   no_legend(EPD5), no_legend(EPD6), 
                   common.legend = FALSE,
                   legend.grob = get_legend(EPD5),
                   legend = "bottom",
                   nrow = 3, ncol = 2) 
  
  save_plot(chart_file("EF", NA, "PS", "HEATMAP_DECADES.png"), EPDG, DEFAULT_EFFORT_PLOTS_WIDE_WIDTH, DEFAULT_EFFORT_PLOTS_WIDE_HEIGHT)

  # EF by year and EUPS

  EPY1 = produce_effort_heatmap_year(EF_EU, Y1) + labs(title = paste("a.", Y1))
  EPY2 = produce_effort_heatmap_year(EF_EU, Y2) + labs(title = paste("b.", Y2))
  EPY3 = produce_effort_heatmap_year(EF_EU, Y3) + labs(title = paste("c.", Y3))
  EPY4 = produce_effort_heatmap_year(EF_EU, Y4) + labs(title = paste("d.", Y4))
  EPY5 = produce_effort_heatmap_year(EF_EU, YL) + labs(title = paste("e.", YL))
  EPD0 = produce_effort_heatmap_decade(EF_EU, min(DL)) + labs(title = paste("f.", paste(DL, collapse = "-"), "(decade)"))
  
  EPD0 = EPD0 + 
    theme(legend.title = element_blank(),
          legend.text = element_text(size = LEGEND_TEXT_SIZE_SMALLER)) + 
    guides(fill = guide_legend(nrow = 1))
  
  EPYG = ggarrange(no_legend(EPY1), no_legend(EPY2), 
                   no_legend(EPY3), no_legend(EPY4), 
                   no_legend(EPY5), no_legend(EPD0),
                   common.legend = FALSE,
                   legend.grob = get_legend(EPD0),
                   legend = "bottom",
                   nrow = 3, ncol = 2) 
  
  save_plot(chart_file("EF", NA, "PS", "HEATMAP_YEARS_EU.png"), EPYG, DEFAULT_EFFORT_PLOTS_WIDE_WIDTH, DEFAULT_EFFORT_PLOTS_WIDE_HEIGHT)
  
  EPYGPPT = ggarrange(no_legend(EPY1), no_legend(EPY2), no_legend(EPY3), 
                      no_legend(EPY4), no_legend(EPY5), no_legend(EPD0),
                      common.legend = FALSE,
                      legend.grob = get_legend(EPD0),
                      legend = "bottom",
                      nrow = 2, ncol = 3) 
  
  save_plot(chart_file("EF", NA, "PS", "HEATMAP_YEARS_EU_wide.png"), EPYGPPT, DEFAULT_PLOT_WIDTH, DEFAULT_PLOT_HEIGHT)
  
  # EF by year and OTPS
  
  EOY1 = produce_effort_heatmap_year(EF_OT, Y1) + labs(title = paste("a.", Y1))
  EOY2 = produce_effort_heatmap_year(EF_OT, Y2) + labs(title = paste("b.", Y2))
  EOY3 = produce_effort_heatmap_year(EF_OT, Y3) + labs(title = paste("c.", Y3))
  EOY4 = produce_effort_heatmap_year(EF_OT, Y4) + labs(title = paste("d.", Y4))
  EOY5 = produce_effort_heatmap_year(EF_OT, YL) + labs(title = paste("e.", YL))
  EOD0 = produce_effort_heatmap_decade(EF_OT, min(DL)) + labs(title = paste("f.", paste(DL, collapse = "-"), "(decade)"))
  
  EOD0 = EOD0 + 
    theme(legend.title = element_blank(),
          legend.text = element_text(size = LEGEND_TEXT_SIZE_SMALLER)) + 
    guides(fill = guide_legend(nrow = 1))
  
  EOYG = ggarrange(no_legend(EOY1), no_legend(EOY2), 
                   no_legend(EOY3), no_legend(EOY4), 
                   no_legend(EOY5), no_legend(EOD0),
                   common.legend = FALSE,
                   legend.grob = get_legend(EOD0),
                   legend = "bottom",
                   nrow = 3, ncol = 2) 
  
  save_plot(chart_file("EF", NA, "PS", "HEATMAP_YEARS_OT.png"), EOYG, DEFAULT_EFFORT_PLOTS_WIDE_WIDTH, DEFAULT_EFFORT_PLOTS_WIDE_HEIGHT)

d_info("EF initialized...")