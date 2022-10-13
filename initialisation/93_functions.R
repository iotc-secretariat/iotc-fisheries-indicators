charts_folder = function(dataset = "NC", species_code, subfolder) { 
  folder = paste0("../outputs/charts/", dataset)
  
  if(!is.na(species_code)) folder = paste0(folder, "/", species_code)
  if(!is.na(subfolder)) folder = paste0(folder, "/", subfolder)
  
  return(folder)
}

chart_file = function(dataset = "NC", species_code, subfolder, filename) {
  folder = charts_folder(dataset, species_code, subfolder)
  
  return(paste0(folder, "/", filename))
}

all_years_in_data = function(data_raw, data_est, data_raised) {
  FIRST_YEAR_RAW    = min(data_raw$YEAR)
  FIRST_YEAR_EST    = ifelse(!is_available(data_est),    FIRST_YEAR_RAW, min(data_est$YEAR))
  FIRST_YEAR_RAISED = ifelse(!is_available(data_raised), FIRST_YEAR_EST, min(data_raised$YEAR))
  
  if(FIRST_YEAR_EST != FIRST_YEAR_RAW)    warning(paste0("!!! Difference detected in first year between ESTIMATED (", FIRST_YEAR_EST,    ") and RAW (",    FIRST_YEAR_RAW,    ") data"))
  if(FIRST_YEAR_EST != FIRST_YEAR_RAISED) warning(paste0("!!! Difference detected in first year between ESTIMATED (", FIRST_YEAR_EST,    ") and RAISED (", FIRST_YEAR_RAISED, ") data"))
  if(FIRST_YEAR_RAISED != FIRST_YEAR_RAW) warning(paste0("!!! Difference detected in first year between RAISED (",    FIRST_YEAR_RAISED, ") and RAW (",    FIRST_YEAR_RAW,    ") data"))
  
  LAST_YEAR_RAW    = max(data_raw$YEAR)
  LAST_YEAR_EST    = ifelse(!is_available(data_est),    LAST_YEAR_RAW, max(data_est$YEAR))
  LAST_YEAR_RAISED = ifelse(!is_available(data_raised), LAST_YEAR_EST, max(data_raised$YEAR))
  
  if(LAST_YEAR_EST != LAST_YEAR_RAW)    warning(paste0("!!! Difference detected in last year between ESTIMATED (", LAST_YEAR_EST,    ") and RAW (",    LAST_YEAR_RAW,    ") data"))
  if(LAST_YEAR_EST != LAST_YEAR_RAISED) warning(paste0("!!! Difference detected in last year between ESTIMATED (", LAST_YEAR_EST,    ") and RAISED (", LAST_YEAR_RAISED, ") data"))
  if(LAST_YEAR_RAISED != LAST_YEAR_RAW) warning(paste0("!!! Difference detected in last year between RAISED (",    LAST_YEAR_RAISED, ") and RAW (",    LAST_YEAR_RAW,    ") data"))

  return (max(FIRST_YEAR_RAW, FIRST_YEAR_EST, FIRST_YEAR_RAISED):min(LAST_YEAR_RAW, LAST_YEAR_EST, LAST_YEAR_RAISED))
}

first_year = function(years) {
  return (head(sort(+years), 1))
}

last_year = function(years) {
  return (tail(sort(+years), 1))
}

last_n_years = function(years, n) {
  Y_E = last_year(years)
  Y_S = Y_E - n + 1
  
  return (Y_S:Y_E)
}

last_5_years = function(years) {
  return (last_n_years(years, 5))
}

last_10_years = function(years) {
  return (last_n_years(years, 10))
}

last_n_ennium = function(years, n = 5) {
  Y_S = floor(last_year(years) / n) * n # Beginning of last n-ennium
  Y_E = Y_S + n - 1                     # End of last n-ennium
  
  return (Y_S:Y_E)  
}

last_complete_n_ennium = function(years, n = 5) {
  LN = last_n_ennium(years, n)
  
  if(last_year(LN) > last_year(years)) {
    Y_E = first_year(LN) - 1
    Y_S = Y_E - n + 1
  } else {
    Y_S = first_year(LN)
    Y_E = last_year(LN)
  }
  
  return (Y_S:Y_E)
}
  
last_quinquennium = function(years) { 
  return (last_n_ennium(years, 5))
}

last_complete_quinquennium = function(years) { 
  return (last_complete_n_ennium(years, 5))
}

last_decade = function(years) { 
  return (last_n_ennium(years, 10))
}

last_complete_decade = function(years) { 
  return (last_complete_n_ennium(years, 10))
}

first_n_ennium = function(years, n = 5) {
  Y_S = floor(first_year(years) / n) * n # Beginning of last n-ennium
  Y_E = Y_S + n - 1                     # End of last n-ennium
  
  return (Y_S:Y_E)  
}

first_complete_n_ennium = function(years, n = 5) {
  FQ = first_n_ennium(years, n)
  
  if(first_year(FQ) < first_year(years)) {
    Y_S = last_year(FQ) + 1
    Y_E = Y_S + n - 1
  } else {
    Y_S = first_year(FQ)
    Y_E = last_year(FQ)
  }
  
  return (Y_S:Y_E)
}

first_quinquennium = function(years) { 
  return (first_n_ennium(years, 5))
}

first_complete_quinquennium = function(years) { 
  return (first_complete_n_ennium(years, 5))
}

first_decade = function(years) { 
  return (first_n_ennium(years, 10))
}

first_complete_decade = function(years) { 
  return (first_complete_n_ennium(years, 10))
}

fy   = first_year
ly   = last_year
lny  = last_n_years
l5y  = last_5_years
l10y = last_10_years
lq   = last_quinquennium
lcq  = last_complete_quinquennium
ld   = last_decade
lcd  = last_complete_decade
fq   = first_quinquennium
fcq  = first_complete_quinquennium
fd   = first_decade
fcd  = first_complete_decade

# Wrappers for decade extraction
floor_decade    = function(value){ return(value - value %% 10) }
ceiling_decade  = function(value){ return(floor_decade(value)+10) }
round_to_decade = function(value){ return(round(value / 10) * 10) }

# Rounding function
round_bin = function(size, bin_size) { return (floor(size / bin_size) * bin_size + bin_size/2) }

#prettyNum with default ',' as big mark
pn = function(number, big.mark = ",") {
  return(prettyNum(number, big.mark = big.mark))
}

#prettyNum with no big mark
pnn = function(number) {
  return(pn(number, ""))
}

#Round to hundreds
rh = function(number) {
  return(r_to(number, 100))
}

#Round to thousands
rt = function(number) {
  return(r_to(number, 1000))
}

#Round to tens of thousands
rtt = function(number) {
  return(r_to(number, 10000))
}

#Round to hundreds of thousands
rht = function(number) {
  return(r_to(number, 100000))
}

#Rounds to a base (a multiple of 10)
r_to = function(number, base) {
  return(round(number / base) * base) 
}

max_catch = function(data) {
  return(data[which.max(CATCH)])
}

# max_catch_y = function(data) {
#   return(data[, .(CATCH = sum(CATCH)), keyby = .(YEAR)][which.max(CATCH)])
# }

max_catch_y = function(data) {
  if(nrow(data)>0)
  {
    res = data[, .(CATCH = sum(CATCH)), keyby = .(YEAR)][which.max(CATCH)]
  } else {
    res = NA
  }
  return(res)
}

min_catch = function(data) {
  return(data[which.min(CATCH)])
}

# min_catch_y = function(data) {
#   return(data[, .(CATCH = sum(CATCH)), keyby = .(YEAR)][which.min(CATCH)])
# }

min_catch_y = function(data) {
  if(nrow(data)>0)
  {
    res = data[, .(CATCH = sum(CATCH)), keyby = .(YEAR)][which.min(CATCH)]
  } else {
    res = NA
  }
  return(res)
}

tot_catch = function(data) {
  return(sum(data$CATCH))  
}

# avg_catch_y = function(data) {
#   years = min(data$YEAR):max(data$YEAR)
#   return(tot_catch(data) / length(years))
# }

avg_catch_y = function(data) {
  if(nrow(data)>0)
  {
    years = min(data$YEAR):max(data$YEAR)
    res = tot_catch(data) / length(years)
    } else {
    res = NA
            }
  return(res)
}

mean_catch_y = function(data) {
  return(mean(data[, CATCH = sum(CATCH), keyby = .(YEAR)]$CATCH))
}

# Wrappers for catch aggregations
catches_by_year = function(data) {
  return(data[, .(CATCH = round(sum(CATCH))), keyby=.(YEAR)])
}

catches_by_year_and_fleet = function(data) {
  return(data[, .(CATCH = round(sum(CATCH))), keyby=.(YEAR, FLEET, FLEET_CODE)])
}

catches_by_year_and_fishery_type = function(data) {
  return(data[, .(CATCH = round(sum(CATCH))), keyby=.(YEAR, FISHERY_TYPE, FISHERY_TYPE_CODE)])
}

catches_by_year_and_fishery_group = function(data) {
  return(data[, .(CATCH = round(sum(CATCH))), keyby=.(YEAR, FISHERY_GROUP, FISHERY_GROUP_CODE)])
}

catches_by_year_and_fishery = function(data) {
  return(data[, .(CATCH = round(sum(CATCH))), keyby=.(YEAR, FISHERY, FISHERY_CODE)])
}

### Handling of fishery colors

fishery_colors_for_SF = function(sf_data) {
  fisheries = sort(unique(sf_data$AW_FISHERY))
  
  FISHERY_COLORS = data.table(
    FISHERY = fisheries,
    FILL    = SF_FISHERY_CONFIG[FISHERY_NAME %in% fisheries]$FISHERY_COLOR,
    OUTLINE = darken(SF_FISHERY_CONFIG[FISHERY_NAME %in% fisheries]$FISHERY_COLOR, amount = 0.2)
  )
  
  return(FISHERY_COLORS)
}

### Utility methods to handle plots etc.

no_legend = function(plot) {
  return(plot + theme(legend.position = "none"))
}

### Handling of georeferenced pie maps

#### CATCHES (from the Catch-and-Effort dataset)

produce_map_decade = function(data, decade, value, fill_by, reference_value = 20000, unit = NA, show_scatterpie_legend = FALSE, custom_colors = NA) {
  geo_map = geo.grid.pie(data[YEAR >= decade & YEAR < min(decade + 10, 2019)],
                         value,
                         fill_by = fill_by,
                         colors = custom_colors,
                         reference_value = reference_value,
                         use_centroid = GEO_PIEMAP_USE_CENTROID,
                         opacity = GEO_PIEMAP_OPACITY,
                         show_high_seas = GEO_SHOW_HS,
                         show_scatterpie_legend = show_scatterpie_legend,
                         unit = unit,
                         legend_title = "" # paste0("Average catches, ", decade, "-", min(decade + 9, 2019), " (t / year)")
  )
  
  return(geo_map)
}

produce_map_year = function(data, year, value, fill_by, reference_value = 20000, unit = NA, show_scatterpie_legend = FALSE, custom_colors = NA) {
  geo_map = geo.grid.pie(data[YEAR == year],
                         value,
                         fill_by = fill_by,
                         colors = custom_colors,
                         reference_value = reference_value,
                         use_centroid = GEO_PIEMAP_USE_CENTROID,
                         opacity = GEO_PIEMAP_OPACITY,
                         show_high_seas = GEO_SHOW_HS,
                         show_scatterpie_legend = show_scatterpie_legend,
                         unit = unit,
                         legend_title = "" #paste0("Average catches, ", year, " (t / year)")
                         
  )
  
  return(geo_map)
}

produce_catch_map_decade = function(data, decade, fill_by, reference_value, unit = "t", show_scatterpie_legend = FALSE) {
  l_info(paste0("Producing catch map for decade (", decade, ")"), WPB_SPECIES)
  return(produce_map_decade(data, decade, CATCH, fill_by, reference_value, unit, show_scatterpie_legend = show_scatterpie_legend))
}

produce_catch_map_year = function(data, year, fill_by, reference_value, unit = "t", show_scatterpie_legend = FALSE) {
  l_info(paste0("Producing catch map for year (", year, ")"), WPB_SPECIES)
  return(produce_map_year(data, year, CATCH, fill_by, reference_value, unit, show_scatterpie_legend = show_scatterpie_legend))
}

produce_effort_map_decade = function(data, decade, fleet_colors, show_scatterpie_legend = FALSE) {
  data = data[YEAR >= decade & YEAR < min(decade + 10, 2019)]
  
  fleet_colors = fleet_colors[FILL_BY %in% unique(data$LL_FLEET)]
  
  data$EFFORT = data$EFFORT / 1E06
  
  geo_map = geo.grid.pie(data,
                         EFFORT,
                         fill_by = "LL_FLEET",
                         colors = fleet_colors,
                         reference_value = 10, #Million hooks
                         use_centroid = GEO_PIEMAP_USE_CENTROID,
                         opacity = GEO_PIEMAP_OPACITY,
                         show_high_seas = GEO_SHOW_HS,
                         show_scatterpie_legend = show_scatterpie_legend,
                         unit = "M hooks",
                         yearly_average = TRUE,
                         legend_title = "" # paste0("Average catches, ", decade, "-", min(decade + 9, 2019), " (t / year)")
  )
  
  return(geo_map)
}

produce_effort_map_year = function(data, year, fleet_colors, show_scatterpie_legend = FALSE, custom_colors) {
  data = data[YEAR == year]
  
  fleet_colors = fleet_colors[FILL_BY %in% unique(data$LL_FLEET)]
  
  data$EFFORT = data$EFFORT / 1E06
  
  geo_map = geo.grid.pie(data,
                         EFFORT,
                         fill_by = "LL_FLEET",
                         colors = fleet_colors,
                         reference_value = 10, #Million hooks
                         use_centroid = GEO_PIEMAP_USE_CENTROID,
                         opacity = GEO_PIEMAP_OPACITY,
                         show_high_seas = GEO_SHOW_HS,
                         show_scatterpie_legend = show_scatterpie_legend,
                         unit = "M hooks",
                         yearly_average = TRUE,
                         legend_title = "" # paste0("Average catches, ", decade, "-", min(decade + 9, 2019), " (t / year)")
  )
  
  return(geo_map)
}

### Handling of effort heatmaps

EF_BREAKS  = c(0, 1, 2, 5, 10, 15, 25, 50, 100, 150, 250, 500)
GRID    = grid_1x1
PALETTE = heat.colors
ALT_PALETTE = function(n) { return(rev(rainbow_hcl(n = n))) }

produce_effort_heatmap_decade = function(data, decade, custom_palette = PALETTE) {
  geo.grid.heatmap(data[YEAR >= decade & YEAR < decade + 10], 
                   value = EFFORT, 
                   yearly_average = TRUE,
                   standard_grid = GRID, 
                   show_high_seas = TRUE,
                   breaks = EF_BREAKS,
                   palette = custom_palette,
                   unit = "fishing days", legend_title = "Average effort")
}

produce_effort_heatmap_year = function(data, year, custom_palette = PALETTE) {
  geo.grid.heatmap(data[YEAR == year], 
                   value = EFFORT, 
                   yearly_average = TRUE,
                   standard_grid = GRID, 
                   show_high_seas = TRUE,
                   breaks = EF_BREAKS,
                   palette = custom_palette,
                   unit = "fishing days", legend_title = "Average effort")
}

increase_legend_text = function(plot, title_size = 15, text_size = 13) {
  if(!is.na(title_size)) plot = plot + theme(legend.title = element_text(size = title_size))
  if(!is.na(text_size)) plot = plot + theme(legend.text = element_text(size = text_size))
  
  return(plot)
}

increase_x_axis_text = function(plot, title_size = 15, text_size = NA) {
  if(!is.na(title_size)) plot = plot + theme(axis.title.x = element_text(size = title_size))
  if(!is.na(text_size)) plot = plot + theme(axis.text.x = element_text(size = text_size))
  
  return(plot)
}

increase_y_axis_text = function(plot, title_size = 15, text_size = NA) {
  if(!is.na(title_size)) plot = plot + theme(axis.title.y = element_text(size = title_size))
  if(!is.na(text_size)) plot = plot + theme(axis.text.y = element_text(size = text_size))
  
  return(plot)
}

increase_axis_text = function(plot, title_size = 15, text_size = NA) {
  return(
    increase_y_axis_text(
      increase_x_axis_text(plot, title_size, text_size), 
      title_size, text_size
    )
  )
}

update_QS_plots_legend = function(qs_plot) {
  return (qs_plot + theme(legend.text = element_text(size = 8),
                          legend.title = element_text(size = 10)))
}

prepare_flextable = function(x, font_family = "Calibri", font_size_header = 10, font_size_body = 10, 
                             default_bg_highlight   = TRUE, 
                             default_text_highlight = TRUE) {
  FT = 
    flextable(data = x) %>%
    flextable::font(part = "all", fontname = font_family) %>%
    fontsize(part = "header", size = font_size_header) %>%
    fontsize(part = "body",   size = font_size_body) %>%
    border_outer(fp_border(width = 2)) %>%
    align(j = 2:ncol(x), part = "header", align = "center") %>%
    colformat_double(digits = 0, big.mark = ",") %>%
    autofit()
  
  if(default_bg_highlight) {
    FT = 
      FT %>% 
      bg(part = "all", bg = "lightgrey", i = 1) %>%
      bg(part = "all", bg = "lightgrey", j = 1)
  }
  
  if(default_text_highlight) {
    FT = 
      FT %>%
      bold(part = "header") %>%
      bold(j = 1)
  }
  
  return(FT)
}

# catch_table_colourer = function(data, color_from = "wheat", color_to = "green") {
#   return(
#     col_numeric(
#       palette = c(color_from, color_to),
#       domain = c(min(data$CATCH), max(data$CATCH))
#     )
#   )
# }

catch_table_colourer = function(data, color_from = "wheat", color_to = "green") {
  return(
    col_numeric(
      palette = c(color_from, color_to),
      domain = c(0, ceiling(max(data$CATCH)))
    )
  )
}

catch_trend_for_fg = function(data, fishery_group_code) {
  FGC = colors_for_fishery_group(fishery_group_code)
  
  return(
    catch_last_trends_bar_for(
      data[YEAR %in% NC_RP_Y & FISHERY_GROUP_CODE == fishery_group_code], 
      categorize_by = FLEET_CODE, 
      fill = FGC$FILL,
      outline = FGC$OUTLINE
    )
  )
}

catch_trend_for = function(data, fishery_code) {
  FC = colors_for_fishery(fishery_code)
  
  return(
    catch_last_trends_bar_for(
      data[YEAR %in% NC_RP_Y & FISHERY_CODE == fishery_code], 
      categorize_by = FLEET_CODE, 
      fill = FC$FILL,
      outline = FC$OUTLINE
    )
  )
}

load_environment = function(file_name, envir = parent.frame()) {
  l_info(paste0("Loading environment from ", file_name, "..."))
  load(file_name, envir)
  l_info(paste0("Finished loading environment from ", file_name, "!"))
}

save_environment = function(file_name, excluded_object_names = NA, include_plots = FALSE, include_functions = FALSE) {
  l_info(paste0("Saving environment to ", file_name, " [ include_plots: ", include_plots, ", include_functions: ", include_functions, "]..."))
  current_environment = globalenv()
  environment_to_save = c()
  
  if(!is_available(excluded_object_names)) {
    excluded_object_names = c()
  }
  
  excluded_object_names = append(excluded_object_names, c("file_name", 
                                                          "include_plots", 
                                                          "include_functions", 
                                                          "current_environment", 
                                                          "excluded_object_names")) 
  
  for(object_name in ls(current_environment)) {
    object_type = class(get(object_name))
    
    if(!object_name %in% excluded_object_names &
       (!"function" %in% object_type | include_functions) & 
       (!"ggplot" %in% object_type | include_plots)
    ) {
      environment_to_save = append(environment_to_save, object_name)
    }
  }
  
  save(file = file_name, list = environment_to_save)
  
  l_info(paste0("Finished saving environment to ", file_name, "..."))
  
  return(environment_to_save)
}

# Convert data.frame or data.table to formatted flextable

flextabularisation = function(DataTable){
  
  FLEXTABLE = 
    DataTable %>%
    flextable() %>%
    flextable::font(fontname = "Calibri", part = "all") %>%
    flextable::fontsize(size = 10, part = "all") %>%
    bg(part = "header", bg = "grey") %>%
    bold(part = "header", bold = TRUE) %>%
    bg(i = 1, part = "body", bg = "white") %>%
    colformat_int(j = 1, big.mark = "") %>% 
    border_inner(border = fp_border(style = "dotted")) %>%
    border_outer(border = fp_border(width = 2)) %>%
    align  (i = 1, align = "center", part = "header") %>%
    align  (j = 1, align = "center", part = "all") %>%
    # merge_v(j = c(1, 2)) %>%
    valign (j = c(1, 2), valign = "top", part = "body") %>% 
    fix_border_issues() %>%
    autofit()
  
  return(FLEXTABLE)
}

