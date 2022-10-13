print("Initializing IOTC libs...")

# Install/load libraries required for analysis
pacman::p_load("iotc.base.common.plots")

# Initialise species colors
initialize_all_species_colors()
initialize_all_gears_colors()

ALL_FI_COLORS = all_fishery_colors()

print("IOTC libs fully initialized")