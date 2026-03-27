# =============================================================================
# Rarefaction and Extrapolation Curves for Canopy and Regeneration Layers
# Using the iNEXT package
# =============================================================================

# Load required libraries
library(tidyverse)
library(iNEXT)

# =============================================================================
# 1. Read and prepare data
# =============================================================================

# Read the canopy layer data
canopy <- read.csv("canopy_plotid.csv")

# Read the regeneration layer data
reg <- read.csv("reg_plotid.csv")

# Rename columns: 'spec' -> 'species', 'comp'/'compartment' -> 'compartment'
canopy <- canopy %>%
  rename(species = spec)

reg <- reg %>%
  rename(species = spec,
         compartment = comp)

# Map compartment codes to descriptive names in regeneration data
# N3 = logged, N15 = unlogged
reg <- reg %>%
  mutate(compartment = case_when(
    compartment == "N3"  ~ "logged",
    compartment == "N15" ~ "unlogged"
  ))

# =============================================================================
# 2. Aggregate abundance by species and compartment (for each layer)
# =============================================================================

# Canopy layer: total abundance per species per compartment
canopy_agg <- canopy %>%
  group_by(compartment, species) %>%
  summarise(abundance = sum(abundance), .groups = "drop")

# Regeneration layer: total abundance per species per compartment
reg_agg <- reg %>%
  group_by(compartment, species) %>%
  summarise(abundance = sum(abundance), .groups = "drop")

# =============================================================================
# 3. Convert to named abundance vectors (input format for iNEXT)
# =============================================================================

# Function to create a named abundance vector from aggregated data
make_abundance_vector <- function(df, comp_name) {
  df_sub <- df %>% filter(compartment == comp_name)
  abund <- df_sub$abundance
  names(abund) <- df_sub$species
  return(abund)
}

# Canopy layer abundance vectors
canopy_logged   <- make_abundance_vector(canopy_agg, "logged")
canopy_unlogged <- make_abundance_vector(canopy_agg, "unlogged")

# Regeneration layer abundance vectors
reg_logged   <- make_abundance_vector(reg_agg, "logged")
reg_unlogged <- make_abundance_vector(reg_agg, "unlogged")

# Create named lists for iNEXT
canopy_list <- list(logged = canopy_logged, unlogged = canopy_unlogged)
reg_list    <- list(logged = reg_logged, unlogged = reg_unlogged)

# =============================================================================
# 4. Run iNEXT analysis
# =============================================================================

# Run iNEXT for both layers (q = 0 for species richness)
canopy_inext <- iNEXT(canopy_list, q = 0, datatype = "abundance")
reg_inext    <- iNEXT(reg_list, q = 0, datatype = "abundance")

# =============================================================================
# 5. Plot rarefaction and extrapolation curves
# =============================================================================

# Define custom colours matching the figure: pink for logged, cyan for unlogged
custom_colours <- c("logged" = "#FF69B4", "unlogged" = "#00CED1")

# --- Panel A: Canopy Layer ---
p_canopy <- ggiNEXT(canopy_inext, type = 1) +
  scale_colour_manual(values = custom_colours) +
  scale_fill_manual(values = custom_colours) +
  scale_shape_manual(values = c(logged = 16, unlogged = 17)) +
  labs(
    title = "Rarefaction Curves for Canopy Layer",
    x = "Number of individuals",
    y = "Species diversity"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# --- Panel B: Regeneration Layer ---
p_reg <- ggiNEXT(reg_inext, type = 1) +
  scale_colour_manual(values = custom_colours) +
  scale_fill_manual(values = custom_colours) +
  scale_shape_manual(values = c(logged = 16, unlogged = 17)) +
  labs(
    title = "Rarefaction Curves for Regeneration Layer",
    x = "Number of individuals",
    y = "Species diversity"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# =============================================================================
# 6. Combine into a two-panel figure (A and B)
# =============================================================================

library(ggpubr)

combined_plot <- ggarrange(
  p_canopy, p_reg,
  labels = c("A", "B"),
  ncol = 2, nrow = 1,
  common.legend = TRUE,
  legend = "bottom"
)

# Save the combined figure
ggsave("rarefaction_curves.png", combined_plot, width = 13, height = 7, dpi = 300)

# Print to screen
print(combined_plot)
