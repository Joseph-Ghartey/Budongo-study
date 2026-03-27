# =============================================================================
# Diversity Indices Boxplots for Regeneration and Canopy Layers
# =============================================================================

# Load required libraries
library(tidyverse)
library(ggpubr)

# =============================================================================
# 1. Read in the diversity data
# =============================================================================

canopy_index <- read.csv("canopy_indexes.csv", sep = ",", header = TRUE)
reg_index    <- read.csv("reg_indexes.csv", sep = ",", header = TRUE)

# =============================================================================
# 2. Reshape from wide to long format (include ALL four diversity indices)
# =============================================================================
names(canopy_index)[names(canopy_index) == "Compartment"] <- "compartment"
names(reg_index)[names(reg_index) == "Compartment"] <- "compartment"


canopy_long <- canopy_index %>%
  pivot_longer(
    cols = c(pielou, shannon, Inv_simpson, simpson),
    names_to = "index",
    values_to = "value"
  )

reg_long <- reg_index %>%
  pivot_longer(
    cols = c(pielou, shannon, Inv_simpson, simpson),
    names_to = "index",
    values_to = "value"
  )

# =============================================================================
# 3. Rename and recode compartment column
# =============================================================================

canopy_long <- canopy_long %>%
  mutate(compartment = recode(compartment, "N3" = "logged", "N15" = "unlogged"))

reg_long <- reg_long %>%
  mutate(compartment = recode(compartment, "N3" = "logged", "N15" = "unlogged"))

# Standardise index labels to match the figure x-axis
canopy_long <- canopy_long %>%
  mutate(index = recode(index,
                        "Inv_simpson" = "Inv_simpson",
                        "pielou"      = "pielou",
                        "shannon"     = "shannon",
                        "simpson"     = "simpson"
  ))

reg_long <- reg_long %>%
  mutate(index = recode(index,
                        "Inv_simpson" = "Inv_simpson",
                        "pielou"      = "pielou",
                        "shannon"     = "shannon",
                        "simpson"     = "simpson"
  ))

# Set factor order to match figure (Inv_simpson, pielou, shannon, simpson)
index_order <- c("Inv_simpson", "pielou", "shannon", "simpson")
canopy_long$index <- factor(canopy_long$index, levels = index_order)
reg_long$index    <- factor(reg_long$index, levels = index_order)

# =============================================================================
# 4. Compute Wilcoxon p-values for each index (logged vs unlogged)
# =============================================================================

compute_pvalues <- function(df) {
  df %>%
    group_by(index) %>%
    summarise(
      p_value = wilcox.test(
        value[compartment == "logged"],
        value[compartment == "unlogged"]
      )$p.value,
      .groups = "drop"
    )
}

reg_pvals    <- compute_pvalues(reg_long)
canopy_pvals <- compute_pvalues(canopy_long)

# Format p-value labels (show "p<0.001" if below 0.001, else "p = x.xxx")
format_p <- function(p) {
  ifelse(p < 0.001, "p<0.001", paste0("p = ", sprintf("%.3f", p)))
}

reg_pvals    <- reg_pvals %>% mutate(label = format_p(p_value))
canopy_pvals <- canopy_pvals %>% mutate(label = format_p(p_value))

# Determine y-position for each p-value label (place above the max value)
get_y_positions <- function(df, pvals) {
  y_pos <- df %>%
    group_by(index) %>%
    summarise(y_max = max(value, na.rm = TRUE), .groups = "drop")
  pvals %>% left_join(y_pos, by = "index") %>%
    mutate(y_pos = y_max * 1.1)
}

reg_pvals    <- get_y_positions(reg_long, reg_pvals)
canopy_pvals <- get_y_positions(canopy_long, canopy_pvals)

# =============================================================================
# 5. Create boxplots
# =============================================================================

# Custom colours matching the figure
custom_fills <- c("logged" = "cyan", "unlogged" = "violet")

# --- Panel A: Regeneration Layer ---
reg_boxplot <- ggplot(reg_long, aes(x = index, y = value, fill = compartment)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.8) +
  geom_text(
    data = reg_pvals,
    aes(x = index, y = y_pos, label = label),
    inherit.aes = FALSE,
    size = 3.5, vjust = -0.3
  ) +
  labs(
    x = "Diversity Variable",
    y = "Diversity Value",
    title = "Diversity Indices of Regeneration Layer",
    fill = "compartment"
  ) +
  scale_fill_manual(values = custom_fills) +
  theme_minimal() +
  theme(
    panel.border    = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x     = element_text(size = 11),
    axis.text.y     = element_text(size = 11),
    axis.title.x    = element_text(size = 13),
    axis.title.y    = element_text(size = 13),
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    legend.position  = c(0.92, 0.92),
    legend.justification = c(1, 1)
  )

# --- Panel B: Canopy Layer ---
canopy_boxplot <- ggplot(canopy_long, aes(x = index, y = value, fill = compartment)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.8) +
  geom_text(
    data = canopy_pvals,
    aes(x = index, y = y_pos, label = label),
    inherit.aes = FALSE,
    size = 3.5, vjust = -0.3
  ) +
  labs(
    x = "Diversity Variable",
    y = "Diversity Value",
    title = "Diversity Indices of Canopy Layer",
    fill = "compartment"
  ) +
  scale_fill_manual(values = custom_fills) +
  theme_minimal() +
  theme(
    panel.border    = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x     = element_text(size = 11),
    axis.text.y     = element_text(size = 11),
    axis.title.x    = element_text(size = 13),
    axis.title.y    = element_text(size = 13),
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    legend.position  = c(0.92, 0.92),
    legend.justification = c(1, 1)
  )

# =============================================================================
# 6. Combine into a two-panel figure (A and B)
# =============================================================================

combined_plot <- ggarrange(
  reg_boxplot, canopy_boxplot,
  labels = c("A", "B"),
  ncol = 2, nrow = 1,
  common.legend = TRUE,
  legend = "right"
)

# Save the combined figure
ggsave("diversity_indices_boxplots.png", combined_plot,
       width = 14, height = 6, dpi = 300)

# Print to screen
print(combined_plot)
