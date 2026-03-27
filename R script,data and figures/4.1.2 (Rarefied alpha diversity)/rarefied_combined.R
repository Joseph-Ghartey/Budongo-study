# Clear environment
rm(list=ls())

# Loading packages
library(vegan)
library(ggplot2)
library(tidyverse)
library(tidyr)


#this is done for the logged, upload the unlogged and repeat the process

rarefied_logged<-read.csv('rarefied_logged.csv', header = TRUE, sep = ',')

rarefied_unlogged<-read.csv('rarefied_unlogged.csv', header = TRUE, sep = ',')

setdiff(rarefied_logged$plot.id, rarefied_logged$plot.id)
setdiff(rarefied_unlogged$plot.id, rarefied_unlogged$plot.id)

# plot.id as character
rarefied_logged$plot.id <- as.character(rarefied_logged$plot.id)
rarefied_unlogged$plot.id <- as.character(rarefied_unlogged$plot.id)


# Check the structure 
cat("Data Structure:\n")
str(rarefied_unlogged)
cat("First Few Rows:\n")
print(head(rarefied_unlogged))

cat("Data Structure:\n")
str(rarefied_logged)
cat("First Few Rows:\n")
print(head(rarefied_logged))


# Check unique values in the 'layer' column
cat("Unique Values in 'layer' Column:\n")
print(unique(rarefied_logged$layer))

cat("Unique Values in 'layer' Column:\n")
print(unique(rarefied_unlogged$layer))


# Create canopy and regeneration data subsets 
canopy_rarefied <- rarefied_logged[rarefied_logged$layer == "canopy", ]
regen_rarefield <- rarefied_logged[rarefied_logged$layer == "regeneration", ]

canopy_rarefied1 <- rarefied_unlogged[rarefied_unlogged$layer == "canopy", ]
regen_rarefield1 <- rarefied_unlogged[rarefied_unlogged$layer == "regeneration", ]


# checking plot id
common_plots <- intersect(canopy_rarefied$plot.id, regen_rarefield$plot.id)
cat("Common Plots:\n")
print(common_plots)

common_plots1 <- intersect(canopy_rarefied1$plot.id, regen_rarefield1$plot.id)
cat("Common Plots:\n")
print(common_plots1)

# Filter the data 
canopy_rarefied <- canopy_rarefied[canopy_rarefied$plot.id %in% common_plots, ]
regen_rarefield <- regen_rarefield[regen_rarefield$plot.id %in% common_plots, ]

canopy_rarefied1 <- canopy_rarefied1[canopy_rarefied1$plot.id %in% common_plots1, ]
regen_rarefield1 <- regen_rarefield1[regen_rarefield1$plot.id %in% common_plots1, ]

# Merge the canopy and regeneration data on plot.id
merged_data <- merge(canopy_rarefied, regen_rarefield, by = "plot.id", suffixes = c("_canopy", "_regen"))

merged_data1 <- merge(canopy_rarefied1, regen_rarefield1, by = "plot.id", suffixes = c("_canopy", "_regen"))


# verify merged data content
cat("Merged Data1:\n")
print(head(merged_data1))

cat("Merged Data:\n")
print(head(merged_data))


merged_data$Forest_Type <- 'Logged'
merged_data1$Forest_Type <- 'Unlogged'

names(merged_data)[names(merged_data) == 'Forest_Type']<-'Forest Type'
names(merged_data1)[names(merged_data1) == 'Forest_Type']<-'Forest Type'

# Combine the data frames
combined_data <- rbind(merged_data, merged_data1)



combined_new<-ggplot(combined_data, aes(x = rarefied1_canopy, y = rarefied1_regen, color = `Forest Type`)) +
  geom_point(size = 3) +
  geom_abline() +
  annotate("text", x = 2.5, y = 2.3, label = "1:1", size = 4, hjust = 0) +  # Add text annotation for 1:1 line
  labs(
    title = "Alpha diversity (plot species richness) between Canopy and Regeneration layer",
    x = "Rarefied Alpha Diversity of Canopy",
    y = "Rarefied Alpha Diversity of Regeneration"
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 17),
    plot.title.position = "plot",
    legend.position = c(0.05, 0.97),  # Set legend position to (0.05, 0.97)
    legend.justification = c(0, 1),  # Align legend to top left corner of (0.05, 0.97)
    axis.title = element_text(size = 15),  # Increase axis title size
    axis.text = element_text(size = 15)  # Increase axis text size
  ) +
  scale_x_continuous(limits = c(1.8, 8), breaks = seq(1.8, 8, 2)) +
  scale_y_continuous(limits = c(1.8, 8), breaks = seq(1.8, 8, 2)) +
  coord_fixed(ratio = 1) +
  scale_color_manual(values = c('Logged' = 'blue', 'Unlogged' = 'red'))

ggsave(
  filename = "combined_new.png",
  plot = combined_new,
  width = 16,
  height = 10,
  dpi = 300
)




