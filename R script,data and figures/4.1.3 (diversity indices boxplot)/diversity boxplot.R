library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

#read in the diversity data 
#this is saved to the folder
canopy_index<-read.csv('canopy_indexes.csv', sep = ',', header = TRUE)
reg_index<-read.csv('reg_indexes.csv', sep = ',', header = TRUE)

# Reshape the data from wide to long format
canopy_long <- canopy_index %>%
  pivot_longer(cols = c(pielou, shannon, Inv_simpson), names_to = "index", values_to = "value")

reg_long <- reg_index %>%
  pivot_longer(cols = c(pielou, shannon, Inv_simpson), names_to = "index", values_to = "value")

names(canopy_long)[names(canopy_long) == "comp"] <- "Compartment"
names(reg_long)[names(reg_long) == "comp"] <- "Compartment"

canopy_long <- canopy_long %>%
  mutate(Compartment = recode(Compartment, "N3" = "logged", "N15" = "unlogged"))

reg_long <- reg_long %>%
  mutate(Compartment = recode(Compartment, "N3" = "logged", "N15" = "unlogged"))

# Create the boxplot
canopy_boxplot <- ggplot(canopy_long, aes(x = index, y = value, fill = Compartment)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.8) +
  labs(x = "Diversity Variables", y = "Value", title = "Diversity Indices of Canopy Layer") +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = c(0.95, 0.95),  # Top right corner
    legend.justification = c(0.9, 0.9),  # Adjust to the top right corner
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  scale_fill_manual(values = c("logged" = "cyan", "unlogged" = "violet"))  # Customize colors if necessary

print(canopy_boxplot)


reg_boxplot <- ggplot(reg_long, aes(x = index, y = value, fill = Compartment)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.8) +
  labs(x = "Diversity Variables", y = "Value", title = "Diversity Indices of Regeneration Layer") +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = c(0.95, 0.95),  # Top right corner
    legend.justification = c(0.9, 0.9),  # Adjust to the top right corner
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  scale_fill_manual(values = c("logged" = "cyan", "unlogged" = "violet"))  # Customize colors if necessary

print(reg_boxplot)


# Add error bars - this did not work
reg <- boxplot +
  geom_errorbar(data = summary_data, aes(x = index, ymin = mean_value - se_value, ymax = mean_value + se_value,  = comp),
                position = position_dodge(width = 0.8), width = 0.2, size = 1, color = "black")


print(boxplot)
# Add p-values above the boxplots manually done 

reg_p_valu <- c(0.293, 0.000, 0.003)

# add annotation for p values and ensure the 0.000 apear instead of 0
regeneration <- reg_boxplot +
  annotate("text", x = c(1, 2, 3), y = c(6, 1.5, 3), 
           label = paste("p =", sprintf("%.3f", reg_p_valu)), vjust = -0.5)
print(regeneration)

# Display the plot
canopy <- canopy_boxplot +
  annotate("text", x = 1.2, y = 2.9, label = "p<0.001", vjust = -0.5)

print(canopy)


#lets try removing the y axis and legend on the canopy layer
canopy <- canopy + theme(axis.title.y = element_blank(),  legend.position = "none")

#combine the plots 
combined_plot<-grid.arrange(regeneration, canopy, ncol = 2)

library(gridExtra)
library(grid)

# heading for the plot
grid.arrange(combined_plot, top = textGrob("Diversity Indices of Canopy and Regeneration Layer Between the Forest Types", gp = gpar(fontsize = 16, fontface = "bold")))


canopy <- canopy + theme(axis.title.x = element_blank())
regeneration <- regeneration + theme(axis.title.x = element_blank())



#save the graph with ggsave
ggsave("diversityt.png", dpi = 300, width = 8, height = 6, units = "in")
ggsave("diversity1.tiff", dpi = 300, width = 8, height = 6, units = "in", compression = "lzw")

