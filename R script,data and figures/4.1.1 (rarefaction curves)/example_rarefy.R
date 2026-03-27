#logged forest 

library(iNEXT)
library(ggplot2)
library(dplyr)

canopy_data<-read.csv('canopy_plotid.csv', header = TRUE, sep = ',')
reg_plotid<-read.csv('reg_plotid.csv', header = TRUE, sep = ',')

names(canopy_data)[names(canopy_data) == "comp"] <- "Compartment"
names(canopy_data)[names(canopy_data) == "spec"] <- "species"

names(reg_plotid)[names(reg_plotid) == "comp"] <- "Compartment"
names(reg_plotid)[names(reg_plotid) == "spec"] <- "species"


data$species <- trimws(data$species)

# Filter canopy and regeneration data
canopy_data <- data %>% filter(layer == "canopy")
regeneration_data <- data %>% filter(layer == "regeneration")

regeneration_data



# Ensure the column names are correct
colnames(canopy_data)
colnames(reg_plotid)

# Aggregate species abundance for each layer
canopy_abundance <- canopy_data %>%
  group_by(species) %>%
  summarize(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  ungroup()

reg_abundance <- reg_plotid %>%
  group_by(species) %>%
  summarize(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  ungroup()

# Convert to named vector
canopy_abundance_vector <- canopy_abundance$total_abundance
names(canopy_abundance_vector) <- canopy_abundance$species

reg_abundance_vector <- reg_abundance$total_abundance
names(reg_abundance_vector) <- reg_abundance$species




# Combine the two lists into a single list with named elements for iNEXT
combined_abundance_list <- list(
  canopy = canopy_abundance_vector,
  regeneration = reg_abundance_vector
)

# Run iNEXT for each layer, standardizing the number of individuals
inext_result <- iNEXT(combined_abundance_list, q=0, datatype="abundance")

# Plot the rarefaction/extrapolation curves
plot <- ggiNEXT(inext_result, type=1) + 
  theme_minimal() +
  labs(x = "Number of individuals", y = "Species diversity") +
  scale_color_manual(values = c("canopy" = "green", "regeneration" = "purple")) +
  scale_fill_manual(values = c("canopy" = "green", "regeneration" = "purple")) +
  theme(
    legend.position = c(0.1, 0.9),  # Position the legend inside the plot area
    legend.justification = c("left", "top")
  )

# Display the plot
print(plot)
