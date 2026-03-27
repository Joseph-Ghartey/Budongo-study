#grouping my data to logged and unlogged 
# Combine both data frames into one and add a source column to keep track of origin
combined <- bind_rows(
  soil_regen %>% mutate(source = "soil_regen"),
  soil_trees %>% mutate(source = "soil_trees")
)

soil_logged <- soil_properties %>% 
  filter(grepl("^Logged", plot.id))

# Create 'unlogged' data frame
soil_unlogged <- soil_properties %>% 
  filter(grepl("^Unlogged", plot.id))
  
print(combined)
  
unique(combined$plot.id)





soil_properties<-read.csv('soil_properties.csv', header = TRUE, sep = ',')


# Load necessary packages
library(vegan)
library(ggplot2)
library(dplyr)

# Assuming your species data is in 'species_data' with columns 'plot.id', 'species', 'compartment'
# and your soil data is in 'soil_data' with columns 'plot.id', 'CEC', 'Al', 'Ca', 'Mg', 'K', 'pH'



unique(soil_trees$plot.id)
unique(soil_properties$plot.id)

print(common_unlogged)

# Ensure plots match between the soil properties and logged data, same for unlogged 
common_unlogged <- intersect(unlogged$plot.id, soil_properties$plot.id)

logged <- logged %>% filter(plot.id %in% common_logged)
unlogged <- unlogged %>% filter(plot.id %in% common_unlogged)

print(common_plots)

# Prepare species matrix for NMDS
matrix_logged <- dcast(logged, plot.id ~ species, value.var = "count", fun.aggregate = sum, fill = 0)
matrix_unlogged <- dcast(unlogged, plot.id ~ species, value.var = "count", fun.aggregate = sum, fill = 0)


# Convert to matrix and set row names
matrix_unlogged <- as.data.frame(matrix_unlogged)
rownames(matrix_unlogged) <- matrix_unlogged$plot.id
matrix_unlogged <- matrix_unlogged[, -1]  # Remove the plot.id column

write.csv(matrix_unlogged, file = 'matrix_unlogged.csv')

plot <- logged$plot.id
compartment <- logged$compartment
df_logged <- data.frame(plot, compartment)
df_logged <- unique(df_logged)

plot <- unlogged$plot.id
compartment <- unlogged$compartment
df_unlogged <- data.frame(plot, compartment)
df_unlogged <- unique(df_unlogged)

# Running NMDS analysis
dist_logged <- vegdist(matrix_logged, index = "bray")
mds_logged <- metaMDS(dist_logged)

dist_unlogged <- vegdist(matrix_unlogged, index = "bray")
mds_unlogged <- metaMDS(dist_unlogged)

# Fit soil variables to NMDS
envfit_logged <- envfit(mds_logged ~ CEC + Al + Ca + Mg + Na + K + PH, data = soil_logged, perm = 999)
envfit_unlogged <- envfit(mds_unlogged ~ CEC + Al + Ca + Mg + Na + K + PH, data = soil_unlogged, perm = 999)


# Extract environmental vectors
envfit_logged_vector <- as.data.frame(scores(envfit_logged, display = "vectors"))
envfit_logged_vector$variables <- rownames(envfit_logged_vector)

envfit_unlogged_vector <- as.data.frame(scores(envfit_unlogged, display = "vectors"))
envfit_unlogged_vector$variables <- rownames(envfit_unlogged_vector)


# Creating a dataframe with MDS results, plot ID and compartment 
mds_logged_values <- as.data.frame(mds_logged$points)
mds_logged_values$plot <- rownames(mds_logged_values)
df_logged <- merge(df_logged, mds_logged_values, by="plot")
df_logged$compartment <- as.factor(df_logged$compartment)

mds_unlogged_values <- as.data.frame(mds_unlogged$points)
mds_unlogged_values$plot <- rownames(mds_unlogged_values)
df_unlogged <- merge(df_unlogged, mds_unlogged_values, by="plot")
df_unlogged$compartment <- as.factor(df_unlogged$compartment)


# Subsetting to 0.01 and 0.99 percentile

qmds1 <- quantile(df_logged$MDS1, prob = c(0.01, 0.99))
qmds2 <- quantile(df_logged$MDS2, prob = c(0.01, 0.99))
df_logged <- subset(df_logged, df_logged$MDS1 > qmds1[1] & df_logged$MDS1 < qmds1[2] & df_logged$MDS2 > qmds2[1] & df_logged$MDS2 < qmds2[2])

qmds1 <- quantile(df_unlogged$MDS1, prob = c(0.01, 0.99))
qmds2 <- quantile(df_unlogged$MDS2, prob = c(0.01, 0.99))
df_unlogged <- subset(df_unlogged, df_unlogged$MDS1 > qmds1[1] & df_unlogged$MDS1 < qmds1[2] & df_unlogged$MDS2 > qmds2[1] & df_unlogged$MDS2 < qmds2[2])


scaling_factor_logged <- max(abs(df_logged$MDS1), abs(df_logged$MDS2)) / max(abs(envfit_logged_vector$NMDS1), abs(envfit_logged_vector$NMDS2))
scaling_factor_unlogged <- max(abs(df_unlogged$MDS1), abs(df_unlogged$MDS2)) / max(abs(envfit_unlogged_vector$NMDS1), abs(envfit_unlogged_vector$NMDS2))


# Apply the scaling factor to the environmental vectors
envfit_logged_vector <- envfit_logged_vector %>%
  mutate(NMDS1 = NMDS1 * scaling_factor_logged, NMDS2 = NMDS2 * scaling_factor_logged)

envfit_unlogged_vector <- envfit_unlogged_vector %>%
  mutate(NMDS1 = NMDS1 * scaling_factor_unlogged, NMDS2 = NMDS2 * scaling_factor_unlogged)


# Plot NMDS with environmental vectors
logged_nmds <- ggplot(df_logged, aes(x = MDS1, y = MDS2)) +
  geom_point(size = 2, shape = 21)+ 
  geom_segment(data = envfit_logged_vector, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text(data = envfit_logged_vector, aes(x = NMDS1, y = NMDS2, label = variables), 
            hjust = 1.5, vjust = 1.5) +
  theme(panel.border = element_rect(fill = NA), 
        panel.background = element_rect(fill = NA),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


print(logged_nmds)



unlogged_nmds <- ggplot(df_unlogged, aes(x = MDS1, y = MDS2)) +
  geom_point(size = 2, shape = 21)+ 
  geom_segment(data = envfit_unlogged_vector, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text(data = envfit_unlogged_vector, aes(x = NMDS1, y = NMDS2, label = variables), 
            hjust = 1.5, vjust = 1.5) +
  theme(panel.border = element_rect(fill = NA), 
        panel.background = element_rect(fill = NA),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

print(unlogged_nmds)


