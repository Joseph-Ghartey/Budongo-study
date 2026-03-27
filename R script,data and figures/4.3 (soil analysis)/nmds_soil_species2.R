# Load necessary packages
library(vegan)
library(ggplot2)
library(dplyr)

soil_properties<-read.csv('soil_properties.csv', header = TRUE, sep = ',')
soil_trees<-read.csv('soil_trees.csv', header = TRUE, sep = ',')
soil_regen<-read.csv('soil_regen.csv', header = TRUE, sep = ',')



#this is a continuation from nmds_soil_species ############

soil_logged <- soil_properties %>% 
  filter(grepl("^Logged", plot.id))

# Create 'unlogged' data frame
soil_unlogged <- soil_properties %>% 
  filter(grepl("^Unlogged", plot.id))

#write this from the saved dataframe 
matrix_logged<-read.csv('matrix_logged.csv', header = TRUE, sep = ',')
matrix_unlogged<-read.csv('matrix_unlogged.csv', header = TRUE, sep = ',')


#becasue this was loaded from the saved data, the row name changed to X 
matrix_logged <- as.data.frame(matrix_logged)
rownames(matrix_logged) <- matrix_logged$X
matrix_logged <- matrix_logged[, -1] 

matrix_unlogged <- as.data.frame(matrix_unlogged)
rownames(matrix_unlogged) <- matrix_unlogged$X
matrix_unlogged <- matrix_unlogged[, -1] 

# Running NMDS analysis with species scores
# Run NMDS analysis with species scores
new_mds_logged <- metaMDS(matrix_logged, trymax = 100, k = 2)
new_mds_unlogged <- metaMDS(matrix_unlogged, trymax = 100, k = 2)

new_mds_logged$stress
new_mds_unlogged$stress

# Running NMDS analysis
beta_dist <- vegdist(matrix_logged, index = "bray")
mds <- metaMDS(beta_dist)


# Fit soil variables to NMDS
new_envfit_logged <- envfit(new_mds_logged ~ Al + Ca + Mg + Na + K + PH, data = soil_logged, perm = 999)
new_envfit_unlogged <- envfit(new_mds_unlogged ~ Al + Ca + Mg + Na + K + PH, data = soil_unlogged, perm = 999)

# Extract environmental vectors
new_envfit_logged_vector <- as.data.frame(scores(new_envfit_logged, display = "vectors"))
new_envfit_logged_vector$variables <- rownames(new_envfit_logged_vector)

new_envfit_unlogged_vector <- as.data.frame(scores(new_envfit_unlogged, display = "vectors"))
new_envfit_unlogged_vector$variables <- rownames(new_envfit_unlogged_vector)

# Extract NMDS scores for plots and species
plot_logged_values <- as.data.frame(new_mds_logged$points)
plot_logged_values$plot <- rownames(plot_logged_values)
colnames(plot_logged_values) <- c("MDS1", "MDS2", "plot")

species_logged <- as.data.frame(scores(new_mds_logged, display = "species"))
species_logged$species <- rownames(species_logged)
colnames(species_logged) <- c("MDS1", "MDS2", "species")

plot_unlogged_values <- as.data.frame(new_mds_unlogged$points)
plot_unlogged_values$plot <- rownames(plot_unlogged_values)
colnames(plot_unlogged_values) <- c("MDS1", "MDS2", "plot")

species_unlogged <- as.data.frame(scores(new_mds_unlogged, display = "species"))
species_unlogged$species <- rownames(species_unlogged)
colnames(species_unlogged) <- c("MDS1", "MDS2", "species")

# Add a type column to each data frame for plotting
new_df_logged <- cbind(plot_logged_values, type = "plot")
new_df_unlogged <- cbind(plot_unlogged_values, type = "plot")
species_logged <- cbind(species_logged, type = "species")
species_unlogged <- cbind(species_unlogged, type = "species")


# Subsetting to 0.01 and 0.99 percentile

#for plots
new_qmds1 <- quantile(new_df_logged$MDS1, prob = c(0.01, 0.99))
new_qmds2 <- quantile(new_df_logged$MDS2, prob = c(0.01, 0.99))
new_df_logged <- subset(new_df_logged, new_df_logged$MDS1 > new_qmds1[1] & new_df_logged$MDS1 < new_qmds1[2] & new_df_logged$MDS2 > new_qmds2[1] & new_df_logged$MDS2 < new_qmds2[2])

new_qmds1 <- quantile(new_df_unlogged$MDS1, prob = c(0.01, 0.99))
new_qmds2 <- quantile(new_df_unlogged$MDS2, prob = c(0.01, 0.99))
new_df_unlogged <- subset(new_df_unlogged, new_df_unlogged$MDS1 > new_qmds1[1] & new_df_unlogged$MDS1 < new_qmds1[2] & new_df_unlogged$MDS2 > new_qmds2[1] & new_df_unlogged$MDS2 < new_qmds2[2])


#for species 
qmds1 <- quantile(species_logged$MDS1, prob = c(0.01, 0.99))
qmds2 <- quantile(species_logged$MDS2, prob = c(0.01, 0.99))
species_logged <- subset(species_logged, species_logged$MDS1 > qmds1[1] & species_logged$MDS1 < qmds1[2] & species_logged$MDS2 > qmds2[1] & species_logged$MDS2 < qmds2[2])

# Combine species and plot points for NMDS plots
combined_logged <- rbind(
  new_df_logged[, c("MDS1", "MDS2", "type")],
  species_logged[, c("MDS1", "MDS2", "type")]
)

combined_unlogged <- rbind(
  new_df_unlogged[, c("MDS1", "MDS2", "type")],
  species_unlogged[, c("MDS1", "MDS2", "type")]
)




scaling_factor_logged <- max(abs(combined_logged$MDS1), abs(combined_logged$MDS2)) / max(abs(new_envfit_logged_vector$NMDS1), abs(new_envfit_logged_vector$NMDS2))
scaling_factor_unlogged <- max(abs(combined_unlogged$MDS1), abs(combined_unlogged$MDS2)) / max(abs(new_envfit_unlogged_vector$NMDS1), abs(new_envfit_unlogged_vector$NMDS2))

scaling_factor_unlogged<-scaling_factor_unlogged/2


# Apply the scaling factor to the environmental vectors
new_envfit_logged_vector <- new_envfit_logged_vector %>%
  mutate(NMDS1 = NMDS1 * scaling_factor_logged, NMDS2 = NMDS2 * scaling_factor_logged)

new_envfit_unlogged_vector <- new_envfit_unlogged_vector %>%
  mutate(NMDS1 = NMDS1 * scaling_factor_unlogged, NMDS2 = NMDS2 * scaling_factor_unlogged)






logged_nmds1 <- ggplot() +
  geom_point(data = combined_logged[combined_logged$type == "plot",], aes(x = MDS1, y = MDS2, shape = type), size = 3, fill = 'black') +
  geom_point(data = combined_logged[combined_logged$type == "species",], aes(x = MDS1, y = MDS2, shape = type), color = "red", size = 1.8) +
  geom_segment(data = new_envfit_logged_vector, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text(data = new_envfit_logged_vector, aes(x = NMDS1, y = NMDS2, label = variables), 
            color = "blue", hjust = 0.8, vjust = -0.5) +
  scale_shape_manual(values = c("plot" = 21, "species" = 17)) +
  theme(panel.border = element_rect(fill = NA), 
        panel.background = element_rect(fill = NA),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = c(0.95, 0.95), # Inside top-right corner
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "white", color = NA),
        legend.text = element_text(size = 12, face = "bold"), # Increase and bold legend text
        legend.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5))+ # Increase and bold legend title
  labs(shape = "Type", title = "Logged Forest") +
  guides(shape = guide_legend(override.aes = list(color = c("black", "red"))))

print(logged_nmds1)






unlogged_nmds1 <- ggplot() +
  geom_point(data = combined_unlogged[combined_unlogged$type == "plot",], aes(x = MDS1, y = MDS2, shape = type), size = 3, fill = 'black') +
  geom_point(data = combined_unlogged[combined_unlogged$type == "species",], aes(x = MDS1, y = MDS2, shape = type), color = "red", size = 1.8) +
  geom_segment(data = new_envfit_unlogged_vector, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text(data = new_envfit_unlogged_vector, aes(x = NMDS1, y = NMDS2, label = variables), 
            color = "blue", hjust = 1.5, vjust = 1.5) +
  scale_shape_manual(values = c("plot" = 21, "species" = 17)) +
  theme(panel.border = element_rect(fill = NA), 
        panel.background = element_rect(fill = NA),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = c(0.95, 0.95), # Inside top-right corner
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "white", color = NA),
        legend.text = element_text(size = 12, face = "bold"), # Increase and bold legend text
        legend.title = element_text(size = 12, face = "bold"), # Increase and bold legend title
        plot.title = element_text(hjust = 0.5))+
  labs(shape = "Type", title = "Unogged Forest") +
  guides(shape = guide_legend(override.aes = list(color = c("black", "red"))))


print(unlogged_nmds1)

p_values <- sapply(soil_properties[ , c("Al", "Ca", "K", "Mg", "Na", "PH")], 
                   function(x) summary(aov(x ~ compartment, data = soil_properties))[[1]][["Pr(>F)"]][1])
print(p_values)

