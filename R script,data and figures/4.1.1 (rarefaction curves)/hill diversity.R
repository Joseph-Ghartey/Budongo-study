
main<-read.csv('main.csv', header = TRUE, sep = ',')
# Install and load the required packages
install.packages(c("iNEXT", "ggplot2"))
library(iNEXT)
library(ggplot2)
library(dplyr)
names(main)[names(main) == "comp"] <- "compartment"

main <- main %>%
  mutate(compartment = recode(compartment, "N3" = "logged", "N15" = "unlogged"))
# Step 1: Calculate species richness per plot (unique species per plot)
species_richness_per_plot <- main %>%
  group_by(compartment, plot.id) %>%
  summarize(species_richness = n_distinct(species)) %>%
  ungroup()

# View the species richness for each plot
print(species_richness_per_plot)

# Step 2: Create a dataframe where each row is a plot, and columns represent species counts
species_abundance_per_plot <- main %>%
  group_by(compartment, plot.id, species) %>%
  summarize(count = n()) %>%  # Count occurrences of each species in each plot
  spread(species, count, fill = 0) %>%
  ungroup()

# Step 3: Prepare the dataset for iNEXT (we're using richness, not raw abundance)
# Each plot becomes a row, and species counts per plot are arranged in columns
# We split the data into two: logged and unlogged compartments

logged_data <- species_abundance_per_plot %>% filter(compartment == "logged")
unlogged_data <- species_abundance_per_plot %>% filter(compartment == "unlogged")

# Convert these to matrices for input into iNEXT
list_data <- list(
  logged = c(nrow(logged_data), colSums(logged_data[-c(1:2)] > 0)),  # Number of plots and species occurrence
  unlogged = c(nrow(unlogged_data), colSums(unlogged_data[-c(1:2)] > 0))
)

# Step 4: Calculate diversity indices using iNEXT (Hill numbers: q = 0, 1, 2)
iNEXT_result <- iNEXT(list_data, q = c(0, 1, 2), datatype = "incidence_freq", endpoint = 200)

# Step 5: Prepare the data for plotting using ggplot2
plot_data <- fortify(iNEXT_result, type = 1)

# Step 6: Rename columns for clearer reference in the plot
plot_data <- plot_data %>%
  rename(
    q = Order.q,         # Hill number: 0 (richness), 1 (Shannon), 2 (Simpson)
    m = x,               # Number of sampling units (plots)
    qD = y,              # Diversity estimate
    qD.LCL = y.lwr,      # Lower confidence limit
    qD.UCL = y.upr,      # Upper confidence limit
    site = Assemblage    # Compartment (logged/unlogged)
  )

# Step 7: Create the diversity plot for logged and unlogged compartments
ggplot(plot_data, aes(x = m, y = qD, color = site, group = interaction(site, q))) +
  geom_line(data = subset(plot_data, Method == "Rarefaction" | Method == "Observed"), linewidth = 1) +  # Solid line for rarefaction and observed data
  geom_line(data = subset(plot_data, Method == "Extrapolation"), linewidth = 1, linetype = "dotted") +  # Dotted line for extrapolated data
  geom_point(data = subset(plot_data, Method == "Observed"), size = 3) +  # Points at observed data
  geom_ribbon(aes(ymin = qD.LCL, ymax = qD.UCL, fill = site), alpha = 0.2, color = NA) +  # Shaded confidence interval
  facet_wrap(~ q, scales = "fixed", labeller = labeller(q = c("0" = "Species Richness", "1" = "Shannon Index", "2" = "Simpson Index"))) +
  scale_color_manual(values = c("logged" = "blue", "unlogged" = "green")) +
  scale_fill_manual(values = c("logged" = "blue", "unlogged" = "green")) +
  labs(x = "Number of plots", y = "Diversity", color = "Compartment", fill = "Compartment") +
  ggtitle("Diversity of Regeneration Layer") +  # Add title
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "white", color = "black", size = 1.2),  # Box around each facet
        panel.background = element_blank(),  # Remove panel background for cleaner look
        panel.border = element_rect(color = "black", fill = NA, size = 1),  # Black border around each plot
        panel.grid.major = element_blank(),  # Remove major grids
        panel.grid.minor = element_blank(),  # Remove minor grids
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))  # Center and format the title





