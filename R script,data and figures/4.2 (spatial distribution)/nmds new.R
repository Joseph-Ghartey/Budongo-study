# Clear environment
rm(list=ls())

# Loading packages
library(vegan)
library(ggplot2)
library(tidyverse)
library(tidyr)

# Importing data
reg <- read.csv("regeneration.csv", sep=",", header = TRUE)
dat <- read.csv("main.csv", sep=",", header = TRUE)

names(reg)[names(reg) == 'comp']<-'compartment'
names(dat)[names(dat) == 'comp']<-'compartment'

reg <- reg %>%
  mutate(compartment = recode(compartment, "N3" = "reg_logged", "N15" = "reg_unlogged"))
dat <- dat %>%
  mutate(compartment = recode(compartment, "N3" = "reg_logged", "N15" = "reg_unlogged"))


# Select the relevant columns from each data frame
reg_extract<- reg[, c("spec", "count", "plot.id", "compartment")]
canopy_extract <- dat[, c("spec", "count", "plot.id", "compartment")]

# Combine the two data frames
dat <- rbind(reg_extract, canopy_extract)
dat<-read.csv('dat.csv', header = TRUE, sep = ',')

write.csv(dat, file = 'dat.csv')

# Rearranging data
dat$ef <- 1 # Adding extra column for counting individuals
alpha.n <- as.data.frame(tapply(dat$count, list(dat$plot.id, dat$spec), sum))
alpha.n[is.na(alpha.n)] <- 0


rownames(alpha.n) <- rownames(alpha.n)

plot <- dat$plot.id
compartment <- dat$compartment
df <- data.frame(plot, compartment)
df <- unique(df)

# Running NMDS analysis
beta_dist <- vegdist(alpha.n, index = "bray")
mds <- metaMDS(beta_dist)


#perform permanova to estimate p value and R 
compartment_ordered <- df$compartment[match(rownames(alpha.n), df$plot)]
compartment_factor <- factor(compartment_ordered)
df$compartment<-as.factor(df$compartment)

adonis2_result <- adonis2(beta_dist ~ compartment_ordered)
print(adonis2_result)

write.csv(pairwise_adonis_result, file = 'Anova1.csv')

# since the levels are 3 lets perform pairwise 
install.packages("devtools")
library(devtools)
devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

pairwise_adonis2_result <- pairwise.adonis(beta_dist, factors = compartment_ordered)
print(pairwise_adonis_result)
?pairwise.adonis2

# Print the results
print(pairwise_adonis2_result)

write.csv(adonis2_result, file = 'p value for nmds_canopy')

# Creating a dataframe with MDS results, plot ID and com
mds_values <- as.data.frame(mds$points)
mds_values$plot <- rownames(mds_values)
df <- merge(df, mds_values, by="plot")
df$compartment <- as.factor(df$compartment)

# Subsetting to 0.01 and 0.99 percentile

qmds1 <- quantile(df$MDS1, prob = c(0.01, 0.99))
qmds2 <- quantile(df$MDS2, prob = c(0.01, 0.99))
df <- subset(df, df$MDS1 > qmds1[1] & df$MDS1 < qmds1[2] & df$MDS2 > qmds2[1] & df$MDS2 < qmds2[2])

# Plotting results of NMDS analysis
nmds_canopy<-ggplot(df, aes(x = MDS1, y = MDS2,  fill=compartment)) +
  scale_color_brewer(palette="Spectral") +
  scale_fill_brewer(palette="Spectral") +
  geom_point(size=2, shape=21) +
  stat_ellipse(aes(color = compartment), lwd=1, level=0.95) +  # level=0.95 draws the ellipses as 95% confidence level for a multivariate t-distribution
  theme(panel.border = element_rect(fill = NA), 
        panel.background = element_rect(fill = NA),
        axis.text.x.bottom = element_text(size = 15),
        axis.text.y.left = element_text(size = 15),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))

#redraw the graph
nmds_combined <- ggplot(df, aes(x = MDS1, y = MDS2, fill = compartment)) +
  scale_fill_manual(values = c("yellow", "red", 'cyan', 'green')) +
  geom_point(size = 2, shape = 21) +
  stat_ellipse(aes(color = compartment), lwd = 1, level = 0.95) +  # level=0.95 draws the ellipses as 95% confidence level for a multivariate t-distribution
  scale_color_manual(values = c("yellow", "red", 'cyan', 'green')) +
  theme(panel.border = element_rect(fill = NA), 
        panel.background = element_rect(fill = NA),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = c(0.85, 0.86),  # Position legend inside plot at top right corner
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

print(nmds_combined)


print(nmds_canopy)

stress<-mds$stress
p<-0.001
R <- 0.236675

combined<-nmds_combined + annotate("text", x = Inf, y = -Inf, label = paste("stress:", round(stress, 3)),
                               hjust = 1.1, vjust =-0.3) + labs(title = "Canopy Layer")
combined <- nmds_combined + 
  annotate("text", x = Inf, y = -Inf, label = paste("P = ", round(p, 3), "\nR² = ", round(R, 3), "\nStress:", round(stress, 3)),
           hjust = 1.1, vjust = -0.3, size = 5, color = "black") +
  labs(title = "NMDS Ordination for Logged and Unlogged Forests")+
  theme(plot.title = element_text(hjust = 0.5)) #place the title in the middle 

print(combined)

ggsave("ordination_graph.png", combined, width = 10, height = 7, dpi = 300)


write.csv(pairwise_adonis_result, file = 'pairwise permanova.csv')
