#### Data Analyses and Visualizations for Stovall et al. 2022 ####
#by Jeremy Schallner, Qiuyan Yu, Nicole Pietrasiak

## Load Packages ##
library(corrplot)
library(tidyverse)
library(cowplot)
library(reshape2)
library(Hmisc)
library(vegan)
# install ggvegan
# install.packages("remotes")
# remotes::install_github("gavinsimpson/ggvegan")
library(ggvegan)
library(ggrepel)
library(relaimpo)

## Read in data ##
# Set working directory to file location before proceeding
data <- read.csv("Stovall_all_data_2021.csv")
str(data)
summary(data)

#### Table 2 - Summary Statistics ####

# subset bsc cover data
bsc_cover <- data[, c(2, 28:37)]

summary.table <- bsc_cover %>%
  summarise(across(where(is.numeric), list(mean = mean, min = min, max = max)))

means <- as.matrix(summary.table[, c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28)])
mins <- as.matrix(summary.table[, c(2, 5, 8, 11, 14, 17, 20, 23, 26, 29)])
maxs <- as.matrix(summary.table[, c(3, 6, 9, 12, 15, 18, 21, 24, 27, 30)])

empty.summary.table <- matrix(nrow = 3, ncol = 11)
empty.summary.table[, 1] <- c("Mean", "Min", "Max")
names <- colnames(bsc_cover)
colnames(empty.summary.table) <- names
empty.summary.table[1, 2:11] <- means
empty.summary.table[2, 2:11] <- mins
empty.summary.table[3, 2:11] <- maxs

summary.table2 <- rbind(bsc_cover, empty.summary.table)
summary.table2

#### Paired t test for Soil Stability Scores ####

stability <- data[, c(2, 73:74)]

stability.ttest <- t.test(stability$Ave_Sur_Score, stability$Ave_Sub_Score,
  data = stability, paired = TRUE, conf.level = 0.95)

stability.ttest

#### Figure 2 - Boxplots and Correlation ####

## Figure 2A - Cover Boxplot ##

# subset cover data
cover <- data[, c(2, 45:49)]
# reshape data for ggplot
cover_long <- melt(cover)

# cover boxplot
cover.plot <- ggplot(data = cover_long, aes(x = variable, y = value)) +
  geom_hline(aes(yintercept = 0), color = "darkgrey", lty = 1) +
  geom_boxplot() +
  scale_x_discrete(name = "", labels = c("Bare Soil", "Physical Crust",
                                         "Biological Soil Crust", "Plant", 
                                         "Litter")) +
  scale_y_continuous(name = "Cover (%)") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

cover.plot

## Figure 2B - BSC Cover Boxplot ##

# subset bsc cover data
bsc_cover <- data[, c(2, 29:37)]
# reshape data for ggplot
bsc_cover_long <- melt(bsc_cover)

# bsc cover boxplot
bsc.cover.plot <- ggplot(data = bsc_cover_long, aes(x = variable, y = value)) +
  geom_hline(aes(yintercept = 0), color = "darkgrey", lty = 1) +
  geom_boxplot() +
  scale_x_discrete(name = "", labels = c("PC", "PCAC", "IC", "LAC", "DAC", "CLC",
                                         "GLC", "RMC", "SMC")) +
  scale_y_continuous(name = "Cover (%)", limits = c(0, 80)) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

bsc.cover.plot

## Figure 2C - Soil Stability Boxplot ##

# subset stability data
stability <- data[, c(2, 73:74)]
# reshape data for ggplot
stability_long <- melt(stability)

# stability boxplot
stability.plot <- ggplot(data = stability_long, aes(x = variable, y = value)) +
  geom_hline(aes(yintercept = 0), color = "darkgrey", lty = 1) +
  geom_boxplot() +
  scale_x_discrete(name = "", labels = c("Surface", "Subsurface")) +
  scale_y_continuous(name = "Stability Score", limits = c(0, 6)) +
  annotate("text", x = 2, y = 5.25, label = "p < 0.0001") +
  annotate("text", x = 1, y = 5, label = "***") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

stability.plot

## Figure 2D - Correlation Plot ##
# %Biocrust/PC stability correlation #

# subset data for stability correlation
stability2 <- data[, c(2, 46:47, 73:74, 86, 87)]
str(stability2)

# tests of normality 
histogram(stability2$Percent_Biocrust)
histogram(stability2$Percent_PC)
histogram(stability2$Ave_Sur_Score)
histogram(stability2$Ave_Sub_Score)

shapiro.test(stability2$Percent_Biocrust)
shapiro.test(stability2$Percent_PC)
shapiro.test(stability2$Ave_Sur_Score)
shapiro.test(stability2$Ave_Sub_Score)

# additional tests of normality for cover categories
shapiro.test(bsc_cover$P_PC)
shapiro.test(bsc_cover$P_PC_AC)
shapiro.test(bsc_cover$P_IC)
shapiro.test(bsc_cover$P_LAC)
shapiro.test(bsc_cover$P_DAC)
shapiro.test(bsc_cover$P_CLC)
shapiro.test(bsc_cover$P_GLC)
shapiro.test(bsc_cover$P_RMC)
shapiro.test(bsc_cover$P_SMC)
shapiro.test(cover$Percent_BS)
shapiro.test(cover$Percent_Plant)
shapiro.test(cover$Percent._Litter)

# correlation tests of the variables
cor.1 <- cor.test(stability2$Percent_Biocrust, stability2$Ave_Sur_Score, method = "spearman")
cor.2 <- cor.test(stability2$Percent_Biocrust, stability2$Ave_Sub_Score, method = "spearman")
cor.3 <- cor.test(stability2$Percent_PC, stability2$Ave_Sur_Score, method = "spearman")
cor.4 <- cor.test(stability2$Percent_PC, stability2$Ave_Sub_Score, method = "spearman")

# return spearman's rho estimate
cor.1$estimate
cor.2$estimate
cor.3$estimate
cor.4$estimate

# % biocrust vs surface stability plot
s2.1 <- ggplot(data = stability2, aes(x = Percent_Biocrust, y = Ave_Sur_Score)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  scale_x_continuous(limits = c(0, 80), name = "") +
  scale_y_continuous(limits = c(0, 6), name = "Average Surface 
Stability Score") +
  annotate("text", x = 20, y = 0.5, label = paste("rho =", round(cor.1$estimate, 3)),
           size = 4) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# %biocrust vs subsurface plot
s2.2 <- ggplot(data = stability2, aes(x = Percent_Biocrust, y = Ave_Sub_Score)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  scale_x_continuous(limits = c(0, 80), name = "Biocrust Cover (%)") +
  scale_y_continuous(limits = c(0, 6), name = "Average Subsurface 
Stability Score") +
  annotate("text", x = 60, y = 5.5, label = paste("rho =", round(cor.2$estimate, 3)), size = 4) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# % PC vs surface stability plot
s2.3 <- ggplot(data = stability2, aes(x = Percent_PC, y = Ave_Sur_Score)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  scale_x_continuous(limits = c(0, 80), name = "") +
  scale_y_continuous(limits = c(0, 6), name = "") +
  annotate("text", x = 20, y = 0.5, label = paste("rho =", round(cor.3$estimate, 3)), size = 4) +
  theme(panel.background = element_rect(fill = "white", color = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# % PC vs subsurface stability plot
s2.4 <- ggplot(data = stability2, aes(x = Percent_PC, y = Ave_Sub_Score)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  scale_x_continuous(limits = c(0, 80), name = "Physical Crust Cover (%)") +
  scale_y_continuous(limits = c(0, 6), name = "") +
  annotate("text", x = 60, y = 5.5, label = paste("rho =", round(cor.4$estimate, 3)), size = 4) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# create matrix of 4 correlation plots
corplotmatrix <- plot_grid(s2.1, s2.3, s2.2, s2.4, ncol = 2)
corplotmatrix

## Figure 2 - Combined ##

# open new plotting window
dev.new()
# plot grid of all figure 2 plots
fullboxplot <- plot_grid(cover.plot, bsc.cover.plot, stability.plot, corplotmatrix, ncol = 2, labels = c("A", "B", "C", "D"))
fullboxplot
# close plotting window
dev.off()

#### Figure 3 - Correlation Plot ####

# subset data for figure 3 correlation plot
corrdata <- as.data.frame(c(data[, 28:37], data[, 65:75], data[, 76:85]))
# convert to matrix for analysis
cor.matrix <- as.matrix(corrdata)

# color palette for plot
col <- colorRampPalette(rev(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA")))

# correlation plot of soil physical props.
corplot.1 <- corrplot(cor(data[, 28:37], data[, 65:75], method = "spearman"),
  method = "color", col = col(200),
  addCoef.col = "black", # Add coefficient of correlation
  tl.col = "black", tl.srt = 45, tl.cex = 1
) # Text label color and rotation

# correlation plot of soil chemical props.
corplot.2 <- corrplot(cor(data[, 28:37], data[, 76:85], method = "spearman"),
  method = "color", col = col(200),
  addCoef.col = "black", # Add coefficient of correlation
  tl.col = "black", tl.srt = 45
) # Text label color and rotation

# correlation test for full data matrix (used to find p-values)
corr.all <- cor.mtest(cor.matrix, method = "spearman")
corr.all

# correlation plot showing full correlation matrix with only sig. relationships visualized
corrplot(cor(cor.matrix, method = "spearman"),
  method = "color", col = col(200),
  addCoef.col = "black", # Add coefficient of correlation
  p.mat = corr.all$p, insig = "blank", # use p-value to blank insig. values
  tl.col = "black", tl.srt = 45, tl.cex = 0.75
)

#### Figure 4 - NMDS ####

# subset community data
community_matrix <- data[, 4:11]
# subset environmental data
env <- data[, 65:85]
# NMDS
NMDS <- metaMDS(community_matrix, k = 2)
# stressplot
stressplot(NMDS)
# environmental fit with select variables
fit <- envfit(NMDS, subset(env, select = c(
  pH, SAR, clay, veryfine,
  medium, verycoarse, EC, NO3, P, K, NH4, Ca, Mg, Na, AWC, Ave_Sur_Score,
  Ave_Sub_Score
)), perm = 999, na.rm = TRUE, p.max = 0.05)

# extract NMDS values for ggplot
NMDS.gg <- fortify(NMDS)
# add transect names to sites in NMDS results
trans_ID <- data[, 2]
length(trans_ID) <- max(c(length(NMDS.gg$Label), length(trans_ID)))
NMDS.gg <- cbind(NMDS.gg, trans_ID)
# extract environmental fit for ggplot
fit.gg <- fortify(fit)

# add p-values to environmental fit for ggplot
fitvalues <- as.list(fit$vectors)
pval <- as.data.frame(fitvalues$pvals)
fit.gg2 <- cbind(fit.gg, pval)

## Final NMDS graph with sites, species, and sig. environmental fit ##
NMDS.plot <- ggplot(NMDS.gg, aes(NMDS1, NMDS2)) +
  geom_hline(aes(yintercept = 0), color = "darkgrey", lty = 2) + # line at 0 y
  geom_vline(aes(xintercept = 0), color = "darkgrey", lty = 2) + # line at 0 x
  scale_x_continuous(limits = c(-1.5, 1.5)) + # x scale limits (can also set breaks using breaks=)
  scale_y_continuous(limits = c(-1, 1)) + # y scale limits
  geom_point(data = subset(NMDS.gg, Score == "species"), aes(shape = "Species"), color = "black", size = 2) + # species points
  geom_point(data = subset(NMDS.gg, Score == "sites"), aes(shape = "Sites"), color = "black", size = 2) + # site points
  scale_shape_manual(values = c(15, 2)) + # define shapes
  geom_text_repel(
    data = subset(NMDS.gg, Score == "species"), aes(label = Label), color = "black", size = 3.5,
    point.padding = 5
  ) + # species labels
  geom_text_repel(
    data = subset(NMDS.gg, Score == "sites"), aes(label = trans_ID), color = "black", size = 3.5,
    point.padding = 5
  ) + # site labels
  geom_segment(
    data = subset(fit.gg2, fitvalues$pval <= 0.05), aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), color = "black",
    arrow = arrow(length = unit(0.02, "npc"))
  ) + # envfit arrows
  geom_text_repel(
    data = subset(fit.gg2, fitvalues$pval <= 0.05), aes(label = Label), color = "black", size = 3.5,
    point.padding = 5
  ) + # envfit labels
  theme(
    legend.position = c(0.85, 0.85), legend.title = element_blank(),
    legend.background = element_rect(fill = "white", linetype = "solid")
  ) + # legend aesthetics
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  ) # other aesthetics
NMDS.plot


#### Supplementary Materials ####

#### Table S1 ####

soil_phys <- data[, c(2, 65:75)]

summary.tableS1 <- soil_phys %>%
  summarise(across(where(is.numeric), list(mean = mean, min = min, max = max)))

meansS1 <- as.matrix(summary.tableS1[, c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31)])
minsS1 <- as.matrix(summary.tableS1[, c(2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32)])
maxsS1 <- as.matrix(summary.tableS1[, c(3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33)])

empty.summary.tableS1 <- matrix(nrow = 3, ncol = 12)
empty.summary.tableS1[, 1] <- c("Mean", "Min", "Max")
namesS1 <- colnames(soil_phys)
colnames(empty.summary.tableS1) <- namesS1
empty.summary.tableS1[1, 2:12] <- meansS1
empty.summary.tableS1[2, 2:12] <- minsS1
empty.summary.tableS1[3, 2:12] <- maxsS1

summary.tableS1 <- rbind(soil_phys, empty.summary.tableS1)
summary.tableS1

#### Table S2 ####

soil_chem <- data[, c(2, 76:85)]

summary.tableS2 <- soil_chem %>%
  summarise(across(where(is.numeric), list(mean = mean, min = min, max = max)))

meansS2 <- as.matrix(summary.tableS2[, c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28)])
minsS2 <- as.matrix(summary.tableS2[, c(2, 5, 8, 11, 14, 17, 20, 23, 26, 29)])
maxsS2 <- as.matrix(summary.tableS2[, c(3, 6, 9, 12, 15, 18, 21, 24, 27, 30)])

empty.summary.tableS2 <- matrix(nrow = 3, ncol = 11)
empty.summary.tableS2[, 1] <- c("Mean", "Min", "Max")
names <- colnames(soil_chem)
colnames(empty.summary.tableS2) <- names
empty.summary.tableS2[1, 2:11] <- meansS2
empty.summary.tableS2[2, 2:11] <- minsS2
empty.summary.tableS2[3, 2:11] <- maxsS2

summary.tableS2 <- rbind(soil_chem, empty.summary.tableS2)
summary.tableS2

#### Figure S1 ####
Pcover <- data[, 29:37]
soil_chem <- data[, c(76:85)]

par(mfrow=c(3,3), oma=c(0,4,0,0))
for (i in 1: ncol(Pcover)){
  com_soil <- cbind(cover=Pcover[, i], soil_chem);
  ols.sat <- lm(cover~., data=com_soil);
  
  relimp <- calc.relimp(ols.sat, type = c("lmg"), rela = TRUE)
  barplot(relimp@lmg*100, horiz =T, names.arg=names(com_soil)[-1], cex.names=1.5,
          main=(paste(substr(names(Pcover)[i],3,nchar(names(Pcover)[i])), 
                      "R2=", round(summary(ols.sat)$r.squared, 2))),
          xlab="Relative importance (%)", cex.axis=1.5, cex.lab=1.5, cex.main=1.5, las=1)
}

#### Figure S2 ####
Pcover <- data[, 29:37]
soil_phys <- data[, c(65:66, 68:72, 75)]

par(mfrow=c(3,3), oma=c(0,4,0,0))
for (i in 1: ncol(Pcover)){
  com_soil <- cbind(cover=Pcover[, i], soil_phys);
  ols.sat <- lm(cover~., data=com_soil);
  
  relimp <- calc.relimp(ols.sat, type = c("lmg"), rela = TRUE)
  barplot(relimp@lmg*100, horiz =T, names.arg=names(com_soil)[-1], cex.names=1.5,
          main=(paste(substr(names(Pcover)[i],3,nchar(names(Pcover)[i])), 
                      "R2=", round(summary(ols.sat)$r.squared, 2))),
          xlab="Relative importance (%)", cex.axis=1.5, cex.lab=1.5, cex.main=1.5, las=1)
}

#### Figure S3 ####
Pcover <- data[, 29:37]
soil_phys_chem <- data[, c(65:66, 68:72, 75:85)]

par(mfrow=c(3,3), oma=c(0,4,0,0))
for (i in 1: ncol(Pcover)){
  com_soil <- cbind(cover=Pcover[, i], soil_phys_chem);
  ols.sat <- lm(cover~., data=com_soil);
  
  relimp <- calc.relimp(ols.sat, type = c("lmg"), rela = TRUE)
  barplot(relimp@lmg*100, horiz =T, names.arg=names(com_soil)[-1], cex.names=1.5,
          main=(paste(substr(names(Pcover)[i], 3, nchar(names(Pcover)[i])), 
                      "R2=", round(summary(ols.sat)$r.squared, 2))),
          xlab="Relative importance (%)", cex.axis=1.5, cex.lab=1.5, cex.main=1.5, las=1)
}


