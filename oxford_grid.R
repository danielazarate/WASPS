#!/bin/R
# Mini script to make an oxford grid from linkage map data.
# Input file is a text file of two columns, column 1 is continuous cM (Haldane) \
# Second column is continuous position. 

# Load some libraries
library(dplyr)
library(ggplot2)
library(ggplot2)
library(wesanderson)
library(RColorBrewer)

# set working directory
setwd("~/Desktop/WASPS/vespula_consobrina/final_linkage_map/")

# Open the file,
# V1 is scaffold, the V2 is continuous cM and the V3 is continuous position. 
consobrina <- read.delim("v.consobrina.inv.2.0.coordinates.txt", header = F)

# attach linkage
attach(consobrina)

# Rename columns using dplyr
consobrina <- consobrina %>% 
  rename(
    scaffold = V1,
    centiMorgan = V2,
    position = V3
    )

# Find the MAX of cM and position:
max(centiMorgan) # 4341.348
max(position) # 162020712

# Define the number of colors you want
nb.cols <- 24
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)

# To determine intercepts, use the last position of each scaffold, starting with Scaffold 1 \
# and add 25 to the value because we added 50 to each breakpoint. 

# Turn off scientific notation:
options(scipen = 999)

# Use ggplot to create scatterplot:
# y is continuous cm, x is continuous position 
ggplot(consobrina, aes(x=position, y=centiMorgan, colour = scaffold)) +
  scale_color_manual(values = mycolors) +
  geom_point(size=0.8) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(face = "italic", size = 15),
        axis.title = element_text(face="plain",size=15), 
        legend.position = "bottom", legend.title = element_blank()
        ) +
  xlab("Vespula pennsylvanica reference physical position (million)") + ylab("Vespula consobrina genetic position (cM)") +
  geom_vline(xintercept = c(19691610, 38516729, 50697645, 60929545, 69609158, 
                            78157058, 86838448, 94789086, 103144845, 111374554, 
                            118267077, 123680790, 126931588, 131864406,
                            136599416, 141006867, 143442778, 146037339, 
                            148996535, 150538794, 154339211, 157836820,
                            1612129350, 162020737)) +
  geom_hline(yintercept = c(341.625, 621.928, 754.188, 1028.372, 1257.656, 
                            1468.305, 1616.952, 1675.715, 1873.537, 2010.959,
                            2163.998, 2371.684, 2584.954, 2772.705, 2940.213,
                            3056.924, 3202.033, 3385.195, 3530.284, 3633.821, 
                            3723.198, 3860.051, 3922.74, 4059.349, 4183.608, 4298.03, 4391.348)) +
  scale_x_continuous(expand=c(0, 0), limits=c(0, 162020737), labels = scales::unit_format(unit = NULL, scale = 1e-6), breaks = seq(0,162020737, by = 20000000 )) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, 4392.348)) + guides(col = guide_legend(nrow=2, override.aes = list(shape = 15, size = 10)))


# Set the high-resolution png file 
png(filename = "~/Desktop/WASPS/vespula_consobrina/Vespula.consobrina.oxford.grid.v2.jpg", 
    width = 15, height = 15, res = 400, units = 'in', 
    type = c("quartz")) # for high resolution figure 

dev.off()

