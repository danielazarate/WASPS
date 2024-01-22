#!/bin/R
# A helpful R script to plot recombination rates across scaffolds.

# Set working directory:
setwd("~/Desktop/WASPS/vespula_vidua/final_linkage_map/")

# Load some libraries:
library(tidyr)
library(ggplot2)
library(stringr)
library(gridExtra)
library(grid)
library(cowplot)
library(dplyr)

# Import the scaffold01 centimorgan x physical distance data:
# No changed necessary to graph each scaffold individually: 
linkageMap <- read.table("v.vidua.ori.inv.linkMap.xlsx.condensed", stringsAsFactors = F, header = FALSE) 

# Isolate Scaffolds from the linkageMap:
scaff1<-linkageMap[str_detect(linkageMap$V1, "Scaffold01"), ]
scaff2<-linkageMap[str_detect(linkageMap$V1, "Scaffold02"), ]
scaff3<-linkageMap[str_detect(linkageMap$V1, "Scaffold03"), ]
scaff4<-linkageMap[str_detect(linkageMap$V1, "Scaffold04"), ]
scaff5<-linkageMap[str_detect(linkageMap$V1, "Scaffold05"), ]
scaff6<-linkageMap[str_detect(linkageMap$V1, "Scaffold06"), ]
scaff7<-linkageMap[str_detect(linkageMap$V1, "Scaffold07"), ]
scaff8<-linkageMap[str_detect(linkageMap$V1, "Scaffold08"), ]
scaff9<-linkageMap[str_detect(linkageMap$V1, "Scaffold09"), ]
scaff10<-linkageMap[str_detect(linkageMap$V1, "Scaffold10"), ]
scaff11<-linkageMap[str_detect(linkageMap$V1, "Scaffold11"), ]
scaff12<-linkageMap[str_detect(linkageMap$V1, "Scaffold12"), ]
scaff13<-linkageMap[str_detect(linkageMap$V1, "Scaffold13"), ]
scaff14<-linkageMap[str_detect(linkageMap$V1, "Scaffold14"), ]
scaff15<-linkageMap[str_detect(linkageMap$V1, "Scaffold15"), ]
scaff16<-linkageMap[str_detect(linkageMap$V1, "Scaffold16"), ]
scaff17<-linkageMap[str_detect(linkageMap$V1, "Scaffold17"), ]
scaff18<-linkageMap[str_detect(linkageMap$V1, "Scaffold18"), ]
scaff19<-linkageMap[str_detect(linkageMap$V1, "Scaffold19"), ]
scaff20<-linkageMap[str_detect(linkageMap$V1, "Scaffold20"), ]
scaff21<-linkageMap[str_detect(linkageMap$V1, "Scaffold21"), ]
scaff22<-linkageMap[str_detect(linkageMap$V1, "Scaffold22"), ]
scaff23<-linkageMap[str_detect(linkageMap$V1, "Scaffold23"), ]
 # No 24
scaff25<-linkageMap[str_detect(linkageMap$V1, "Scaffold25"), ]

# Rename the scaffold file column headers:

# Make a list of all the dataframes;
scaffolds <- c("scaff1", "scaff2", "scaff3", "scaff4", "scaff5", "scaff6", 
            "scaff7", "scaff8", "scaff9", "scaff10", "scaff11", "scaff12", "scaff13",
            "scaff14", "scaff15", "scaff16", "scaff17", "scaff18", "scaff19", "scaff20",
            "scaff21", "scaff22", "scaff23", "scaff24", "scaff25")

for (i in scaffolds) {
  x=get(i)
  colnames(x) <- c("scaffold", "centiMorgan", "position")
  assign(i,x)
}

# Turn off scientific notation:
options(scipen = 999)

# Make a nice ggplot2 dot graph for Scaffold01:
scaff1$title <- "Scaffold 01"
scaffPlot1 <- ggplot(data=scaff1, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "firebrick2", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot1

# And then do this for every single Scaffold, with each one differing on presentation:

# Scaffold 2 should color the two linkage groups differently and then add black lines \
# to demonstrate linkage group breaks. 

scaff2$title <- "Scaffold 02"
scaffPlot2 <- ggplot(data=scaff2, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(aes(colour = ifelse(centiMorgan>650, 'chocolate2', 'darkred')), size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) + 
  scale_color_identity() + geom_hline(yintercept = c(621.928)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot2

# Scaffold 03, the one outlier needs to be removed. 

# ReNumber row names as they are out of order following removal of NaN rows:
rownames(scaff3) <- NULL

# Remove the one outlier:
scaff3.1 <- scaff3[-c(62), ]

scaff3.1$title <- "Scaffold 03"
scaffPlot3 <- ggplot(data=scaff3.1, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "mediumpurple4", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot3

# Scaffold 4 doesn't need any work:

scaff4$title <- "Scaffold 04"
scaffPlot4 <- ggplot(data=scaff4, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "skyblue4", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot4

# Scaffold 5 needs one outlier removed:

# ReNumber row names as they are out of order following removal of NaN rows:
rownames(scaff5) <- NULL
# Remove the one outlier:
scaff5.1 <- scaff5[-c(51), ]

scaff5.1$title <- "Scaffold 05"
scaffPlot5 <- ggplot(data=scaff5.1, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "steelblue2", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_for4mat(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot5

# Scaffold 06 needs to be broken into two LGs:

scaff6$title <- "Scaffold 06"
scaffPlot6 <- ggplot(data=scaff6, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(aes(colour = ifelse(centiMorgan>1600, 'chartreuse4', 'mediumaquamarine')), size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  scale_color_identity() + geom_hline(yintercept = c(1616.952)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot6

# Scaffold 7 is fine, doesn't need anything:

scaff7$title <- "Scaffold 07"
scaffPlot7 <- ggplot(data=scaff7, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "mediumseagreen", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot7

# Scaffold 8 needs to be broken into two LGs:

scaff8$title <- "Scaffold 08"
scaffPlot8 <- ggplot(data=scaff8, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(aes(colour = ifelse(centiMorgan>2010, 'seagreen3', 'darkcyan')), size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  scale_color_identity() + geom_hline(yintercept = c(2010.959)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot8

# Scaffold 9 doesn't need anything:

scaff9$title <- "Scaffold 09"
scaffPlot9 <- ggplot(data=scaff9, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "olivedrab4", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot9

# Scaffold 10 doesn't need anything:

scaff10$title <- "Scaffold 10"
scaffPlot10 <- ggplot(data=scaff10, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "plum4", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot10

# Scaffold 11 doesn't need anything:

scaff11$title <- "Scaffold 11"
scaffPlot11 <- ggplot(data=scaff11, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "magenta4", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot11

# Scaffold 12 doesn't need anything:

scaff12$title <- "Scaffold 12"
scaffPlot12 <- ggplot(data=scaff12, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "maroon", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot12

# Scaffold 13 doesn't need anything:

scaff13$title <- "Scaffold 13"
scaffPlot13 <- ggplot(data=scaff13, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "sienna3", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot13

# Scaffold 14 doesn't need anything:

scaff14$title <- "Scaffold 14"
scaffPlot14 <- ggplot(data=scaff14, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "orange2", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot14

# Scaffold 15 doesn't need anything:

scaff15$title <- "Scaffold 15"
scaffPlot15 <- ggplot(data=scaff15, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "darkorange1", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot15

# Scaffold 16 doesn't need anything:

scaff16$title <- "Scaffold 16"
scaffPlot16 <- ggplot(data=scaff16, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "goldenrod1", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot16

# Scaffold 17 doesn't need anything:

scaff17$title <- "Scaffold 17"
scaffPlot17 <- ggplot(data=scaff17, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "gold2", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot17

# Scaffold 18 doesn't need anything:

scaff18$title <- "Scaffold 18"
scaffPlot18 <- ggplot(data=scaff18, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "yellow3", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot18

# Scaffold 19 doesn't need anything:

scaff19$title <- "Scaffold 19"
scaffPlot19 <- ggplot(data=scaff19, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "goldenrod3", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot19

# Scaffold 20 doesn't need anything:

scaff20$title <- "Scaffold 20"
scaffPlot20 <- ggplot(data=scaff20, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "orange3", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot20

# Scaffold 21 needs one outlier removed:
# ReNumber row names as they are out of order following removal of NaN rows:
rownames(scaff21) <- NULL
# Remove the one outlier:
scaff21.1 <- scaff21[-c(32), ]

scaff21.1$title <- "Scaffold 21"
scaffPlot21 <- ggplot(data=scaff21.1, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "darkorange4", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot21

# Scaffold 22 doesn't need anything:

scaff22$title <- "Scaffold 22"
scaffPlot22 <- ggplot(data=scaff22, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "indianred", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot22

# Scaffold 23 doesn't need anything:

scaff23$title <- "Scaffold 23"
scaffPlot23 <- ggplot(data=scaff23, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "lightpink3", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot23

# Scaffold 25 doesn't need anything:

scaff25$title <- "Scaffold 25"
scaffPlot25 <- ggplot(data=scaff25, aes(x=position, y=centiMorgan, group=1)) +
  geom_point(colour = "hotpink1", size=1) + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
scaffPlot25


# For 24 PCA plots, in a 4x6 grid:
plot_grid(scaffPlot1, scaffPlot2, scaffPlot3, scaffPlot4, scaffPlot5, scaffPlot6, scaffPlot7,
          scaffPlot8, scaffPlot9, scaffPlot10, scaffPlot11, scaffPlot12, scaffPlot13, scaffPlot14,
          scaffPlot15, scaffPlot16, scaffPlot17, scaffPlot18, scaffPlot19, scaffPlot20, scaffPlot21,
          scaffPlot22, scaffPlot23, scaffPlot25, ncol = 4, nrow = 6) 

# Set the high-resolution png file 
png(filename = "~/Desktop/WASPS/vespula_consobrina/Vespula.consobrina.allscaffolds.png", 
    width = 12, height = 10, res = 400, units = 'in', 
    type = c("quartz")) # for high resolution figure 
dev.off()

# Now create the same 4x 6 grid with the recombination rates

# Chang working directory to the appropriate directory:
setwd("~/Desktop/WASPS/vespula_consobrina/scaffs/")

# Read in the tripleStats.txt file produced from SlopeCalc.py on HPCC:
recomb1 <- read.table("scaff.01.tripleStats.txt", stringsAsFactors = F, header = TRUE) 
recomb2 <- read.table("scaff.02.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb2.1 <- read.table("scaff.02.1.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb3 <- read.table("scaff.03.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb4 <- read.table("scaff.04.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb5 <- read.table("scaff.05.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb6 <- read.table("scaff.06.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb6.1 <- read.table("scaff.06.01.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb7 <- read.table("scaff.07.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb8 <- read.table("scaff.08.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb8.1 <- read.table("scaff.08.01.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb9 <- read.table("scaff.09.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb9.1 <- read.table("scaff.09.01.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb10 <- read.table("scaff.10.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb11 <- read.table("scaff.11.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb11.1 <- read.table("scaff.11.01.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb12 <- read.table("scaff.12.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb13 <- read.table("scaff.13.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb14 <- read.table("scaff.14.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb15 <- read.table("scaff.15.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb16 <- read.table("scaff.16.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb17 <- read.table("scaff.17.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb18 <- read.table("scaff.18.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb19 <- read.table("scaff.19.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb20 <- read.table("scaff.20.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb21 <- read.table("scaff.21.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb22 <- read.table("scaff.22.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb23 <- read.table("scaff.23.tripleStats.txt", stringsAsFactors = F, header = TRUE)
recomb25 <- read.table("scaff.25.tripleStats.txt", stringsAsFactors = F, header = TRUE)

# Make a list of all the dataframes;
recombination <- list(recomb1, recomb2, recomb2.1, recomb3, recomb4, recomb5, 
               recomb6, recomb6.1, recomb7, recomb8, recomb8.1, recomb9, recomb9.1,
               recomb10, recomb11, recomb11.1,recomb12, recomb13,
               recomb14, recomb15, recomb16, recomb17, recomb18, recomb19, recomb20,
               recomb21, recomb22, recomb23, recomb25)

# Remove NaNs from all the dataframes in the list 
recombination_clean <- lapply(recombination, function(x) { na.omit(x)})

# Recombination files have been edited prior to import into R. 
# Outliers have already been removed. 

# Log transform the slope values:
# Add log values as a new column: 
# Calculate absolute value of logs because they first are negative values:

logtransform <- lapply(recombination_clean, function(x) { x$logSlope <- abs(log(x$slope)); return(x) })

# Very small positive values generally turn into large (e.g. 11) negative values. 
# Zeros turn into -Inf values. 
# Very small negative values turn into NaNs. 
# Define two functions that replace "NaN" and "Inf" values in the dataframe:
# Replaces all Nan values: 

finiteList <- lapply(logtransform, function(x) { x$logSlope[!is.finite(x$logSlope)] <- 0; return(x) })

# Before I get into the individual recombination graphs, let's name all of the dateframes:

names(finiteList) <- c("rcmb1", "rcmb2", "rcmb2.1", "rcmb3", "rcmb4", "rcmb5", "rcmb6", 
                       "rcmb6.1","rcmb7", "rcmb8", "rcmb8.1","rcmb9", "rcmb9.1", "rcmb10", 
                       "rcmb11", "rcmb11.1", "rcmb12", "rcmb13", "rcmb14", "rcmb15", "rcmb16", "rcmb17", 
                       "rcmb18", "rcmb19", "rcmb20", "rcmb21", "rcmb22", "rcmb23", "rcmb25")

## RECOMBINATION 01 ## 

# Make a nice ggplot2 line graph per Scaffold:
finiteList[['rcmb1']]$title <- "Scaffold 01"
recomb1.plot <- ggplot(data=finiteList[['rcmb1']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(color="firebrick2")+
  geom_point(color="firebrick2") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15))  + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb1.plot

# 5 negative values turned to Zeros. 

## RECOMBINATION 02 ## 

# Scaffold 02 is split into two parts and then plotted on the same graph: 

finiteList[['rcmb2']]$title <- "Scaffold 02"
finiteList[['rcmb2.1']]$title <- "Scaffold 02"

recomb2.plot = ggplot() + 
  geom_point(data = finiteList[['rcmb2']], aes(x = mean, y = logSlope), color = "darkred") +
  geom_point(data = finiteList[['rcmb2.1']], aes(x = mean, y = logSlope), color = "chocolate2") +
  geom_line(data = finiteList[['rcmb2']], aes(x = mean, y = logSlope), colour = "darkred") +
  geom_line(data = finiteList[['rcmb2.1']], aes(x = mean, y = logSlope), colour = "chocolate2") +
  geom_vline(xintercept = c(21595000)) +
  theme_bw() + scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(axis.title = element_text(face="plain",size=15)) + facet_grid(. ~ title)
recomb2.plot

# 14 negative values turned to Zeros for part1 (large part)
# 2 negative values turned to Zeros for part2 (small part)

## RECOMBINATION 03 ## 

finiteList[['rcmb3']]$title <- "Scaffold 03"
recomb3.plot <- ggplot(data=finiteList[['rcmb3']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "mediumpurple4")+
  geom_point(colour = "mediumpurple4") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb3.plot

# 6 negative values turned to Zeros for part2

## RECOMBINATION 04 ## 

finiteList[['rcmb4']]$title <- "Scaffold 04"
recomb4.plot <- ggplot(data=finiteList[['rcmb4']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "skyblue4")+
  geom_point(colour = "skyblue4") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb4.plot

# 5 negative values turned to Zeros

## RECOMBINATION 05 ## 

finiteList[['rcmb5']]$title <- "Scaffold 05"
recomb5.plot <- ggplot(data=finiteList[['rcmb5']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "steelblue2")+
  geom_point(colour = "steelblue2") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb5.plot

# 6 negative values turned to Zeros 

## RECOMBINATION 06 ## 
# Scaffold 06 is split into two parts and then plotted on the same graph: 

finiteList[['rcmb6']]$title <- "Scaffold 06"
finiteList[['rcmb6.1']]$title <- "Scaffold 06"
recomb6.plot <- ggplot() +
  geom_point(data = finiteList[['rcmb6']], aes(x = mean, y = logSlope), color = "mediumaquamarine") +
  geom_point(data = finiteList[['rcmb6.1']], aes(x = mean, y = logSlope), color = "chartreuse4") + 
  geom_line(data = finiteList[['rcmb6']], aes(x = mean, y = logSlope), colour = "mediumaquamarine") +
  geom_line(data = finiteList[['rcmb6.1']], aes(x = mean, y = logSlope), colour = "chartreuse4") +
  theme_bw() + geom_vline(xintercept = c(71000000)) +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb6.plot

# 1 negative values turned to Zeros for part 1 (smaller)
# 8 negative values turned to Zeros for part 2 (bigger)

## RECOMBINATION 07 ## 

finiteList[['rcmb7']]$title <- "Scaffold 07"
recomb7.plot <- ggplot(data=finiteList[['rcmb7']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "mediumseagreen")+
  geom_point(colour = "mediumseagreen") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb7.plot

# 6 negative values turned to Zeros

## RECOMBINATION 08 ##  
# Needs both lines to be plotted on the same graph:

finiteList[['rcmb8']]$title <- "Scaffold 08"
finiteList[['rcmb8.1']]$title <- "Scaffold 08"
recomb8.plot <- ggplot() +
  geom_point(data = finiteList[['rcmb8']], aes(x = mean, y = logSlope), color = "darkcyan") +
  geom_point(data = finiteList[['rcmb8.1']], aes(x = mean, y = logSlope), color = "seagreen3") + 
  geom_line(data = finiteList[['rcmb8']], aes(x = mean, y = logSlope), colour = "darkcyan") +
  geom_line(data = finiteList[['rcmb8.1']], aes(x = mean, y = logSlope), colour = "seagreen3") +
  theme_bw() + geom_vline(xintercept = c(92000000)) +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb8.plot

# 2 negative values turned to Zeros for part 1 (smaller)
# 2 negative values turned to Zeros for part 2 (bigger)

## RECOMBINATION 09 ## 
# Needs both lines to be plotted on the same graph:

finiteList[['rcmb9']]$title <- "Scaffold 09"
finiteList[['rcmb9.1']]$title <- "Scaffold 09"
recomb9.plot <- ggplot() +
  geom_point(data = finiteList[['rcmb9']], aes(x = mean, y = logSlope), color = "olivedrab4") +
  geom_point(data = finiteList[['rcmb9.1']], aes(x = mean, y = logSlope), color = "olivedrab4") + 
  geom_line(data = finiteList[['rcmb9']], aes(x = mean, y = logSlope), colour = "olivedrab4") +
  geom_line(data = finiteList[['rcmb9.1']], aes(x = mean, y = logSlope), colour = "olivedrab4") +
  theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb9.plot

# 4 negative values turned to Zeros for part 1 (smaller)
# 1 negative values turned to Zeros for part 1 (bigger)

## RECOMBINATION 10 ## 

finiteList[['rcmb10']]$title <- "Scaffold 10"
recomb10.plot <- ggplot(data=finiteList[['rcmb10']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "plum4")+
  geom_point(colour = "plum4") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb10.plot

# 2 negative values turned to Zeros

## RECOMBINATION 11 ## 
# Needs both lines to be plotted on the same graph:

finiteList[['rcmb11']]$title <- "Scaffold 11"
finiteList[['rcmb11.1']]$title <- "Scaffold 11"
recomb11.plot <- ggplot() +
  geom_point(data = finiteList[['rcmb11']], aes(x = mean, y = logSlope), color = "magenta4") +
  geom_point(data = finiteList[['rcmb11.1']], aes(x = mean, y = logSlope), color = "magenta4") + 
  geom_line(data = finiteList[['rcmb11']], aes(x = mean, y = logSlope), colour = "magenta4") +
  geom_line(data = finiteList[['rcmb11.1']], aes(x = mean, y = logSlope), colour = "magenta4") +
  theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb11.plot

# 1 negative values turned to Zeros for part 1 (smaller)
# 3 negative values turned to Zeros for part 2 (bigger)

## RECOMBINATION 12 ## 

finiteList[['rcmb12']]$title <- "Scaffold 12"
recomb12.plot <- ggplot(data=finiteList[['rcmb12']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "maroon")+
  geom_point(colour = "maroon") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb12.plot

# 3 negative values turned to Zeros

## RECOMBINATION 13 ## 

finiteList[['rcmb13']]$title <- "Scaffold 13"
recomb13.plot <- ggplot(data=finiteList[['rcmb13']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "sienna3")+
  geom_point(colour = "sienna3") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb13.plot

# 3 negative values turned to Zeros

## RECOMBINATION 14 ## 

finiteList[['rcmb14']]$title <- "Scaffold 14"
recomb14.plot <- ggplot(data=finiteList[['rcmb14']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "orange2")+
  geom_point(colour = "orange2") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb14.plot

# 2 negative values turned to Zeros

## RECOMBINATION 15 ## 

finiteList[['rcmb15']]$title <- "Scaffold 15"
recomb15.plot <- ggplot(data=finiteList[['rcmb15']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "darkorange1")+
  geom_point(colour = "darkorange1") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb15.plot

# 5 negative values turned to Zeros

## RECOMBINATION 16 ## 

finiteList[['rcmb16']]$title <- "Scaffold 16"
recomb16.plot <- ggplot(data=finiteList[['rcmb16']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "goldenrod1")+
  geom_point(colour = "goldenrod1") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb16.plot

# 3 negative values turned to Zeros

## RECOMBINATION 17 ## 

finiteList[['rcmb17']]$title <- "Scaffold 17"
recomb17.plot <- ggplot(data=finiteList[['rcmb17']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "gold2")+
  geom_point(colour = "gold2") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb17.plot

# 1 negative values turned to Zeros

## RECOMBINATION 18 ## 

finiteList[['rcmb18']]$title <- "Scaffold 18"
recomb18.plot <- ggplot(data=finiteList[['rcmb18']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "yellow3")+
  geom_point(colour = "yellow3") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb18.plot

# 3 negative values turned to Zeros

## RECOMBINATION 19 ## 

finiteList[['rcmb19']]$title <- "Scaffold 19"
recomb19.plot <- ggplot(data=finiteList[['rcmb19']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "goldenrod3")+
  geom_point(colour = "goldenrod3") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb19.plot

# 3 negative values turned to Zeros

## RECOMBINATION 20 ## 

finiteList[['rcmb20']]$title <- "Scaffold 20"
recomb20.plot <- ggplot(data=finiteList[['rcmb20']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "orange3")+
  geom_point(colour = "orange3") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb20.plot

# 2 negative values turned to Zeros

## RECOMBINATION 21 ## 

finiteList[['rcmb21']]$title <- "Scaffold 21"
recomb21.plot <- ggplot(data=finiteList[['rcmb21']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "darkorange4")+
  geom_point(colour = "darkorange4") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb21.plot

# 4 negative values turned to Zeros

## RECOMBINATION 22 ## 

finiteList[['rcmb22']]$title <- "Scaffold 22"
recomb22.plot <- ggplot(data=finiteList[['rcmb22']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "indianred")+
  geom_point(colour = "indianred") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb22.plot

# 2 negative values turned to Zeros

## RECOMBINATION 23 ## 

finiteList[['rcmb23']]$title <- "Scaffold 23"
recomb23.plot <- ggplot(data=finiteList[['rcmb23']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "lightpink3")+
  geom_point(colour = "lightpink3") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb23.plot

# 2 negative values turned to Zeros

## RECOMBINATION 25 ## 

finiteList[['rcmb25']]$title <- "Scaffold 25"
recomb25.plot <- ggplot(data=finiteList[['rcmb25']], aes(x=mean, y=logSlope, group=1)) +
  geom_line(colour = "hotpink1")+
  geom_point(colour = "hotpink1") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + facet_grid(. ~ title)
recomb25.plot

# 2 negative values turned to Zeros


# For 24 line plots, in a 4x6 grid:
plot_grid(recomb1.plot, recomb2.plot, recomb3.plot, recomb4.plot, recomb5.plot, recomb6.plot, recomb7.plot, recomb8.plot, 
          recomb9.plot, recomb10.plot, recomb11.plot, recomb12.plot, recomb13.plot, recomb14.plot, recomb15.plot, 
          recomb16.plot, recomb17.plot, recomb18.plot, recomb19.plot, recomb20.plot, recomb21.plot,
          recomb22.plot, recomb23.plot, recomb25.plot, ncol = 4, nrow = 6) 

# Set the high-resolution png file 
png(filename = "~/Desktop/WASPS/vespula_consobrina/Vespula.consobrina.recombination.map.png", 
    width = 12, height = 10, res = 400, units = 'in', 
    type = c("quartz")) # for high resolution figure 
dev.off()


## EXTRA CODE STORAGE BOX ## 

# If needed, pull out dataframes from the list:
foo <- as.data.frame(finiteList[1])

x.grob <- textGrob("V. pennsylvanica physical position (kb)", gp=gpar(fontface="bold", fontsize=10))
y.grob <- textGrob("V. consobrina genetic position (centiMorgan)", gp=gpar(fontface="bold", fontsize=10), rot = 90)
plot <- plot_grid(recomb1.plot, recomb1.log.plot, ncol = 2, nrow = 1) 
grid.arrange(arrangeGrob(plot, left = y.grob, bottom = x.grob))

# If I want to make a figure where I plot the scaffold markers above and then the \
# recombination figure below, start with a nice line graph.
# For example: Scaffold 01: 

scaffPlot <- ggplot(data=scaff01, aes(x=V3, y=centiMorgan, group=1)) +
  geom_line(color="firebrick2")+
  geom_point(color="firebrick2") + theme_bw() +
  scale_x_continuous(labels = scales::unit_format(unit = NULL, scale = 1e-6)) +
  theme(axis.title = element_text(face="plain",size=15)) +  ylab("Genetic Distance (centiMorgan)") +
  theme(axis.title.x = element_blank())

scaffPlot

# I need to make this into a loop:
s01<- scaff01[, 1:3] 
colnames(s01) <- c("Scaffold", "centiMorgan", "Position")
write.table(s25, "scaff25.txt", quote = FALSE, row.names = FALSE)

# For two plots side by side
plot_grid(scaffPlot, recombPlot, labels=c("A", "B"), ncol = 1, nrow = 2)


