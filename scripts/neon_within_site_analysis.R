## TITLE:         NEON within-site analysis of Organismal Data
## AUTHOR:        Phoebe Zarnetske, Cameo Arnold 
## COLLABORATORS: Sydne Record (Bryn Mawr), Ben Baiser (UFL), Angela Strecker (PSU), 
##                John M. Grady (MSU/Bryn Mawr), Jonathan Belmaker (Tel Aviv U), Mao-Ning Tuanmu (Academia Sinica),
##                Lydia Beaudrot (Rice U), Kate Thibault 
## DATA:          NEON organismal data: all species, all years, all sites
## PROJECT:       "NEON's continental-scale biodiversity"
## DATE:          initiated: June 28, 2018; last run:

## This script reads NEON's organismal richness data at the plot level, 
# per all available sites, and per year. 
# Linear models are generated to investigate how disturbance variables explain the
# variation in richness across plots, per taxonomic group.

## On HPCC; add this command to load recent R version
#module swap GNU GNU/4.9
#module load OpenMPI 1.10.0
#module load R/3.3.2

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

# Set file paths
google_drive <- '/Volumes/GoogleDrive/My Drive/Research/NEON_LTER_2018/data' # if you are Phoebe
setwd(file.path(google_drive, 'final_data/neon')) # GD location
# file path location for LTER disturbance and richness data by NEON plot
data_path <- file.path(google_drive, 'final_data/neon')
fig_path <- file.path(google_drive, 'output')

#Install/load packages
for (package in c("lme4", "dplyr", "purrr", "reshape2", "lubridate", "ggplot2")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# This code for ggplot2 sets the theme to mostly black and white 
# (Arial font, and large font, base size=24)
theme_set(theme_bw(12))
theme_update(axis.text.x = element_text(size = 10, angle = 90),
             axis.text.y = element_text(size = 10))

# ----------------------------------------
# Load all organismal richness and within-site neon plot-level disturbance data 
# ----------------------------------------

# Read in plot-level richness data from each taxon
# The raw data were all pulled on 20 June 2018; github/NEON-biodiversity/neonbiodiversity/code/neon_organism_api_export.R 

# Cameo: in the R script used to generate these disturbance data, rename the file from
#"combined_disturbance_data.csv" to "neon_plotid_dist2disturbance.csv" 
dist<-read.csv(file.path(data_path,"./disturbance/combined_distance_data.csv"), stringsAsFactors = FALSE)
mammals <- read.csv(file.path(data_path,"./richness/mammal_richness_cumulative_plot.csv"), stringsAsFactors = FALSE)
# etc. fill these in for all plot-level data

# add a column for taxa
mammals$taxa<-mammal
# do this for each taxonomic group

# merge all taxonomic data together; before you do this, make sure there is a column fr
rich<-cbind(mammals, ...) 
# merge the disturbance distances with the richness plot-level dataset
dist.rich<-merge(dist,rich,by=c("siteID","plotID"),all.x=T, all.y=T)

# take a look at the relationship between richness and distance to disturbance, by year, by disturbance type
p <- ggplot(dist.rich, aes(x=distance_m, y=richness)) +
  geom_boxplot(aes(color=factor(dist_type), group=factor(dist_type))) +
  facet_wrap(~taxa)  

# convert data back to wide format for easier modeling (might need to load reshape package)
# can also probably do this in dplyr
# First add a line of code that removes all columns except siteID, plotID, taxa, richness, dist_type, distance_m
***** Add line of code here *******
# Wide format
dist.rich1<-reshape(dist.rich,
                  v.names="dist_type",    # the variable you want to transpose to wide format
                  idvar=c("siteID","plotID"),  # your independent variable(s); careful because if you
                  # keep the other columns in your dataset, it may be confusing 
                  # how the other columns relate to these new columns
                  timevar="distance_m",  # what do you want to fill the column, "v.names" with?
                  direction="wide") # the direction (can also be long)

# For mammals, run linear model to investigate how disturbance explains richness variability
# change to reflect actual column names in dist.rich1 
m1<-lm(dist.rich1$richness~dist.rich1$disturbance1distance+
         dist.rich1$disturbance2distance+
         dist.rich1$disturbance3distance+
         dist.rich1$disturbance4distance)
summary(m1)
anova(m1)

# Check out these example scripts to assess normality, outliers, etc: https://www.statmethods.net/stats/rdiagnostics.html
# use qqplot() at least
# do data look normal? if not, then we will need to transform

