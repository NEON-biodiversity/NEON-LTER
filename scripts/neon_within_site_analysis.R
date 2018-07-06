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
google_drive <- 'G:\\My Drive\\NEON_LTER_2018\\data'
setwd(file.path(google_drive, 'final_data\\neon')) # GD location
# file path location for LTER disturbance and richness data by NEON plot
data_path <- file.path(google_drive, 'final_data\\neon')
fig_path <- file.path(google_drive, 'output')

#Install/load packages
for (package in c("lme4", "plyr", "dplyr", "purrr", "raster", "reshape2", "lubridate", "ggplot2")) {
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
#disturbance distance
dist<-read.csv(file.path(data_path,"./disturbance/neon_plotid_dist2disturbance.csv"), stringsAsFactors = FALSE)

#organismal data
mammals <- read.csv(file.path(data_path,"./richness/mammal_richness_cumulative_plot.csv"), stringsAsFactors = FALSE)
plants<- read.csv(file.path(data_path,"./richness/plant_richness_cumulative_plot.csv"))
birds<- read.csv(file.path(data_path, "./richness/bird_richness_cumulative_plot.csv"))
#beetles<- read.csv(file.path(data_path, "./richness/beetles_richness_cumulative_plot.csv"))
#macroinvert<- read.csv(file.path(data_path, "./richness/macroinvert_richness_cumulative_plot.csv"))
#mosq<- read.csv(file.path(data_path, "./richness/mosq_richness_cumulative_plot.csv"))
#phytop<- read.csv(file.path(data_path,./richness/phytop_richness_cumulative_plot.csv"))
#trees<- read.csv(file.path(data_path,"./richness/tree_richness_cumulative_plot.csv"))
#zoop<- read.csv(file.path(data_path,"./richness/zoop_richness_cumulative_plot.csv"))

#Check files
head(mammals)
head(birds)
head(plants)
#head(beetles)
#head(macroinvert)
#head(mosq)
#head(phytop)
#head(trees)
#head(zoop)

# add a column for taxa
mammals$taxa<-"mammal"
plants$taxa<-"plant"
birds$taxa<-"bird"
#beetles$taxa<-"beetle"
#macroinvert$taxa<-"macroinvertebrates"
#mosq$taxa<-"mosquito"
#phytop$taxa<-"phytoplankton"
#trees$taxa<-"tree"
#zoop$taxa<-"zooplankton"

# do this for each taxonomic group
head(mammals)
head(birds)
head(plants)
#head(beetles)
#head(macroinvert)
#head(mosq)
#head(phytop)
#head(trees)
#head(zoop)

# merge all taxonomic data together; before you do this, make sure there is a column for taxa.
rich<-rbind(mammals, birds, plants)

#subset data for Toolik.
unique(rich$siteID)
richness<-rich[rich$siteID=="TOOL",]
head(richness)
str(richness)

# merge the disturbance distances with the richness plot-level dataset
dist.rich<-merge(dist,rich,by=c("siteID","plotID"),all.x=T, all.y=T)
head(dist.rich)

#subset Toolik for now because we only have disturbance data for it.
unique(dist.rich$siteID)
dist.rich<-dist.rich[dist.rich$siteID=="TOOL",]
head(dist.rich)
unique(dist.rich$siteID)
summary(dist.rich)

#subset maximum richness per plotID.
#attach(dist.rich)
#dist.rich.max<-aggregate(dist.rich[c("richness")],list(siteID=siteID,plotID=plotID, taxa=taxa, year=year, dist_type=dist_type, distance_m=distance_m), FUN=max, na.rm=T)
#detach(dist.rich)

dist.rich.max<-dist.rich%>%
  group_by(plotID, taxa, dist_type) %>%
  slice(which.max(richness))
head(dist.rich.max)
dist.rich.max<-data.frame(dist.rich.max)
unique(dist.rich.max$plotID)
write.csv(dist.rich.max, file="dist.richtest.csv")
#some plotIDs are NA for richness because they have not been surveyed yet.

# take a look at the relationship between richness and distance to disturbance, by year, by disturbance type
p <- ggplot(dist.rich.max, aes(x=distance_m, y=richness)) +
  geom_point(aes(color=factor(dist_type), group=factor(dist_type))) +
  facet_wrap(~taxa, scales="free_y")  
p
#look at richness distribution by taxa
q <- ggplot(dist.rich.max, aes(richness)) +
  geom_histogram() +
  facet_wrap(~taxa, scales="free")  
q
#log transform for scatter plot
r <- ggplot(dist.rich.max, aes(x=distance_m, y=richness)) +
  geom_point(aes(color=factor(dist_type), group=factor(dist_type))) +
  facet_wrap(~taxa, scales="free_y") + scale_y_log10()  
r
#log transform for histogram
q <- ggplot(dist.rich.max, aes(richness)) +
  geom_histogram() +
  facet_wrap(~taxa, scales="free") + scale_x_log10()  
q
##################################################################################################################################################################
# ------------------------------------------------------------
# Now we are going to look at elevation with species richness
# ------------------------------------------------------------

#elevation subset from all NEON spatial data
terrestrial<- readOGR("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data\\neon\\spatial_data\\All_NEON_TOS_Plots_V4\\All_Neon_TOS_Centroid_V4.shp")
plot(terrestrial, col=cm.colors(5), alpha=1, legend=F, main="Terrestrial Data")

#To subset the data specifically to your site. You must use the exact name as it is written in the data. To look this up, use the following function to list all of the names of the field sites.
unique(terrestrial$siteNam)
arc<-terrestrial[terrestrial$siteNam=="Toolik Lake",]
summary(arc)
plot(arc)

#subset elevation data
myvars <- c("elevatn", "siteID", "plotID")
elevation <- arc[myvars]
head(elevation)
str(elevation)
#Currently, it is in latitude and longitude, but in order to measure distance in meters we need to reproject the data into UTMs. You should look up the appropriate zone for your site. For the Toolik Lake Field Station we needed UTM Zone 6. 
arc_elev<-spTransform(elevation, CRS("+proj=utm +zone=6 ellps=WGS84"))
str(arc_elev)

write.csv(arc_elev, file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\arc_elevation.csv", row.names=F)
head(arc_elev)
#writing a csv is necessary to merge the files. It will NOT work without first writing a csv and importing it again. 

#merge the elevation data with richness plot-level dataset
tool_elev<-read.csv("G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\arc_elevation.csv")
elev.rich<-merge(tool_elev,richness,by=c("siteID", "plotID"), all.x=T, all.y=T)
head(elev.rich)

#subset maximum richness per plotID.
rich.max<-elev.rich%>%
  group_by(plotID, taxa, elevatn) %>%
  slice(which.max(richness))
head(rich.max)
rich.max<-data.frame(rich.max)
unique(rich.max$plotID)
write.csv(rich.max, file="elev.rich.csv")

# take a look at the relationship between richness and elevation, by year
e <- ggplot(rich.max, aes(x=elevatn, y=richness)) +
  geom_point(aes()) + facet_wrap(~taxa, scales="free_y")  
e

# log transform for scatter plot
el <- ggplot(rich.max, aes(x=elevatn, y=richness)) +
  geom_point(aes()) +
  facet_wrap(~taxa, scales="free_y") + scale_y_log10()  
el

##################################################################################################################################################################
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


Sent from Mail for Windows 10

