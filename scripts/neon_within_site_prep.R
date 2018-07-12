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
rich<-rbind(birds, plants)

#subset data for Toolik and Harvard Forest
unique(rich$siteID)
#Use for adding harvard forest data
#ht_rich<-rich[rich$siteID=="TOOL" | rich$siteID== "HARV",]
#ht_rich$siteID<-factor(ht_rich$siteID)
#head(ht_rich)
#str(ht_rich)
tool_rich<-rich[rich$siteID=="TOOL",]
head(tool_rich)
str(tool_rich)

#Take the maximum richness value before merging with distance data
names(tool_rich)
tool_rich_max<-tool_rich%>%
  group_by(plotID, taxa) %>%
  slice(which.max(richness))
head(tool_rich_max)
tool_rich_max<-data.frame(tool_rich_max)
unique(tool_rich_max$plotID)
write.csv(tool_rich_max, file="richness_max.csv")
#some plotIDs are NA for richness because they have not been surveyed yet.

#subset
keep=c("siteID", "plotID", "richness", "taxa")
tool_rich_max<-tool_rich_max[,keep]
head(tool_rich_max)

#merge with distance data
dist_rich<- merge(dist, tool_rich_max, by=c("siteID","plotID"),all.x=T)
head(dist_rich)

#----------------------------------------------------------
# Convert distance file to wide format for modeling
#----------------------------------------------------------
# Wide format for disturbance
dist_rich1<-reshape(dist_rich, v.names="distance_m",    # the values you want to transpose to wide format
               idvar=c("siteID","plotID", "siteNam", "richness", "taxa"),  # your independent variable(s); careful because if you keep the other columns in your dataset, it may be confusing how the other columns relate to these new columns
               timevar="dist_type",  # the name of the grouping variable.
               direction="wide") # the direction (can also be long)
str(dist_rich1)
head(dist_rich1)

# ------------------------------------------------------------
# Now we are going to look at elevation with species richness
# ------------------------------------------------------------

library(rgdal)

#elevation subset from all NEON spatial data
terrestrial<- readOGR("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data\\neon\\spatial_data\\All_NEON_TOS_Plots_V4\\All_Neon_TOS_Centroid_V4.shp")
plot(terrestrial, col=cm.colors(5), alpha=1, legend=F, main="Terrestrial Data")
head(terrestrial)

#To subset the data specifically to your site. You must use the exact name as it is written in the data. To look this up, use the following function to list all of the names of the field sites.
unique(terrestrial$siteNam)

#Figure out why the richness HARV & TOOL subset didn't work before running the elevation extraction.
#arc<-terrestrial[terrestrial$siteNam=="Toolik Lake","Harvard Forest",]
arc<-terrestrial[terrestrial$siteNam=="Toolik Lake",]
summary(arc)
names(arc)
plot(arc)

#subset elevation, slope, aspect, and landcover from NEON terrestrial data
myvars <- c("elevatn", "siteID", "plotID", "nlcdCls", "slpGrdn", "slpAspc")
env <- arc[myvars]
head(env)
str(env)

#Currently, it is in latitude and longitude, but in order to measure distance in meters we need to reproject the data into UTMs. You should look up the appropriate zone for your site. For the Toolik Lake Field Station we needed UTM Zone 6. 
arc_env<-spTransform(env, CRS("+proj=utm +zone=6 ellps=WGS84"))
str(arc_env)
arc_env<-data.frame(arc_env)

#We need to make sure we have one value for slope and elevation before proceeding. This shows the duplicate entries by plotID
arc_env[duplicated(arc_env$plotID),]

#Take the average of slp, asp, and elev by plotID.
attach(arc_env)
arc_env<-aggregate(arc_env[c("elevatn","slpAspc","slpGrdn")],list(plotID=plotID, nlcdCls=nlcdCls),FUN=mean)
detach(arc_env)
head(arc_env)

#Check it
arc_env[duplicated(arc_env$plotID),]

write.csv(arc_env, file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\arc_environment.csv", row.names=F)
head(arc_env)
#writing a csv is necessary to merge the files. It will NOT work without first writing a csv and importing it again. 

#merge dist_rich1 and arc_env
names(dist_rich1)
names(arc_env)
toolik<-merge(arc_env, dist_rich1, by=c("plotID"),all.x=T)
head(toolik)

#Check for duplicates
toolik[duplicated(toolik$plotID),]
#Now that we have a code with all of the environment, richness, and distance data we can start subsetting by taxa.
#--------------------------------------------------------------------------------
#Separate birds and plants richness
#--------------------------------------------------------------------------------
#birds
arc_bird<-toolik[toolik$taxa=="bird",]
head(arc_bird)

#plants
arc_plant<-toolik[toolik$taxa=="plant",]
head(arc_plant)

save.image("neon_within_site_prep.RData")
#################################################################################
# take a look at the relationship between plant richness and elevation
ep <- ggplot(arc_plant, aes(x=elevatn, y=richness)) +
  geom_point(aes()) 
ep

# log transform for scatter plot
epl <- ggplot(arc_plant, aes(x=elevatn, y=richness)) +
  geom_point(aes()) + scale_y_log10()  
epl

# take a look at the relationship between bird richness and elevation, by year
er <- ggplot(arc_bird, aes(x=elevatn, y=richness)) +
  geom_point(aes()) 
er

# log transform for scatter plot
erl <- ggplot(arc_bird, aes(x=elevatn, y=richness)) +
  geom_point(aes()) + scale_y_log10()  
erl
#################################################################################
#Histograms

hist(arc_plant$richness)
hist(arc_bird$richness)

#################################################################################
#log transformations

#plant
arc_plant$ln.richness<-log(arc_plant$richness)
head(arc_plant)
hist(arc_plant$ln.richness)


#birds
arc_bird$ln.richness<-log(arc_bird$richness)
head(arc_bird)
hist(arc_bird$ln.richness)
#################################################################################
#Now you're ready for modeling. See script neon_within_site_analysis.


