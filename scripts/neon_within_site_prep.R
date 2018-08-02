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
dist<-read.csv(file.path(data_path,"./disturbance/neon_plotid_dist2disturbance_arc.csv"), stringsAsFactors = FALSE)

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

#subset data for Toolik
unique(rich$siteID)
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
write.csv(tool_rich_max, file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\arc\\richness_max.csv")
#some plotIDs are NA for richness because they have not been surveyed yet.

#subset
keep=c("siteID", "plotID", "richness", "taxa")
tool_rich_max<-tool_rich_max[,keep]
head(tool_rich_max)

#merge with distance data
dist_rich<- merge(dist, tool_rich_max, by=c("siteID","plotID"),all.x=T)
head(dist_rich)
names(dist_rich)
#----------------------------------------------------------
# Convert distance file to wide format for modeling
#----------------------------------------------------------
# Wide format for disturbance
dist_rich1<-reshape(dist_rich, v.names="distance_m",    # the values you want to transpose to wide format
               idvar=c("siteID","plotID", "richness", "taxa"),  # your independent variable(s); careful because if you keep the other columns in your dataset, it may be confusing how the other columns relate to these new columns
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

#subset toolik
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

write.csv(arc_env, file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\arc\\arc_environment.csv", row.names=F)
head(arc_env)
#writing a csv is necessary to merge the files. It will NOT work without first writing a csv and importing it again. 

#merge dist_rich1 and arc_env
names(dist_rich1)
names(arc_env)
toolik<-merge(arc_env, dist_rich1, by=c("plotID", "nlcdCls", "elevatn", "slpAspc", "slpGrdn"),all.x=T)
head(toolik)

#Get rid of NAs for plots not sampled yet.
toolik<- subset(toolik,is.na(toolik$richness)==F)
toolik$richness

#Get rid of unused Factor levels
toolik$nlcdCls <- factor(toolik$nlcdCls)
toolik$nlcdCls
toolik$plotID <- factor(toolik$plotID)
toolik$plotID

#Check for duplicates
toolik[duplicated(toolik$plotID),]
#There should be two sets of plotIDs -  one for birds and one for plants.
#-------------------------------------------------------------------------------
# Now we will pull in temperature and precipitation data
#-------------------------------------------------------------------------------
#Toolik Temp Data
library(rgdal)
arc_temp<-raster("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data\\arc\\gis_data\\ak_tempmean\\tmeananl\\hdr.adf")
plot(arc_temp)
plot(arc, add=T)
arc_temp_spdf = SpatialPointsDataFrame(arc@data[,20:19], proj4string=arc_temp@crs,arc@data)
arc_temp_spdf

tool_temp<- extract(arc_temp, arc, tool_temp=TRUE, method='bilinear')
plot(tool_temp)
tool_temp_avg<-(tool_temp/100)
tool_temp_avg

#transform to dataframe
class(tool_temp_avg)
tool_temp_avg<-data.frame(tool_temp_avg)
head(tool_temp_avg)
names(tool_temp_avg)[names(tool_temp_avg)=="tool_temp_avg"]<-"arc_tmean"
head(tool_temp_avg)

#Toolik Precipitaiton Data
arc_ppt<-raster("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data\\arc\\gis_data\\ak_precipmean\\pptanl\\hdr.adf")
plot(arc_ppt)
plot(arc, add=T)
arc_ppt_spdf = SpatialPointsDataFrame(arc@data[,20:19], proj4string=arc_ppt@crs,arc@data)
arc_ppt_spdf
tool_ppt<- extract(arc_ppt, arc, tool_ppt=TRUE, method='bilinear')
plot(tool_ppt)
tool_ppt #mean values of precipitation in meters per neon plot in Toolik

#transform to dataframe
class(tool_ppt)
tool_ppt<-data.frame(tool_ppt)
class(tool_ppt)
names(tool_ppt)[names(tool_ppt)=="arc_ppt"]<-"arc_precip"
head(tool_ppt)

#combine with original shape file.
#Toolik
arc<-data.frame(arc@data)
toolik_ppt<-cbind(tool_ppt, arc)
summary(toolik_ppt)
toolik_temp<-cbind(tool_temp_avg, arc)
summary(toolik_temp)
names(toolik_temp)
names(toolik_ppt)

#subset
keep=c("plotID", "arc_tmean")
toolik_temp1<-toolik_temp[,keep]
head(toolik_temp1)

#Check for duplicates
toolik_temp1[duplicated(toolik_temp1$plotID),]

#Take the average. We want one value per plotID
attach(toolik_temp1)
toolik_temp1_mean<-aggregate(toolik_temp1[c("arc_tmean")],list(plotID=plotID),FUN=mean)
detach(toolik_temp1)
head(toolik_temp1_mean)

#Check again for duplicates
toolik_temp1_mean[duplicated(toolik_temp1_mean$plotID),]
names(toolik_temp1_mean)[names(toolik_temp1_mean)=="arc_tmean"]<-"tool_tmean"
head(toolik_temp1_mean)

#subset
keep=c("plotID", "tool_ppt")
toolik_ppt1<-toolik_ppt[,keep]
head(toolik_ppt1)

#Check for duplicates
toolik_ppt1[duplicated(toolik_ppt1$plotID),]

#Take the average. We want one value per plotID
attach(toolik_ppt1)
toolik_ppt1_mean<-aggregate(toolik_ppt1[c("tool_ppt")],list(plotID=plotID),FUN=mean)
detach(toolik_ppt1)

#Check again for duplicates
toolik_ppt1_mean[duplicated(toolik_ppt1_mean$plotID),]

#combine
ttp<-merge(toolik_ppt1_mean, toolik_temp1_mean, by="plotID")
head(ttp)

#Check again for duplicates
ttp[duplicated(ttp$plotID),]

#combine with toolik
ttp1<- merge(toolik, ttp, by="plotID")
head(ttp1)

#Now that we have a code with all of the environment, richness, and distance data we can start subsetting by taxa.
#-------------------------------------------------------------------------------
#Separate birds and plants richness
#--------------------------------------------------------------------------------
#birds
arc_bird<-ttp1[ttp1$taxa=="bird",]
head(arc_bird)

#plants
arc_plant<-ttp1[ttp1$taxa=="plant",]
head(arc_plant)
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
save.image("neon_within_site_prep.RData")
#################################################################################
#Now you're ready for modeling. See script neon_within_site_analysis.


