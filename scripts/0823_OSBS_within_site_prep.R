# Title:        osbs Within Site Analysis
# Site:         osbs Forest
# Data Sources: 
# Authors:      Phoebe Zarnetske, Cameo Arnold, Huijie Wei
# Date:         20 Aug 2018

#Clear environment
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
osbs_dist<-read.csv("G:/My Drive/NEON_LTER_2018/data/final_data/neon/disturbance/neon_plotid_dist2disturbance_osbs.csv")

#organismal data
mammals <- read.csv("./richness/mammal_richness_cumulative_plot.csv")
plants<- read.csv("./richness/plant_richness_cumulative_plot.csv")
birds<- read.csv( "./richness/bird_richness_cumulative_plot.csv")
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

#Check each taxonomic group
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
rich<-rbind(birds, plants,mammals)

#subset data for OSBS
unique(rich$siteID)
#use for adding OSBS data
osbs_rich<-rich[rich$siteID=="OSBS",]
head(osbs_rich)
str(osbs_rich)

#Take the maximum richness value before merging with distance data
names(osbs_rich)
osbs_rich_max<-osbs_rich%>%
  group_by(plotID, taxa) %>%
  slice(which.max(richness))
head(osbs_rich_max)
osbs_rich_max<-data.frame(osbs_rich_max)
unique(osbs_rich_max$plotID)
write.csv(osbs_rich_max, file="richness_max_osbs.csv")
#some plotIDs are NA for richness because they have not been surveyed yet.

#subset
keep=c("siteID", "plotID", "richness", "taxa")
osbs_rich_max<-osbs_rich_max[,keep]
head(osbs_rich_max)

#merge with distance data
osbs_dist_rich<- merge(osbs_dist, osbs_rich_max, by=c("siteID","plotID"),all.x=T)
head(osbs_dist_rich)
names(osbs_dist_rich)
#----------------------------------------------------------
# Convert distance file to wide format for modeling
#----------------------------------------------------------
# Wide format for disturbance
osbs_dist_rich1<-reshape(osbs_dist_rich, v.names="distance_m",    # the values you want to transpose to wide format
                    idvar=c("siteID","plotID", "richness", "taxa"),  # your independent variable(s); careful because if you keep the other columns in your dataset, it may be confusing how the other columns relate to these new columns
                    timevar="dist_type",  # the name of the grouping variable.
                    direction="wide") # the direction (can also be long)
str(osbs_dist_rich1)
head(osbs_dist_rich1)

write.csv(osbs_dist_rich1, file="~/Documents/NEON_LTER_2018/dist_rich.csv", row.names=F)

## add the fire frequency and the burn times data
library(rgdal)
firelog<-readOGR("G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\osbs\\osbs_points\\01osbs_points.shp")
names(firelog)

#subset elevation, slope, aspect, and landcover fire frequency and last burn terrestrial data
myvars_fire <- c("elevatn", "siteID", "plotID", "nlcdCls", "slpGrdn", "slpAspc","Freq")
env_fire <- firelog[myvars_fire]
head(env_fire)
str(env_fire)

write.csv(env_fire,file = "osbs_fire_data.csv",row.names = F)

#Currently, it is in latitude and longitude, but in order to measure distance in meters we need to reproject the data into UTMs. You should look up the appropriate zone for your site. For osbs we needed UTM Zone 17. 
env_fire_ter<-spTransform(env_fire, CRS("+proj=utm +zone=17 ellps=WGS84"))
str(env_fire_ter)
env_fire_ter<-data.frame(env_fire_ter)

#We need to make sure we have one value for slope and elevation before proceeding. This shows the duplicate entries by plotID
env_fire_ter[duplicated(env_fire_ter$plotID),]

#Take the average of slp, asp, and elev by plotID.
attach(env_fire_ter)
env_fire_ter<-aggregate(env_fire_ter[c("elevatn","slpAspc","slpGrdn","Freq")],list(plotID=plotID, nlcdCls=nlcdCls),FUN=mean)
detach(env_fire_ter)
head(env_fire_ter)

#Check it
env_fire_ter[duplicated(env_fire_ter$plotID),]

## merge the fire data and richness data
head(osbs_dist_rich1)
head(env_fire)
osbs_dist_rich2<-left_join(osbs_dist_rich1, env_fire@data, by=c("plotID", "nlcdCls", "elevatn", "slpAspc", "slpGrdn"))
head(osbs_dist_rich2)

write.csv(osbs_dist_rich2,file = "dist_rich_fire_data.csv",row.names = F)

# ------------------------------------------------------------
# Now we are going to look at elevation with species richness
# ------------------------------------------------------------

#elevation subset from all NEON spatial data
terrestrial<- readOGR("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data\\neon\\spatial_data\\All_NEON_TOS_Plots_V4\\All_NEON_TOS_Centroid_V4.shp")
plot(terrestrial, col=cm.colors(5), alpha=1, legend=F, main="Terrestrial Data")
head(terrestrial)

#To subset the data specifically to your site. You must use the exact name as it is written in the data. To look this up, use the following function to list all of the names of the field sites.
unique(terrestrial$siteNam)

#subset osbs
osbs_ter<-terrestrial[terrestrial$siteNam=="Ordway-Swisher Biological Station",]
summary(osbs_ter)
names(osbs_ter)
plot(osbs_ter)

#subset elevation, slope, aspect, and landcover from NEON terrestrial data
myvars <- c("elevatn", "siteID", "plotID", "nlcdCls", "slpGrdn", "slpAspc")
env <- osbs_ter[myvars]
head(env)
str(env)

#Currently, it is in latitude and longitude, but in order to measure distance in meters we need to reproject the data into UTMs. You should look up the appropriate zone for your site. For osbs we needed UTM Zone 17. 
osbs_ter_env<-spTransform(env, CRS("+proj=utm +zone=17 ellps=WGS84"))
str(osbs_ter_env)
osbs_ter_env<-data.frame(osbs_ter_env)

#We need to make sure we have one value for slope and elevation before proceeding. This shows the duplicate entries by plotID
osbs_ter_env[duplicated(osbs_ter_env$plotID),]

#Take the average of slp, asp, and elev by plotID.
attach(osbs_ter_env)
osbs_ter_env<-aggregate(osbs_ter_env[c("elevatn","slpAspc","slpGrdn")],list(plotID=plotID, nlcdCls=nlcdCls),FUN=mean)
detach(osbs_ter_env)
head(osbs_ter_env)

#Check it
osbs_ter_env[duplicated(osbs_ter_env$plotID),]

write.csv(osbs_ter_env, file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\osbs\\osbs_environment.csv", row.names=F)
head(osbs_ter_env)
#writing a csv is necessary to merge the files. It will NOT work without first writing a csv and importing it again. 

#merge dist_rich1 and osbs_ter_env
names(osbs_dist_rich2)
names(osbs_ter_env)
osbs<-merge(osbs_ter_env, osbs_dist_rich2, by=c("plotID", "nlcdCls", "elevatn", "slpAspc", "slpGrdn"),all=T)
head(osbs)

#Get rid of NAs for plots not sampled yet.
osbs<- subset(osbs,is.na(osbs$richness)==F)
osbs$richness
osbs[is.na(osbs)] <- 0
head(osbs)

#Get rid of unused Factor levels
osbs$nlcdCls <- factor(osbs$nlcdCls)
osbs$nlcdCls
osbs$plotID <- factor(osbs$plotID)
osbs$plotID

#Check for duplicates
osbs[duplicated(osbs$plotID),]

write.csv(osbs,"osbs_disturbance",row.names = F)

#Now that we have a code with all of the environment, richness, and distance data we can start subsetting by taxa.
#-------------------------------------------------------------------------------
#Separate birds and plants richness
#--------------------------------------------------------------------------------
#birds
osbs_bird<-osbs[osbs$taxa=="bird",]
head(osbs_bird)

#plants
osbs_plant<-osbs[osbs$taxa=="plant",]
head(osbs_plant)

#mammals
osbs_mammal<-osbs[osbs$taxa=="mammal",]
head(osbs_mammal)
#################################################################################
# take a look at the relationship between plant richness and elevation
ep <- ggplot(osbs_plant, aes(x=elevatn, y=richness)) +
  geom_point(aes()) 
ep

# log transform for scatter plot
epl <- ggplot(osbs_plant, aes(x=elevatn, y=richness)) +
  geom_point(aes()) + scale_y_log10()  
epl

# take a look at the relationship between bird richness and elevation, by year
er <- ggplot(osbs_bird, aes(x=elevatn, y=richness)) +
  geom_point(aes()) 
er

# log transform for scatter plot
erl <- ggplot(osbs_bird, aes(x=elevatn, y=richness)) +
  geom_point(aes()) + scale_y_log10()  
erl

# take a look at the relationship between mammal richness and elevation, by year
em <- ggplot(osbs_mammal, aes(x=elevatn, y=richness)) +
  geom_point(aes()) 
em

# log transform for scatter plot
eml <- ggplot(osbs_mammal, aes(x=elevatn, y=richness)) +
  geom_point(aes()) + scale_y_log10()  
eml

#################################################################################
#Histograms

hist(osbs_plant$richness)
hist(osbs_bird$richness)
hist(osbs_mammal$richness)

#################################################################################
#log transformations

#plant
osbs_plant$ln.richness<-log(osbs_plant$richness)
head(osbs_plant)
hist(osbs_plant$ln.richness)


#birds
osbs_bird$ln.richness<-log(osbs_bird$richness)
head(osbs_bird)
hist(osbs_bird$ln.richness)

#mammals
osbs_mammal$ln.richness<-log(osbs_mammal$richness)
head(osbs_mammal)
hist(osbs_mammal$ln.richness)

save.image("neon_within_site_prep.RData")
