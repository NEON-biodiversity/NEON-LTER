## Title: Calculating the distances between disturbances and NEON sites
## Date: 14 Augest 2018
## Author: Huijie Wei
## Sources:  Cameo Arnold 

# devtools::install_github("tidyverse/googledrive")
library("googledrive")

# Start with a clear workspace and set working directory to google drive -> NEON_LTER_2018 folder -> data -> raw_data
# rm(list=ls())
setwd("~/Documents/NEON_LTER_2018")

#install these packages and load the libraries
library(sp)
library(raster)
library(rgdal)
library(ggplot2)
library (ggmap)
library (ggsn)
library(gridExtra)
# install.packages("rgeos")
library(rgeos)

# Import terrestrial data shape files from file (Google Drive)
terrestrial <- readOGR(dsn="./All_NEON_TOS_Plots_V4/All_NEON_TOS_Centroid_V4.shp")
plot(terrestrial, col=cm.colors(5), alpha=1, legend=F, main="Terrestrial Data")

#To subset the data specifically to your site. You must use the exact name as it is written in the data. 
# To look this up, use the following function to list all of the names of the field sites.
unique(terrestrial$siteNam)
osbs<-terrestrial[terrestrial$siteNam=="Ordway-Swisher Biological Station",]
summary(osbs)
plot(osbs)

#Currently, it is in latitude and longitude, but in order to measure distance in meters we need to reproject 
# the data into UTMs. You should look up the appropriate zone for your site. 
# For the Ordway-Swisher Biological Station we needed UTM Zone 17. 
osbs_UTM<-spTransform(osbs, CRS("+proj=utm +zone=17 ellps=WGS84"))
head(osbs_UTM)
######################################################################################
#Next we need to import and reproject all of the relevant disturbance shape files.
# After importing, always plot to be sure you have the appropriate shapes. 
# Also, check if the files are in UTMs. If not, reproject the same way we did for the NEON terrestrial data. 
# We need UTMs to measure distance in meters.

# OSBS boundary shapefiles data
osbs_boun <- readOGR(dsn="./OSBS Boundary",layer = "OSBS_boundary")
plot(osbs_boun,  col=terrain.colors(10), alpha=1, legend=F, main="OSBS_boundary")
summary(osbs_boun)
osbs_boun<-spTransform(osbs_boun,CRS("+proj=utm +zone=17 ellps=WGS84"))
summary(osbs_boun)

#import os_mu shapefiles--region data
osmu <- readOGR("./OSBS_os_mu", "os_mu")
plot(osmu, col=rainbow(10),
     alpha=1, 
     legend=F, main="os_mu")
summary(osmu)
osmu<-spTransform(osmu,CRS("+proj=utm +zone=17 ellps=WGS84"))
summary(osmu)

#import NEON bird plot data
neonbird<- readOGR("./OSBS_NEON_bird_plots", "NEON_bird_plots")
plot(neonbird, col=grey(1:100/100), legend=FALSE, main="NEON Bird plots")
summary(neonbird)
neonbird<-spTransform(neonbird,CRS("+proj=utm +zone=17 ellps=WGS84"))
summary(neonbird)

#import neon
neon<- readOGR("./OSBS_NEON", "Export_Output")
plot(neon, col=rainbow(16),
     alpha=1,
     legend=F, main="NEON")
summary(neon)
neon<-spTransform(neon,CRS("+proj=utm +zone=17 ellps=WGS84"))
summary(neon)

#import fnai
FNAI<- readOGR("./OSBS_FNAI", "FNAI")
plot(FNAI, col=cm.colors(10), alpha=1, legend=F, main="FNAI")
summary(FNAI)
FNAI<-spTransform(FNAI,CRS("+proj=utm +zone=17 ellps=WGS84"))
summary(FNAI)

# osmu burn frequncy  #import frequency
osmu_burn<- readOGR("./OSBS_osmu_burn", "osmu_burn")
plot(osmu_burn, col=cm.colors(10), alpha=1, legend=F, main="fire frequency")
summary(osmu_burn)
osmu_burn<-spTransform(osmu_burn,CRS("+proj=utm +zone=17 ellps=WGS84"))
summary(osmu_burn)

#import building
building<-readOGR("./OSBS_buildings","buildings_facilities_selection")
plot(building,col=rainbow(10),alpha=1,legend=F,main="Building")
summary(building)
building<-spTransform(building,CRS("+proj=utm +zone=17 ellps=WGS84"))
summary(building)

#import roads
roads<-readOGR("./OSBS_roads","roads")
plot(roads,legend=F, main="Roads")
summary(roads)
roads<-spTransform(roads,CRS("+proj=utm +zone=17 ellps=WGS84"))
summary(roads)

#################################################################################

#subset osbs data
names(osbs_UTM)
osbs_variables <- c("elevatn", "siteID", "plotID", "nlcdCls", "slpGrdn", "slpAspc")
osbs_UTM <- osbs_UTM[osbs_variables]
head(osbs_UTM)
str(osbs_UTM)

#Now we are going to measure distance between NEON data collection points and the fire.
##### not works for osbs for it has too much fire in each site

#Now let's calculate the distance for all of the disturbances in relation to the NEON collection sites.

#building
building_dist<-apply(gDistance(osbs_UTM,building,byid = TRUE),2,min)
building_dist<-data.frame(building_dist)
#rename column
names(building_dist)[names(building_dist)=="building_dist"]<-"building_dist"
head(building_dist)
#Merge data frames to make one complete dataset
osbs1<- merge(osbs_UTM, building_dist, by=0, all=TRUE)
#Check
head(building_dist)
head(osbs1)

#roads
roads_dist<-apply(gDistance(osbs_UTM,roads,byid = TRUE),2,min)
roads_dist<-data.frame(roads_dist)
#rename column
names(roads_dist)[names(roads_dist)=="roads_dist"]<-"roads_dist"
head(roads_dist)
#merge data frame to make one complete dataset
osbs2<-merge(osbs_UTM,roads_dist,by=0,all=TRUE)
#check
head(roads_dist)
head(osbs2)
#Merge all data frames together for a complete data set
combined.dist<-merge(osbs1,osbs2,by="plotID",all=T)

write.csv(combined.dist,file="~/Documents/NEON_LTER_2018/osbs_dist.csv", row.names=FALSE)
##################################################################################################################################################################
#Now we need to clean up the data frame and select only the columns we need.
#Select important columns
keep=c("plotID", "siteID", "elevatn", "nlcdCls", "slpGrdn", "slpAspc","building_dist", "roads_dist")

# Check that all names in keep are in combined.dist5. You want this result to be zero. 
# If it is anything other than zero, you need to figure out what the appropriate column names are.
keep[!keep %in% names(combined.dist)]

combined.distances <- combined.dist
combined.distances@data <- combined.distances@data[,keep] 
write.csv(combined.distances, file="~/Documents/NEON_LTER_2018/osbs_dist.csv", row.names=F)

#Make these data a data frame.
combined.distances<-data.frame(combined.distances@data)
class(combined.distances)
head(combined.distances)

##################################################################################################################################################################
#Now we need to put our distance data into long format.
#Long Format of combined.distances to characterize dist_type
library(dplyr)
library(reshape2)
combined.distances<-read.csv("~/Documents/NEON_LTER_2018/osbs_dist.csv")
head(combined.distances)

#We have some unneccessary columns that came with the spatial point data frame. 
# So, we will exclude those to further clean up our data frame.
combined.distances$coords.x1<-NULL
combined.distances$coords.x2<-NULL
combined.distances$optional<-NULL
head(combined.distances)

#Create the long format
combined.distances<-melt(combined.distances, id=c("plotID", "siteID", "elevatn", "nlcdCls", "slpGrdn", "slpAspc"))
head(combined.distances)

#Rename columns to specify disturbance type and distance in meters.
names(combined.distances)[names(combined.distances)=="variable"]<-"dist_type"
names(combined.distances)[names(combined.distances)=="value"]<-"distance_m"
head(combined.distances)

#Look at what is described in the distance type to be sure all of your disturbance distances are included.
sort(unique(combined.distances$dist_type))

#To better inform the users of these data, we should make the disturbance names more informative. 
# So rename them to fit the exact disturbance type that you are trying to describe.
combined.distances$dist_type<- as.character(combined.distances$dist_type)
combined.distances$dist_type[combined.distances$dist_type=="building_dist"]<-"building"
combined.distances$dist_type[combined.distances$dist_type=="roads_dist"]<-"roads"
sort(unique(combined.distances$dist_type))

write.csv(combined.distances,file="~/Documents/NEON_LTER_2018/neon_plotid_dist2disturbance_osbs.csv", row.names=FALSE)

#To save the workspace with all of our data and final products, use the following code:
save.image("NEON_OSBS_LTER.RData")
###################################################################################################################################################################Title: Calculating the surface area of a shapefile 

load("NEON_OSBS_LTER.RData")

# building area calculation
building_area<-gArea(building)
building_area
building_area<-data.frame(building_area)
building_area
#rename column
names(building_area)[names(building_area)=="building_area"]<-"building_area"
head(building_area)
#merge data
area_add<- merge(combined.distances, building_area, by=0, all=TRUE)
head(building_area)
head(area_add)
#change all NA value to 0
area_add[is.na(area_add)] <- 0
head(area_add)

#roads area caclulation
roads_area<-gArea(roads)
roads_area
roads_area<-data.frame(roads_area)
roads_area
#rename column
names(roads_area)[names(roads_area)=="roads_area"]<-"roads_area"
head(roads_area)
#merge data
area_add1<-merge(combined.distances,roads_area,by=0,all=TRUE)
head(roads_area)
head(area_add1)
#change all NA values to 0
area_add1[is.na(area_add1)]<-0
head(area_add1)
#merge data frame
final<-merge(area_add1,area_add,by=c("Row.names","plotID", "siteID", "elevatn", "nlcdCls", "slpAspc", "slpGrdn", "dist_type", "distance_m"), all=T)
names(final)

write.csv(final,file="~/Documents/NEON_LTER_2018/osbs_final_plot_level.csv", row.names=FALSE)
##################################################################################################################################################################
#Now we need to clean up the data frame and select only the columns we need.
#Select important columns
keep=c("plotID", "siteID","elevatn", "nlcdCls", "slpAspc", "slpGrdn", "dist_type", "distance_m", "building_area","roads_area" )

# Check that all names in keep are in combined.dist3. You want this result to be zero. If it is anything other than zero, you need to figure out what the appropriate column names are.
keep[!keep %in% names(final)]

plot_level_dataframe <- final
plot_level_dataframe@data <- plot_level_dataframe@data[,keep] 
write.csv(plot_level_dataframe, file="~/Documents/NEON_LTER_2018/osbs_final_plot_level.csv", row.names=F)

