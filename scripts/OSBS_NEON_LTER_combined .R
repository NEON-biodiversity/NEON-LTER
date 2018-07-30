## Title: Calculating the distances between disturbances and NEON sites
## Date: 
## Author: Cameo Chilcutt
## Sources:  osbs-whj

# devtools::install_github("tidyverse/googledrive")
library("googledrive")

# Start with a clear workspace and set working directory to google drive -> NEON_LTER_2018 folder -> data -> raw_data
rm(list=ls())
# setwd('/Users/hjwei/Google Drive/NEON_LTER_2018/data/raw_data')
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
osmu <- readOGR("./os_mu", "os_mu")
plot(osmu, col=rainbow(10),
     alpha=1, 
     legend=F, main="os_mu")
summary(osmu)
osmu<-spTransform(osmu,CRS("+proj=utm +zone=17 ellps=WGS84"))
summary(osmu)

#import NEON bird plot data
neonbird<- readOGR("./NEON_bird_plots", "NEON_bird_plots")
plot(neonbird, col=grey(1:100/100), legend=FALSE, main="NEON Bird plots")
summary(neonbird)
neonbird<-spTransform(neonbird,CRS("+proj=utm +zone=17 ellps=WGS84"))
summary(neonbird)

#import neon
neon<- readOGR("./NEON", "Export_Output")
plot(neon, col=rainbow(16),
     alpha=1,
     legend=F, main="NEON")
summary(neon)
neon<-spTransform(neon,CRS("+proj=utm +zone=17 ellps=WGS84"))
summary(neon)

#import fnai
FNAI<- readOGR("./FNAI", "FNAI")
plot(FNAI, col=cm.colors(10), alpha=1, legend=F, main="FNAI")
summary(FNAI)
FNAI<-spTransform(FNAI,CRS("+proj=utm +zone=17 ellps=WGS84"))
summary(FNAI)

# osmu burn frequncy  #import frequency
osmu_burn<- readOGR("./osmu_burn", "osmu_burn")
plot(osmu_burn, col=cm.colors(10), alpha=1, legend=F, main="fire frequency")
summary(osmu_burn)
osmu_burn<-spTransform(osmu_burn,CRS("+proj=utm +zone=17 ellps=WGS84"))
summary(osmu_burn)


#################################################################################
#Now we are going to measure distance between NEON data collection points and the Anaktuvuk fire.

osbs_burn_dist<- apply(gDistance(osbs_UTM, osmu,byid=TRUE),2,min)
osbs_burn_dist<- data.frame(osbs_burn_dist)
head(osbs_burn_dist)

#Right now the column name is the same as the name of the data frame.
# This can be confusing for R when we want to merge these data with the arc_UTM data.
# So, we need to rename it using the code below.
names(osbs_burn_dist)[names(osbs_burn_dist)=="osbs_burn_dist"] <- "burn_dist"
head(osbs_burn_dist)

#Now we are going to merge the data frames to make one complete dataset
osbs1<- merge(osbs_UTM, osbs_burn_dist, by=0, all=TRUE)

#Always check both data sets to make sure the distance measurements are the same as the original distance calculation file.
head(osbs_burn_dist)
head(osbs1)

#Now let's calculate the distance for all of the disturbances in relation to the NEON collection sites.

# boundary
osbs_boun_dist<- apply(gDistance(osbs_UTM, osbs_boun,byid=TRUE),2,min)
osbs_boun_dist<- data.frame(osbs_boun_dist)
head(osbs_boun_dist)
#Rename column
names(osbs_boun_dist)[names(osbs_boun_dist)=="osbs_boun_dist"] <- "boun_dist"
head(osbs_boun_dist)
#Merge data frames
osbs2<- merge(osbs_UTM, osbs_boun_dist, by=0, all=TRUE)
#Check
head(osbs_boun_dist)
head(osbs2)
#Merge both arc1 and arc2 dataframes to include all of the data for both burn_dist and boun_dist.
combined.dist<-merge(osbs1, osbs2, by="plotID", all=T)
write.csv(combined.dist,file="combined_distance_data.csv", row.names=FALSE)

# neon bird
osbs_neonbird_dist<- apply(gDistance(osbs_UTM, neonbird,byid=TRUE),2,min)
osbs_neonbird_dist<- data.frame(osbs_neonbird_dist)
head(osbs_neonbird_dist)
#Rename column
names(osbs_neonbird_dist)[names(osbs_neonbird_dist)=="osbs_neonbird_dist"] <- "neonbird_dist"
head(osbs_neonbird_dist)
#Merge data 
osbs3<- merge(osbs_UTM, osbs_neonbird_dist, by=0, all=TRUE)
head(osbs_neonbird_dist)
head(osbs3)
#Merge all data frames together
combined.dist1<-merge(combined.dist, osbs3, by="plotID", all=T)
write.csv(combined.dist1,file="combined_distance_data.csv", row.names=FALSE)

# fnai
osbs_FNAI_dist<- apply(gDistance(osbs_UTM, FNAI,byid=TRUE),2,min)
osbs_FNAI_dist<- data.frame(osbs_FNAI_dist)
#Rename column
names(osbs_FNAI_dist)[names(osbs_FNAI_dist)=="osbs_FNAI_dist"] <- "FNAI_dist"
head(osbs_FNAI_dist)
#Merge data 
osbs4<- merge(osbs_UTM, osbs_FNAI_dist, by=0, all=TRUE)
#Check
head(osbs_FNAI_dist)
head(osbs4)
#Merge all data frames together for a complete data set
combined.dist2<-merge(combined.dist1, osbs4, by="plotID", all=T)
write.csv(combined.dist2,file="combined_distance_data.csv", row.names=FALSE)

# neon
osbs_neon_dist<- apply(gDistance(osbs_UTM, neon,byid=TRUE),2,min)
osbs_neon_dist<- data.frame(osbs_neon_dist)
#Rename column
names(osbs_neon_dist)[names(osbs_neon_dist)=="osbs_neon_dist"] <- "neon_dist"
head(osbs_neon_dist)
#Merge data frames to make one complete dataset
osbs5<- merge(osbs_UTM, osbs_neon_dist, by=0, all=TRUE)
#Check
head(osbs_neon_dist)
head(osbs5)
#Merge all data frames together for a complete data set
combined.dist3<-merge(combined.dist2, osbs5, by="plotID", all=T)

write.csv(combined.dist3,file="~/Documents/NEON_LTER_2018/combined_distance_data.csv", row.names=FALSE)
##################################################################################################################################################################
#Now we need to clean up the data frame and select only the columns we need.
#Select important columns
keep=c("plotID", "siteID", "siteNam", "burn_dist",	"boun_dist",	"neonbird_dist",	"FNAI_dist", "neon_dist" )

# Check that all names in keep are in combined.dist3. You want this result to be zero. 
# If it is anything other than zero, you need to figure out what the appropriate column names are.
keep[!keep %in% names(combined.dist3)]

combined.distances <- combined.dist3
combined.distances@data <- combined.distances@data[,keep] 
write.csv(combined.distances, file="~/Documents/NEON_LTER_2018/combined_distance_data.csv", row.names=F)

#Make these data a data frame.
combined.distances<-data.frame(combined.distances@data)
class(combined.distances)
head(combined.distances)

##################################################################################################################################################################
#Now we need to put our distance data into long format.
#Long Format of combined.distances to characterize dist_type
library(dplyr)
library(reshape2)
combined.distances<-read.csv("~/Documents/NEON_LTER_2018/combined_distance_data.csv")
head(combined.distances)

#We have some unneccessary columns that came with the spatial point data frame. 
# So, we will exclude those to further clean up our data frame.
combined.distances$coords.x1<-NULL
combined.distances$coords.x2<-NULL
combined.distances$optional<-NULL
head(combined.distances)

#Create the long format
combined.distances<-melt(combined.distances, id=c("plotID", "siteID", "siteNam"))
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
combined.distances$dist_type[combined.distances$dist_type=="burn_dist"]<-"burn"
combined.distances$dist_type[combined.distances$dist_type=="boun_dist"]<-"boundary"
combined.distances$dist_type[combined.distances$dist_type=="neonbird_dist"]<-"neonbird"
combined.distances$dist_type[combined.distances$dist_type=="FNAI_dist"]<-"FNAI"
combined.distances$dist_type[combined.distances$dist_type=="neon_dist"]<-"neon"
sort(unique(combined.distances$dist_type))

write.csv(combined.distances,file="~/Documents/NEON_LTER_2018/combined_distance_data.csv", row.names=FALSE)

