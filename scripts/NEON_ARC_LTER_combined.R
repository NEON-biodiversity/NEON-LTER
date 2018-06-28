## Title: Calculating the distances between disturbances and NEON sites
## Date: 27 June 2018
## Author: Cameo Chilcutt
## Sources: 

# Start with a clear workspace and set working directory to google drive -> NEON_LTER_2018 folder -> data -> raw_data
rm(list=ls())
setwd("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data")

#install these packages and load the libraries
library(rgdal)
library(rgeos)

# Import terrestrial data shape files from Google Drive
terrestrial <- readOGR(".\\neon\\spatial_data\\All_NEON_TOS_Plots_V4\\All_NEON_TOS_Centroid_V4.shp")
plot(terrestrial, col=cm.colors(5), alpha=1, legend=F, main="Terrestrial Data")

#To subset the data specifically to your site. You must use the exact name as it is written in the data. To look this up, use the following function to list all of the names of the field sites.
unique(terrestrial$siteNam)
arc<-terrestrial[terrestrial$siteNam=="Toolik Lake",]
summary(arc)
plot(arc)

#Currently, it is in latitude and longitude, but in order to measure distance in meters we need to reproject the data into UTMs. You should look up the appropriate zone for your site. For the Toolik Lake Field Station we needed UTM Zone 6. 
arc_UTM<-spTransform(arc, CRS("+proj=utm +zone=6 ellps=WGS84"))
head(arc_UTM)
######################################################################################
#Next we need to import and reproject all of the relevant disturbance shape files. After importing, always plot to be sure you have the appropriate shapes. Also, check if the files are in UTMs. If not, reproject the same way we did for the NEON terrestrial data. We need UTMs to measure distance in meters.

#Camp Buildings 2013 Data
cb13 <- readOGR(".\\arc\\gis_data\\camp_buildings_2013$data", "Camp_Buildings_2013")
plot(cb13, col=rainbow(10), alpha=1, legend=F, main="Camp Buildings 2013")
summary(cb13)

#Gravel Pads Data
gravelpads <- readOGR(".\\arc\\gis_data\\gravel_pads$data", "gravel_pads")
plot(gravelpads, col=grey(1:100/100), legend=FALSE, main="Gravel Pads")
summary(gravelpads)

#Roads & Trails Data
rt<- readOGR(".\\arc\\gis_data\\roads_trails$data", "transport_100423")
plot(rt, col=cm.colors(10), alpha=1, legend=F, main="Road_Trails")
summary(rt)
rt<-spTransform(rt, CRS("+proj=utm +zone=6 ellps=WGS84"))
summary(rt)

#Pipeline Data
pipeline<- readOGR(".\\arc\\gis_data\\taps$data", "pipeline_proj")
plot(pipeline, col=heat.colors(5), alpha=1, legend=F, main="Pipeline")
summary(pipeline)
pipeline<-spTransform(pipeline, CRS("+proj=utm +zone=6 ellps=WGS84"))
summary(pipeline)

#Cut out of Toolik Lake Field Site
toolik<- readOGR(".\\arc\\gis_data\\toolik_rna$data", "Toolik_RNA")
plot(toolik, col=terrain.colors(8), alpha=1, legend=F, main="Toolik Lake Field Station")
summary(toolik)
toolik<-spTransform(toolik, CRS("+proj=utm +zone=6 ellps=WGS84"))
summary(toolik)

#Anaktuvuk Burn Perimeter Data
anaktuvuk<-readOGR(".\\arc\\gis_data\\anaktuvuk_burn_perim$data", "progression_perimeters_0822-0930")
plot(anaktuvuk, col=grey(1:100/100), main="Anaktuvuk Burn Perimeter")
summary(anaktuvuk)
anaktuvuk<-spTransform(anaktuvuk, CRS("+proj=utm +zone=6 ellps=WGS84"))
summary(anaktuvuk)

#Thermokarst Data
thermokarst<- readOGR(".\\arc\\gis_data\\watersheds_research$data\\Watersheds_Research", "Thermokarst_Watershed")
plot(thermokarst, col=blues9, main="Thermokarst")
summary(thermokarst)

#Toolik Inlet Water Source
toolwater<- readOGR(".\\arc\\gis_data\\watersheds_research$data\\Watersheds_Research", "Toolik_inlet_Watershed")
plot(toolwater, col="blue", main="Toolik Inlet Water")
summary(toolwater)
#################################################################################
#Now we are going to measure distance between NEON data collection points and the Anaktuvuk fire.
arc_burn_dist<- apply(gDistance(arc_UTM, anaktuvuk,byid=TRUE),2,min)
arc_burn_dist<- data.frame(arc_burn_dist)
head(arc_burn_dist)

#Right now the column name is the same as the name of the data frame. This can be confusing for R when we want to merge these data with the arc_UTM data. So, we need to rename it using the code below.
names(arc_burn_dist)[names(arc_burn_dist)=="arc_burn_dist"] <- "burn_dist"
head(arc_burn_dist)

#Now we are going to merge the data frames to make one complete dataset
arc1<- merge(arc_UTM, arc_burn_dist, by=0, all=TRUE)

#Always check both data sets to make sure the distance measurements are the same as the original distance calculation file.
head(arc_burn_dist)
head(arc1)

#Now let's calculate the distance for all of the disturbances in relation to the NEON collection sites.

#Camp Buildings
arc_bldgs_dist<- apply(gDistance(arc_UTM, cb13,byid=TRUE),2,min)
arc_bldgs_dist<- data.frame(arc_bldgs_dist)
head(arc_bldgs_dist)

#Rename column
names(arc_bldgs_dist)[names(arc_bldgs_dist)=="arc_bldgs_dist"] <- "bldgs_dist"
head(arc_bldgs_dist)

#Merge data frames
arc2<- merge(arc_UTM, arc_bldgs_dist, by=0, all=TRUE)

#Check
head(arc_bldgs_dist)
head(arc2)

#Merge both arc1 and arc2 dataframes to include all of the data for both burn_dist and bldgs_dist.
combined.dist<-merge(arc1, arc2, by="plotID", all=T)
write.csv(combined.dist,file="combined_distance_data.csv", row.names=FALSE)

#Pipeline
arc_pipe_dist<- apply(gDistance(arc_UTM, pipeline,byid=TRUE),2,min)
arc_pipe_dist<- data.frame(arc_pipe_dist)
head(arc_pipe_dist)

#Rename column
names(arc_pipe_dist)[names(arc_pipe_dist)=="arc_pipe_dist"] <- "pipeline_dist"
head(arc_pipe_dist)

#Merge data 
arc3<- merge(arc_UTM, arc_pipe_dist, by=0, all=TRUE)
head(arc_pipe_dist)
head(arc3)

#Merge all data frames together
combined.dist1<-merge(combined.dist, arc3, by="plotID", all=T)
write.csv(combined.dist1,file="combined_distance_data.csv", row.names=FALSE)

#Thermokarst
arc_thermo_dist<- apply(gDistance(arc_UTM, thermokarst,byid=TRUE),2,min)
arc_thermo_dist<- data.frame(arc_thermo_dist)

#Rename column
names(arc_thermo_dist)[names(arc_thermo_dist)=="arc_thermo_dist"] <- "thermokarst_dist"
head(arc_thermo_dist)

#Merge data 
arc4<- merge(arc_UTM, arc_thermo_dist, by=0, all=TRUE)

#Check
head(arc_thermo_dist)
head(arc4)

#Merge all data frames together for a complete data set
combined.dist2<-merge(combined.dist1, arc4, by="plotID", all=T)
write.csv(combined.dist2,file="combined_distance_data.csv", row.names=FALSE)

#Nearest Water Source
arc_water_dist<- apply(gDistance(arc_UTM, toolwater,byid=TRUE),2,min)
arc_water_dist<- data.frame(arc_water_dist)

#Rename column
names(arc_water_dist)[names(arc_water_dist)=="arc_water_dist"] <- "water_dist"
head(arc_water_dist)

#Merge data frames to make one complete dataset
arc5<- merge(arc_UTM, arc_water_dist, by=0, all=TRUE)

#Check
head(arc_water_dist)
head(arc5)

#Merge all data frames together for a complete data set
combined.dist3<-merge(combined.dist2, arc5, by="plotID", all=T)
write.csv(combined.dist3,file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\combined_distance_data.csv", row.names=FALSE)
##################################################################################################################################################################
#Now we need to clean up the data frame and select only the columns we need.
#Select important columns
keep=c("plotID", "siteID", "siteNam", "burn_dist",	"bldgs_dist",	"pipeline_dist",	"thermokarst_dist", "water_dist" )

# Check that all names in keep are in combined.dist3. You want this result to be zero. If it is anything other than zero, you need to figure out what the appropriate column names are.
keep[!keep %in% names(combined.dist3)]

combined.distances <- combined.dist3
combined.distances@data <- combined.distances@data[,keep] 
write.csv(combined.distances, file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\combined_distance_data.csv", row.names=F)

#Make these data a data frame.
combined.distances<-data.frame(combined.distances@data)
class(combined.distances)
head(combined.distances)

##################################################################################################################################################################
#Now we need to put our distance data into long format.
#Long Format of combined.distances to characterize dist_type
library(dplyr)
library(reshape2)
combined.distances<-read.csv("G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\combined_distance_data.csv")
head(combined.distances)

#We have some unneccessary columns that came with the spatial point data frame. So, we will exclude those to further clean up our data frame.
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

#To better inform the users of these data, we should make the disturbance names more informative. So rename them to fit the exact disturbance type that you are trying to describe.
combined.distances$dist_type<- as.character(combined.distances$dist_type)
combined.distances$dist_type[combined.distances$dist_type=="burn_dist"]<-"burn"
combined.distances$dist_type[combined.distances$dist_type=="bldgs_dist"]<-"buildings"
combined.distances$dist_type[combined.distances$dist_type=="pipeline_dist"]<-"pipeline"
combined.distances$dist_type[combined.distances$dist_type=="thermokarst_dist"]<-"thermokarst"
combined.distances$dist_type[combined.distances$dist_type=="water_dist"]<-"water source"
sort(unique(combined.distances$dist_type))

#To save the workspace with all of our data and final products, use the following code:
save.image("NEON_ARC_LTER_combined.RData")
