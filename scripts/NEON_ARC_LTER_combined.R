## Title: Calculating the distances between disturbances and NEON sites
## Date: 27 June 2018
## Author: Cameo Chilcutt
## Sources: 

# Start with a clear workspace and set working directory to google drive -> NEON_LTER_2018 folder -> data -> raw_data
rm(list=ls())
setwd("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data")

#libraries
library(rgdal)
library(ggmap)
library(rgeos)
library (ggplot2)
library(raster)
library (ggsn)
library(gridExtra)
library(grid)

# Import terrestrial data shape files
terrestrial <- readOGR(".\\neon\\spatial_data\\All_NEON_TOS_Plots_V4\\All_NEON_TOS_Centroid_V4.shp")
plot(terrestrial, col=cm.colors(5), alpha=1, legend=F, main="Terrestrial Data")

#Subset Data
arc<-terrestrial[terrestrial$siteNam=="Toolik Lake",]
summary(arc)
plot(arc)

#Reproject arc data to UTM Zone 6 
arc_UTM<-spTransform(arc, CRS("+proj=utm +zone=6 ellps=WGS84"))
arc_UTM
######################################################################################
#Import and reproject relevant shape files

#import Camp Buildings 2013
CB13 <- readOGR(".\\arc\\gis_data\\camp_buildings_2013$data", "Camp_Buildings_2013")

#plot Camp Buildings 2013
plot(CB13, col=rainbow(10),
     alpha=1,
     legend=F, main="Camp Buildings 2013")

#import Gravel Pads
GravelPads <- readOGR(".\\arc\\gis_data\\gravel_pads$data", "gravel_pads")

#plot Gravel Pads
plot(GravelPads, col=grey(1:100/100), legend=FALSE, main="Gravel Pads")

#import roads_trails$data
Roads<- readOGR(".\\arc\\gis_data\\roads_trails$data", "transport_100423")

#plot roads_trails$data
plot(Roads, col=cm.colors(10), alpha=1, legend=F, main="Road_Trails")

#import taps$data
Taps<- readOGR(".\\arc\\gis_data\\taps$data", "pipeline_proj")

#plot taps$data
plot(Taps, col=heat.colors(5), alpha=1, legend=F, main="Taps")

#import toolik_rna$data
ToolikRNA <- readOGR(".\\arc\\gis_data\\toolik_rna$data", "Toolik_RNA")

#plot toolik_rna$data
plot(ToolikRNA, col=terrain.colors(8),
     alpha=1,
     legend=F, main="Toolik_RNA")

#Anaktuvuk_Burn_Perim
AnaktuvukBurnPerim<-readOGR(".\\arc\\gis_data\\anaktuvuk_burn_perim$data", "progression_perimeters_0822-0930")
plot(AnaktuvukBurnPerim, col=grey(1:100/100), main="Progression_perimeters")

#import thermokarst data
ThermokarstWater<- readOGR(".\\arc\\gis_data\\watersheds_research$data\\Watersheds_Research", "Thermokarst_Watershed")
plot(ThermokarstWater, col=blues9, main="Thermokarst")

#import Toolik Inlet Water Source
ToolikinletWater<- readOGR(".\\arc\\gis_data\\watersheds_research$data\\Watersheds_Research", "Toolik_inlet_Watershed")
plot(ToolikinletWater, col="blue", main="Toolik Inlet Water")
#################################################################################
#Now we are going to measure distance between NEON data collection points and the Anaktuvuk fire.

library(sp)
library(spdep)

arc_burn_dist<- apply(gDistance(arc_UTM, AnaktuvukBurnPerim,byid=TRUE),2,min)
arc_burn_dist<- data.frame(arc_burn_dist)

#Rename columns
names(arc_burn_dist)[names(arc_burn_dist)=="arc_burn_dist"] <- "burn_dist"
head(arc_burn_dist)

#Merge data frames to make one complete dataset
arc1<- merge(arc_UTM, arc_burn_dist, by=0, all=TRUE)

head(arc_burn_dist)
head(arc1)

plot(arc1)
summary(arc1)
###########################################################################################################
#Now we are going to measure distance between NEON data collection points and the Camp Buildings (2013).
arc_bldgs_dist<- apply(gDistance(arc_UTM, CB13,byid=TRUE),2,min)
arc_bldgs_dist<- data.frame(arc_bldgs_dist)
arc_bldgs_dist
#Rename column
names(arc_bldgs_dist)[names(arc_bldgs_dist)=="arc_bldgs_dist"] <- "bldgs_dist"
head(arc_bldgs_dist)

#Merge data frames to make one complete dataset
arc2<- merge(arc_UTM, arc_bldgs_dist, by=0, all=TRUE)

head(arc_bldgs_dist)
head(arc2)

plot(arc2)
summary(arc2)
#Merge all data frames together for a complete data set
complete<-merge(arc1, arc2, by="plotID", all=T)
write.csv(complete,file="complete.csv", row.names=FALSE)

#######################################################################################
#Now we are going to measure distance between NEON data collection points and the pipeline.

arc_taps_dist<- apply(gDistance(arc_UTM, Taps,byid=TRUE),2,min)
arc_taps_dist<- data.frame(arc_taps_dist)
arc_taps_dist
#Rename column
names(arc_taps_dist)[names(arc_taps_dist)=="arc_taps_dist"] <- "taps_dist"
head(arc_taps_dist)

#Merge data frames to make one complete dataset
arc3<- merge(arc_UTM, arc_taps_dist, by=0, all=TRUE)

head(arc_taps_dist)
head(arc3)

#Merge all data frames together for a complete data set
complete1<-merge(complete, arc3, by="plotID", all=T)
write.csv(complete1,file="complete.csv", row.names=FALSE)

#################################################################################
#Now we are going to measure distance between NEON data collection points and the thermokarst disturbance.
arc_thermo_dist<- apply(gDistance(arc_UTM, ThermokarstWater,byid=TRUE),2,min)
arc_thermo_dist<- data.frame(arc_thermo_dist)

#Rename column
names(arc_thermo_dist)[names(arc_thermo_dist)=="arc_thermo_dist"] <- "thermo_dist"
head(arc_thermo_dist)

#Merge data frames to make one complete dataset
arc4<- merge(arc_UTM, arc_thermo_dist, by=0, all=TRUE)

head(arc_thermo_dist)
head(arc4)

#Merge all data frames together for a complete data set
complete2<-merge(complete1, arc4, by="plotID", all=T)
write.csv(complete2,file="complete.csv", row.names=FALSE)

#################################################################################
#Now we are going to measure distance between NEON data collection points and the nearest water source which is .
arc_water_dist<- apply(gDistance(arc_UTM, ToolikinletWater,byid=TRUE),2,min)
arc_water_dist<- data.frame(arc_water_dist)

#Rename column
names(arc_water_dist)[names(arc_water_dist)=="arc_water_dist"] <- "water_dist"
head(arc_water_dist)

#Merge data frames to make one complete dataset
arc5<- merge(arc_UTM, arc_water_dist, by=0, all=TRUE)

head(arc_water_dist)
head(arc5)

#Merge all data frames together for a complete data set
complete3<-merge(complete2, arc5, by="plotID", all=T)
write.csv(complete3,file="complete.csv", row.names=FALSE)
#######################################################################################
#Let's simplify the dataset 
#Select the columns you want to keep and everything else will no longer be apart of the dataset.
keep=c("plotID", "siteID", "siteNam", "burn_dist",	"bldgs_dist",	"taps_dist",	"thermo_dist", "water_dist" )

# Check that all names in keep are in complete3
keep[!keep %in% names(complete3)]

complete.data <- complete3
complete.data@data <- complete.data@data[,keep] 
names(complete.data)[names(complete.data)=='plotID'] <- 'neon_plotID'
write.csv(complete.data, file="complete.csv", row.names=F)
