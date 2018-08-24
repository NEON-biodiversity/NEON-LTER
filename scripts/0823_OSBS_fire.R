#Close graphics devices
graphics.off()

# Set working directory
setwd("~/Documents/NEON_LTER_2018")
# Google drive directory: "NEON_LTER_2018/data/raw_data/neon"

#libraries
library(raster)
library(rgdal)
library(ggplot2)
library (ggmap)
library (ggsn)
library(gridExtra)


# This code for ggplot2 sets the theme to mostly black and white 
# (Arial font, and large font, base size=24)
theme_set(theme_bw(12))
theme_update(axis.text.x = element_text(size = 10, angle = 90),
             axis.text.y = element_text(size = 10))

#----------------------------------------------------------
# Prepare the burn data
#----------------------------------------------------------

# read NEON point data
#elevation subset from all NEON spatial data
library(rgdal)
terrestrial<- readOGR(dsn="./All_NEON_TOS_Plots_V4/All_NEON_TOS_Centroid_V4.shp")
osbs_ter<-terrestrial[terrestrial$siteNam=="Ordway-Swisher Biological Station",]
osbs_map<-spTransform(osbs_ter, CRS("+proj=utm +zone=17 ellps=WGS84"))
plot(osbs_map)
points(osbs_map, col="red")
osbs_map$plotID

## seperate the plot ID
library(dplyr)
osbs_map$ID = strsplit(as.character(osbs_map$plotID), '_') %>% 
  lapply(function(x) x[2]) %>% unlist() %>% as.numeric()

#import os_mu shapefiles(contains regions name)
osmu <- readOGR(dsn="./OSBS_os_mu/os_mu.shp")
#plot os_mu
plot(osmu, col=rainbow(10),alpha=1, legend=F, main="os_mu")
head(osmu)

#Reprojection
maposmu<-spTransform(osmu, CRS("+proj=utm +zone=17 ellps=WGS84"))

# read the burn file (includes burn times ,last burn)
burn<-read.csv('./data/2017firelog.csv')
head(burn)
## fire frequency data
frequency<-table(burn$MU)
frequency<-data.frame(frequency)
head(frequency)

frequency$MSU<-frequency$Var1
head(frequency)
write.csv(frequency,file="firefrequency", row.names=F)

# merge the fire and Geographical file

maposmu@data=data.frame(maposmu@data,frequency[match(maposmu$MSU,frequency$MSU),])

# crop fire data
library(raster)
library(rgeos)
fire_crop<-crop(maposmu,extent(osbs_map))
plot(fire_crop)
points(osbs_map,col="red")

names(fire_crop)

# export fire map
writeOGR(obj=fire_crop,dsn="./firelog", layer ="05_osbs_fire_map",driver = "ESRI Shapefile" )

#find osbs neon point data
osbs_fire<-over(osbs_map,fire_crop)
osbs_fire
length(osbs_fire)


# Find NEON plots within fire polygons
osbs_points <- intersect(osbs_map,fire_crop) # points (15)
names(osbs_points)
head(osbs_points)
osbs_points<-osbs_points[c(1:44,46:52)]

#subset from osbs_points
NEON_burntimes <- subset(osbs_points, select=c("Freq","plotID"))

# Find fire polygons containing NEON points
osbs_polygons <- intersect(fire_crop,osbs_map) 
points(osbs_points, col="red")

# Export points and polygons

names(osbs_points)
writeOGR(obj=osbs_points, dsn= "./osbs_points", layer="01osbs_points", driver="ESRI Shapefile") # this is in geographical projection
writeOGR(obj=osbs_polygons, dsn= "./osbs_polygons", layer="01osbs_polygons", driver="ESRI Shapefile") # this is in geographical projection

# Export osbs_cut for Within_site_prep.R
write.csv(osbs_fire, file="osbs_fire.csv")

names(osbs_fire)
