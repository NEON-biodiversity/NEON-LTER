## Title: Pulling NEON data to overlay on basemap
## Date: 21 June 2018
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

# Import terrestrial data shape files
terrestrial <- readOGR(".\\neon\\spatial_data\\All_NEON_TOS_Plots_V4\\All_NEON_TOS_Centroid_V4.shp")
plot(terrestrial, col=cm.colors(5), alpha=1, legend=F, main="Terrestrial Data")

#Subset Data
arc<-terrestrial[terrestrial$siteNam=="Toolik Lake",]
plot(arc)

######################################################################################
#Import and reproject relevant shape files

#import Camp Buildings 2013
CB13 <- readOGR(".\\arc\\gis_data\\camp_buildings_2013$data", "Camp_Buildings_2013")

#plot Camp Buildings 2013
plot(CB13, col=rainbow(10),
     alpha=1,
     legend=F, main="Camp Buildings 2013")

#Reprojection
mapCamp13<-spTransform(CB13, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapCamp13)
mapCamp13

#fortify for mapping
mapCamp13.f = fortify(mapCamp13)

#import Gravel Pads
GravelPads <- readOGR(".\\arc\\gis_data\\gravel_pads$data", "gravel_pads")

#plot Gravel Pads
plot(GravelPads, col=grey(1:100/100), legend=FALSE, main="Gravel Pads")

#Reprojection
map_GP<-spTransform(GravelPads, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(map_GP)
map_GP

#fortify for mapping
map_GP.f = fortify(map_GP)

#import roads_trails$data
Roads<- readOGR(".\\arc\\gis_data\\roads_trails$data", "transport_100423")

#plot roads_trails$data
plot(Roads, col=cm.colors(10), alpha=1, legend=F, main="Road_Trails")

#Reprojection
mapTrails<-spTransform(Roads, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapTrails)
mapTrails

#fortify for mapping
mapTrails.f = fortify(mapTrails)

#import taps$data
Taps<- readOGR(".\\arc\\gis_data\\taps$data", "pipeline_proj")

#plot taps$data
plot(Taps, col=heat.colors(5), alpha=1, legend=F, main="Taps")

#Reprojection
maptaps<-spTransform(Taps, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(maptaps)
maptaps

#fortify for mapping
maptaps.f = fortify(maptaps)

#import toolik_rna$data
ToolikRNA <- readOGR(".\\arc\\gis_data\\toolik_rna$data", "Toolik_RNA")

#plot toolik_rna$data
plot(ToolikRNA, col=terrain.colors(8),
     alpha=1,
     legend=F, main="Toolik_RNA")

#Reprojection
mapToolik<-spTransform(ToolikRNA, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapToolik)
mapToolik

#fortify for mapping
mapToolik.f = fortify(mapToolik)

#Anaktuvuk_Burn_Perim
AnaktuvukBurnPerim<-readOGR(".\\arc\\gis_data\\anaktuvuk_burn_perim$data", "progression_perimeters_0822-0930")
plot(AnaktuvukBurnPerim, col=grey(1:100/100), main="Progression_perimeters")

#Reprojection
mapAnaktuvukBurnPerim<-spTransform(AnaktuvukBurnPerim, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapAnaktuvukBurnPerim)

#fortify for mapping
mapAnaktuvukBurnPerim.f = fortify(mapAnaktuvukBurnPerim)
#################################################################################
#Now we are going to measure distance between NEON data collection points and the Anaktuvuk fire.

library(sp)
library(spdep)

arc_burn_dist<- apply(gDistance(arc, mapAnaktuvukBurnPerim,byid=TRUE),2,min)
arc_burn_dist<- data.frame(arc_burn_dist)

#Rename column
names(arc_burn_dist)[names(arc_burn_dist)=="arc_burn_dist"] <- "burn_dist"
head(arc_burn_dist)

#Merge data frames to make one complete dataset
arc1<- merge(arc, arc_burn_dist, by=0, all=TRUE)

head(arc_burn_dist)
head(arc1)

plot(arc1)
summary(arc1)
write.csv(arc1,file="arc1.csv", row.names=FALSE)

###########################################################################################################
#Now that we have the distances, we can color code them to make a more complete map.
# Create new column filled with default colour
arc1$distance_class = "middle"
# Set new column values to appropriate colours
arc1$distance_class[arc1$burn_dist>=0.76]="far"
arc1$distance_class[arc1$burn_dist<=0.60]="near"

##############################################################################################

#Let's create a complete map showing all of the NEON site data with the disturbance data. 
toolmap <- readOGR(".//arc//gis_data//toolik_map.shp")
basemap<- ggplot(toolmap) + 
geom_polygon(aes(x = long, y = lat, group = group), data = mapAnaktuvukBurnPerim, alpha = 0.8, 
color = "red", fill = "red",
size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), 
data = mapToolik,
alpha = 0, 
color = "black", 
size = 0.2) + xlab("Longitude")+ ylab("Latitude") + ggtitle("Toolik Lake Field Station: Disturbance Patterns")+ theme(plot.title = element_text(hjust = .5)) + scalebar(x.min= -151.3, x.max= -152.0, y.min= 68.35, y.max= 68.42, dist= 50, location= "bottomleft", dd2km = TRUE, st.size=2, st.dist = .2, height = 0.5, model="WGS84") + geom_polygon(aes(x = long, y = lat, group = group), 
data = mapTrails,
alpha = 0, 
color = "purple", 
size = 0.2)+ geom_polygon(aes(x = long, y = lat, group = group), 
data = mapCamp13,
alpha = 0.8, 
color = "white", fill = "white",
size = 0.2)+ geom_polygon(aes(x = long, y = lat, group = group), 
data = mapThermokarstWater,
alpha = 0.8, 
color = "blue", fill = "blue",
size = 0.2)+ geom_polygon(aes(x = long, y = lat, group = group), 
data = map_GP,
alpha = 0, 
color = "grey", 
size = 0.2)+ geom_polygon(aes(x = long, y = lat, group = group), 
data = maptaps,
alpha = 0, 
color = "orange", 
size = 0.2)
dist_basemap<-basemap + coord_cartesian(ylim = c(68.3, 69.6), xlim = c(-152, -148.5), expand = FALSE)
dist_basemap + geom_point(aes(x = longitd, y = latitud, color = distance_class), data = arc1@data) +
  scale_color_manual(values = c(far='slateblue', middle='forestgreen', near='goldenrod'))

#The points showing distance_class are the NEON points distance from the Anaktuvuk fire. These data were calculated from the section about and now we can visualize them on a map in relation to the Toolik Lake Field Station and the other disturbances shown on the map.





