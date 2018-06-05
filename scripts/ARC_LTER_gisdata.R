# Title:			  Writing NEON Data in R
# Site:			  ARC LTER
# Data Sources:	  https://toolik.alaska.edu/gis/data/
# Authors:		  Cameo Chilcutt
# Date:			  4 June 2018

# Set working directory to where you house "data" directory (with subdirectories raw_data, final_data, scripts, output)
setwd("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data")
rm(list = ls())

#libraries
library(raster)
library(rgdal)
library(ggplot2)
library (ggmap)
library (ggsn)

#import Camp Buildings 2010 shapefiles
CB10 <- readOGR(".\\arc\\gis_data\\camp_buildings$data\\Camp Buildings", "campbuilds_2010_100909")

#plot Camp Buildings 2010
plot(CB10,  col=terrain.colors(10), alpha=1, legend=F, main="Camp Buildings 2010")

#Reprojection
mapCamp10<-spTransform(CB10, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapCamp10)
mapCamp10

#import Camp Buildings 2013
CB13 <- readOGR(".\\arc\\gis_data\\camp_buildings_2013$data", "Camp_Buildings_2013")

#plot Camp Buildings 2013
plot(B13, col=rainbow(10),
     alpha=1,
     legend=F, main="Camp Buildings 2013")

#Reprojection
mapCamp13<-spTransform(CB13, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapCamp13)
mapCamp13

#import Gravel Pads
GravelPads <- readOGR(".\\arc\\gis_data\\gravel_pads$data", "gravel_pads")

#plot Gravel Pads
plot(GravelPads, col=grey(1:100/100), legend=FALSE, main="Gravel Pads")

#Reprojection
map_GP<-spTransform(GravelPads, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(map_GP)
map_GP

#import hyd_toolik_arc
Hydtoolarc <- readOGR(".\\arc\\gis_data\\hyd_toolik_arc_101111$data", "hyd_toolik_arc101111")

#plot hyd_toolik_arc
plot(Hydtoolarc, col=rainbow(4),
     alpha=1,
     legend=F, main="Hyd_toolik_arc")

#Reprojection
maptoolikarc<-spTransform(Hydtoolarc, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(maptoolikarc)
maptoolikarc

#import hyd_toolik_poly
Hydtoolpoly<- readOGR(".\\arc\\gis_data\\hyd_toolik_poly101111$data", "hyd_toolik_poly101111")

#plot hyd_toolik_poly
plot(Hydtoolpoly, col=cm.colors(10), alpha=1, legend=F, main="Hyd_toolik_poly")

#Reprojection
maptoolikpoly<-spTransform(Hydtoolpoly, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(maptoolikpoly)
maptoolikpoly

#import research_plots_2009$data **Not Working, Trying to fix**
#shape_path_5 <- "C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\research_plots_2009$data"
#shape_name_5 <- "Research_Plots_2009_NAD83_UTM_100504"
#shape_extent_5 <- readOGR(shape_path_5, shape_name_5)

#plot research_plots
#plot(shape_extent_5,  col=grey(1:100/100), legend=FALSE, main="Research Plots 2009")

#import roads_trails$data
Roads<- readOGR(".\\arc\\gis_data\\roads_trails$data", "transport_100423")


#plot roads_trails$data
plot(Roads, col=cm.colors(10), alpha=1, legend=F, main="Road_Trails")

#Reprojection
mapTrails<-spTransform(Roads, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapTrails)
mapTrails

#import taps$data
Taps<- readOGR(".\\arc\\gis_data\\taps$data", "pipeline_proj")

#plot taps$data
plot(Taps, col=heat.colors(5), alpha=1, legend=F, main="Taps")

#Reprojection
maptaps<-spTransform(Taps, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(maptaps)
maptaps

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

#import watersheds_research
CrumpWater <- readOGR(".\\arc\\gis_data\\watersheds_research$data\\Watersheds_Research", "Crump_Watershed")
ImnaviatWater<- readOGR(".\\arc\\gis_data\\watersheds_research$data\\Watersheds_Research", "Imnaviat_Watershed")
KlingWater<- readOGR(".\\arc\\gis_data\\watersheds_research$data\\Watersheds_Research", "Kling_Watershed")
KuparukWater<- readOGR(".\\arc\\gis_data\\watersheds_research$data\\Watersheds_Research", "Kuparuk_Watershed")
LostLakeWater<- readOGR(".\\arc\\gis_data\\watersheds_research$data\\Watersheds_Research", "Lost_Lake_Watershed")
OksrukuyikWater<- readOGR(".\\arc\\gis_data\\watersheds_research$data\\Watersheds_Research", "Oksrukuyik_Watershed")
ThermokarstWater<- readOGR(".\\arc\\gis_data\\watersheds_research$data\\Watersheds_Research", "Thermokarst_Watershed")
ToolikinletWater<- readOGR(".\\arc\\gis_data\\watersheds_research$data\\Watersheds_Research", "Toolik_inlet_Watershed")

#plot watersheds research
plot(CrumpWater, col=cm.colors(8), alpha=1, legend=F, main="Crump Watersheds Research")
plot(ImnaviatWater, col=cm.colors(8), alpha=1, legend=F, main="Imnaviat Watersheds Research")
plot(KlingWater, col=cm.colors(8), alpha=1, legend=F, main="Kling Watersheds Research")
plot(KuparukWater, col=cm.colors(8), alpha=1, legend=F, main="Kuparuk Watersheds Research")
plot(LostLakeWater, col=cm.colors(8), alpha=1, legend=F, main="Lost Lake Watersheds Research")
plot(OksrukuyikWater, col=cm.colors(8), alpha=1, legend=F, main="Oksrukuyik Watersheds Research")
plot(ThermokarstWater, col=cm.colors(8), alpha=1, legend=F, main="Thermokarst Watersheds Research")
plot(ToolikinletWater, col=cm.colors(8), alpha=1, legend=F, main="Toolik inlet Watersheds Research")

#Reprojection
mapCrumpWater<-spTransform(CrumpWater, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
mapImnaviatWater<-spTransform(ImnaviatWater, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
mapKlingWater<-spTransform(KlingWater, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
mapKuparukWater<-spTransform(KuparukWater, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
mapLostLakeWater<-spTransform(LostLakeWater, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
mapOksrukuyikWater<-spTransform(OksrukuyikWater, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
mapThermokarstWater<-spTransform(ThermokarstWater, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
mapToolikinletWater<-spTransform(ToolikinletWater, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapCrumpWater)
plot(mapImnaviatWater)
plot(mapKlingWater)
plot(mapKuparukWater)
plot(mapLostLakeWater)
plot(mapOksrukuyikWater)
plot(mapThermokarstWater)
plot(mapToolikinletWater)

#100fcont_uk
fcont_uk<-readOGR(".\\arc\\gis_data\\100fcont_uk$data", "100fcont_uk")
plot(fcont_uk, col=grey(1:100/100), main="fcont_uk")

#Reprojection **Not working**
#mapfcont<-spTransform(fcont_uk, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
#plot(mapfcont)
#map100fcont

#Anak_Burn_Lakes
AnakBurnLakes<-readOGR(".\\arc\\gis_data\\anak_burn_lakes$data", "Anaktuvuk_Burn_Lakes")
plot(AnakBurnLakes, col=heat.colors (10), main="Burn_Lakes\n NEON Harvard Forest Field Site")

#Reprojection
mapAnakBurnLakes<-spTransform(AnakBurnLakes, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapAnakBurnLakes)

#Anak_Burn_Perim_Rocha
AnakBurnPerimRocha<-readOGR(".\\arc\\gis_data\\anak_burn_perim_rocha$data", "Anak_Burn_Perim_Rocha")
plot(AnakBurnPerimRocha, col=grey(1:100/100), main="Anak_Burn_Perim\n NEON Harvard Forest Field Site")

#Reprojection
mapAnakBurnPerimRocha<-spTransform(AnakBurnPerimRocha, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapAnakBurnPerimRocha)

#Anak_Burn_Rivers
AnakBurnRivers<-readOGR(".\\arc\\gis_data\\anak_burn_rivers$data", "Anaktuvuk_Burn_Rivers")
plot(AnakBurnRivers, col=grey(1:100/100), main="Anak_Burn_River\n NEON Harvard Forest Field Site")

#Reprojection
mapAnakBurnRivers<-spTransform(AnakBurnRivers, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapAnakBurnRivers)

#Anaktuvuk_Burn_Perim
AnaktuvukBurnPerim<-readOGR(".\\arc\\gis_data\\anaktuvuk_burn_perim$data", "progression_perimeters_0822-0930")
plot(AnaktuvukBurnPerim, col=grey(1:100/100), main="Progression_perimeters\n NEON Harvard Forest Field Site")

#Reprojection
mapAnaktuvukBurnPerim<-spTransform(AnaktuvukBurnPerim, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapAnaktuvukBurnPerim)
############################################################################################################
#BaseMap of Arctic Field Site
Arctic_Map<-raster("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data\\arc\\gis_data\\nasa_modis_2013$data\\alaska_tmo_2013168_geo.tif")
plot(Arctic_Map)
#########################################################################################
#Clipping satellite image to size of Toolik research area
toolik <- crop(Arctic_Map, mapToolik)
plot(toolik)
#########################################################################################
#Setting the extent
NewExtent<-extent(-149.600, -149.589, 68.625, 68.630)
extent(toolik)<- NewExtent
plot(toolik)
plot(mapCamp10, add=TRUE)
################################################################################################
#Adding titles
title(main="Camp Buildings 2010 - Alaska Field Site", xlab="Longitude", ylab="Latitude")
###############################################################################################
#Google Base Maps
##Extent of Toolik Field Station
##NewExtent<-extent(-149.600, -149.589, 68.625, 68.630)

#Obtain basemap of the Arctic field site through google maps 
#(can also generate map types "roadmap", "terrain", "satellite", "hybrid")

arctic_map <- get_map(location = c(lon = -150.5, lat = 69.3),
                      color = "color",
                      source = "google",
                      maptype = "terrain",
                      zoom = 7)

#Visualize the shapefile and make sure it loaded in correctly
ggplot(data = mapAnaktuvukBurnPerim, aes(x = long, y = lat, group = group)) + geom_path()

#Use the function 'fortify' to turn this shapefile into a dataframe so it's easy to plot on map
mapAnaktuvukBurnPerim.f = fortify(mapAnaktuvukBurnPerim)

#Generate a full map with both the google map, polygon, north arrow and scale bar.
ggmap(arctic_map) + 
  geom_polygon(aes(x = long, y = lat, group = group), 
               data = mapAnaktuvukBurnPerim.f,
               alpha = 0.8, 
               color = "blue",
               size = 0.2) + xlab("Longitude")+ ylab("Latitude") + ggtitle("Toolik Field Station: Digitized Burn Perimeter")+ theme(plot.title = element_text(hjust = .5)) + scalebar(x.min= -153.3, x.max= -153.0, y.min= 68.35, y.max= 68.42, dist= 50, location= "bottomleft", dd2km = TRUE, st.size=4, st.dist = .6, height = 0.5, model="WGS84")
+ north2(arctic_map, x = 0.30, y = 0.31, scale = 0.15, symbol = 1)

