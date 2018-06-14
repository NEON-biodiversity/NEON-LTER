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
library(gridExtra)

#import Camp Buildings 2010 shapefiles
CB10 <- readOGR(".\\arc\\gis_data\\camp_buildings$data\\Camp Buildings", "campbuilds_2010_100909")

#plot Camp Buildings 2010
plot(CB10,  col=terrain.colors(10), alpha=1, legend=F, main="Camp Buildings 2010")

#Reprojection
mapCamp10<-spTransform(CB10, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapCamp10)
mapCamp10

#fortify for mapping
mapCamp10.f = fortify(mapCamp10)

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

#fortify for mapping
maptoolikarc.f = fortify(maptoolikarc)

#import hyd_toolik_poly
Hydtoolpoly<- readOGR(".\\arc\\gis_data\\hyd_toolik_poly101111$data", "hyd_toolik_poly101111")

#plot hyd_toolik_poly
plot(Hydtoolpoly, col=cm.colors(10), alpha=1, legend=F, main="Hyd_toolik_poly")

#Reprojection
maptoolikpoly<-spTransform(Hydtoolpoly, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(maptoolikpoly)
maptoolikpoly

#fortify for mapping
maptoolikpoly.f = fortify(maptoolikpoly)

#import research_plots_2009$data  **Issues with shapefile**
Research_plots <- readOGR(".\\arc\\gis_data\\research_plots_2009$data", "Research_Plots_2009_NAD83_UTM_100504")
Research_plots
summary(Research_plots)
class(Research_plots)

#Reprojection
#mapResearchplots<-spTransform(Research_plots, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
#plot(mapResearchplots)
#mapResearchplots

#plot research_plots
#plot(Research_plots, col=heat.colors(1), alpha=1, legend=F, main="Research Plots Toolik")

#fortify for mapping
#mapResearchplots.f = fortify(mapResearchplots)

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

#fortify for mapping
mapCrumpWater.f = fortify(mapCrumpWater)
mapImnaviatWater.f = fortify(mapImnaviatWater)
mapKlingWater.f = fortify(mapKlingWater)
mapKuparukWater.f = fortify(mapKuparukWater)
mapLostLakeWater.f = fortify(mapLostLakeWater)
mapOksrukuyikWater.f = fortify(mapOksrukuyikWater)
mapThermokarstWater.f = fortify(mapThermokarstWater)
mapToolikinletWater.f = fortify(mapToolikinletWater)

#100fcont_uk
fcont_uk<-readOGR(".\\arc\\gis_data\\100fcont_uk$data", "100fcont_uk")
plot(fcont_uk, col=grey(1:100/100), main="fcont_uk")

#Reprojection **Not working**
#mapfcont<-spTransform(fcont_uk, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
#plot(mapfcont)
#map100fcont

#Anak_Burn_Lakes
AnakBurnLakes<-readOGR(".\\arc\\gis_data\\anak_burn_lakes$data", "Anaktuvuk_Burn_Lakes")
plot(AnakBurnLakes, col=heat.colors (10), main="Burn_Lakes")

#Reprojection
mapAnakBurnLakes<-spTransform(AnakBurnLakes, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapAnakBurnLakes)

#fortify for mapping
mapAnakBurnLakes.f = fortify(mapAnakBurnLakes)

#Anak_Burn_Perim_Rocha
AnakBurnPerimRocha<-readOGR(".\\arc\\gis_data\\anak_burn_perim_rocha$data", "Anak_Burn_Perim_Rocha")
plot(AnakBurnPerimRocha, col=grey(1:100/100), main="Anak_Burn_Perim")

#Reprojection
mapAnakBurnPerimRocha<-spTransform(AnakBurnPerimRocha, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapAnakBurnPerimRocha)

#fortify for mapping
mapAnakBurnPerimRocha.f = fortify(mapAnakBurnPerimRocha)

#Anak_Burn_Rivers
AnakBurnRivers<-readOGR(".\\arc\\gis_data\\anak_burn_rivers$data", "Anaktuvuk_Burn_Rivers")
plot(AnakBurnRivers, col=heat.colors (6), main="Anak_Burn_River")

#Reprojection
mapAnakBurnRivers<-spTransform(AnakBurnRivers, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapAnakBurnRivers)

#fortify for mapping
mapAnakBurnRivers.f = fortify(mapAnakBurnRivers)

#Anaktuvuk_Burn_Perim
AnaktuvukBurnPerim<-readOGR(".\\arc\\gis_data\\anaktuvuk_burn_perim$data", "progression_perimeters_0822-0930")
plot(AnaktuvukBurnPerim, col=grey(1:100/100), main="Progression_perimeters")

#Reprojection
mapAnaktuvukBurnPerim<-spTransform(AnaktuvukBurnPerim, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapAnaktuvukBurnPerim)

#fortify for mapping
mapAnaktuvukBurnPerim.f = fortify(mapAnaktuvukBurnPerim)
############################################################################################################
#Create Basemap
#code from Generating_google_basemaps by Beth Gerstner



##Extent of Toolik Field Station
##NewExtent<-extent(-149.600, -149.589, 68.625, 68.630)

#Obtain basemap of the Arctic field site through google maps 
#(can also generate map types "roadmap", "terrain", "satellite", "hybrid")

arctic_map1 <- get_map(location = c(lon = -149.5, lat = 69.5),
                      color = "color",
                      source = "google",
                      maptype = "satellite",
                      zoom = 7)


#Visualize the shapefile and make sure it loaded in correctly
ggplot(data = mapAnakBurnPerimRocha, aes(x = long, y = lat, group = group)) + geom_path()

##To add multiple shapefiles do the same steps as all of the above except for the last step (generating the full map).
##Instead use the code below for that step:

basemap<- ggmap(arctic_map1) + 
  geom_polygon(aes(x = long, y = lat, group = group), data = mapAnakBurnRivers.f, 
   alpha = 0, 
   color = "steelblue2",
   size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), 
   data = mapAnakBurnPerimRocha.f,
   alpha = 0, 
   color = "indianred4", 
   size = 0.2) + xlab("Longitude")+ ylab("Latitude") + ggtitle("Toolik Lake Field Station: Disturbance Patterns")+ theme(plot.title = element_text(hjust = .5)) + scalebar(x.min= -152.3, x.max= -153.0, y.min= 68.35, y.max= 68.42, dist= 50, location= "bottomleft", dd2km = TRUE, st.size=2, st.dist = .4, height = 0.5, model="WGS84") + geom_polygon(aes(x = long, y = lat, group = group), 
   data = mapAnakBurnLakes.f,
   alpha = 0, 
   color = "indianred3", 
   size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), 
data = mapAnaktuvukBurnPerim.f, ## <----for this line insert the name of the shapefile you want to add (this has to have gone through the same steps as above for the other shapefile)
alpha = 0, 
color = "indianred3",
size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapToolik.f, 
  alpha = 0, 
   color = "darkorchid",
size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = maptoolikarc.f, 
alpha = 0, 
color = "steelblue2",
size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = maptoolikpoly.f, 
alpha = 0, 
color = "steelblue2",
size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapToolikinletWater.f, 
alpha = 0, 
color = "green",
size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapCamp10.f, ## <----for this line insert the name of the shapefile you want to add (this has to have gone through the same steps as above for the other shapefile)
alpha = 0.8, 
color = "white",
size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapCamp13.f, ## <----for this line insert the name of the shapefile you want to add (this has to have gone through the same steps as above for the other shapefile)
alpha = 0.8, 
color = "black",
size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = map_GP.f, 
alpha = 0, 
color = "grey",
size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapCrumpWater.f, 
alpha = 0.8, 
color = "steelblue2",
size = 0.2)+ geom_polygon(aes(x = long, y = lat, group = group), data = mapImnaviatWater.f, 
alpha = 0.8, 
color = "steelblue2",
size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapKlingWater.f, 
alpha = 0.8, 
color = "steelblue2",
size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapKuparukWater.f, 
alpha = 0.8, 
color = "steelblue2",
size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapLostLakeWater.f, 
alpha = 0.8, 
color = "steelblue2",
size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapOksrukuyikWater.f, 
alpha = 0.8, 
color = "steelblue2",
size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = maptaps.f, 
alpha = 0, 
color = "gold1",
size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapThermokarstWater.f, 
alpha = 0.2, 
color = "ivory",
size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapTrails.f, 
alpha = 0, 
color = "plum4",
size = 0.2) 

basemap
