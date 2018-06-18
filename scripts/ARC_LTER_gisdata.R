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
#Creating a basemap with a shapefile
# 15 June 18

#alaskamap <- readOGR(".//arc//gis_data//alaskamap.shp")
#plot(alaskamap, main="Alaska LTER Toolik Lake Station")
#toolikmap <- readOGR(".//arc//gis_data//toolmap.shp")
#plot(toolikmap, main="Toolik Lake Station: Alaska LTER")

#elevation<- raster("G://My Drive//Cloud_forest_SROP//data//Elevation_srtm//srtm_1km.tif")
#elevation
#plot(elevation)

#elevation_crop <- mask(elevation, toolikmap)
#extent <- setExtent(elevation_crop, )

#bb<- extent(68.8, 150.7, 12.54, 17.03)
#extent(elevation_crop) <- bb
#tool <-setExtent (elevation_crop, bb)
#plot(tool)

alaska1<-raster("G:/My Drive/NEON_LTER_2018/data/raw_data/arc/gis_data/nasa_modis_2013$data/alaska_tmo_2013168_geo.tif")
plot(alaska1)

alaska_crop<- crop(alaska1, mapToolik)
plot(alaska_crop)

#Clipping satellite image to size of Toolik research area
toolik_crop <- crop(alaska1, mapToolik)
plot(toolik_crop)
#########################################################################################
#Setting the extent
NewExtent <-extent(-152.86, -145.37, 67.72, 69.77)
extent(toolik_crop)<- NewExtent
plot(toolik_crop, col= cm.colors(7), main="Alaska LTER: Toolik Lake Station", legend=F)


# overlay shp files on alaskamap
plot(mapAnaktuvukBurnPerim,
     col="red",
     alpha=0.8,
     add=T,
     legend=T)

plot(mapAnakBurnLakes,
     col="red",
     alpha=0.8,
     add=T,
     legend=T)

plot(mapAnakBurnPerimRocha,
     col="red",
     alpha=0.4,
     add=T,
     legend=T)

plot(mapAnakBurnRivers,
     col="red",
     alpha=0.4,
     add=T,
     legend=T)

plot(mapTrails,
     col="grey",
     alpha=0.4,
     add=T,
     legend=T)

plot(mapCamp10,
     col="white",
     alpha=0.4,
     add=T,
     legend=T)

plot(mapCamp13,
     col="white"),
     alpha=0.4,
     add=T,
     legend=T)

plot(mapCrumpWater,
     col="blue",
     alpha=0.4,
     add=T,
     legend=T)

plot(map_GP,
     col="grey",
     alpha=0.4,
     add=T,
     legend=T)

plot(maptoolikarc,
     col="pink",
     alpha=0,
     add=T,
     legend=T)

plot(maptoolikpoly,
     col= "pink"),
     alpha=0,
     add=T,
     legend=T)

plot(mapImnaviatWater,
     col="blue",
     alpha=0.4,
     add=T,
     legend=T)

plot(mapKlingWater,
     col="blue",
     alpha=0.4,
     add=T,
     legend=T)

plot(mapKuparukWater,
     col="blue",
     alpha=0.4,
     add=T,
     legend=T)

plot(mapLostLakeWater,
     col="blue",
     alpha=0.4,
     add=T,
     legend=T)

plot(mapOksrukuyikWater,
     col="blue",
     alpha=0.4,
     add=T,
     legend=T)

plot(maptaps,
     col="gold1",
     alpha=0.4,
     add=T,
     legend=T)

plot(mapThermokarstWater,
     col="green",
     alpha=0.4,
     add=T,
     legend=T)

plot(mapToolikinletWater,
     col="blue",
     alpha=0.4,
     add=T,
     legend=T)

legend(-165,74, legend=c("Reference Locations", "1= Choco, Colombia, 100 m", "2= Palmira, Colombia, 1000 m", "3= San Jose de Alluriquin, Ecuador, 780 m", 
                        "4= Cotopaxi National Park, Ecuador, 4700 m", "5= Quito, Ecuador, 2700 m"),
       col=c("dark green"), cex=0.50, pch=c(15,173,173,173,173,173))

#############################################################################################################################################################################
#Create Basemap with google maps
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

###################################################################################
#Zoomed Map
arctic_map_zoomed <- get_map(location = c(lon = -149.65, lat = 68.75),
                       color = "color",
                       source = "google",
                       maptype = "satellite",
                       zoom = 9)

##To add multiple shapefiles do the same steps as all of the above except for the last step (generating the full map).
##Instead use the code below for that step:

basemap_zoomed<- ggmap(arctic_map_zoomed) + 
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
        size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapToolikinletWater.f, 
                                   alpha = 0, 
                                   color = "steelblue2",
                                   size = 0.2)+ geom_polygon(aes(x = long, y = lat, group = group), data = maptoolikarc.f, 
        alpha = 0, 
        color = "steelblue2",
        size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = maptoolikpoly.f, 
        alpha = 0, 
        color = "steelblue2",
       size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = map_GP.f, 
      alpha = 0, 
      color = "grey",
      size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapCrumpWater.f, 
  alpha = 0, 
  color = "steelblue2",
  size = 0.2)+ geom_polygon(aes(x = long, y = lat, group = group), data = mapImnaviatWater.f, 
  alpha = 0, 
  color = "steelblue2",
  size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapKlingWater.f, 
alpha = 0, 
  color = "steelblue2",
 size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapKuparukWater.f,
alpha = 0, 
color = "steelblue2",
size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapLostLakeWater.f, 
alpha = 0, 
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
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapCamp10.f, ## <----for this line insert the name of the shapefile you want to add (this has to have gone through the same steps as above for the other shapefile)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 alpha = 0.8, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 color = "white",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 size = 0.2) + geom_polygon(aes(x = long, y = lat, group = group), data = mapCamp13.f, ## <----for this line insert the name of the shapefile you want to add (this has to have gone through the same steps as above for the other shapefile)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            alpha = 0.8, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            color = "black",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            size = 0.2)+ geom_polygon(aes(x = long, y = lat, group = group), data = mapToolik.f, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      alpha = 0.9, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      color = "green",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      size = 0.2) 

basemap_zoomed
###################################################################################################
#NEON data overlay
# R functions for pulling NEON data from the server

library(httr)
library(jsonlite)

display_neon_filenames <- function(productCode) {
  require(httr)
  require(jsonlite)
  req <- GET(paste0("http://data.neonscience.org/api/v0/products/", productCode))
  avail <- fromJSON(content(req, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)
  urls <- unlist(avail$data$siteCodes$availableDataUrls)
  get_filenames <- function(x) fromJSON(content(GET(x), as = 'text'))$data$files$name
  files_test <- sapply(urls, get_filenames, simplify = FALSE)
  files_test[[which.max(sapply(files_test,length))]]
}

## Function to pull all data for a given data product

pull_all_neon_data <- function(productCode, nametag, pkg = 'basic', bind = TRUE){
  require(httr)
  require(jsonlite)
  require(dplyr)
  
  # Get list of URLs for all site - month combinations for that data product.
  req <- GET(paste0("http://data.neonscience.org/api/v0/products/", productCode))
  avail <- fromJSON(content(req, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)
  urls <- unlist(avail$data$siteCodes$availableDataUrls)
  
  # Loop through and get the data from each URL.
  res <- list()
  
  pb <- txtProgressBar(min=0, max=length(urls), style=3)
  count <- 0
  
  for (i in urls) {
    count <- count + 1
    setTxtProgressBar(pb, count)
    # Get the URLs for the site-month combination
    req_i <- GET(i)
    files_i <- fromJSON(content(req_i, as = 'text'))
    urls_i <- files_i$data$files$url
    # Read data from the URLs given by the API, skipping URLs that return an error.
    data_i <- try(read.delim(
      grep(paste0('(.*',nametag,'.*', pkg, '.*)'), urls_i, value = TRUE), 
      sep = ',', stringsAsFactors = FALSE), TRUE)
    if (!inherits(data_i, 'try-error')) res[[length(res) + 1]] <- data_i
}
  
  close(pb)
# Return as a single data frame or as a list of data frames, 
# depending on what option was selected.
  if (bind) {
    do.call(rbind, res)
  } else {
    res
  }
  }
## Function to get spatial information (coordinates) for a site or plot

get_site_locations <- function(siteID, what = 'site') {
  require(httr)
  require(jsonlite)
  require(purrr)
  # URLs of all spatial information about the site
  req <- GET(paste0("http://data.neonscience.org/api/v0/locations/", siteID))
  site_loc <- fromJSON(content(req, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)
  
  if (what == 'site') {
    # If only coordinates of the entire site are needed, return them
    return(data.frame(site_loc$data[1:19]))
  } else {
    # If "what" is some type of plot, find all URLs for that plot type
    urls <- grep(what, site_loc$data$locationChildrenUrls, value = TRUE)
    # Get the coordinates for each of those plots from each URL and return them
    loc_info <- map_dfr(urls, function(url) {
      req <- GET(url)
      loc <- fromJSON(content(req, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)
      loc[[1]][1:19]
    })
    return(loc_info)
  }
}

# Downloading mammal and bird data

# You can look in the data product catalog and manually figure out what the product codes are for small mammal trap data and for bird point count data, but I've provided them here. The `DP1` in the code indicates that this is Level 1 data. For Level 1 data, quality controls were run (Level 0 would be `DP0` meaning completely raw data) but the actual values are still raw values measured in the field, not some kind of calculated quantity (Level 2 and higher would be derived values).

mammal_code <- 'DP1.10072.001'
bird_code <- 'DP1.10003.001'

## Mammal download

# Let's take a look at what files are available for NEON small mammal trapping data for a given site-month combination. Running this takes a minute or two and requires an internet connection because we are querying the API.

display_neon_filenames(mammal_code)

# You can see that there are a lot of files available for one site. However the one we are interested in is the file containing the mammals caught per trap per night in the basic data package (expanded data package contains other variables that might be needed for quality control but that we are not interested in here). Let's pull that CSV file for all site-month combinations and combine it into one huge data frame that we can run analysis on. We specify we want everything belonging to the mammal code that contains the string `pertrapnight` in the file name, and by default only get the basic data package. Running this code on your own machine will take quite a few minutes since it has to download a lot of data, but you should get a progress bar showing how much time is remaining.

mammal_data <- pull_all_neon_data(productCode = mammal_code, 
                                  nametag = 'pertrapnight')

# Now let's take a look at what is in that data frame . . . 

str(mammal_data)

## Bird data

# Next, let's look at what data are available for birds.

display_neon_filenames(bird_code)

# Since the string `count` is in the name of the data file that we want for each site-month combination (the raw point count data for birds), we use that to pull point count data for each month and stick it all into one big data frame.

bird_data <- pull_all_neon_data(productCode = bird_code, 
                                nametag = 'count')

# Let's see what is in that data frame . . . 

str(bird_data)

#############################################################################################################################

# Making maps with NEON data

## Map within a single site

# First, let's make a map of bird species richness within one of the NEON sites. There are multiple bird survey grids at a site. As an example, we will use the Toolik Lake Arctic LTER site. 
#First, let's find the species richness at each bird plot at that site by taking the subset of the bird data from ARC LTER, grouping by plot, and counting the number of unique taxon IDs.

library(ggplot2)

(bird_arc <- bird_data %>% 
    filter(siteID %in% 'TOOL') %>%
    group_by(plotID) %>%
    summarize(richness = length(unique(taxonID))))

subset(bird_data, siteID == 'TOOL')$namedLocation

# We have 13 plots with varying species richness. Unfortunately, the coordinates of the plots are not included in the bird data. The plot metadata must be pulled separately from the API for each site. Here, I get the locations for all bird survey grids at Oak Ridge, then join them with the richness data.

bird_arc_locations <- get_site_locations(siteID = 'TOOL', what = 'basePlot')

# The location names are given as a long string so we need to extract the substring before the first period to join the locations with the richness values. We are using the base R function `strsplit()` to split the string on each period, and the `map_chr()` function from the package `purrr` to pull the first item out of each list element returned by `strsplit()`. Since `strsplit()` returns a list, `map_chr()` helps us transform the result of splitting the string back into a vector that becomes a data frame column.

library(purrr)

bird_arc <- bird_arc_locations %>%
  mutate(locationName = map_chr(strsplit(locationName, '\\.'), 1)) %>%
  rename(plotID = locationName) %>%
  right_join(bird_arc)

dim(bird_arc)
names(bird_arc)

# There are now a lot of useful spatial columns along with richness for each of the bird plots at Toolik. Let's make a map with each plot represented by a point colored by the bird species richness there, and labeled with the elevation rounded to the nearest meter for good measure. 
ggplot(bird_arc, 
       aes(x = locationUtmEasting, y = locationUtmNorthing)) +
  geom_point(aes(color = richness), size = 3) +
  # The vjust argument below moves the text down slightly.
  geom_text(aes(label = paste(round(locationElevation), 'm')), vjust = 1.2) +
  scale_color_gradient(low = 'blue', high = 'red') +
  theme_bw()

# So we've made a map of how bird richness varies spatially within the boundaries of the Oak Ridge study site. As an exercise, subset the bird data for a different site and make a map of a different variable such as the total number of individual birds observed at each survey grid in a particular year.

## Map of all sites in the contiguous USA

# It might also be interesting to plot bird richness across the United States. While before we used the projected coordinates provided by NEON, here we can use the latitudes and longitudes, then use the mapping capability of `ggplot2` to draw a map with our preferred projection.

# First, find the species richness at each site by counting the number of unique taxa.

bird_richness <- bird_data %>%
  group_by(siteID) %>%
  summarize(richness = length(unique(taxonID)))

# Next we need to pull the site centroids in lat-long for all the sites in the bird richness dataset. We do this by accessing the API again. A function from `purrr` called `map_dfr()` helps us loop through all the sites, get the overall location info, and tidily store it in a single data frame that we can easily join with the richness data frame.

site_coordinates <- map_dfr(bird_richness$siteID, get_site_locations, what = 'site')

bird_richness <- site_coordinates %>%
  rename(siteID = locationName) %>%
  right_join(bird_richness)

# Filter out everything that is not Alaska

bird_richness <- bird_richness %>%
  filter(between(locationDecimalLatitude, 59, 71))

# Create a map using the built-in US state borders, with site points colored by bird species richness. Use the `coord_map()` function to specify a projection. The Albers equal-area projection with arguments `lat0 = 23` and `lat1 = 30` is ideal for mapping the continental USA. We also use `coord_map()` to specify the range of latitude and longitude to plot.  The `borders('state')` and `borders('world')` elements add the state borders and the borders of Canada and Mexico, respectively.

birdplot<- ggplot(bird_richness, 
       aes(x = locationDecimalLongitude, y = locationDecimalLatitude, fill = richness)) +
  borders('state') +
  borders('world') +
  geom_point(pch = 21, size = 3) +
  scale_fill_gradient(low = 'blue', high = 'red') +
  theme_bw() +
  coord_map(projection = 'albers', 
            lat0 = 58, 
            lat1 = 71, 
            xlim = c(-166, -140), 
            ylim = c(58,71))
#########################################################################################################################################
# Now, let's make a map of mammal species richness within one of the NEON sites. 

(mammal_arc <- mammal_data %>% 
    filter(siteID %in% 'TOOL') %>%
    group_by(plotID) %>%
    summarize(richness = length(unique(taxonID))))

subset(mammal_data, siteID == 'TOOL')$namedLocation

# We have several plots with varying species richness. 
#Unfortunately, the coordinates of the plots are not included in the mammal data. 
#The plot metadata must be pulled separately from the API for each site. 
#Here, I get the locations for all mammal survey grids at Toolik, then join them with the richness data.

mammal_arc_locations <- get_site_locations(siteID = 'TOOL', what = 'basePlot')

# The location names are given as a long string so we need to extract the substring before the first period to join the locations with the richness values. We are using the base R function `strsplit()` to split the string on each period, and the `map_chr()` function from the package `purrr` to pull the first item out of each list element returned by `strsplit()`. Since `strsplit()` returns a list, `map_chr()` helps us transform the result of splitting the string back into a vector that becomes a data frame column.

mammal_arc <- mammal_arc_locations %>%
  mutate(locationName = map_chr(strsplit(locationName, '\\.'), 1)) %>%
  rename(plotID = locationName) %>%
  right_join(mammal_arc)

dim(mammal_arc)
names(mammal_arc)

# There are now a lot of useful spatial columns along with richness for each of the bird plots at Toolik. Let's make a map with each plot represented by a point colored by the bird species richness there, and labeled with the elevation rounded to the nearest meter for good measure. 


ggplot(mammal_arc, 
       aes(x = locationUtmEasting, y = locationUtmNorthing)) +
  geom_point(aes(color = richness), size = 3) +
  # The vjust argument below moves the text down slightly.
  geom_text(aes(label = paste(round(locationElevation), 'm')), vjust = 1.2) +
  scale_color_gradient(low = 'blue', high = 'red') +
  theme_bw()

# So we've made a map of how mammal richness varies spatially within the boundaries of the Toolik study site. As an exercise, subset the mammal data for a different site and make a map of a different variable such as the total number of individual mammals observed at each survey grid in a particular year.

## Map of all sites in the contiguous USA

# It might also be interesting to plot mammal richness across the United States. While before we used the projected coordinates provided by NEON, here we can use the latitudes and longitudes, then use the mapping capability of `ggplot2` to draw a map with our preferred projection.

# First, find the species richness at each site by counting the number of unique taxa.

mammal_richness <- mammal_data %>%
  group_by(siteID) %>%
  summarize(richness = length(unique(taxonID)))

# Next we need to pull the site centroids in lat-long for all the sites in the mammal richness dataset. We do this by accessing the API again. A function from `purrr` called `map_dfr()` helps us loop through all the sites, get the overall location info, and tidily store it in a single data frame that we can easily join with the richness data frame.

site_coordinates <- map_dfr(mammal_richness$siteID, get_site_locations, what = 'site')

mammal_richness <- site_coordinates %>%
  rename(siteID = locationName) %>%
  right_join(mammal_richness)

# Filter out everything that is not Alaska

mammal_richness <- mammal_richness %>%
  filter(between(locationDecimalLatitude, 59, 71))

# Create a map using the built-in US state borders, with site points colored by bird species richness. Use the `coord_map()` function to specify a projection. The Albers equal-area projection with arguments `lat0 = 23` and `lat1 = 30` is ideal for mapping the continental USA. We also use `coord_map()` to specify the range of latitude and longitude to plot.  The `borders('state')` and `borders('world')` elements add the state borders and the borders of Canada and Mexico, respectively.

mammalplot<- ggplot(mammal_richness, 
       aes(x = locationDecimalLongitude, y = locationDecimalLatitude, fill = richness)) +
  borders('state') +
  borders('world') +
  geom_point(pch = 21, size = 3) +
  scale_fill_gradient(low = 'blue', high = 'red') +
  theme_bw() +
  coord_map(projection = 'albers', 
            lat0 = 58, 
            lat1 = 71, 
            xlim = c(-166, -140), 
            ylim = c(58,71))

#############################################################################################################################################################################################################################################################################################
# Side by side plot

grid.arrange(mammalplot, birdplot, ncol =2)


# As an exercise, do the same for mammal richness and for elevation.

#############################################################################################################################
# Using NEON data to test hypotheses

## Bergmann's Rule

# Bergmann's rule is a famous macroecological pattern stating that average body sizes of (warm-blooded) animals should increase with increasing latitude. Bergmann, a physiologist working in the 19th century, speculated that animals living in colder areas further from the equator tend to be larger so that they have a smaller surface area to volume ratio and can more effectively retain heat. Later, people proposed additional mechanisms for why the pattern might exist. Also, some people even debated whether the pattern exists at all! **add more background material here**. Here, we tackle this question using the NEON small mammal dataset.

# NEON's mammal and bird data have latitudes for all the sites included in the data. Only the mammal data is suitable for testing Bergmann's rule because only the mammals are captured and weighed. 

### Processing data

# We should first find the mean body mass for each species at each site, along with their latitudes. We will use functions from the R package `dplyr` to quickly manipulate the data frame.

# The mammal data frame is huge, with over 600K rows. Most of the rows record trap-nights where no mammal was captured. Let's get rid of those.

library(dplyr)
nrow(mammal_data)
table(mammal_data$trapStatus)

# You can see that only status 4 and 5 correspond to one or more mammals caught in the trap. Filter the data frame to only keep those rows. We use the function `grepl()` which matches a regular expression to a vector of strings and returns `TRUE` if they match. The regular expression `"4|5"` means any string with the numerals 4 or 5 in it.

mammal_data <- mammal_data %>%
  filter(grepl('4|5', trapStatus))

nrow(mammal_data)

# We are down to ~70K rows where mammals were captured. Many of the mammals were not weighed so keep only the rows with a non-NA value for weight.

mammal_data <- mammal_data %>%
  filter(!is.na(weight))

nrow(mammal_data)

# We are down below 60K rows. Next, many records are for mammals where the same individual was captured multiple times. Keep only the rows where the recapture status is "N" for not a recapture.

mammal_data <- mammal_data %>%
  filter(recapture %in% 'N')

nrow(mammal_data)

# We are now at ~25K rows. Let's take the mean value of each species at each site, as well as the number of individuals used to calculate the mean, then get rid of any species-site combination with less than 5 individuals to ensure that we have a good estimate of the mean for all the species-site combinations.

mammal_means <- mammal_data %>%
  group_by(siteID, taxonID) %>%
  summarize(mean_mass = mean(weight), n_individuals = n()) %>%
  filter(n_individuals >= 5)

# In addition, we need to get rid of the species that are found at 2 or fewer sites because it is not robust to try to infer a trend within species from so few data points.

mammal_means <- mammal_means %>%
  ungroup %>%
  group_by(taxonID) %>%
  mutate(n_sites = length(siteID)) %>%
  filter(n_sites >= 3)

mammal_means

# We are left with 177 species-site combinations.

### Visualizing data

# Now that we have the mean value of body mass for each species and each site, let's make a scatterplot to visualize the pattern. We need to get the mean latitude value for each site and join it with our mean mass data frame so that we can plot mass versus latitude. Since all the sites are in the northern hemisphere, all latitude values are positive in the NEON dataset.

latitudes <- mammal_data %>%
  group_by(siteID) %>%
  summarize(latitude = mean(decimalLatitude))

mammal_means <- left_join(mammal_means, latitudes)

library(ggplot2)

p <- ggplot(mammal_means, aes(x = latitude, y = mean_mass, color = taxonID)) +
  geom_point() +
  theme_bw()

p

# That's hard to see any pattern in, so let's try looking at the y-axis on a log scale.

p + 
  scale_y_log10(name = 'Mass (g)')

# Still hard to see a pattern so just for visualization purposes, plot a separate simple linear regression for each species on the scatterplot.

p +
  scale_y_log10(name = 'Mass (g)') +
  stat_smooth(method = 'lm', aes(group = taxonID), se = FALSE)

### Testing the hypothesis

# From the last plot we made, it looks like a number of species have positive trends where body size increases with latitude moving away from the equator as Bergmann predicted. However there are clearly some exceptions, and a lot of the positive trends are pretty weak. That gives us a rough visual test of the hypothesis: I would argue we can say that it probably isn't supported or might have weak support at best from this small mammal dataset, but other people might have a different opinion.

# A formal way to test the hypothesis that body mass increases with latitude is to use a mixed-effects model. Our model will fit a random intercept to each species (because each species has its own characteristic mass) and use latitude as the fixed effect (it estimates a single slope of body mass change versus latitude change across all species). 

# First, let's confirm that the log transformation is a good idea by looking at histograms of the untransformed mass values as well as the log-transformed values.

p_hist <- ggplot(mammal_means, aes(x = mean_mass)) +
  geom_histogram(bins = 20) + 
  theme_bw() +
  scale_y_continuous(expand = c(0,0))

p_hist + scale_x_continuous(name = 'Untransformed mass (g)')

p_hist + scale_x_log10(name = 'Log10 transformed mass (g)')

# The untransformed values are highly skewed, with many small species and a long tail of large-bodied species. It seems appropriate to fit the model with log-transformed data.

# We will use the `lmer` function from the R package `lme4` to fit the mixed model.

library(lme4)

randomintercept_fit <- lmer(log10(mean_mass) ~ latitude + (1|taxonID), data = mammal_means)

summary(randomintercept_fit)

# The default summary information shows that the coefficient on latitude is positive but close to zero. We can generate a confidence interval on that coefficient by fitting the model many times with bootstrapped resamples of the original dataset. This takes a few seconds.

confint(randomintercept_fit, method = 'boot', nsim = 999)

# The confidence interval of the slope coefficient on latitude overlaps zero, meaning we have no support for the statement that there is any relationship between small mammal body mass and latitude, whether positive or negative.

# Another summary statistic we might be interested in is the variation explained by the fixed effect (latitude). A method was recently developed to partition that variation from the total variation and get an R-squared for it.

library(r2glmm)

r2beta(randomintercept_fit, method = 'kr', partial = FALSE)









#####################################################################################
#Creating a legend for basemap
legend<-read.csv("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data\\arc\\gis_data\\Legend.csv")
# create a blank ggplot theme
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_blank(),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         legend.position="right",
                         plot.title = element_text(size=16)))

mapplot<-ggplot() + 
  geom_polygon(data = legend, aes(x=long, y = lat, group = group), fill = NA, colour="darkgray", size=0.25)+
  geom_map(data=data,map=basemap,aes(map_id=country, x=lon, y=lat),fill = data$color, colour = "gray") +
  coord_equal() +
  theme_opts   

mapplot + theme(legend.position = "right")
