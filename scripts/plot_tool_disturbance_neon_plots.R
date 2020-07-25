##This script is to download an appropriate google map, add a shapefile, 
##and then include a scale bar and north arrow.
##By: Beth Gerstner with updates by Sydne Record for Toolik disturbance map

#Download packages ggplot2, ggsn, ggmap, rgdal

#Load in libraries
library (ggmap) # generates google maps
library(rgdal) # read in shapefile
library(ggplot2) # visualize shapefile
library (ggsn) # add scale bar and north arrow


##Extent of Toolik Field Station
##NewExtent<-extent(-149.600, -149.589, 68.625, 68.630)

#Obtain basemap of the Arctic field site through google maps 
#(can also generate map types "roadmap", "terrain", "satellite", "hybrid")

arctic_map <- get_map(location = c(lon = -150.5, lat = 69.3),
                      color = "color",
                      source = "google",
                      maptype = "terrain",
                      zoom = 7)

# Google drive file path
google_drive <- 'C:/Users/srecord/Dropbox/LTER-NEON_land-use/NEON_LTER_2018'
setwd(file.path (google_drive))
#libraries
library(raster)
library(rgdal)
library(ggplot2)
library (ggmap)
library (ggsn)
library(gridExtra)

#import neon plots
neonplots<-readOGR("data/raw_data/neon/spatial_data/All_NEON_TOS_Plots_V4/All_Neon_TOS_Centroid_V4.shp")
plot(neonplots)
neonplots<-(spTransform(neonplots, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")))
neonplots.f = data.frame(neonplots)

# Select only toolik plots
tool_neon_plots.f <- fortify(subset(neonplots.f, siteID=='TOOL'))

#import a cutout of toolik lake field station
toolik<- readOGR("data/raw_data/arc/gis_data/toolik_cutout/Toolik_RNA.shp")
plot(toolik)
maptoolik<-(spTransform(toolik, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")))
maptoolik.f = fortify(maptoolik)

#import Camp Buildings 2010 shapefiles
CB10 <- readOGR("data/raw_data/arc/gis_data/camp_buildings/campbuilds_2010_100909.shp")

#plot Camp Buildings 2010
plot(CB10,  col=terrain.colors(10), alpha=1, legend=F, main="Camp Buildings 2010")

#Reprojection
mapCamp10<-spTransform(CB10, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapCamp10)
mapCamp10

#fortify for mapping
mapCamp10.f = fortify(mapCamp10)
#import roads_trails$data
Roads<- readOGR("data/raw_data/arc/gis_data/roads/roads.shp")

#plot roads
plot(Roads, col=cm.colors(10), alpha=1, legend=F, main="Road_Trails")

#Reprojection
mapTrails<-spTransform(Roads, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapTrails)
mapTrails

#fortify for mapping
mapTrails.f = fortify(mapTrails)

#import taps$data
pipeline<- readOGR("data/raw_data/arc/gis_data/pipeline/pipeline.shp")

#plot taps$data
plot(pipeline, col=heat.colors(5), alpha=1, legend=F, main="pipeline")

#Reprojection
mappipeline<-spTransform(pipeline, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mappipeline)
mappipeline

#fortify for mapping
mappipeline.f = fortify(mappipeline)

#Watershed
ThermokarstWater<- readOGR("data/raw_data/arc/gis_data/watershed/Thermokarst_Watershed.shp")

#plot watersheds research
plot(ThermokarstWater, col=cm.colors(8), alpha=1, legend=F, main="Thermokarst Watersheds Research")

#Reprojection
mapThermokarstWater<-spTransform(ThermokarstWater, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

plot(mapThermokarstWater)

#fortify for mapping
mapThermokarstWater.f = fortify(mapThermokarstWater)

#Anak_Burn_Perim_Rocha
burn<-readOGR("data/raw_data/arc/gis_data/burn/Anak_Burn_Perim_Rocha.shp")
plot(burn, col=grey(1:100/100), main="Anaktuvuk Burn")

#Reprojection
mapburn<-spTransform(burn, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapburn)

#fortify for mapping
mapburn.f = fortify(mapburn)

#Zoomed Map
arctic_map_zoomed <- get_map(location = c(lon = -149.65, lat = 68.75),
                             color = "color",
                             source = "google",
                             maptype = "satellite",
                             zoom = 9)

##To add multiple shapefiles do the same steps as all of the above except for the last step (generating the full map).
##Instead use the code below for that step:

basemap_zoomed<- ggmap(arctic_map_zoomed) + 
  geom_point(data=tool_neon_plots.f, aes(x=coords.x1, y=coords.x2), color="orange") +

  geom_polygon(aes(x = long, y = lat, group = group), data = mapburn.f, 
               color = "purple",
               size = 0.2) + 
  geom_polygon(aes(x = long, y = lat, group = group), data = mapCamp10.f,
               alpha = 0, 
               color = "purple", 
               size = 0.2) + xlab("Longitude")+ ylab("Latitude") + 
  ggtitle("Toolik Lake Field Station: Disturbance Patterns") + 
  theme(plot.title = element_text(hjust = .5)) + 
 # scalebar(x.min= -153.0, x.max= -152.3, y.min= 68.35, y.max= 68.42, dist= 50, location= "bottomleft", dd2km = TRUE, st.size=2, st.dist = .4, height = 0.5, model="WGS84") + 
  geom_polygon(aes(x = long, y = lat, group = group), data = mappipeline.f,
               alpha = 0, 
               color = "yellow", 
               size = 0.2) + 
  geom_polygon(aes(x = long, y = lat, group = group), 
               data = mapTrails.f, 
               alpha = 0, 
               color = "skyblue",
               size = 0.2) + 
  geom_polygon(aes(x = long, y = lat, group = group), data = mapThermokarstWater.f, 
               alpha = 0.8, 
               color = "pink",
               size = 0.2) + 
  geom_polygon(aes(x = long, y = lat, group = group), data = maptoolik.f, 
               alpha = 0, 
               color = "white",
               size = 0.2) 

basemap_zoomed 

