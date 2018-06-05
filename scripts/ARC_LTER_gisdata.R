# Title:			  Reading in & Plotting NEON Data in R
# Site:			    ARC LTER
# Data Sources:	https://toolik.alaska.edu/gis/data/
# Authors:		  Cameo Chilcutt
# Date:			    4 June 2018

setwd("C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\camp_buildings$data\\Camp Buildings")
rm(list = ls())

#libraries
library(raster)
library(rgdal)

#import Camp Buildings 2010 shapefiles
shape_path_CB10 <- "C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\camp_buildings$data\\Camp Buildings"
shape_name_CB10 <- "campbuilds_2010_100909"
shape_extent_CB10 <- readOGR(shape_path_CB10, shape_name_CB10)

#plot Camp Buildings 2010
plot(shape_extent_CB10,  col=terrain.colors(10),
     alpha=1,
     legend=F, main="Camp Buildings 2010")


#import Camp Buildings 2013
shape_path_CB13 <- "C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\camp_buildings_2013$data"
shape_name_CB13 <- "Camp_Buildings_2013"
shape_extent_CB13 <- readOGR(shape_path_CB13, shape_name_CB13)

#plot Camp Buildings 2013
plot(shape_extent_CB13, col=rainbow(10),
     alpha=1,
     legend=F, main="Camp Buildings 2013")

#import Gravel Pads
shape_path_GP <- "C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\gravel_pads$data"
shape_name_GP <- "gravel_pads"
shape_extent_GP <- readOGR(shape_path_GP, shape_name_GP)

#plot Gravel Pads
plot(shape_extent_GP, col=grey(1:100/100), legend=FALSE, main="Gravel Pads")


#import hyd_toolik_arc
shape_path_Hydtoolarc <- "C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\hyd_toolik_arc_101111$data"
shape_name_Hydtoolarc <- "hyd_toolik_arc101111"
shape_extent_Hydtoolarc <- readOGR(shape_path_Hydtoolarc, shape_name_Hydtoolarc)

#plot hyd_toolik_arc
plot(shape_extent_Hydtoolarc, col=rainbow(4),
     alpha=1,
     legend=F, main="Hyd_toolik_arc")

#import hyd_toolik_poly
shape_path_Hydtoolpoly <- "C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\hyd_toolik_poly101111$data\\files for website"
shape_name_Hydtoolpoly <- "hyd_toolik_poly101111"
shape_extent_Hydtoolpoly <- readOGR(shape_path_Hydtoolpoly, shape_name_Hydtoolpoly)

#plot hyd_toolik_poly
plot(shape_extent_Hydtoolpoly, col=cm.colors(10),
     alpha=1,
     legend=F, main="Hyd_toolik_poly")

#import research_plots_2009$data **Not Working, Trying to fix**
#shape_path_5 <- "C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\research_plots_2009$data"
#shape_name_5 <- "Research_Plots_2009_NAD83_UTM_100504"
#shape_extent_5 <- readOGR(shape_path_5, shape_name_5)

#plot research_plots
#plot(shape_extent_5,  col=grey(1:100/100), legend=FALSE, main="Research Plots 2009")

#import roads_trails$data
shape_path_Roads <- "C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\roads_trails$data"
shape_name_Roads <- "transport_100423"
shape_extent_Roads <- readOGR(shape_path_Roads, shape_name_Roads)

#plot roads_trails$data
plot(shape_extent_Roads, col=cm.colors(10),
     alpha=1,
     legend=F, main="Road_Trails")

#import taps$data
shape_path_Taps <- "C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\taps$data"
shape_name_Taps <- "pipeline_proj"
shape_extent_Taps <- readOGR(shape_path_Taps, shape_name_Taps)

#plot taps$data
plot(shape_extent_Taps, col=heat.colors(5),
     alpha=1,
     legend=F, main="Taps")

#import toolik_rna$data
shape_path_ToolikRNA <- "C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\toolik_rna$data"
shape_name_ToolikRNA <- "Toolik_RNA"
shape_extent_ToolikRNA <- readOGR(shape_path_ToolikRNA, shape_name_ToolikRNA)

#plot toolik_rna$data
plot(shape_extent_ToolikRNA, col=terrain.colors(8),
     alpha=1,
     legend=F, main="Toolik_RNA")

#import watersheds_research
shape_path_Water <- "C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\watersheds_research$data\\Watersheds_Research"
shape_name_Water <- "Crump_Watershed"
shape_extent_Water <- readOGR(shape_path_Water, shape_name_Water)

#plot watersheds research
plot(shape_extent_Water, col=cm.colors(8),
     alpha=1,
     legend=F, main="Watersheds Research")

##########################################################################################################
#Krymsen's Part
shape_path <- "C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\100fcont_uk$data"
shape_name<-"100fcont_uk"
shape_extant<-readOGR(shape_path, shape_name)
plot(shape_extant, col=grey(1:100/100), main="100f\n NEON Harvard Forest Field Site")

shape_path_1<-"C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\anak_burn_lakes$data"
shape_name_1<-"Anaktuvuk_Burn_Lakes"
shape_extant_1<-readOGR(shape_path_1, shape_name_1)
plot(shape_extant_1, col=heat.colors (10), main="Burn_Lakes\n NEON Harvard Forest Field Site")

shape_path_2<-"C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\anak_burn_perim_rocha$data"
shape_name_2<-"Anak_Burn_Perim_Rocha"
shape_extant_2<-readOGR(shape_path_2,shape_name_2)
plot(shape_extant_2, col=grey(1:100/100), main="Anak_Burn_Perim\n NEON Harvard Forest Field Site")

shape_path_3<-"C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\anak_burn_rivers$data"
shape_name_3<-"Anaktuvuk_Burn_Rivers"
shape_extant_3<-readOGR(shape_path_3, shape_name_3)
plot(shape_extant_3, col=grey(1:100/100), main="Anak_Burn_river\n NEON Harvard Forest Field Site")

shape_path_4<-"C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\anaktuvuk_burn_perim$data"
shape_name_4<-"progression_perimeters_0822-0930"
shape_extant_4<-readOGR(shape_path_4, shape_name_4)
plot(shape_extant_4, col=grey(1:100/100), main="Progression_perimeters\n NEON Harvard Forest Field Site")

########################################################################################################
#Download packages ggplot and ggmap

#Load in libraries
library(ggplot2)
library (ggmap)
############################################################################################################

# reproject to longlat
#Camp Buildings 2010
mapG<-spTransform(shape_extent_CB10, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapG)
mapG
#ToolikRNA data
mapToolik<-spTransform(shape_extent_ToolikRNA, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(mapToolik)
mapToolik

#BaseMap of Arctic Field Site
Arctic_Map<-raster("C:\\Users\\Cameo Chilcutt\\Documents\\NEON\\nasa_modis_2013$data\\alaska_tmo_2013168_geo.tif")
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
plot(mapG, add=TRUE)

