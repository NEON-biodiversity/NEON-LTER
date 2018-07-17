# Title:        Konza Prairie Burn Data
# Site:         Konza Prairie
# Data Sources: http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/da-search-all.html 
# Authors:      Kyra Hoerr  
# Date:         28 June 2018

# Set working directory

# Libraries
library(raster)
library(rgdal)
library(ggplot2)
library (ggmap)
library (ggsn)
library(gridExtra)
library(GISTools)
library(maptools)

# Read NEON point data
terrestrial <- readOGR(dsn="C:/Users/Kyra/Documents/Harvard/data/NEON_Spatial/All_NEON_TOS_Plots_V4", "All_Neon_TOS_Centroid_V4")
knz <- terrestrial[terrestrial$siteNam=="Konza Prairie Biological Station",]
knz_map <- spTransform(knz, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(knz_map)
points(knz_map, col="red")
#####################################################################################
## Read in spatial data

# Read GIS02
# get all files with the .shp extension from working directory 
setwd("C:/Users/Kyra/Documents/Harvard/data/KP/LTER/GIS02")

shps <- dir(getwd(), "*.shp$")

for (shp in shps) {
  assign(shp, readOGR(shp))
}
# Rename GIS032 
names(GIS032.shp) <- c("CODE", "NAME", "AREA", "PERIMETER", "ACRES", "HECTARES", "DATAID", "DATACODE")

# Stack GIS02
GIS02 <- rbind(GIS020.shp,GIS021.shp,GIS022.shp,GIS023.shp,GIS024.shp,GIS025.shp,GIS026.shp,GIS027.shp,GIS028.shp,GIS029.shp,GIS030.shp,GIS031.shp,GIS032.shp)
GIS02

# Rename map032 
names(map032) <- c("CODE", "NAME", "AREA", "PERIMETER", "ACRES", "HECTARES", "DATAID", "DATACODE")

# Stack GIS02
GIS02 <- rbind(map020, map021, map022, map023, map024, map025, map026, map027, map028, map029, map030, map031, map032)
GIS02

# Find frequency by watershed code
KNZ_freq <- as.data.frame(table(GIS02$NAME))
names(KNZ_freq)[1] <- "NAME"
KNZ_freq

# Merge frequency with map_GIS02
GIS02@data = data.frame(map_GIS02@data, KNZ_freq[match(GIS02$NAME,KNZ_freq$NAME),])
KNZ_freq_map <- GIS02[,-9]

# Export KNZ frequency data
writeOGR(obj=KNZ_freq_map, dsn= "C:/Users/Kyra/Documents/Harvard/data/KP/LTER", layer="KNZ_freq_map", driver="ESRI Shapefile") # this is in geographical projection

##############################################################################################

# Read GIS05
# get all files with the .shp extension from working directory 
setwd("C:/Users/Kyra/Documents/Harvard/data/KP/LTER/GIS05")

shps <- dir(getwd(), "*.shp$")

for (shp in shps) {
  assign(shp, readOGR(shp))
}

# Stack GIS05
GIS05 <- rbind(GIS050.shp,GIS051.shp,GIS052.shp,GIS053.shp,GIS054.shp,GIS055.shp,GIS056.shp,GIS057.shp,GIS058.shp,
               GIS059.shp,GIS060.shp,GIS061.shp,GIS062.shp,GIS063.shp,GIS064.shp,GIS065.shp,GIS066.shp,GIS067.shp,GIS068.shp,
               GIS069.shp,GIS070.shp,GIS071.shp,GIS072.shp,GIS073.shp,GIS074.shp,GIS075.shp,GIS076.shp,GIS077.shp,GIS078.shp,
               GIS079.shp,GIS080.shp,GIS081.shp,GIS082.shp,GIS083.shp,GIS084.shp,GIS085.shp,GIS086.shp,GIS087.shp,GIS088.shp)
GIS05

# Find fire frequency by watershed code
KNZ_fire_freq <- as.data.frame(table(GIS05$NAME))
names(KNZ_fire_freq)[1] <- "NAME"
KNZ_fire_freq

# Merge frequency with GIS05
GIS05@data = data.frame(GIS05@data, KNZ_fire_freq[match(GIS05$NAME,KNZ_fire_freq$NAME),])
KNZ_fire_freq_map <- GIS05[,-10] # number corresponds to "names.1"

# Export KNZ frequency data
writeOGR(obj=KNZ_fire_freq_map, dsn= "C:/Users/Kyra/Documents/Harvard/data/KP/LTER", layer="KNZ_fire_freq_map", driver="ESRI Shapefile") # this is in geographical projection

######################################################################################
# Read GIS13
# get all files with the .shp extension from working directory 
setwd("C:/Users/Kyra/Documents/Harvard/data/KP/LTER/GIS13")

shps <- dir(getwd(), "*.shp$")

for (shp in shps) {
  assign(shp, readOGR(shp))
}

# Stack GIS13
GIS13 <- rbind(GIS130.shp,GIS131.shp,GIS132.shp,GIS133.shp,GIS134.shp,GIS135.shp,GIS136.shp,GIS137.shp,GIS138.shp,
               GIS139.shp,GIS140.shp,GIS141.shp,GIS142.shp,GIS143.shp,GIS144.shp,GIS145.shp,GIS146.shp,GIS147.shp,GIS148.shp,
               GIS149.shp)
GIS13

# Find fire frequency by watershed code
KNZ_sup_freq <- as.data.frame(table(GIS13$NAME))
names(KNZ_sup_freq)[1] <- "NAME"
KNZ_sup_freq

# Merge frequency with map_GIS02
GIS13@data = data.frame(GIS13@data, KNZ_sup_freq[match(GIS13$NAME,KNZ_sup_freq$NAME),])
GIS13
KNZ_sup_freq_map <- GIS13[,-10] # number corresponds to "names.1"
KNZ_sup_freq_map

# Combine GIS05 and GIS13 (fire history and supplemental burn history)
final_burn_hist <- rbind(KNZ_fire_freq_map,KNZ_sup_freq_map)

# Export KNZ frequency data
writeOGR(obj=final_burn_hist, dsn= "C:/Users/Kyra/Documents/Harvard/data/KP/LTER", layer="KNZ_complete_burn_map", driver="ESRI Shapefile") # this is in geographical projection

