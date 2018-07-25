# Title:        Konza Prairie Burn Data
# Site:         Konza Prairie
# Data Sources: 
# Authors:      Kyra Hoerr  
# Date:         16 July 2018

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
terrestrial <- readOGR(dsn="KNZ_basemap/All_NEON_TOS_Plots_V4", "All_Neon_TOS_Centroid_V4")
knz <- terrestrial[terrestrial$siteNam=="Konza Prairie Biological Station",]
knz_map <- spTransform(knz, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(knz_map)
points(knz_map, col="red")

# Read watershed code data
code_data <- read.csv("KNZ_basemap/KFH011.csv") # needs to be put on G drive
#####################################################################################
## Read in spatial data

# Read GIS02
# get all files with the .shp extension from working directory 
#setwd("C:/Users/Kyra/Documents/Harvard/data/KP/LTER/GIS02") 

setwd('C:/Users/srecord/Dropbox/HF_REU/2018/Kyra_Hoerr/LTER/GIS02')
shps <- dir(getwd(), "*.shp$")

for (shp in shps) {
  assign(shp, readOGR(shp))
}
 Rename GIS032 
names(GIS032.shp) <- c("CODE", "NAME", "AREA", "PERIMETER", "ACRES", "HECTARES", "DATAID", "DATACODE")

# Determine maximum extent of all shapefiles
shpextents <- matrix(nrow=length(shps), ncol=5)
colnames(shpextents) <- c('watershed','xmin','xmax','ymin','ymax')

for(i in 1:length(shps)){
  tempshp <- readOGR(shps[i])
  shpextents[i,1] <- shps[i]
  shpextents[i,2:5] <- as.vector(extent(tempshp))
}

# determine max extent covered by all shapefiles. this will become the extent of the rasterized shapefiles.
totalextent <- extent(as.numeric(min(shpextents[,2])),as.numeric(max(shpextents[,3])), as.numeric(min(shpextents[,4])), as.numeric(max(shpextents[,5])))

# Assign a null object for the start of the raster stack. 
GIS02_burnstack <-stack()

for(i in 1:length(shps)){
  tempshp <- readOGR(shps[i])
  blank_raster <- raster(nrow = 100, ncol = 100, extent(totalextent))
  temp_burn_raster <- rasterize(tempshp, blank_raster, fun='mean') # note field needs to be filled in for this to work. currently does not work
  GIS02_burnstack <- addLayer(GIS02_burnstack, temp_burn_raster)
}

names(GIS02_burnstack) <- shps

# Stack GIS02
GIS02 <- rbind(GIS020.shp,GIS021.shp,GIS022.shp,GIS023.shp,GIS024.shp,GIS025.shp,GIS026.shp,GIS027.shp,GIS028.shp,GIS029.shp,GIS030.shp,GIS031.shp,GIS032.shp)
GIS02

# Rename map032 
names(map032) <- c("CODE", "NAME", "AREA", "PERIMETER", "ACRES", "HECTARES", "DATAID", "DATACODE")

# Stack GIS02
GIS02 <- rbind(map020, map021, map022, map023, map024, map025, map026, map027, map028, map029, map030, map031, map032)
GIS02

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
KNZ_fire_freq_map <- GIS05[,-10] # number corresponds to "code.1"

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

# Merge frequency with map_GIS13
GIS13@data = data.frame(GIS13@data, KNZ_sup_freq[match(GIS13$NAME,KNZ_sup_freq$NAME),])
GIS13
KNZ_sup_freq_map <- GIS13[,-10] # number corresponds to "names.1"
KNZ_sup_freq_map

# Combine GIS05 and GIS13 (fire history and supplemental burn history)
final_burn_hist <- rbind(KNZ_fire_freq_map,KNZ_sup_freq_map)
final_burn_hist <- spTransform(final_burn_hist, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
write.csv(final_burn_hist, file = "final_burn_hist.csv")

# Export KNZ frequency data
writeOGR(obj=final_burn_hist, dsn= "C:/Users/Kyra/Documents/Harvard/data/KP/LTER", layer="KNZ_complete_burn_map", driver="ESRI Shapefile") # this is in geographical projection

# Find NEON points
NEON_burn <- over(knz_map,final_burn_hist)
NEON_burn$plotID <- knz_map$plotID
write.csv(NEON_burn, file = "NEON_burn.csv")
