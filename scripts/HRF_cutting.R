# Title:        Harvard Forest Cutting Data
# Site:         Harvarad Forest
# Data Sources: http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/da-search-all.html 
# Authors:      Kyra Hoerr  
# Date:         28 June 2018

#set file paths 
root_folder <- "G:/Shared drives/MacrosystemsBiodiversity"
setwd(file.path(root_folder))
save_path <- file.path(root_folder, 'subprojects/MSB_landuse/Summer_Students/Site_histories/HARV/HARV_L1_Data')
import_path1 <- file.path(root_folder, 'subprojects/MSB_landuse/Summer_Students/Site_histories/HARV/HARV_L0_Data')
import_path2 <- file.path(root_folder, '')

# Libraries
library(raster)
library(rgdal)
library(ggplot2)
library (ggmap)
library (ggsn)
library(gridExtra)

# Read NEON point data
terrestrial <- readOGR(dsn="C:/Users/Kyra/Documents/Harvard/data/NEON_Spatial/All_NEON_TOS_Plots_V4", "All_Neon_TOS_Centroid_V4")
hrf <- terrestrial[terrestrial$siteNam=="Harvard Forest",]
hrf_map <- spTransform(hrf, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(hrf_map)
points(hrf_map, col="red")

# Read Mass cutting data
# Read csvs
plan <- read.csv("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Timber/hf080-01-plan.csv", header=TRUE, stringsAsFactors=FALSE)
volume <- read.csv("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Timber/hf080-02-volume.csv", header=TRUE, stringsAsFactors=FALSE)

# Read shapefile
cutting <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Timber/hf080-03-gis", "all_cutting_plans_v4")
cut_data <- spTransform(cutting, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# Change cut_data name
names(cut_data)[1] <- "file.id"

# Merge plan and volume data with cut_data
cut_data@data = data.frame(cut_data@data, plan[match(cut_data$file.id, plan$file.id),])
cut_data@data = data.frame(cut_data@data, volume[match(cut_data$file.id, volume$file.id),])
cut_data

# Crop cutting data
cut_crop <- crop(cut_data, extent(hrf_map))
plot(cut_crop)
points(hrf_map, col="red")

# Export cutting map
writeOGR(obj=cut_crop, dsn= "C:/Users/Kyra/Documents/Harvard/data/HF/LTER", layer="hrf_cutting_map", driver="ESRI Shapefile") # this is in geographical projection

# Find HF NEON point data
hrf_cut <- over(hrf_map,cut_crop)
hrf_cut
length(hrf_cut)

# Find NEON plots within cutting polygons
hrf_points <- intersect(hrf_map,cut_crop) # points (35)

#subset from hrf_points
NEON_cut <- subset(hrf_points, select=c("fis.year","plotID"))

# Find cutting polygons containing NEON points
hrf_polygons <- intersect(cut_crop,hrf_map) # polygons (25)
points(hrf_points, col="red")

# Export points and polygons
writeOGR(obj=hrf_points, dsn= "C:/Users/Kyra/Documents/Harvard/data/HF/LTER", layer="hrf_points", driver="ESRI Shapefile") # this is in geographical projection
writeOGR(obj=hrf_polygons, dsn= "C:/Users/Kyra/Documents/Harvard/data/HF/LTER", layer="hrf_polygons", driver="ESRI Shapefile") # this is in geographical projection

# Find frequencies of cut types
as.data.frame(table(hrf_points$typereg1))
as.data.frame(table(hrf_points$typereg2))
as.data.frame(table(hrf_points$typereg3))



# Export hrf_cut for Within_site_prep.R
write.csv(hrf_cut, file="hrf_cut.csv")
