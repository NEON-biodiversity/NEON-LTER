# Title:        Massachusets Timber Harvesting Study
# Site:         Harvard Forest
# Data Sources: http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/da-search-all.html 
# Authors:      Kyra Hoerr  
# Date:         20 June 2018

# Set working directory
setwd("G:/My Drive/NEON_LTER_2018/data/raw_data")

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
