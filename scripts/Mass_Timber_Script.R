# Title:        Massachusets Timber Harvesting Study
# Site:         Harvard Forest
# Data Sources: http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/da-search-all.html 
# Authors:      Kyra Hoerr  
# Date:         20 June 2018

# Libraries
library(raster)
library(rgdal)
library(ggplot2)
library (ggmap)
library (ggsn)
library(gridExtra)

# Read Data
# Read NEON data
terrestrial <- readOGR(dsn="C:/Users/Kyra/Documents/Harvard/data/HF/NEON_Spatial/All_NEON_TOS_Plots_V4", "All_Neon_TOS_Centroid_V4")
hrf <- terrestrial[terrestrial$siteNam=="Harvard Forest",]
hrf_map <- spTransform(hrf, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(hrf_map)
points(hrf_map, col="red")

# Read csvs
plan <- read.csv("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Timber/hf080-01-plan.csv", header=TRUE, stringsAsFactors=FALSE)
volume <- read.csv("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Timber/hf080-01-plan.csv", header=TRUE, stringsAsFactors=FALSE)
# Read shapefile
cutting <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Timber/hf080-03-gis", "all_cutting_plans_v4")
cut_data <- spTransform(cutting, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(cut_data)

# Create a matrix
NEON_matrix <- matrix(
  c(rep("N/A")), 
  nrow=57,
  ncol=146
)

# Map overlapping points
# crop cut data for mapping
cut_cropped <- crop(cut_data, extent(hrf_map))
plot(cut_cropped)
points(hrf_map, col="red")

# Find overlapping points
overlap_2 <- over(hrf_map, cut_data)

# Test overlapping points (comare to GIS results)
total2 <- 0
for (j in 1:length(overlap_2[,1])) {
  if(!is.na(overlap_2[j,1]))  {
    total2 <- total2 + 1
    }
}
print(total2)

# Create overlap_ids vector
overlap_ids <- c()
for (k in 1:length(overlap_2[,1])) {
  if (is.na(overlap_2[j,1])) {
    overlap_ids <- append(overlap_ids, overlap_2[k,1])
    }
}
print(overlap_ids)

# Enter data in matrix
for (i in 1:length(rownames(overlap))) {
  NEON_matrix[i,1] <- overlap[i,13]
 NEON_matrix[i,2] <- overlap[i,2]
 NEON_matrix[i,3:74] <- as.matrix(plan[i,2:ncol(plan)])
 NEON_matrix[i,75:146] <- as.matrix(volume[i,2:ncol(volume)])
}
