# Title:			  Massachusets Timber Harvesting Study
# Site:			    Harvard Forest
# Data Sources:	http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/da-search-all.html 
# Authors:	   	Kyra Hoerr  
# Date:			    20 June 2018

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

# Read overlap data
overlap <- read.csv("C:/Users/Kyra/Documents/Harvard/data/HF/NEON_Spatial/NEON_points.csv")
overlap_ids <- overlap$FILE_ID

# Create a matrix
NEON_matrix <- matrix(
  c(rep("N/A")), 
  nrow=57,
  ncol=146
)

# Create a for loop
col1 <- as.character(overlap_ids)

# Ignore this for now
for (k in 1:length(overlap_ids)) {
  col1 <- as.character(overlap_ids)
  NEON_matrix[k,1] <- col1[k]
}


for (i in 1:length(rownames(overlap))) {
  NEON_matrix[i,1] <- overlap[i,13]
 NEON_matrix[i,2] <- overlap[i,2]
 NEON_matrix[i,3:74] <- as.matrix(plan[i,2:ncol(plan)])
 NEON_matrix[i,75:146] <- as.matrix(volume[i,2:ncol(volume)])
}

# Create Overlap data
cut_x <- coordinates(cut_data)[,1]
cut_y <- coordinates(cut_data)[,2]

# Method 1 to find overlap
overlap_1 <- point.in.polygon(hrf_map$longitd, hrf_map$latitud, cut_x, cut_y, mode.checked=FALSE)
# Method 2 to find overlap
overlap_2 <- over(hrf_map, cut_data)

# Test of method 1
total1 <- 0
for (k in 1:length(overlap_1)) {
   if(overlap_1[k] > 0){
     total1 <- total1 + 1
   }
}
print(total1)

# Test of method 2
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
