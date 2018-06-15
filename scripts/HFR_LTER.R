# Title:			  Writing HRF LTER Data in R
# Site:			    Harvard Forest
# Data Sources:	http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/da-search-all.html 
# Authors:	   	Kyra Hoerr  
# Date:			    14 June 2018

# libraries
library(raster)
library(rgdal)
library(ggplot2)
library (ggmap)
library (ggsn)
library(gridExtra)

# Set working directory
setwd("C:/Users/Kyra/Documents/Harvard/data/HF/LTER")

# Read hf014 (Mass_Landcover)
hf014_01 <- read.csv("~/Harvard/data/HF/LTER/Mass_Landcover/hf014-01-census-pre1900.csv")
hf014_02 <- read.csv("~/Harvard/data/HF/LTER/Mass_Landcover/hf014-02-census-post1900.csv")
acre1801 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Landcover/hf014-03-gis", "1801acreage")
acre1830 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Landcover/hf014-03-gis", "1830acreage")
acre1860 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Landcover/hf014-03-gis", "1860acreage")
acre1885 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Landcover/hf014-03-gis", "1885acreage")
acre1905 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Landcover/hf014-03-gis", "1905acreage")
forest1907 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Landcover/hf014-03-gis", "1907foresttype_acreage")
acre1920 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Landcover/hf014-03-gis", "1920sacreage")
forest1920 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Landcover/hf014-03-gis", "1920sforests_acreage")
town_dates <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Landcover/hf014-03-gis", "town_settlement_date")

# Read hf110 (GIS_HF)
hurricane <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/GIS_HF/hf110-01-gis/Harvard_Forest_Properties_GIS_Layers", "1938_hurricane_damage")
compart <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/GIS_HF/hf110-01-gis/Harvard_Forest_Properties_GIS_Layers", "compartments")
gates <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/GIS_HF/hf110-01-gis/Harvard_Forest_Properties_GIS_Layers", "hf_gates_smeyer_m")
linear <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/GIS_HF/hf110-01-gis/Harvard_Forest_Properties_GIS_Layers", "linear_features")
silvic <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/GIS_HF/hf110-01-gis/Harvard_Forest_Properties_GIS_Layers", "silviculture_treatments_08_21_2010")
soils <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/GIS_HF/hf110-01-gis/Harvard_Forest_Properties_GIS_Layers", "soils")
stands1908 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/GIS_HF/hf110-01-gis/Harvard_Forest_Properties_GIS_Layers", "stands_1908_9")
stands1912 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/GIS_HF/hf110-01-gis/Harvard_Forest_Properties_GIS_Layers", "stands_1912_13")
stands1919 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/GIS_HF/hf110-01-gis/Harvard_Forest_Properties_GIS_Layers", "stands_1919")
stands1923 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/GIS_HF/hf110-01-gis/Harvard_Forest_Properties_GIS_Layers", "stands_1923")
stands1929 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/GIS_HF/hf110-01-gis/Harvard_Forest_Properties_GIS_Layers", "stands_1929_30")
stands1937 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/GIS_HF/hf110-01-gis/Harvard_Forest_Properties_GIS_Layers", "stands_1937")
stands1947 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/GIS_HF/hf110-01-gis/Harvard_Forest_Properties_GIS_Layers", "stands_1947")
stands1956 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/GIS_HF/hf110-01-gis/Harvard_Forest_Properties_GIS_Layers", "stands_1956")
stands1986 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/GIS_HF/hf110-01-gis/Harvard_Forest_Properties_GIS_Layers", "stands_1986_93")
tracts <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/GIS_HF/hf110-01-gis/Harvard_Forest_Properties_GIS_Layers", "tracts")

# Read hf248 (Land Owner Decisions)
sum248 <- read.csv("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Land_Owner_Decisions/hf248-01-summary-data.csv")
# PRoblem with this one
# M234 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Land_Owner_Decisions/hf248-04-gis-data/Petersham", "M234")
# more data here; looks weird

# Read hf080 (Mass_Timber)
hf080_01 <- read.csv("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Timber/hf080-01-plan.csv")
hf080_02 <- read.csv("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Timber/hf080-01-plan.csv")
hf080_3 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/LTER/Mass_Timber/hf080-03-gis", "all_cutting_plans_v4")

# Read hf042 (Quabbin_Conservation)
# working on this data set
# hf042 <- readOGR("")

# Read NEON spatial data
Aquatic <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/NEON_Spatial/HF-NEON", "NEONAquaticSite")
Distributed <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/NEON_Spatial/HF-NEON", "NEONDistributedPlots")
Phenology <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/NEON_Spatial/HF-NEON", "NEONPhenologyTransect")
Ticks <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/NEON_Spatial/HF-NEON", "NEONTickPlots")
Tower_1 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/NEON_Spatial/HF-NEON", "NEONTowerBoardwalkSoilArray")
Tower_2 <- readOGR("C:/Users/Kyra/Documents/Harvard/data/HF/NEON_Spatial/HF-NEON", "NEONUpdatedTowerPlots")


