## Title: Calculating the distances between disturbances and NEON sites
## Date: 27 June 2018
## Author: Cameo Chilcutt
## Sources: 

# Start with a clear workspace and set working directory to google drive -> NEON_LTER_2018 folder -> data -> raw_data
rm(list=ls())
setwd("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data")

#install these packages and load the libraries
library(rgdal)
library(rgeos)
library(raster)
# Import terrestrial data shape files from Google Drive
terrestrial <- readOGR(".\\neon\\spatial_data\\All_NEON_TOS_Plots_V4\\All_NEON_TOS_Centroid_V4.shp")
plot(terrestrial, legend=F, main="Terrestrial Data")

#To subset the data specifically to your site. You must use the exact name as it is written in the data. To look this up, use the following function to list all of the names of the field sites.
#Toolik
unique(terrestrial$siteNam)
arc<-terrestrial[terrestrial$siteNam=="Toolik Lake",]
summary(arc)
plot(arc)
#Harvard Forest
hrf<-terrestrial[terrestrial$siteNam=="Harvard Forest",]
summary(hrf)
plot(hrf)
#Konza
knz<-terrestrial[terrestrial$siteNam=="Konza Prairie Biological Station",]
summary(knz)
plot(knz)
#Currently, it is in latitude and longitude, but in order to measure distance in meters we need to reproject the data into UTMs. You should look up the appropriate zone for your site. For the Toolik Lake Field Station we needed UTM Zone 6. Harvard Forest should be UTM 18 or 19. Konza is 14.
#Toolik
arc_UTM<-spTransform(arc, CRS("+proj=utm +zone=6 ellps=WGS84"))
head(arc_UTM)
#Harvard Forest
hrf_UTM<-spTransform(hrf, CRS("+proj=utm +zone=19 ellps=WGS84"))
head(hrf_UTM)
#Konza
knz_UTM<-spTransform(knz, CRS("+proj=utm +zone=14 ellps=WGS84"))
head(knz_UTM)
###############################################################################
#Next we need to import and reproject all of the relevant disturbance shape files. After importing, always plot to be sure you have the appropriate shapes. Also, check if the files are in UTMs. If not, reproject the same way we did for the NEON terrestrial data. We need UTMs to measure distance in meters.
#ARC data
#Camp Buildings 2013 Data
cb13 <- readOGR(".\\arc\\gis_data\\camp_buildings_2013$data", "Camp_Buildings_2013")
plot(cb13, col=rainbow(10), alpha=1, legend=F, main="Camp Buildings 2013")
summary(cb13)

#Roads & Trails Data
rt<- readOGR(".\\arc\\gis_data\\roads_trails$data", "transport_100423")
plot(rt, legend=F, main="Road_Trails")
summary(rt)
rt<-spTransform(rt, CRS("+proj=utm +zone=6 ellps=WGS84"))
summary(rt)

#Pipeline Data
pipeline<- readOGR(".\\arc\\gis_data\\taps$data", "pipeline_proj")
plot(pipeline, legend=F, main="Pipeline")
summary(pipeline)
pipeline<-spTransform(pipeline, CRS("+proj=utm +zone=6 ellps=WGS84"))
summary(pipeline)

#Anaktuvuk Burn Perimeter Data
anaktuvuk<-readOGR(".\\arc\\gis_data\\anaktuvuk_burn_perim$data", "progression_perimeters_0822-0930")
plot(anaktuvuk, col=grey(1:100/100), main="Anaktuvuk Burn Perimeter")
summary(anaktuvuk)
anaktuvuk<-spTransform(anaktuvuk, CRS("+proj=utm +zone=6 ellps=WGS84"))
summary(anaktuvuk)

#Thermokarst Data
thermokarst<- readOGR(".\\arc\\gis_data\\watersheds_research$data\\Watersheds_Research", "Thermokarst_Watershed")
plot(thermokarst, col=blues9, main="Thermokarst")
summary(thermokarst)

#Toolik Inlet Water Source
toolwater<- readOGR(".\\arc\\gis_data\\watersheds_research$data\\Watersheds_Research", "Toolik_inlet_Watershed")
plot(toolwater, col="blue", main="Toolik Inlet Water")
summary(toolwater)
################################################################################
# Harvard Forest - HRF data

#Massachusetts Timber Harvesting Data
cutting <- readOGR(".\\hrf\\LTER\\Mass_Timber\\hf080-03-gis", "all_cutting_plans_v4")
cut_data <- spTransform(cutting, CRS("+proj=utm +zone=19 ellps=WGS84"))
plot(cut_data)

## 1830 map data
#Buildings
building_data <- readOGR(".\\hrf\\LTER\\Map_1830", "1830buildings")
buildings <- spTransform(building_data, CRS("+proj=utm +zone=19 ellps=WGS84"))
plot(buildings)

#Roads
roads_data <- readOGR(".\\hrf\\LTER\\Map_1830", "1830roads")
hrf_roads <- spTransform(roads_data, CRS("+proj=utm +zone=19 ellps=WGS84"))
plot(hrf_roads)

################################################################################
#KNZ data
#Roads

#Burn History
burn1972 <- readOGR(".\\knz\\LTER\\GIS05", "GIS050")
map_burn72 <- spTransform(burn1972, CRS("+proj=utm +zone=14 ellps=WGS84"))
head(map_burn72)

burn1973 <- readOGR(".\\knz\\LTER\\GIS05", "GIS051")
map_burn73 <- spTransform(burn1973, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1974 <- readOGR(".\\knz\\LTER\\GIS05", "GIS052")
map_burn74 <- spTransform(burn1974, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1975 <- readOGR(".\\knz\\LTER\\GIS05", "GIS053")
map_burn75 <- spTransform(burn1975, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1976 <- readOGR(".\\knz\\LTER\\GIS05", "GIS054")
map_burn76 <- spTransform(burn1976, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1977 <- readOGR(".\\knz\\LTER\\GIS05", "GIS055")
map_burn77 <- spTransform(burn1977, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1978 <- readOGR(".\\knz\\LTER\\GIS05", "GIS056")
map_burn78 <- spTransform(burn1978, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1979 <- readOGR(".\\knz\\LTER\\GIS05", "GIS057")
map_burn79 <- spTransform(burn1979, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1980 <- readOGR(".\\knz\\LTER\\GIS05", "GIS058")
map_burn80 <- spTransform(burn1980, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1981 <- readOGR(".\\knz\\LTER\\GIS05", "GIS059")
map_burn81 <- spTransform(burn1981, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1982 <- readOGR(".\\knz\\LTER\\GIS05", "GIS060")
map_burn82 <- spTransform(burn1982, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1983 <- readOGR(".\\knz\\LTER\\GIS05", "GIS061")
map_burn83 <- spTransform(burn1983, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1984 <- readOGR(".\\knz\\LTER\\GIS05", "GIS062")
map_burn84 <- spTransform(burn1984, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1985 <- readOGR(".\\knz\\LTER\\GIS05", "GIS063")
map_burn85 <- spTransform(burn1985, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1986 <- readOGR(".\\knz\\LTER\\GIS05", "GIS064")
map_burn86 <- spTransform(burn1986, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1987 <- readOGR(".\\knz\\LTER\\GIS05", "GIS065")
map_burn87 <- spTransform(burn1987, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1988 <- readOGR(".\\knz\\LTER\\GIS05", "GIS066")
map_burn88 <- spTransform(burn1988, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1989 <- readOGR(".\\knz\\LTER\\GIS05", "GIS067")
map_burn89 <- spTransform(burn1989, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1990 <- readOGR(".\\knz\\LTER\\GIS05", "GIS068")
map_burn90 <- spTransform(burn1990, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1991 <- readOGR(".\\knz\\LTER\\GIS05", "GIS069")
map_burn91 <- spTransform(burn1991, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1992 <- readOGR(".\\knz\\LTER\\GIS05", "GIS070")
map_burn92 <- spTransform(burn1992, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1993 <- readOGR(".\\knz\\LTER\\GIS05", "GIS071")
map_burn93 <- spTransform(burn1993, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1994 <- readOGR(".\\knz\\LTER\\GIS05", "GIS072")
map_burn94 <- spTransform(burn1994, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1995 <- readOGR(".\\knz\\LTER\\GIS05", "GIS073")
map_burn95 <- spTransform(burn1995, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1996 <- readOGR(".\\knz\\LTER\\GIS05", "GIS074")
map_burn96 <- spTransform(burn1996, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1997 <- readOGR(".\\knz\\LTER\\GIS05", "GIS075")
map_burn97 <- spTransform(burn1997, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1998 <- readOGR(".\\knz\\LTER\\GIS05", "GIS076")
map_burn98 <- spTransform(burn1998, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn1999 <- readOGR(".\\knz\\LTER\\GIS05", "GIS077")
map_burn99 <- spTransform(burn1999, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn2000 <- readOGR(".\\knz\\LTER\\GIS05", "GIS078")
map_burn00 <- spTransform(burn2000, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn2001 <- readOGR(".\\knz\\LTER\\GIS05", "GIS079")
map_burn01 <- spTransform(burn2001, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn2002 <- readOGR(".\\knz\\LTER\\GIS05", "GIS080")
map_burn02 <- spTransform(burn2002, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn2003 <- readOGR(".\\knz\\LTER\\GIS05", "GIS081")
map_burn03 <- spTransform(burn2003, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn2004 <- readOGR(".\\knz\\LTER\\GIS05", "GIS082")
map_burn04 <- spTransform(burn2004, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn2005 <- readOGR(".\\knz\\LTER\\GIS05", "GIS083")
map_burn05 <- spTransform(burn2005, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn2006 <- readOGR(".\\knz\\LTER\\GIS05", "GIS084")
map_burn06 <- spTransform(burn2006, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn2007 <- readOGR(".\\knz\\LTER\\GIS05", "GIS085")
map_burn07 <- spTransform(burn2007, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn2008 <- readOGR(".\\knz\\LTER\\GIS05", "GIS086")
map_burn08 <- spTransform(burn2008, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn2009 <- readOGR(".\\knz\\LTER\\GIS05", "GIS087")
map_burn09 <- spTransform(burn2009, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn2010 <- readOGR(".\\knz\\LTER\\GIS05", "GIS088")
map_burn10 <- spTransform(burn2010, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn2011 <- readOGR(".\\knz\\LTER\\GIS05", "GIS089")
map_burn11 <- spTransform(burn2011, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn2012 <- readOGR(".\\knz\\LTER\\GIS05", "GIS090")
map_burn12 <- spTransform(burn2012, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn2013 <- readOGR(".\\knz\\LTER\\GIS05", "GIS091")
map_burn13 <- spTransform(burn2013, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn2014 <- readOGR(".\\knz\\LTER\\GIS05", "GIS092")
map_burn14 <- spTransform(burn2014, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn2015 <- readOGR(".\\knz\\LTER\\GIS05", "GIS093")
map_burn15 <- spTransform(burn2015, CRS("+proj=utm +zone=14 ellps=WGS84"))

burn2016 <- readOGR(".\\knz\\LTER\\GIS05", "GIS094")
map_burn16 <- spTransform(burn2016, CRS("+proj=utm +zone=14 ellps=WGS84"))

#Combine all burn data to measure distance.
burn<-bind(map_burn72, map_burn73, map_burn74, map_burn75, map_burn76, map_burn77, map_burn78, map_burn79, map_burn80, map_burn81, map_burn82, map_burn83, map_burn84, map_burn85, map_burn86, map_burn87, map_burn88, map_burn89, map_burn90, map_burn91, map_burn92, map_burn93, map_burn94, map_burn95, map_burn96, map_burn97, map_burn98, map_burn99, map_burn00, map_burn01, map_burn02, map_burn03, map_burn04, map_burn05, map_burn06, map_burn07, map_burn08, map_burn09, map_burn10, map_burn11, map_burn12, map_burn13, map_burn14, map_burn15, map_burn16)
plot(burn)

#Roads
knz_roads <- readOGR(".\\knz\\LTER\\GIS10", "gis100")
map_roads <- spTransform(knz_roads, CRS("+proj=utm +zone=14 ellps=WGS84"))

#Nature trails 
knz_trails <- readOGR(".\\knz\\LTER\\GIS11", "GIS110")
map_trails <- spTransform(knz_trails, CRS("+proj=utm +zone=14 ellps=WGS84"))

#Permanent structures
structure_data <- readOGR(".\\knz\\LTER\\GIS19", "GIS190")
structures <- spTransform(structure_data, CRS("+proj=utm +zone=14 ellps=WGS84"))

################################################################################
#Now we are going to measure distance between NEON data collection points and the Anaktuvuk fire.
arc_burn_dist<- apply(gDistance(arc_UTM, anaktuvuk,byid=TRUE),2,min)
arc_burn_dist<- data.frame(arc_burn_dist)
head(arc_burn_dist)

#Right now the column name is the same as the name of the data frame. This can be confusing for R when we want to merge these data with the arc_UTM data. So, we need to rename it using the code below.
names(arc_burn_dist)[names(arc_burn_dist)=="arc_burn_dist"] <- "burn_dist"
head(arc_burn_dist)

#Now we are going to merge the data frames to make one complete dataset
arc1<- merge(arc_UTM, arc_burn_dist, by=0, all=TRUE)

#Always check both data sets to make sure the distance measurements are the same as the original distance calculation file.
head(arc_burn_dist)
head(arc1)

#Now let's calculate the distance for all of the disturbances in relation to the NEON collection sites.
#Camp Buildings
arc_bldgs_dist<- apply(gDistance(arc_UTM, cb13,byid=TRUE),2,min)
arc_bldgs_dist<- data.frame(arc_bldgs_dist)
head(arc_bldgs_dist)

#Rename column
names(arc_bldgs_dist)[names(arc_bldgs_dist)=="arc_bldgs_dist"] <- "bldgs_dist"
head(arc_bldgs_dist)

#Merge data frames
arc2<- merge(arc_UTM, arc_bldgs_dist, by=0, all=TRUE)

#Check
head(arc_bldgs_dist)
head(arc2)

#Merge both arc1 and arc2 dataframes to include all of the data for both burn_dist and bldgs_dist.
combined.dist<-merge(arc1, arc2, by="plotID", all=T)
write.csv(combined.dist,file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\arc_dist.csv", row.names=FALSE)

#Pipeline
arc_pipe_dist<- apply(gDistance(arc_UTM, pipeline,byid=TRUE),2,min)
arc_pipe_dist<- data.frame(arc_pipe_dist)
head(arc_pipe_dist)

#Rename column
names(arc_pipe_dist)[names(arc_pipe_dist)=="arc_pipe_dist"] <- "pipeline_dist"
head(arc_pipe_dist)

#Merge data 
arc3<- merge(arc_UTM, arc_pipe_dist, by=0, all=TRUE)
head(arc_pipe_dist)
head(arc3)

#Merge all data frames together
combined.dist1<-merge(combined.dist, arc3, by="plotID", all=T)
write.csv(combined.dist1,file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\arc_dist.csv", row.names=FALSE)

#Thermokarst
arc_thermo_dist<- apply(gDistance(arc_UTM, thermokarst,byid=TRUE),2,min)
arc_thermo_dist<- data.frame(arc_thermo_dist)

#Rename column
names(arc_thermo_dist)[names(arc_thermo_dist)=="arc_thermo_dist"] <- "thermokarst_dist"
head(arc_thermo_dist)

#Merge data 
arc4<- merge(arc_UTM, arc_thermo_dist, by=0, all=TRUE)

#Check
head(arc_thermo_dist)
head(arc4)

#Merge all data frames together for a complete data set
combined.dist2<-merge(combined.dist1, arc4, by="plotID", all=T)
write.csv(combined.dist2,file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\arc_dist.csv", row.names=FALSE)

#Nearest Water Source
arc_water_dist<- apply(gDistance(arc_UTM, toolwater,byid=TRUE),2,min)
arc_water_dist<- data.frame(arc_water_dist)

#Rename column
names(arc_water_dist)[names(arc_water_dist)=="arc_water_dist"] <- "water_dist"
head(arc_water_dist)

#Merge data frames to make one complete dataset
arc5<- merge(arc_UTM, arc_water_dist, by=0, all=TRUE)

#Check
head(arc_water_dist)
head(arc5)

#Merge all data frames together for a complete data set
combined.dist3<-merge(combined.dist2, arc5, by="plotID", all=T)

#Roads & Trails
arc_rt_dist<- apply(gDistance(arc_UTM, rt,byid=TRUE),2,min)
arc_rt_dist<- data.frame(arc_rt_dist)
head(arc_rt_dist)

#Rename column
names(arc_rt_dist)[names(arc_rt_dist)=="arc_rt_dist"] <- "roads_dist"
head(arc_rt_dist)

#Merge data 
arc6<- merge(arc_UTM, arc_rt_dist, by=0, all=TRUE)
head(arc_rt_dist)
head(arc6)

#Merge all data frames together
combined.dist4<-merge(combined.dist3, arc6, by="plotID", all=T)
write.csv(combined.dist4,file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\arc_dist.csv", row.names=FALSE)
################################################################################
################################################################################
#Let's measure the distance from disturbance for Harvard Forest.
#subset harvard forest data
names(hrf_UTM)
variables <- c("elevatn", "siteID", "plotID", "nlcdCls", "slpGrdn", "slpAspc")
hrf_UTM <- hrf_UTM[variables]
head(hrf_UTM)
str(hrf_UTM)

cutting_dist<- apply(gDistance(hrf_UTM, cut_data,byid=TRUE),2,min)
cutting_dist<- data.frame(cutting_dist)
head(cutting_dist)

#Rename column
names(cutting_dist)[names(cutting_dist)=="cutting_dist"] <- "cut_dist"
head(cutting_dist)

#Now we are going to merge the data frames to make one complete dataset
hrf1<- merge(hrf_UTM, cutting_dist, by=0, all=TRUE)

#Always check both data sets to make sure the distance measurements are the same as the original distance calculation file.
head(cutting_dist)
head(hrf1)

#Buildings
hrf_bldgs_dist<- apply(gDistance(hrf_UTM, buildings,byid=TRUE),2,min)
hrf_bldgs_dist<- data.frame(hrf_bldgs_dist)
head(hrf_bldgs_dist)

#Rename column
names(hrf_bldgs_dist)[names(hrf_bldgs_dist)=="hrf_bldgs_dist"] <- "bldgs_dist"
head(hrf_bldgs_dist)

#Merge data frames
hrf2<- merge(hrf_UTM, hrf_bldgs_dist, by=0, all=TRUE)

#Check
head(hrf_bldgs_dist)
head(hrf2)

#Merge both hrf1 and hrf2 dataframes to include all of the data for both burn_dist and bldgs_dist.
hrf.combined.dist<-merge(hrf1, hrf2, by="plotID", all=T)
write.csv(hrf.combined.dist,file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\hrf_dist.csv", row.names=FALSE)
head(hrf.combined.dist)

#Roads
hrf_roads_dist<- apply(gDistance(hrf_UTM, hrf_roads,byid=TRUE),2,min)
hrf_roads_dist<- data.frame(hrf_roads_dist)
head(hrf_roads_dist)

#Rename column
names(hrf_roads_dist)[names(hrf_roads_dist)=="hrf_roads_dist"] <- "roads_dist"
head(hrf_roads_dist)

#Merge data frames
hrf3<- merge(hrf_UTM, hrf_roads_dist, by=0, all=TRUE)

#Check
head(hrf_roads_dist)
head(hrf3)

#Merge
hrf.combined.dist1<-merge(hrf3, hrf.combined.dist, by="plotID", all=T)
write.csv(hrf.combined.dist1,file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\hrf_dist.csv", row.names=FALSE)
head(hrf.combined.dist1)

################################################################################
################################################################################
#Let's measure the distance from disturbance for Konza.
#subset Konza data
names(knz_UTM)
knz_vars <- c("elevatn", "siteID", "plotID", "nlcdCls", "slpGrdn", "slpAspc")
knz_UTM <- knz_UTM[knz_vars]
head(knz_UTM)
str(knz_UTM)

knz_burn_dist<- apply(gDistance(knz_UTM, burn,byid=TRUE),2,min)
knz_burn_dist<- data.frame(knz_burn_dist)
head(knz_burn_dist)

#Rename column
names(knz_burn_dist)[names(knz_burn_dist)=="knz_burn_dist"] <- "burn_dist"
head(knz_burn_dist)

#Now we are going to merge the data frames to make one complete dataset
knz1<- merge(knz_UTM, knz_burn_dist, by=0, all=TRUE)

#Always check both data sets to make sure the distance measurements are the same as the original distance calculation file.
head(knz_burn_dist)
head(knz1)

#Roads
knz_roads_dist<- apply(gDistance(knz_UTM, map_roads,byid=TRUE),2,min)
knz_roads_dist<- data.frame(knz_roads_dist)
head(knz_roads_dist)

#Rename column
names(knz_roads_dist)[names(knz_roads_dist)=="knz_roads_dist"] <- "roads_dist"
head(knz_roads_dist)

#Merge data frames
knz2<- merge(knz_UTM, knz_roads_dist, by=0, all=TRUE)

#Check
head(knz_roads_dist)
head(knz2)

#Merge both hrf1 and hrf2 dataframes to include all of the data for both burn_dist and bldgs_dist.
knz.combined.dist<-merge(knz1, knz2, by="plotID", all=T)
write.csv(knz.combined.dist,file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\knz_dist.csv", row.names=FALSE)
head(knz.combined.dist)

#Trails
knz_trails_dist<- apply(gDistance(knz_UTM, map_trails,byid=TRUE),2,min)
knz_trails_dist<- data.frame(knz_trails_dist)
head(knz_trails_dist)

#Rename column
names(knz_trails_dist)[names(knz_trails_dist)=="knz_trails_dist"] <- "trails_dist"
head(knz_trails_dist)

#Merge data frames
knz3<- merge(knz_UTM, knz_trails_dist, by=0, all=TRUE)

#Check
head(knz_trails_dist)
head(knz3)

#Merge
knz.combined.dist1<-merge(knz3, knz.combined.dist, by="plotID", all=T)
write.csv(knz.combined.dist1,file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\knz_dist.csv", row.names=FALSE)
head(knz.combined.dist1)

#Buildings
knz_bldgs_dist<- apply(gDistance(knz_UTM, structures,byid=TRUE),2,min)
knz_bldgs_dist<- data.frame(knz_bldgs_dist)
head(knz_bldgs_dist)

#Rename column
names(knz_bldgs_dist)[names(knz_bldgs_dist)=="knz_bldgs_dist"] <- "bldgs_dist"
head(knz_bldgs_dist)

#Merge data frames
knz4<- merge(knz_UTM, knz_bldgs_dist, by=0, all=TRUE)

#Check
head(knz_bldgs_dist)
head(knz4)

#Merge
knz.combined.dist2<-merge(knz4, knz.combined.dist1, by="plotID", all=T)
write.csv(knz.combined.dist2,file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\knz_dist.csv", row.names=FALSE)
head(knz.combined.dist2)

################################################################################
#Now we need to clean up the data frame and select only the columns we need.
#Select important columns
#Toolik
keep=c("plotID", "siteID", "siteNam", "burn_dist",	"bldgs_dist",	"pipeline_dist",	"thermokarst_dist", "water_dist", "roads_dist" )

# Check that all names in keep are in combined.dist3. You want this result to be zero. If it is anything other than zero, you need to figure out what the appropriate column names are.
keep[!keep %in% names(combined.dist4)]

combined.distances <- combined.dist4
combined.distances@data <- combined.distances@data[,keep] 
write.csv(combined.distances, file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\arc_dist.csv", row.names=F)

#Make these data a data frame.
combined.distances<-data.frame(combined.distances@data)
class(combined.distances)
head(combined.distances)

################################################################################
#Harvard Forest
#Now we need to clean up the data frame and select only the columns we need.
names(hrf.combined.dist1)
#Select important columns
keep=c("plotID", "siteID", "cut_dist",	"bldgs_dist", "roads_dist","elevatn", "slpGrdn", "slpAspc", "nlcdCls")

# Check that all names in keep are in combined.dist3. You want this result to be zero. If it is anything other than zero, you need to figure out what the appropriate column names are.
keep[!keep %in% names(hrf.combined.dist1)]

hrf.combined.distances <- hrf.combined.dist1
hrf.combined.distances@data <- hrf.combined.distances@data[,keep] 
write.csv(hrf.combined.distances, file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\hrf_dist.csv", row.names=F)

#Make these data a data frame.
hrf.combined.distances<-data.frame(hrf.combined.distances@data)
class(hrf.combined.distances)
head(hrf.combined.distances)
write.csv(hrf.combined.distances, file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\hrf_dist.csv", row.names=F)
################################################################################
#Konza
#Now we need to clean up the data frame and select only the columns we need.
names(knz.combined.dist2)
#Select important columns
keep=c("plotID", "siteID", "burn_dist",	"bldgs_dist", "roads_dist", "trails_dist", "elevatn", "slpGrdn", "slpAspc", "nlcdCls")

# Check that all names in keep are in combined.dist3. You want this result to be zero. If it is anything other than zero, you need to figure out what the appropriate column names are.
keep[!keep %in% names(knz.combined.dist2)]

knz.combined.distances <- knz.combined.dist2
knz.combined.distances@data <- knz.combined.distances@data[,keep] 
write.csv(knz.combined.distances, file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\knz_dist.csv", row.names=F)

#Make these data a data frame.
knz.combined.distances<-data.frame(knz.combined.distances@data)
class(knz.combined.distances)
head(knz.combined.distances)
write.csv(knz.combined.distances, file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\knz_dist.csv", row.names=F)

################################################################################
#Now we need to put our distance data into long format.
#Long Format of combined.distances to characterize dist_type
library(dplyr)
library(reshape2)
combined.distances<-read.csv("G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\arc_dist.csv")
head(combined.distances)

#We have some unneccessary columns that came with the spatial point data frame. So, we will exclude those to further clean up our data frame.
combined.distances$coords.x1<-NULL
combined.distances$coords.x2<-NULL
combined.distances$optional<-NULL
head(combined.distances)

#Create the long format
combined.distances<-melt(combined.distances, id=c("plotID", "siteID", "siteNam"))
head(combined.distances)

#Rename columns to specify disturbance type and distance in meters.
names(combined.distances)[names(combined.distances)=="variable"]<-"dist_type"
names(combined.distances)[names(combined.distances)=="value"]<-"distance_m"
head(combined.distances)

#Look at what is described in the distance type to be sure all of your disturbance distances are included.
sort(unique(combined.distances$dist_type))

#To better inform the users of these data, we should make the disturbance names more informative. So rename them to fit the exact disturbance type that you are trying to describe.
combined.distances$dist_type<- as.character(combined.distances$dist_type)
combined.distances$dist_type[combined.distances$dist_type=="burn_dist"]<-"burn"
combined.distances$dist_type[combined.distances$dist_type=="bldgs_dist"]<-"buildings"
combined.distances$dist_type[combined.distances$dist_type=="pipeline_dist"]<-"pipeline"
combined.distances$dist_type[combined.distances$dist_type=="thermokarst_dist"]<-"thermokarst"
combined.distances$dist_type[combined.distances$dist_type=="water_dist"]<-"water source"
combined.distances$dist_type[combined.distances$dist_type=="roads_dist"]<-"roads"
sort(unique(combined.distances$dist_type))

write.csv(combined.distances,file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\neon_plotid_dist2disturbance_arc.csv", row.names=FALSE)
################################################################################
#Harvard Forest
#Now we need to put our distance data into long format.
#Long Format of hrf.combined.distances to characterize dist_type
hrf.combined.distances<-read.csv("G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\hrf_dist.csv")
head(hrf.combined.distances)

#Create the long format
hrf.combined.distances<-melt(hrf.combined.distances, id=c("plotID", "siteID"))
head(hrf.combined.distances)

#Rename columns to specify disturbance type and distance in meters.
names(hrf.combined.distances)[names(hrf.combined.distances)=="variable"]<-"dist_type"
names(hrf.combined.distances)[names(hrf.combined.distances)=="value"]<-"distance_m"
head(hrf.combined.distances)

#Look at what is described in the distance type to be sure all of your disturbance distances are included.
sort(unique(hrf.combined.distances$dist_type))

#To better inform the users of these data, we should make the disturbance names more informative. So rename them to fit the exact disturbance type that you are trying to describe.
hrf.combined.distances$dist_type<- as.character(hrf.combined.distances$dist_type)
hrf.combined.distances$dist_type[hrf.combined.distances$dist_type=="bldgs_dist"]<-"buildings"
hrf.combined.distances$dist_type[hrf.combined.distances$dist_type=="roads_dist"]<-"roads"
hrf.combined.distances$dist_type[hrf.combined.distances$dist_type=="cut_dist"]<-"cuttings"
sort(unique(hrf.combined.distances$dist_type))

write.csv(hrf.combined.distances,file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\neon_plotid_dist2disturbance_hrf.csv", row.names=FALSE)
################################################################################
#Konza
#Now we need to put our distance data into long format.
#Long Format of knz.combined.distances to characterize dist_type
knz.combined.distances<-read.csv("G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\knz_dist.csv")
head(knz.combined.distances)

#Create the long format
knz.combined.distances<-melt(knz.combined.distances, id=c("plotID", "siteID", "elevatn", "slpAspc", "slpGrdn", "nlcdCls"))
head(knz.combined.distances)

#Rename columns to specify disturbance type and distance in meters.
names(knz.combined.distances)[names(knz.combined.distances)=="variable"]<-"dist_type"
names(knz.combined.distances)[names(knz.combined.distances)=="value"]<-"distance_m"
head(knz.combined.distances)

#Look at what is described in the distance type to be sure all of your disturbance distances are included.
sort(unique(knz.combined.distances$dist_type))

#To better inform the users of these data, we should make the disturbance names more informative. So rename them to fit the exact disturbance type that you are trying to describe.
knz.combined.distances$dist_type<- as.character(knz.combined.distances$dist_type)
knz.combined.distances$dist_type[knz.combined.distances$dist_type=="bldgs_dist"]<-"buildings"
knz.combined.distances$dist_type[knz.combined.distances$dist_type=="roads_dist"]<-"roads"
knz.combined.distances$dist_type[knz.combined.distances$dist_type=="burn_dist"]<-"burn"
knz.combined.distances$dist_type[knz.combined.distances$dist_type=="trails_dist"]<-"trails"
sort(unique(knz.combined.distances$dist_type))
head(knz.combined.distances)

#Spatial Data is still present, so we will exclude these variables to make our data frame more clear.
knz.combined.distances$coords.x1<-NULL
knz.combined.distances$coords.x2<-NULL	
knz.combined.distances$coords.x1.1<-NULL
knz.combined.distances$coords.x2.1<-NULL	
knz.combined.distances$coords.x1.2<-NULL	
knz.combined.distances$coords.x2.2<-NULL	
knz.combined.distances$coords.x1.3<-NULL	
knz.combined.distances$coords.x2.3<-NULL	
knz.combined.distances$optional<-NULL

head(knz.combined.distances)
class(knz.combined.distances)
write.csv(knz.combined.distances,file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\neon_plotid_dist2disturbance_knz.csv", row.names=FALSE)
################################################################################
#To save the workspace with all of our data and final products, use the following code:
save.image("NEON_LTER.RData")
##################################################################################################################################################################Title: Calculating the surface area of a shapefile 
#Date: 28 June 2018

#Anaktuvuk Burn Area Calculation
anak_area<-gArea(anaktuvuk)
anak_area
anak_area<- data.frame(anak_area)
anak_area

#Rename column
names(anak_area)[names(anak_area)=="anak_area"] <- "burn_area"
head(anak_area)

#Merge data 
area_add<- merge(combined.distances, anak_area, by=0, all=TRUE)
head(anak_area)
head(area_add)

#Change all NA values to 0
area_add[is.na(area_add)] <- 0
head(area_add)

#Camp Buildings Area Calculation
cb13_area<-gArea(cb13)
cb13_area
cb13_area<- data.frame(cb13_area)
cb13_area

#Rename column
names(cb13_area)[names(cb13_area)=="cb13_area"] <- "buildings_area"
head(cb13_area)

#Merge data 
area_add1<- merge(combined.distances, cb13_area, by=0, all=TRUE)
head(cb13_area)
head(area_add1)

#Change all NA values to 0
area_add1[is.na(area_add1)] <- 0
head(area_add1)

#Merge data frames
final<-merge(area_add1, area_add, by=c("Row.names","plotID", "siteID", "dist_type", "distance_m", "siteNam"), all=T)
head(final)

#Thermokarst Area Calculation
thermo_area<-gArea(thermokarst)
thermo_area
thermo_area<- data.frame(thermo_area)
thermo_area

#Rename column
names(thermo_area)[names(thermo_area)=="thermo_area"] <- "thermokarst_area"
head(thermo_area)

#Merge data 
area_add2<- merge(combined.distances, thermo_area, by=0, all=TRUE)
head(thermo_area)
head(area_add2)

#Change all NA values to 0
area_add2[is.na(area_add2)] <- 0
head(area_add2)

#Merge data frames
final2<-merge(final, area_add2, by=c("Row.names","plotID", "siteID", "dist_type", "distance_m", "siteNam"), all=T)
names(final2)

#Water Source Area Calculation
water_area<-gArea(toolwater)
water_area
water_area<- data.frame(water_area)
water_area

#Rename column
names(water_area)[names(water_area)=="water_area"] <- "stream_area"
head(water_area)

#Merge data 
area_add3<- merge(combined.distances, water_area, by=0, all=TRUE)
head(water_area)
head(area_add3)

#Change all NA values to 0
area_add3[is.na(area_add3)] <- 0
head(area_add3)

#Merge data frames
final3<-merge(final2, area_add3, by=c("Row.names","plotID", "siteID", "dist_type", "distance_m", "siteNam"), all=T)
names(final3)

write.csv(final3,file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\final_plot_level.csv", row.names=FALSE)
###############################################################################################################################################################
#Now we need to clean up the data frame and select only the columns we need.
#Select important columns
keep=c("plotID", "siteID", "dist_type", "distance_m", "buildings_area", "burn_area", "thermokarst_area", "stream_area")

# Check that all names in keep are in combined.dist3. You want this result to be zero. If it is anything other than zero, you need to figure out what the appropriate column names are.
keep[!keep %in% names(final3)]

plot_level_dataframe <- final3
class(plot_level_dataframe)
plot_level_dataframe <- plot_level_dataframe[,keep] 
write.csv(plot_level_dataframe, file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\final_plot_level.csv", row.names=F)
###############################################################################
#Next refer to script neon_within_site_prep.R

