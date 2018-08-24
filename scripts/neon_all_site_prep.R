# Title: Cross Site Analysis Prep
# Author: Cameo Chilcutt
# Date: 12 July 2018

# Start with a clear workspace and set working directory.
rm(list=ls())
google_drive <- 'G:\\My Drive\\NEON_LTER_2018\\data'
setwd(file.path(google_drive, 'final_data\\neon')) # GD location
# file path location for LTER disturbance and richness data by NEON plot
data_path <- file.path(google_drive, 'final_data\\neon')
fig_path <- file.path(google_drive, 'output')


#install these packages and load the libraries
library(rgdal)
library(rgeos)
library(raster)
library(devtools)
library(reshape2)
library(dplyr) 
library(sp) 
library(ggplot2)
library(ggmap) 

#Read in these csv files to work with distance calculations
#knz_dist<-read.csv(file.path(data_path, "./disturbance/knz_dist.csv"),stringsAsFactors = FALSE)
hrf_dist<-read.csv(file.path(data_path, "./disturbance/hrf_dist.csv"),stringsAsFactors = FALSE)
tool_dist<-read.csv(file.path(data_path, "./disturbance/arc_dist.csv"),stringsAsFactors = FALSE)
osbs_dist<-read.csv(file.path(data_path, "./disturbance/osbs_dist.csv"),stringsAsFactors = FALSE)
#names(knz_dist)
names(hrf_dist)
names(tool_dist)
names(osbs_dist)

#exclude unnecessary columns
tool_dist$coords.x1<-NULL	
tool_dist$coords.x2<-NULL	
tool_dist$optional<-NULL	
osbs_dist$coords.x1<-NULL
osbs_dist$coords.x2<-NULL
osbs_dist$optional<-NULL
head(tool_dist)
head(osbs_dist)

#organismal data
mammals <- read.csv(file.path(data_path,"./richness/mammal_richness_cumulative_plot.csv"), stringsAsFactors = FALSE)
plants<- read.csv(file.path(data_path,"./richness/plant_richness_cumulative_plot.csv"))
birds<- read.csv(file.path(data_path, "./richness/bird_richness_cumulative_plot.csv"))
#beetles<- read.csv(file.path(data_path, "./richness/beetles_richness_cumulative_plot.csv"))
#macroinvert<- read.csv(file.path(data_path, "./richness/macroinvert_richness_cumulative_plot.csv"))
#mosq<- read.csv(file.path(data_path, "./richness/mosq_richness_cumulative_plot.csv"))
#phytop<- read.csv(file.path(data_path,./richness/phytop_richness_cumulative_plot.csv"))
#trees<- read.csv(file.path(data_path,"./richness/tree_richness_cumulative_plot.csv"))
#zoop<- read.csv(file.path(data_path,"./richness/zoop_richness_cumulative_plot.csv"))

#Check files
head(mammals)
head(birds)
head(plants)
#head(beetles)
#head(macroinvert)
#head(mosq)
#head(phytop)
#head(trees)
#head(zoop)

# add a column for taxa
mammals$taxa<-"mammal"
plants$taxa<-"plant"
birds$taxa<-"bird"
#beetles$taxa<-"beetle"
#macroinvert$taxa<-"macroinvertebrates"
#mosq$taxa<-"mosquito"
#phytop$taxa<-"phytoplankton"
#trees$taxa<-"tree"
#zoop$taxa<-"zooplankton"

# do this for each taxonomic group
head(mammals)
head(birds)
head(plants)
#head(beetles)
#head(macroinvert)
#head(mosq)
#head(phytop)
#head(trees)
#head(zoop)

# merge all taxonomic data together; before you do this, make sure there is a column for taxa.
rich<-rbind(birds, plants)

#subset data for Toolik and Harvard Forest
unique(rich$siteID)
#Use for adding harvard forest and ordway swisher data
all_rich<-rich[rich$siteID=="TOOL" | rich$siteID== "HARV" | rich$siteID== "OSBS",]
all_rich$siteID<-factor(all_rich$siteID)
head(all_rich)

#Take the maximum richness value before merging with distance data
names(all_rich)
all_rich_max<-all_rich%>%
  group_by(plotID, taxa) %>%
  slice(which.max(richness))
head(all_rich_max)
all_rich_max<-data.frame(all_rich_max)
unique(all_rich_max$plotID)
write.csv(all_rich_max, file="G://My Drive//NEON_LTER_2018//data//final_data//neon//richness//bird_plant_max_richness.csv")
#some plotIDs are NA for richness because they have not been surveyed yet.

#subset
keep=c("siteID", "plotID", "richness", "taxa")
all_rich_max<-all_rich_max[,keep]
head(all_rich_max)
###################################################################################
#Now that we have the richness max values for KNZ, TOOL, and HRF, we will combine the distance data for these three sites.
names(tool_dist)
names(hrf_dist)
names(osbs_dist)
#names(knz_dist)

#Toolik
#Rename columns
#Rename columns to specify disturbance type and distance in meters.
names(tool_dist)[names(tool_dist)=="burn_dist"]<-"severe_dist"
names(tool_dist)

#OSBS
names(osbs_dist)[names(osbs_dist)=="building_dist"]<-"bldgs_dist"
head(osbs_dist)

#Konza
#names(knz_dist)[names(knz_dist)=="burn_dist"]<-"severe_dist"
#head(knz_dist)

#Change all 0 values for distance to 0.001
#knz_dist$severe_dist[knz_dist$severe_dist == 0] <- 1
#head(knz_dist)

#Harvard
names(hrf_dist)[names(hrf_dist)=="cut_dist"]<-"severe_dist"
head(hrf_dist)
#Change all 0 values for distance to 0.001
hrf_dist$severe_dist[hrf_dist$severe_dist == 0] <- 1
head(hrf_dist)

#Bind these dataframes. We want to use rbind to stack our data. So, they need to have the same column names across all data frames. We really only care about having bldgs_dist and roads_dist for the distance variables. We need all of the environmental variables.

#Konza
#head(knz_dist)
#knz_vari <- c("siteID", "plotID", "elevatn", "nlcdCls", "slpGrdn", "slpAspc", "bldgs_dist", "roads_dist", "severe_dist")
#knz_dist <- knz_dist[knz_vari]
#head(knz_dist)

#Harvard
head(hrf_dist)
hrf_vari <- c("siteID", "plotID", "elevatn", "nlcdCls", "slpGrdn", "slpAspc", "bldgs_dist", "roads_dist", "severe_dist")
hrf_dist <- hrf_dist[hrf_vari]
head(hrf_dist)

#Toolik
names(tool_dist)
tool_vari <- c("siteID", "plotID", "elevatn", "nlcdCls", "slpGrdn", "slpAspc", "bldgs_dist", "roads_dist", "severe_dist")
tool_dist <- tool_dist[tool_vari]
head(tool_dist)

#OSBS
names(osbs_dist)
osbs_vari <- c("siteID", "plotID", "elevatn", "nlcdCls", "slpGrdn", "slpAspc", "bldgs_dist", "roads_dist", "severe_dist")
osbs_dist <- osbs_dist[osbs_vari]
head(osbs_dist)

all_dist1<-rbind(tool_dist, hrf_dist)
head(all_dist1)
#waiting for wei to add burn distance to her osbs_dist csv so that I can do this next step.
all_dist<-rbind(all_dist1, osbs_dist)
head(all_dist)

#Take the average of slp, asp, and elev by plotID.
attach(all_dist)
all_dist_max<-aggregate(all_dist[c("elevatn","slpAspc","slpGrdn", "bldgs_dist", "roads_dist", "severe_dist")],list(plotID=plotID, siteID=siteID, nlcdCls=nlcdCls),FUN=mean)
detach(all_dist)
head(all_dist_max)

#Check it
all_dist_max[duplicated(all_dist_max$plotID),]

#########################################################################################
#Merge richness data with distance data
names(all_dist_max)
names(all_rich_max)

united  <-merge(all_dist_max, all_rich_max, by=c("plotID", "siteID"))
head(united)
###############################################################################
#-------------------------------------------------------------------------------
#Separate birds and plants richness
#-------------------------------------------------------------------------------
#birds
all_bird<-united[united$taxa=="bird",]
head(all_bird)

#plants
all_plant<-united[united$taxa=="plant",]
head(all_plant)
################################################################################
# take a look at the relationship between plant richness and elevation
pp <- ggplot(all_plant, aes(x=elevatn, y=richness)) +
  geom_point(aes()) 
pp

# log transform for scatter plot
ppl <- ggplot(all_plant, aes(x=elevatn, y=richness)) +
  geom_point(aes()) + scale_y_log10()  
ppl

# take a look at the relationship between bird richness and elevation, by year
br <- ggplot(all_bird, aes(x=elevatn, y=richness)) +
  geom_point(aes()) 
br

# log transform for scatter plot
brl <- ggplot(all_bird, aes(x=elevatn, y=richness)) +
  geom_point(aes()) + scale_y_log10()  
brl
################################################################################
#Histograms

hist(all_plant$richness)
hist(all_bird$richness)

################################################################################
#log transformations

#plant
all_plant$ln.richness.p<-log(all_plant$richness)
head(all_plant)
hist(all_plant$ln.richness.p)

#birds
all_bird$ln.richness.b<-log(all_bird$richness)
head(all_bird)
hist(all_bird$ln.richness.b)

#united
hist(united$bldgs_dist)
united$ln.bldgs_dist<-log(united$bldgs_dist)
hist(united$roads_dist)
united$ln.roads_dist<-log(united$roads_dist)
hist(united$severe_dist)
head(united)

#----------------------------------------------------------
# Convert taxa file to wide format for modeling
#----------------------------------------------------------
# Wide format for taxa
united_wide<-reshape(united, v.names="richness",    # the values you want to transpose to wide format
                    idvar=c("siteID","plotID", "nlcdCls", "elevatn", "slpAspc", "slpGrdn", "bldgs_dist", "roads_dist", "severe_dist","ln.bldgs_dist", "ln.roads_dist"),  # your independent variable(s); careful because if you keep the other columns in your dataset, it may be confusing how the other columns relate to these new columns
                    timevar="taxa",  # the name of the grouping variable.
                    direction="wide") # the direction (can also be long)
str(united_wide)
head(united_wide)

#transformations
hist(united_wide$richness.bird)
united_wide$ln.richness.bird<-log(united_wide$richness.bird)
hist(united_wide$ln.richness.bird)

hist(united_wide$richness.plant)
united_wide$ln.richness.plant<-log(united_wide$richness.plant)
hist(united_wide$ln.richness.plant)

#Change all NA values for distance to 0
united_wide[is.na(united_wide)] <- 0
head(united_wide)
head(united)
################################################################################
#Temperature & Precipitation Data
# Read NEON point data
terrestrial <- readOGR("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data\\neon\\spatial_data\\All_NEON_TOS_Plots_V4\\All_Neon_TOS_Centroid_V4.shp")
knz <- terrestrial[terrestrial$siteNam=="Konza Prairie Biological Station",]
knz_map <- spTransform(knz, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
hrf <- terrestrial[terrestrial$siteNam=="Harvard Forest",]
hrf_map <- spTransform(hrf, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
arc <- terrestrial[terrestrial$siteNam=="Toolik Lake",]
arc_map <- spTransform(arc, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

##############################################################################

# Install PRISM
install_github(repo = "prism", username = "ropensci")
library(prism) ##prism data access

# Mean temp data
options(prism.path = "~/prismtmp")
get_prism_normals(type="tmean",resolution = "800m", annual = TRUE, keepZip=F)

ls_prism_data(name=TRUE)

# Temp data
RS <- prism_stack(ls_prism_data()) ##raster file of data
proj4string(RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

# Extract for NEON sites
KNZ_temp <- extract(RS, knz_map, method='bilinear')
KNZ_temp
knz_temp<-data.frame(KNZ_temp)
names(knz_temp)[names(knz_temp)=="knz_temp"]<-"knz_tmean"
HRF_temp <- extract(RS, hrf_map, method='bilinear')
HRF_temp
hrf_temp<-data.frame(HRF_temp)
hrf_temp
names(hrf_temp)[names(hrf_temp)=="hrf_temp"]<-"hrf_tmean"

# Precipitation data
RS2 <- prism_stack(ls_prism_data()[1,1]) ##raster file of data
proj4string(RS2)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info

# Extract for NEON sites
KNZ_ppt <- extract(RS2, knz_map, method='bilinear')
KNZ_ppt
knz_ppt<-data.frame(KNZ_ppt)
names(knz_ppt)[names(knz_ppt)=="knz_ppt"]<-"knz_precip"
HRF_ppt <- extract(RS2, hrf_map, method='bilinear')
HRF_ppt
hrf_ppt<-data.frame(HRF_ppt)
names(hrf_ppt)[names(hrf_ppt)=="hrf_ppt"]<-"hrf_precip"

#Toolik Temp Data
library(rgdal)
arc_temp<-raster("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data\\arc\\gis_data\\ak_tempmean\\tmeananl\\hdr.adf")
plot(arc_temp)
plot(arc_map, add=T)
arc_temp_spdf = SpatialPointsDataFrame(arc_map@data[,20:19], proj4string=arc_temp@crs,arc_map@data)
arc_temp_spdf

tool_temp<- extract(arc_temp, arc_map, tool_temp=TRUE, method='bilinear')
plot(tool_temp)
tool_temp_avg<-(tool_temp/100)
tool_temp_avg

#transform to dataframe
class(tool_temp_avg)
tool_temp_avg<-data.frame(tool_temp_avg)
head(tool_temp_avg)
names(tool_temp_avg)[names(tool_temp_avg)=="tool_temp_avg"]<-"arc_tmean"
head(tool_temp_avg)

#Toolik Precipitaiton Data
arc_ppt<-raster("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data\\arc\\gis_data\\ak_precipmean\\pptanl\\hdr.adf")
plot(arc_ppt)
plot(arc_map, add=T)
arc_ppt_spdf = SpatialPointsDataFrame(arc_map@data[,20:19], proj4string=arc_ppt@crs,arc_map@data)
arc_ppt_spdf
tool_ppt<- extract(arc_ppt, arc_map, tool_ppt=TRUE, method='bilinear')
plot(tool_ppt)
tool_ppt #mean values of precipitation in meters per neon plot in Toolik


#transform to dataframe
class(tool_ppt)
tool_ppt<-data.frame(tool_ppt)
class(tool_ppt)
names(tool_ppt)[names(tool_ppt)=="arc_ppt"]<-"arc_precip"
head(tool_ppt)

#combine with original shape file.
#Toolik
arc<-data.frame(arc@data)
toolik_ppt<-cbind(tool_ppt, arc)
summary(toolik_ppt)
toolik_temp<-cbind(tool_temp_avg, arc)
summary(toolik_temp)
names(toolik_temp)
names(toolik_ppt)
#subset
keep=c("plotID", "arc_tmean")
toolik_temp1<-toolik_temp[,keep]
head(toolik_temp1)
#Check for duplicates
toolik_temp1[duplicated(toolik_temp1$plotID),]
#Take the average. We want one value per plotID
attach(toolik_temp1)
toolik_temp1_mean<-aggregate(toolik_temp1[c("arc_tmean")],list(plotID=plotID),FUN=mean)
detach(toolik_temp1)
head(toolik_temp1_mean)
#Check again for duplicates
toolik_temp1_mean[duplicated(toolik_temp1_mean$plotID),]
names(toolik_temp1_mean)[names(toolik_temp1_mean)=="arc_tmean"]<-"tool_tmean"
head(toolik_temp1_mean)

keep=c("plotID", "tool_ppt")
toolik_ppt1<-toolik_ppt[,keep]
head(toolik_ppt1)
#Check for duplicates
toolik_ppt1[duplicated(toolik_ppt1$plotID),]
#Take the average. We want one value per plotID
attach(toolik_ppt1)
toolik_ppt1_mean<-aggregate(toolik_ppt1[c("tool_ppt")],list(plotID=plotID),FUN=mean)
detach(toolik_ppt1)
#Check again for duplicates
toolik_ppt1_mean[duplicated(toolik_ppt1_mean$plotID),]

#combine
ttp<-merge(toolik_ppt1_mean, toolik_temp1_mean, by="plotID")
head(ttp)

#Check again for duplicates
ttp[duplicated(ttp$plotID),]

#combine with united wide
ttp1<- merge(united_wide, ttp, by="plotID")
head(ttp1)

#combine with united to have plant and bird richness separate
tpb<-merge(united, ttp, by="plotID")
head(tpb)
################################################################################
#-------------------------------------------------------------------------------
#Separate birds and plants richness
#-------------------------------------------------------------------------------
#birds
tb<-tpb[tpb$taxa=="bird",]
head(tb)

#plants
tp<-tpb[tpb$taxa=="plant",]
head(tp)
################################################################################
#Check again for duplicates
ttp1[duplicated(ttp1$plotID),]

#Konza
knz<-data.frame(knz@data)
konza_ppt<-cbind(knz_ppt, knz)
summary(konza_ppt)
konza_temp<-cbind(knz_temp, knz)
names(konza_temp)

#subset
keep=c("plotID", "KNZ_temp")
knz_temp1<-konza_temp[,keep]
head(knz_temp1)

#Check for duplicates
knz_temp1[duplicated(knz_temp1$plotID),]

#Take the average. We want one value per plotID
attach(knz_temp1)
knz_temp1_mean<-aggregate(knz_temp1[c("KNZ_temp")],list(plotID=plotID),FUN=mean)
detach(knz_temp1)
head(knz_temp1_mean)
#Check again for duplicates
knz_temp1_mean[duplicated(knz_temp1_mean$plotID),]
names(knz_temp1_mean)[names(knz_temp1_mean)=="KNZ_temp"]<-"knz_tmean"
head(knz_temp1_mean)

names(konza_ppt)
keep=c("plotID", "KNZ_ppt")
knz_ppt1<-konza_ppt[,keep]
head(knz_ppt1)
#Check for duplicates
knz_ppt1[duplicated(knz_ppt1$plotID),]
#Take the average. We want one value per plotID
attach(knz_ppt1)
knz_ppt1_mean<-aggregate(knz_ppt1[c("KNZ_ppt")],list(plotID=plotID),FUN=mean)
detach(knz_ppt1)
#Check again for duplicates
knz_ppt1_mean[duplicated(knz_ppt1_mean$plotID),]

#combine
ktp<-merge(knz_ppt1_mean, knz_temp1_mean, by="plotID")
head(ktp)

#Check again for duplicates
ktp[duplicated(ktp$plotID),]

#combine with united wide
kttp<- merge(united_wide, ktp, by="plotID")
head(kttp)

#combine with united to have plant and bird richness separate
kpb<-merge(united, ktp, by="plotID")
head(kpb)
###############################################################################
#------------------------------------------------------------------------------
#Separate birds and plants richness
#------------------------------------------------------------------------------
#birds
kb<-kpb[kpb$taxa=="bird",]
head(kb)

#plants
kp<-kpb[kpb$taxa=="plant",]
head(kp)
###############################################################################
#Check again for duplicates
kttp[duplicated(kttp$plotID),]

#Harvard Forest
hrf<-data.frame(hrf@data)
harvard_ppt<-cbind(hrf_ppt, hrf)
head(harvard_ppt)
harvard_temp<-cbind(hrf_temp, hrf)
head(harvard_temp)
names(harvard_temp)
#subset
keep=c("plotID", "HRF_temp")
hrf_temp1<-harvard_temp[,keep]
head(hrf_temp1)

#Check for duplicates
hrf_temp1[duplicated(hrf_temp1$plotID),]

#Take the average. We want one value per plotID
attach(hrf_temp1)
hrf_temp1_mean<-aggregate(hrf_temp1[c("HRF_temp")],list(plotID=plotID),FUN=mean)
detach(hrf_temp1)
head(hrf_temp1_mean)
#Check again for duplicates
hrf_temp1_mean[duplicated(hrf_temp1_mean$plotID),]
names(hrf_temp1_mean)[names(hrf_temp1_mean)=="HRF_temp"]<-"hrf_tmean"
head(hrf_temp1_mean)

names(harvard_ppt)
keep=c("plotID", "HRF_ppt")
hrf_ppt1<-harvard_ppt[,keep]
head(hrf_ppt1)
#Check for duplicates
hrf_ppt1[duplicated(hrf_ppt1$plotID),]
#Take the average. We want one value per plotID
attach(hrf_ppt1)
hrf_ppt1_mean<-aggregate(hrf_ppt1[c("HRF_ppt")],list(plotID=plotID),FUN=mean)
detach(hrf_ppt1)
names(hrf_ppt1_mean)[names(hrf_ppt1_mean)=="HRF_ppt"]<-"hrf_ppt"
#Check again for duplicates
hrf_ppt1_mean[duplicated(hrf_ppt1_mean$plotID),]

#combine
htp<-merge(hrf_temp1_mean, hrf_ppt1_mean, by="plotID")
head(htp)

#Check again for duplicates
htp[duplicated(htp$plotID),]

#combine with united wide
http<- merge(united_wide, htp, by="plotID")
head(http)

#combine with united to have plant and bird richness separate
hpb<-merge(united, htp, by="plotID")
head(hpb)
################################################################################
#------------------------------------------------------------------------------
#Separate birds and plants richness
#------------------------------------------------------------------------------
#birds
hb<-hpb[hpb$taxa=="bird",]
head(hb)

#plants
hp<-hpb[hpb$taxa=="plant",]
head(hp)
################################################################################
#Check again for duplicates
http[duplicated(http$plotID),]

#Combine all values to one complete data frame for modeling.
names(kttp)
names(kttp)[names(kttp)=="KNZ_ppt"]<-"precipitation"
names(kttp)[names(kttp)=="knz_tmean"]<-"temperature"
names(ttp1)
names(ttp1)[names(ttp1)=="tool_ppt"]<-"precipitation"
names(ttp1)[names(ttp1)=="tool_tmean"]<-"temperature"
names(http)
names(http)[names(http)=="hrf_ppt"]<-"precipitation"
names(http)[names(http)=="hrf_tmean"]<-"temperature"

kt<-rbind(kttp, ttp1)
kt
kth<-rbind(kt,http)
names(kth)

#------------------------------------------------------------------------------
#Combine data for separate plant and bird
#------------------------------------------------------------------------------
names(tb)
names(tb)[names(tb)=="tool_ppt"]<-"precipitation"
names(tb)[names(tb)=="tool_tmean"]<-"temperature"

names(kb)
names(kb)[names(kb)=="KNZ_ppt"]<-"precipitation"
names(kb)[names(kb)=="knz_tmean"]<-"temperature"

names(hb)
names(hb)[names(hb)=="hrf_ppt"]<-"precipitation"
names(hb)[names(hb)=="hrf_tmean"]<-"temperature"

#birds
all_bird1<-rbind(tb, kb)
head(all_bird1)
all_bird<-rbind(all_bird1, hb)
head(all_bird)

names(tp)
names(tp)[names(tp)=="tool_ppt"]<-"precipitation"
names(tp)[names(tp)=="tool_tmean"]<-"temperature"

names(kp)
names(kp)[names(kp)=="KNZ_ppt"]<-"precipitation"
names(kp)[names(kp)=="knz_tmean"]<-"temperature"

names(hp)
names(hp)[names(hp)=="hrf_ppt"]<-"precipitation"
names(hp)[names(hp)=="hrf_tmean"]<-"temperature"
#plants
all_plant1<-rbind(tp, kp)
head(all_plant1)
all_plant<-rbind(all_plant1, hp)
head(all_plant)
################################################################################
#Combined complete data frame.
names(tpb)
names(tpb)[names(tpb)=="tool_ppt"]<-"precipitation"
names(tpb)[names(tpb)=="tool_tmean"]<-"temperature"

names(kpb)
names(kpb)[names(kpb)=="KNZ_ppt"]<-"precipitation"
names(kpb)[names(kpb)=="knz_tmean"]<-"temperature"

names(hpb)
names(hpb)[names(hpb)=="hrf_ppt"]<-"precipitation"
names(hpb)[names(hpb)=="hrf_tmean"]<-"temperature"
kh_final<-rbind(hpb, kpb)
kth_final<-rbind(kh_final, tpb)
head(kth_final)
##################################################################################
save.image("neon_all_site_prep.RData")
#Now you're ready for modeling. See script neon_all_site_analysis.

