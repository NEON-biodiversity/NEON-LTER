# Title: Cross Site Analysis Prep
# Author: Cameo Chilcutt
# Date: 12 July 2018

# Start with a clear workspace and set working directory to google drive -> NEON_LTER_2018 folder -> data -> raw_data
rm(list=ls())
setwd("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data")

#install these packages and load the libraries
library(rgdal)
library(rgeos)
library(raster)

#load these data 
load("NEON_LTER.RData")
load("G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\neon_within_site_prep.RData")
names(knz.combined.distances)
names(hrf.combined.distances)
names(toolik)

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
#Use for adding harvard forest and konza prairie data
all_rich<-rich[rich$siteID=="TOOL" | rich$siteID== "HARV" | rich$siteID== "KONZ",]
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
write.csv(all_rich_max, file="all_richness_max.csv")
#some plotIDs are NA for richness because they have not been surveyed yet.

#subset
keep=c("siteID", "plotID", "richness", "taxa")
all_rich_max<-all_rich_max[,keep]
head(all_rich_max)
##########################################################################################
#Now that we have the richness max values for KNZ, TOOL, and HRF, we will combine the distance data for these three sites.
names(toolik)
names(knz.combined.dist2)
names(hrf.combined.dist1)

#Convert all files to data.frame
knz_final<-data.frame(knz.combined.dist2)
names(knz_final)
hrf_final<-data.frame(hrf.combined.dist1)
names(hrf_final)

#Konza
#We have some unneccessary columns that came with the spatial point data frame. So, we will exclude those to further clean up our data frame.
knz_final$coords.x1<-NULL
knz_final$coords.x2<-NULL
knz_final$coords.x1.1<-NULL
knz_final$coords.x2.1<-NULL
knz_final$coords.x1.2<-NULL
knz_final$coords.x2.2<-NULL
knz_final$coords.x1.3<-NULL
knz_final$coords.x2.3<-NULL
knz_final$optional<-NULL
head(knz_final)

#Harvard Forest
#We have some unneccessary columns that came with the spatial point data frame. So, we will exclude those to further clean up our data frame.
hrf_final$coords.x1<-NULL
hrf_final$coords.x2<-NULL
hrf_final$coords.x1.1<-NULL
hrf_final$coords.x2.1<-NULL
hrf_final$coords.x1.2<-NULL
hrf_final$coords.x2.2<-NULL
hrf_final$optional<-NULL
head(hrf_final)

#Toolik
#Rename columns
#Rename columns to specify disturbance type and distance in meters.
names(toolik)[names(toolik)=="distance_m.roads"]<-"roads_dist"
names(toolik)[names(toolik)=="distance_m.thermokarst"]<-"thermo_dist"
names(toolik)[names(toolik)=="distance_m.pipeline"]<-"pipe_dist"
names(toolik)[names(toolik)=="burn_dist"]<-"severe_dist"
names(toolik)[names(toolik)=="distance_m.buildings"]<-"bldgs_dist"
head(toolik)

#Konza
names(knz_final)[names(knz_final)=="burn_dist"]<-"severe_dist"
knz_final<-data.frame(knz_final)
head(knz_final)

#Change all 0 values for distance to 0.001
knz_final$severe_dist[knz_final$severe_dist == 0] <- 1
head(knz_final)

#Harvard
names(hrf_final)[names(hrf_final)=="cut_dist"]<-"severe_dist"
hrf_final<-data.frame(hrf_final)
head(hrf_final)
#Change all 0 values for distance to 0.001
hrf_final$severe_dist[hrf_final$severe_dist == 0] <- 1
head(hrf_final)

#Bind these dataframes. We want to use rbind to stack our data. So, they need to have the same column names across all data frames. We really only care about having bldgs_dist and roads_dist for the distance variables. We need all of the environmental variables.
#Konza
head(knz_final)
knz_vari <- c("siteID", "plotID", "elevatn", "nlcdCls", "slpGrdn", "slpAspc", "bldgs_dist", "roads_dist", "severe_dist")
knz_final <- knz_final[knz_vari]
head(knz_final)
#Harvard
head(hrf_final)
hrf_vari <- c("siteID", "plotID", "elevatn", "nlcdCls", "slpGrdn", "slpAspc", "bldgs_dist", "roads_dist", "severe_dist")
hrf_final <- hrf_final[hrf_vari]
head(hrf_final)
#Toolik
head(toolik)
tool_vari <- c("siteID", "plotID", "elevatn", "nlcdCls", "slpGrdn", "slpAspc", "bldgs_dist", "roads_dist", "severe_dist")
tool_final <- toolik[tool_vari]
head(tool_final)

all_dist1<-rbind(tool_final, hrf_final)
head(all_dist1)
all_dist<-rbind(all_dist1, knz_final)
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

head(united)
##########################################################################################
#--------------------------------------------------------------------------------
#Separate birds and plants richness
#--------------------------------------------------------------------------------
#birds
all_bird<-united[united$taxa=="bird",]
head(all_bird)

#plants
all_plant<-united[united$taxa=="plant",]
head(all_plant)
#################################################################################
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
#################################################################################
#Histograms

hist(all_plant$richness)
hist(all_bird$richness)

#################################################################################
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
save.image("neon_all_site_prep.RData")
#################################################################################
#Now you're ready for modeling. See script neon_all_site_analysis.

