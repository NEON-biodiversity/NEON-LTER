## TITLE:         NEON within-site analysis of Organismal Data
## AUTHOR:        Phoebe Zarnetske, Cameo Arnold 
## COLLABORATORS: Sydne Record (Bryn Mawr), Ben Baiser (UFL), Angela Strecker (PSU), 
##                John M. Grady (MSU/Bryn Mawr), Jonathan Belmaker (Tel Aviv U), Mao-Ning Tuanmu (Academia Sinica),
##                Lydia Beaudrot (Rice U), Kate Thibault 
## DATA:          NEON organismal data: all species, all years, all sites
## PROJECT:       "NEON's continental-scale biodiversity"
## DATE:          initiated: June 28, 2018; last run:

## This script reads NEON's organismal richness data at the plot level, 
# per all available sites, and per year. 
# Linear models are generated to investigate how disturbance variables explain the
# variation in richness across plots, per taxonomic group.

## On HPCC; add this command to load recent R version
#module swap GNU GNU/4.9
#module load OpenMPI 1.10.0
#module load R/3.3.2

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

# Set file paths
google_drive <- 'G:\\My Drive\\NEON_LTER_2018\\data'
setwd(file.path(google_drive, 'final_data\\neon')) # GD location
# file path location for LTER disturbance and richness data by NEON plot
data_path <- file.path(google_drive, 'final_data\\neon')
fig_path <- file.path(google_drive, 'output')

#Install/load packages
for (package in c("lme4", "plyr", "dplyr", "purrr", "raster", "reshape2", "lubridate", "ggplot2")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# This code for ggplot2 sets the theme to mostly black and white 
# (Arial font, and large font, base size=24)
theme_set(theme_bw(12))
theme_update(axis.text.x = element_text(size = 10, angle = 90),
             axis.text.y = element_text(size = 10))

# ----------------------------------------
# Load all organismal richness and within-site neon plot-level disturbance data 
# ----------------------------------------

# Read in plot-level richness data from each taxon
# The raw data were all pulled on 20 June 2018; github/NEON-biodiversity/neonbiodiversity/code/neon_organism_api_export.R 
#disturbance distance
dist<-read.csv(file.path(data_path,"./disturbance/neon_plotid_dist2disturbance.csv"), stringsAsFactors = FALSE)

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
#Use for adding harvard forest data
#ht_rich<-rich[rich$siteID=="TOOL" | rich$siteID== "HARV",]
#ht_rich$siteID<-factor(ht_rich$siteID)
#head(ht_rich)
#str(ht_rich)
tool_rich<-rich[rich$siteID=="TOOL",]
head(tool_rich)
str(tool_rich)

# merge the disturbance distances with the richness plot-level dataset
dist.rich<-merge(dist,tool_rich,by=c("siteID","plotID"),all.x=T)
names(dist.rich)
unique(dist.rich$plotID)
unique(dist$plotID)

#subset Toolik for now because we only have disturbance data for it.
#need disturbance data for Harvard Forest to create this.
unique(dist.rich$siteID)
dist.rich<-dist.rich[dist.rich$siteID=="TOOL",]
head(dist.rich)
unique(dist.rich$siteID)
summary(dist.rich)

#subset maximum richness per plotID.
#attach(dist.rich)
#dist.rich.max<-aggregate(dist.rich[c("richness")],list(siteID=siteID,plotID=plotID, taxa=taxa, year=year, dist_type=dist_type, distance_m=distance_m), FUN=max, na.rm=T)
#detach(dist.rich)

dist.rich.max<-dist.rich%>%
  group_by(plotID, taxa, dist_type) %>%
  slice(which.max(richness))
head(dist.rich.max)
dist.rich.max<-data.frame(dist.rich.max)
unique(dist.rich.max$plotID)
write.csv(dist.rich.max, file="dist.richtest.csv")
#some plotIDs are NA for richness because they have not been surveyed yet.

# take a look at the relationship between richness and distance to disturbance, by year, by disturbance type
p <- ggplot(dist.rich.max, aes(x=distance_m, y=richness)) +
  geom_point(aes(color=factor(dist_type), group=factor(dist_type))) +
  facet_wrap(~taxa, scales="free_y")  
p
#look at richness distribution by taxa
q <- ggplot(dist.rich.max, aes(richness)) +
  geom_histogram() +
  facet_wrap(~taxa, scales="free")  
q
#log transform for scatter plot
r <- ggplot(dist.rich.max, aes(x=distance_m, y=richness)) +
  geom_point(aes(color=factor(dist_type), group=factor(dist_type))) +
  facet_wrap(~taxa, scales="free_y") + scale_y_log10()  
r
#log transform for histogram
q <- ggplot(dist.rich.max, aes(richness)) +
  geom_histogram() +
  facet_wrap(~taxa, scales="free") + scale_x_log10()  
q
#----------------------------------------------------------
# Convert distance file to wide format for modeling
#----------------------------------------------------------
# Wide format for disturbance
dist1<-reshape(dist, v.names="distance_m",    # the values you want to transpose to wide format
               idvar=c("siteID","plotID", "siteNam"),  # your independent variable(s); careful because if you keep the other columns in your dataset, it may be confusing how the other columns relate to these new columns
               timevar="dist_type",  # the name of the grouping variable.
               direction="wide") # the direction (can also be long)
str(dist1)
head(dist1)

#Remerge dist1 with richness data
# merge the disturbance distances with the richness plot-level dataset
dist.rich1<-merge(dist1,tool_rich,by=c("siteID","plotID"),all.x=T)
names(dist.rich1)
unique(dist.rich1$plotID)
unique(dist1$plotID)

#subset Toolik for now because we only have disturbance data for it.
unique(dist.rich1$siteID)
tool.dist.rich<-dist.rich1[dist.rich1$siteID=="TOOL",]
head(tool.dist.rich)
unique(tool.dist.rich$siteID)
summary(tool.dist.rich)
##################################################################################################################################################################
# ------------------------------------------------------------
# Now we are going to look at elevation with species richness
# ------------------------------------------------------------

library(rgdal)

#elevation subset from all NEON spatial data
terrestrial<- readOGR("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data\\neon\\spatial_data\\All_NEON_TOS_Plots_V4\\All_Neon_TOS_Centroid_V4.shp")
plot(terrestrial, col=cm.colors(5), alpha=1, legend=F, main="Terrestrial Data")
head(terrestrial)

#To subset the data specifically to your site. You must use the exact name as it is written in the data. To look this up, use the following function to list all of the names of the field sites.
unique(terrestrial$siteNam)
#Figure out why the richness HARV & TOOL subset didn't work before running the elevation extraction.
#arc<-terrestrial[terrestrial$siteNam=="Toolik Lake","Harvard Forest",]
arc<-terrestrial[terrestrial$siteNam=="Toolik Lake",]
summary(arc)
names(arc)
plot(arc)

#subset elevation, slope, aspect, and landcover from NEON terrestrial data
myvars <- c("elevatn", "siteID", "plotID", "nlcdCls", "slpGrdn", "slpAspc")
env <- arc[myvars]
head(env)
str(env)

#Currently, it is in latitude and longitude, but in order to measure distance in meters we need to reproject the data into UTMs. You should look up the appropriate zone for your site. For the Toolik Lake Field Station we needed UTM Zone 6. 
arc_env<-spTransform(env, CRS("+proj=utm +zone=6 ellps=WGS84"))
str(arc_env)

write.csv(arc_env, file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\arc_environment.csv", row.names=F)
head(arc_env)
#writing a csv is necessary to merge the files. It will NOT work without first writing a csv and importing it again. 

#merge the elevation data with richness plot-level dataset
arc_env<-read.csv("G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\arc_environment.csv")
env.rich<-merge(arc_env,tool.dist.rich,by=c("siteID", "plotID"), all.x=T, all.y=T)
head(env.rich)
#################################################################################
#Get rid of all other columns except: elevation, slope, aspect, plant richness, bird richness, plotID, siteID, landcover
keep=c("plotID", "siteID", "taxa","richness", "elevatn", "slpAspc", "slpGrdn", "nlcdCls", "distance_m.burn", "distance_m.roads", "distance_m.buildings", "distance_m.pipeline", "distance_m.thermokarst", "distance_m.water source")
keep[!keep %in% names(env.rich)]

tool.env.rich <- env.rich
tool.env.rich <- tool.env.rich[,keep] 
write.csv(tool.env.rich, file="G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\disturbance\\tool_env_rich.csv", row.names=F)
names(tool.env.rich)
head(tool.env.rich)

##################################################################################Separate birds and plants richness
#birds
arc_bird<-tool.env.rich[tool.env.rich$taxa=="bird",]
summary(arc_bird)

#plants
arc_plant<-tool.env.rich[tool.env.rich$taxa=="plant",]
summary(arc_plant)
#################################################################################
#Plants
#subset maximum richness.plant per plotID.
plant.rich.max<-arc_plant%>%
  group_by(plotID, taxa, elevatn, nlcdCls, slpAspc, slpGrdn, distance_m.buildings, distance_m.burn, distance_m.pipeline, distance_m.thermokarst, `distance_m.water source`, distance_m.roads) %>%
  slice(which.max(richness))
head(plant.rich.max)
plant.rich.max<-data.frame(plant.rich.max)
unique(plant.rich.max$plotID)
write.csv(plant.rich.max, file="elev.plantrich.csv")
names(plant.rich.max)

# take a look at the relationship between plant richness and elevation, by year
ep <- ggplot(plant.rich.max, aes(x=elevatn, y=richness)) +
  geom_point(aes()) 
ep

# log transform for scatter plot
epl <- ggplot(plant.rich.max, aes(x=elevatn, y=richness)) +
  geom_point(aes()) + scale_y_log10()  
epl
##################################################################################See which data need to be log transformed by looking at a histogram. If the data is skewed, log transform it before running the model.

#plant
hist(plant.rich.max$richness)
plant.rich.max$ln.richness<-log(plant.rich.max$richness)
summary(plant.rich.max)
hist(plant.rich.max$ln.richness)

#roads
hist(plant.rich.max$distance_m.roads)
plant.rich.max$ln.distance_m.roads<-log(plant.rich.max$distance_m.roads)
summary(plant.rich.max)
hist(plant.rich.max$ln.distance_m.roads)

#buildings
hist(plant.rich.max$distance_m.buildings)

#burn
hist(plant.rich.max$distance_m.burn)
plant.rich.max$ln.distance_m.burn<-log(plant.rich.max$distance_m.burn)
summary(plant.rich.max$ln.distance_m.burn)
hist(plant.rich.max$ln.distance_m.burn)
#################################################################################
#Birds
#subset maximum richness.bird per plotID.
bird.rich.max<-arc_bird%>%
  group_by(plotID, elevatn, nlcdCls, slpAspc, slpGrdn, distance_m.buildings, distance_m.burn, distance_m.pipeline, distance_m.thermokarst, `distance_m.water source`, distance_m.roads) %>%
  slice(which.max(richness))
head(bird.rich.max)
bird.rich.max<-data.frame(bird.rich.max)
unique(bird.rich.max$plotID)
write.csv(bird.rich.max, file="elev.birdrich.csv")
names(bird.rich.max)

# take a look at the relationship between bird richness and elevation, by year
er <- ggplot(bird.rich.max, aes(x=elevatn, y=richness)) +
  geom_point(aes()) 
er

# log transform for scatter plot
erl <- ggplot(bird.rich.max, aes(x=elevatn, y=richness)) +
  geom_point(aes()) + scale_y_log10()  
erl
names(bird.rich.max)
##################################################################################See which data need to be log transformed by looking at a histogram. If the data is skewed, log transform it before running the model.

#birds
hist(bird.rich.max$richness)
bird.rich.max$ln.richness<-log(bird.rich.max$richness)
summary(bird.rich.max)
hist(bird.rich.max$ln.richness)

#roads
hist(bird.rich.max$distance_m.roads)

#buildings
hist(bird.rich.max$distance_m.buildings)

#burn
hist(bird.rich.max$distance_m.burn)
bird.rich.max$ln.distance_m.burn<-log(bird.rich.max$distance_m.burn)
summary(bird.rich.max$ln.distance_m.burn)
hist(bird.rich.max$ln.distance_m.burn)

#water source
hist(bird.rich.max$distance_m.water.source)
bird.rich.max$ln.distance_m.water.source<-log(bird.rich.max$distance_m.water.source)
summary(bird.rich.max$ln.distance_m.water.source)
hist(bird.rich.max$ln.distance_m.water.source)

#elevation
hist(bird.rich.max$elevatn)

#slpGrdn
hist(bird.rich.max$slpGrdn)
bird.rich.max$ln.slpGrdn<-log(bird.rich.max$slpGrdn)
summary(bird.rich.max$ln.slpGrdn)
hist(bird.rich.max$ln.slpGrdn)

#slpAspc
hist(bird.rich.max$slpAspc)
bird.rich.max$ln.slpAspc<-log(bird.rich.max$slpAspc)
summary(bird.rich.max$ln.slpAspc)
hist(bird.rich.max$ln.slpAspc)

names(bird.rich.max)
bird_rich<- na.omit(bird.rich.max)
head(bird_rich)
##################################################################################################################################################################
# This is the first linear model. First is the plant species richness data ~ all your predictor variables.
pm1<-lm(plant.rich.max$ln.richness~plant.rich.max$ln.distance_m.burn+
         plant.rich.max$distance_m.buildings+
         plant.rich.max$distance_m.water.source+
         plant.rich.max$distance_m.roads+
         plant.rich.max$elevatn+
         plant.rich.max$slpGrdn+
         plant.rich.max$slpAspc)
summary(pm1)
#Follow this link if you need help interpreting your summary. https://www.quora.com/How-do-I-interpret-the-summary-of-a-linear-model-in-R

anova(pm1)

# We need to check the normality of this model so that we can ensure that everything is relatively normally distributed.
library(car)
qqPlot(pm1)
plot(pm1)

#################################################################################
# This is the second linear model. First is the bird species richness data ~ all your predictor variables.
bm1<-lm(bird_rich$ln.richness~bird_rich$ln.distance_m.burn+
          bird_rich$distance_m.buildings+
          bird_rich$distance_m.water.source+
          bird_rich$distance_m.roads+
          bird_rich$elevatn+
          bird_rich$slpGrdn+
          bird_rich$slpAspc)
summary(bm1)
#Follow this link if you need help interpreting your summary. https://www.quora.com/How-do-I-interpret-the-summary-of-a-linear-model-in-R

anova(bm1)

# Normality of Residuals
# qq plot for studentized resid
library(car)
qqPlot(bm1)
plot(bm1)

# Check out these example scripts to assess normality, outliers, etc: https://www.statmethods.net/stats/rdiagnostics.html
# use qqplot() at least
# do data look normal? if not, then we will need to transform


