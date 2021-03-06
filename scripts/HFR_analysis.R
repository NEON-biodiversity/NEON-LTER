#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

# set working directory
google_drive <- 'c:/users/srecord/google drive/neon_lter_2018'
setwd(file.path(google_drive)) # GD location

# file path location for LTER disturbance and richness data by NEON plot
#data_path <- file.path(google_drive, 'final_data\\neon')
#fig_path <- file.path(google_drive, 'output')

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
dist<-read.csv("./disturbance/neon_plotid_dist2disturbance_hrf.csv")
##########################################################################

#organismal data
mammals <- read.csv("./richness/mammal_richness_cumulative_plot.csv")
plants<- read.csv("./richness/plant_richness_cumulative_plot.csv")
birds<- read.csv( "./richness/bird_richness_cumulative_plot.csv")
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
rich<-rbind(birds, plants, mammals)

#subset data for Harvard Forest
unique(rich$siteID)
#Use for adding harvard forest data
harv_rich<-rich[rich$siteID=="HARV",]
head(harv_rich)
str(harv_rich)

#Take the maximum richness value before merging with distance data
names(harv_rich)
harv_rich_max<-harv_rich%>%
  group_by(plotID, taxa) %>%
  slice(which.max(richness))
head(harv_rich_max)
harv_rich_max<-data.frame(harv_rich_max)
unique(harv_rich_max$plotID)
write.csv(harv_rich_max, file="richness_max2.csv")
#some plotIDs are NA for richness because they have not been surveyed yet.

#subset
keep=c("siteID", "plotID", "richness", "taxa")
harv_rich_max<-harv_rich_max[,keep]
head(harv_rich_max)

#merge with distance data
dist_rich<- merge(dist, harv_rich_max, by=c("siteID","plotID"),all.x=T)
head(dist_rich)

#----------------------------------------------------------
# Convert distance file to wide format for modeling
#----------------------------------------------------------
# Wide format for disturbance
dist_rich1<-reshape(dist_rich, v.names="distance_m",    # the values you want to transpose to wide format
                    idvar=c("siteID","plotID", "richness", "taxa"),  # your independent variable(s); careful because if you keep the other columns in your dataset, it may be confusing how the other columns relate to these new columns
                    timevar="dist_type",  # the name of the grouping variable.
                    direction="wide") # the direction (can also be long)
str(dist_rich1)
head(dist_rich1)

# ------------------------------------------------------------
# Now we are going to look at elevation with species richness
# ------------------------------------------------------------

library(rgdal)

#elevation subset from all NEON spatial data
terrestrial <- readOGR(dsn="data/raw_data/neon/spatial_data/All_NEON_TOS_Plots_V4", "All_Neon_TOS_Centroid_V4")
plot(terrestrial, col=cm.colors(5), alpha=1, legend=F, main="Terrestrial Data")
head(terrestrial)

#To subset the data specifically to your site. You must use the exact name as it is written in the data. To look this up, use the following function to list all of the names of the field sites.
unique(terrestrial$siteNam)

#subset Harvard Forest
hrf<-terrestrial[terrestrial$siteNam=="Harvard Forest",]
summary(hrf)
names(hrf)
plot(hrf)

#subset elevation, slope, aspect, and landcover from NEON terrestrial data
myvars <- c("elevatn", "siteID", "plotID", "nlcdCls", "slpGrdn", "slpAspc")
env <- hrf[myvars]
head(env)
str(env)

#Currently, it is in latitude and longitude, but in order to measure distance in meters we need to reproject the data into UTMs. You should look up the appropriate zone for your site. For the Toolik Lake Field Station we needed UTM Zone 6. 
hrf_env<-spTransform(env, CRS("+proj=utm +zone=18 ellps=WGS84"))
str(hrf_env)
hrf_env<-data.frame(hrf_env)

#We need to make sure we have one value for slope and elevation before proceeding. This shows the duplicate entries by plotID
hrf_env[duplicated(hrf_env$plotID),]

#Take the average of slp, asp, and elev by plotID.
attach(hrf_env)
hrf_env<-aggregate(hrf_env[c("elevatn","slpAspc","slpGrdn")],list(plotID=plotID, nlcdCls=nlcdCls),FUN=mean)
detach(hrf_env)
head(hrf_env)

#Check it
hrf_env[duplicated(hrf_env$plotID),]
write.csv(hrf_env, file="data/final_data/hrf/hrf_environment.csv", row.names=F)
head(hrf_env)
#writing a csv is necessary to merge the files. It will NOT work without first writing a csv and importing it again. - SR: This is not the case for me.

#merge dist_rich1 and hrf_env
names(dist_rich1)
names(hrf_env)
harv<-merge(hrf_env, dist_rich1, by=c("plotID"),all.x=T)
head(harv)

#Get rid of NAs for plots not sampled yet.
harv<- subset(harv,is.na(harv$richness)==F)
harv$richness

#Get rid of unused Factor levels
harv$nlcdCls <- factor(harv$nlcdCls)
harv$nlcdCls
harv$plotID <- factor(harv$plotID)
harv$plotID

#Check for duplicates
harv[duplicated(harv$plotID),]
#Now that we have a code with all of the environment, richness, and distance data we can start subsetting by taxa.
#--------------------------------------------------------------------------------
#Separate birds and plants richness
#--------------------------------------------------------------------------------
#birds
hrf_bird<-harv[harv$taxa=="bird",]
head(hrf_bird)

#plants
hrf_plant<-harv[harv$taxa=="plant",]
head(hrf_plant)

#mammals
hrf_mammal <- harv[harv$taxa=="mammal",]
head(hrf_mammal)
#################################################################################
# take a look at the relationship between plant richness and elevation
ep <- ggplot(hrf_plant, aes(x=elevatn.x, y=richness)) +
  geom_point(aes()) 
ep

# log transform for scatter plot
epl <- ggplot(hrf_plant, aes(x=elevatn.x, y=richness)) +
  geom_point(aes()) + scale_y_log10()  
epl

# take a look at the relationship between bird richness and elevation, by year
er <- ggplot(hrf_plant, aes(x=elevatn.x, y=richness)) +
  geom_point(aes()) 
er

# log transform for scatter plot
erl <- ggplot(hrf_plant, aes(x=elevatn.x, y=richness)) +
  geom_point(aes()) + scale_y_log10()  
erl
#################################################################################
#Histograms

hist(hrf_plant$richness)
hist(hrf_bird$richness)

#################################################################################
#log transformations

#plant
hrf_plant$ln.richness<-log(hrf_plant$richness)
head(hrf_plant)
hist(hrf_plant$ln.richness)

#birds
hrf_bird$ln.richness<-log(hrf_bird$richness)
head(hrf_bird)
hist(hrf_bird$ln.richness)

##############################################################################
# Check for collinearity of predictor variables
cor(hrf_plant[,c(3:5,13:15)])

# This is the first linear model. First is the plant species richness data ~ all your predictor variables.
pm1<-glm(hrf_plant$richness~hrf_plant$distance_m.roads+
           hrf_plant$distance_m.cuttings+
           hrf_plant$distance_m.buildings+
           hrf_plant$nlcdCls.x+
           hrf_plant$elevatn.x, family='poisson')
Anova(pm1, type='II')
#significant variables: buildings, nlcd, elevation

#Follow this link if you need help interpreting your summary. https://www.quora.com/How-do-I-interpret-the-summary-of-a-linear-model-in-R

# We need to check the normality of this model so that we can ensure that everything is relatively normally distributed.
library(car)
qqPlot(pm1)
plot(pm1)

#Reduced Model 1 - remove builldings
pm2<-glm(hrf_plant$richness~hrf_plant$distance_m.buildings+
           hrf_plant$nlcdCls.x+
           hrf_plant$elevatn.x, family='poisson')
Anova(pm2, type='II')
#significant variables: buildings, nlcdCls, elevation

# Plot the relationship between richness and the builing distance
bldgs.cut<-ggplot(hrf_plant, aes(x = hrf_plant$distance_m.buildings, y =hrf_plant$richness)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue")
bldgs.cut+labs(title="Plants",
               x ="Building Distance", y = "Species Richness")

# Evaluate 

# Plot the relationship between richness and landcover
boxplot(hrf_plant$richness ~ hrf_plant$nlcdCls.x)

# Compare within landcover types
# Create objects for each landcover type
decid <- subset(hrf_plant, nlcdCls=='deciduousForest')
evergr <- subset(hrf_plant, nlcdCls=='evergreenForest')
mixed <- subset(hrf_plant, nlcdCls=='mixedForest')
woody <- subset(hrf_plant, nlcdCls=='woodyWetlands')

# Model 1
decidevergr <- rbind(decid,evergr)
glm1 <-glm(ln.richness~nlcdCls,family='gaussian', data=decidevergr)
Anova(glm1, type="II")

# Model 2
decidmixed <- rbind(decid,mixed)
glm2 <-glm(ln.richness~nlcdCls,family='gaussian', data=decidmixed)
Anova(glm2, type="II")

# Model 3
decidwoody <- rbind(decid,woody)
glm3 <-glm(ln.richness~nlcdCls,family='gaussian', data=decidwoody)
Anova(glm3, type="II")

# Model 4
evergrmixed <- rbind(evergr,mixed)
glm4 <-glm(ln.richness~nlcdCls,family='gaussian', data=evergrmixed)
Anova(glm4, type="II")

# Model 5
evergrwoody <- rbind(evergr,woody)
glm5 <-glm(ln.richness~nlcdCls,family='gaussian', data=evergrwoody)
Anova(glm5, type="II")

# Model 6
mixedwoody <- rbind(mixed,woody)
glm6 <-glm(ln.richness~nlcdCls,family='gaussian', data=mixedwoody)
Anova(glm6, type="II")

# There is significant difference between "Woody Wetlands" and other landcover classes


###########Birds################3
# Check for collinearity of predictor variables
cor(hrf_bird[,c(3:5,13:15)])

# This is the first linear model. First is the plant species richness data ~ all your predictor variables.
pm1<-glm(hrf_birdrichness~hrf_bird$distance_m.roads+
           hrf_bird$distance_m.cuttings+
           hrf_bird$distance_m.buildings+
           hrf_bird$nlcdCls.x+
           hrf_bird$elevatn.x, family='poisson')
Anova(pm1, type='II')
#significant variables: buildings, nlcd, elevation
qqPlot(pm1)
plot(pm1)

#Reduced Model 1 - remove builldings
pm2<-glm(hrf_bird$richness~hrf_bird$distance_m.buildings+
           hrf_bird$nlcdCls.x+
           hrf_bird$elevatn.x, family='poisson')
Anova(pm2, type='II')
# none significant variables

pm3<-glm(hrf_bird$richness~hrf_bird$distance_m.buildings
         , family='poisson')
Anova(pm3, type='II')
# not significant


pm4<-glm(hrf_bird$richness~hrf_bird$nlcdCls.x
         , family='poisson')
Anova(pm4, type='II')
# not singificant
pm5<-glm(hrf_bird$richness~hrf_bird$elevatn.x
         , family='poisson')
Anova(pm5, type='II')
# not significant

# Plot the relationship between richness and the builing distance
bldgs_birds<-ggplot(hrf_bird, aes(x = hrf_bird$distance_m.buildings, y =hrf_bird$richness)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue")
bldgs_birds+labs(title="Birds",
                 x ="Building Distance", y = "Species Richness")
