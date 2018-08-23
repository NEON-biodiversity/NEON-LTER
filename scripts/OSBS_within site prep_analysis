#Close graphics devices
graphics.off()

# Set working directory
setwd("~/Documents/NEON_LTER_2018")
# Google drive directory: "NEON_LTER_2018/data/raw_data/neon"

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
dist<-read.csv(("neon_plotid_dist2disturbance_osbs.csv"), stringsAsFactors = FALSE)

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
rich<-rbind(birds, plants,mammals)

#subset data for OSBS
unique(rich$siteID)
#use for adding OSBS data
osbs_rich<-rich[rich$siteID=="OSBS",]
head(osbs_rich)
str(osbs_rich)

#Take the maximum richness value before merging with distance data
names(osbs_rich)
osbs_rich_max<-osbs_rich%>%
  group_by(plotID, taxa) %>%
  slice(which.max(richness))
head(osbs_rich_max)
osbs_rich_max<-data.frame(osbs_rich_max)
unique(osbs_rich_max$plotID)
write.csv(osbs_rich_max, file="richness_max_osbs.csv")
#some plotIDs are NA for richness because they have not been surveyed yet.

#subset
keep=c("siteID", "plotID", "richness", "taxa")
osbs_rich_max<-osbs_rich_max[,keep]
head(osbs_rich_max)

#merge with distance data
dist_rich<- merge(dist, osbs_rich_max, by=c("siteID","plotID"),all.x=T)
head(dist_rich)
names(dist_rich)
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
terrestrial<- readOGR(dsn="./All_NEON_TOS_Plots_V4/All_NEON_TOS_Centroid_V4.shp")
plot(terrestrial, col=cm.colors(5), alpha=1, legend=F, main="Terrestrial Data")
head(terrestrial)

#To subset the data specifically to your site. You must use the exact name as it is written in the data. To look this up, use the following function to list all of the names of the field sites.
unique(terrestrial$siteNam)

#subset osbs
osbs_ter<-terrestrial[terrestrial$siteNam=="Ordway-Swisher Biological Station",]
summary(osbs_ter)
names(osbs_ter)
plot(osbs_ter)

#subset elevation, slope, aspect, and landcover from NEON terrestrial data
myvars <- c("elevatn", "siteID", "plotID", "nlcdCls", "slpGrdn", "slpAspc")
env <- osbs_ter[myvars]
head(env)
str(env)

#Currently, it is in latitude and longitude, but in order to measure distance in meters we need to reproject the data into UTMs. You should look up the appropriate zone for your site. For osbs we needed UTM Zone 17. 
osbs_ter_env<-spTransform(env, CRS("+proj=utm +zone=17 ellps=WGS84"))
str(osbs_ter_env)
osbs_ter_env<-data.frame(osbs_ter_env)

#We need to make sure we have one value for slope and elevation before proceeding. This shows the duplicate entries by plotID
osbs_ter_env[duplicated(osbs_ter_env$plotID),]

#Take the average of slp, asp, and elev by plotID.
attach(osbs_ter_env)
osbs_ter_env<-aggregate(osbs_ter_env[c("elevatn","slpAspc","slpGrdn")],list(plotID=plotID, nlcdCls=nlcdCls),FUN=mean)
detach(osbs_ter_env)
head(osbs_ter_env)

#Check it
osbs_ter_env[duplicated(osbs_ter_env$plotID),]

write.csv(osbs_ter_env, file="~/Documents/NEON_LTER_2018/osbs_environment.csv", row.names=F)
head(osbs_ter_env)
#writing a csv is necessary to merge the files. It will NOT work without first writing a csv and importing it again. 

#merge dist_rich1 and osbs_ter_env
names(dist_rich1)
names(osbs_ter_env)
osbs<-merge(osbs_ter_env, dist_rich1, by=c("plotID", "nlcdCls", "elevatn", "slpAspc", "slpGrdn"),all=T)
head(osbs)

#Get rid of NAs for plots not sampled yet.
osbs<- subset(osbs,is.na(osbs$richness)==F)
osbs$richness

#Get rid of unused Factor levels
osbs$nlcdCls <- factor(osbs$nlcdCls)
osbs$nlcdCls
osbs$plotID <- factor(osbs$plotID)
osbs$plotID

#Check for duplicates
osbs[duplicated(osbs$plotID),]


#Now that we have a code with all of the environment, richness, and distance data we can start subsetting by taxa.
#-------------------------------------------------------------------------------
#Separate birds and plants richness
#--------------------------------------------------------------------------------
#birds
osbs_bird<-osbs[osbs$taxa=="bird",]
head(osbs_bird)

#plants
osbs_plant<-osbs[osbs$taxa=="plant",]
head(osbs_plant)

#mammals
osbs_mammal<-osbs[osbs$taxa=="mammal",]
head(osbs_mammal)
#################################################################################
# take a look at the relationship between plant richness and elevation
ep <- ggplot(osbs_plant, aes(x=elevatn, y=richness)) +
  geom_point(aes()) 
ep

# log transform for scatter plot
epl <- ggplot(osbs_plant, aes(x=elevatn, y=richness)) +
  geom_point(aes()) + scale_y_log10()  
epl

# take a look at the relationship between bird richness and elevation, by year
er <- ggplot(osbs_bird, aes(x=elevatn, y=richness)) +
  geom_point(aes()) 
er

# log transform for scatter plot
erl <- ggplot(osbs_bird, aes(x=elevatn, y=richness)) +
  geom_point(aes()) + scale_y_log10()  
erl

# take a look at the relationship between mammal richness and elevation, by year
em <- ggplot(osbs_mammal, aes(x=elevatn, y=richness)) +
  geom_point(aes()) 
em

# log transform for scatter plot
eml <- ggplot(osbs_mammal, aes(x=elevatn, y=richness)) +
  geom_point(aes()) + scale_y_log10()  
eml

##**Cameo stopped here in reviewing this code.**

#################################################################################
#Histograms

hist(osbs_plant$richness)
hist(osbs_bird$richness)
hist(osbs_mammal$richness)

#################################################################################
#log transformations

#plant
osbs_plant$ln.richness<-log(osbs_plant$richness)
head(osbs_plant)
hist(osbs_plant$ln.richness)


#birds
osbs_bird$ln.richness<-log(osbs_bird$richness)
head(osbs_bird)
hist(osbs_bird$ln.richness)

#mammals
osbs_mammal$ln.richness<-log(osbs_mammal$richness)
head(osbs_mammal)
hist(osbs_mammal$ln.richness)

save.image("neon_within_site_prep.RData")
#############################################

#Now you're ready for modeling. See script neon_within_site_analysis.


# This is the first linear model. First is the plant species richness data ~ all your predictor variables.
pm1<-lm(osbs_plant$ln.richness~osbs_plant$distance_m.building+
          osbs_plant$distance_m.roads+
          osbs_plant$nlcdCls+
          osbs_plant$elevatn)
summary(pm1)
anova(pm1)
#significant variables: landcover(nlcdCls), 

#Follow this link if you need help interpreting your summary. https://www.quora.com/How-do-I-interpret-the-summary-of-a-linear-model-in-R

# We need to check the normality of this model so that we can ensure that everything is relatively normally distributed.
library(car)
qqPlot(pm1)
plot(pm1)

#Reduced Model 1 - remove building
pm2<-lm(osbs_plant$ln.richness~osbs_plant$distance_m.roads+
          osbs_plant$nlcdCls+
          osbs_plant$elevatn)
summary(pm2)
anova(pm2)
#significant variables: landcover

#Reduced Model 2 - remove roads
pm3<-lm(osbs_plant$ln.richness~osbs_plant$nlcdCls+
          osbs_plant$elevatn)
summary(pm3)
anova(pm3)
#significant variables:  landcover


#Reduced Model 3 - remove elevath
pm4<-lm(osbs_plant$ln.richness~osbs_plant$nlcdCls)
summary(pm4)
anova(pm4)
#significant variables:  landcover

#ANOVA
anova(pm1, pm2) #Not Significant
anova(pm1, pm3) #Not Significant
anova(pm1, pm4) #Not Significant

anova(pm2, pm3) #Not Significant
anova(pm2, pm4)#Not Significant

anova(pm3, pm4) #Not Significant


# Plot the relationship between ln.richness and the distance_m.roads
rich.road<-ggplot(osbs_plant, aes(x = distance_m.roads, y = ln.richness)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue")
rich.road+labs(title="Plants",
               x ="Distance to Road", y = "Species Richness")

# Since land cover is also significant we should plot the richness and look at the relationship between them.
plot(osbs_plant$nlcdCls, osbs_plant$ln.richness,main="Plants", 
     xlab="Landcover Type", ylab="Species Richness")


osbs_plant$nlcdCls
# Compare within landcover types
# Create objects for each landcover type
emerg <- subset(osbs_plant, nlcdCls=='emergentHerbaceousWetlands')
evergr <- subset(osbs_plant, nlcdCls=='evergreenForest')
woody <- subset(osbs_plant, nlcdCls=='woodyWetlands')

# Model 1
emergevergr <- rbind(emerg,evergr)
glm1 <-glm(ln.richness~nlcdCls,family='gaussian', data=emergevergr)
Anova(glm1, type="II")
## significant p<0.001

# Model 2
emergwoody <- rbind(emerg,woody)
glm2 <-glm(ln.richness~nlcdCls,family='gaussian', data=emergwoody)
Anova(glm2, type="II")
## no significant

# Model 3
evergrwoody <- rbind(evergr,woody)
glm3 <-glm(ln.richness~nlcdCls,family='gaussian', data=evergrwoody)
Anova(glm3, type="II")
## significant p<0.001

# There is significant difference between " evergreenForest" and other landcover classes

################################################################################
#Birds
#Correlation Tests this computes all pairwise correlations using Pearson's correlation test.
names(osbs_bird)
pairs.panels(osbs_bird[c(2:5,7,9:11)])
ggsave(filename = 'pairs.pdf', width = 7, height = 7)

# This is the second linear model. First is the bird species richness data ~ all your predictor variables.
bm1<-lm(osbs_bird$ln.richness~
          osbs_bird$distance_m.building+
          osbs_bird$distance_m.roads+
          osbs_bird$nlcdCls+
          osbs_bird$elevatn)
summary(bm1)
anova(bm1)

# Normality of Residuals
qqPlot(bm1)
plot(bm1)

#Reduced model 1 - remove building
bm2<-lm(osbs_bird$ln.richness~osbs_bird$distance_m.roads+
          osbs_bird$nlcdCls+
          osbs_bird$elevatn
)
summary(bm2)
anova(bm2)
#Significant variables: elevath

#Reduced model 2 - remove roads
bm3<-lm(osbs_bird$ln.richness~
          osbs_bird$nlcdCls+
          osbs_bird$elevatn
)
summary(bm3)
anova(bm3)
#Significant variables: elevath

#Reduced model 3 - remove nlcdCls
bm4<-lm(osbs_bird$ln.richness~osbs_bird$elevatn)
summary(bm4)
anova(bm4)
#Significant variables: roads


#ANOVA
anova(bm1, bm2) #Not Significant
anova(bm1, bm3) #Not Significant
anova(bm1, bm4) #Not Significant

anova(bm2, bm3) #Not Significant
anova(bm2, bm4) #Not Significant

anova(bm3, bm4) #Not Significant

#plots
bird.rich.road<-ggplot(osbs_bird, aes(x = distance_m.roads, y =ln.richness)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue")
bird.rich.road+labs(title="Birds",
                    x ="Distance to Road", y = "Species Richness")



# Plot the relationship between ln.richness and the distance_m.roads
bird_rich_road<-ggplot(osbs_bird, aes(x = distance_m.roads, y =ln.richness)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue")
rich.road+labs(title="Birds",
               x ="Distance to Road", y = "Species Richness")


#let's look at the relationship between elevath
bird.rich.temp<-ggplot(osbs_bird, aes(x = elevatn, y =ln.richness)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "green")
bird.rich.temp+labs(title="Birds",
                    x ="Temperature", y = "Species Richness")
#These plots show no relationship as expected from our non significant p-values indicated in the models.


#Let's look at the relationship between bird richness and plant richness.
osbs_bird$ln.richness.b<-osbs_bird$ln.richness
osbs_plant$ln.richness.p<-osbs_plant$ln.richness
bird.plant<-merge(osbs_bird, osbs_plant, by=c("plotID", "nlcdCls", "elevatn", "distance_m.roads"))
names(bird.plant)

#bird v plant model
bird.plant.model<-lm(bird.plant$ln.richness.b~bird.plant$ln.richness.p)
summary(bird.plant.model)

#plot
bird.plant.rich<-ggplot(bird.plant, aes(x = ln.richness.p, y =ln.richness.b)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")
bird.plant.rich+labs(title="Richness", x ="plant richness", y = "bird richness")
#Here we can see a slightly positive correlation between bird and plant richness. 
#
# Although the relationship is not significant, it can still play a role in the variation seen in bird richness.

#Now, we should look at a model that incorporates distance to roads since that was the significant variable for both pm and bm models.

bpm1<-lm(bird.plant$ln.richness.b~bird.plant$ln.richness.p+
           bird.plant$distance_m.roads)
summary(bpm1)

bpm2<-lm(bird.plant$ln.richness.b~bird.plant$ln.richness.p*
           bird.plant1$distance_m.roads)
summary(bpm2)

anova(bpm1, bpm2)

#Correlation test
cor.test(bird.plant$ln.richness.b,bird.plant$ln.richness.p)

#Now that within site analysis is complete, we can move on to cross-site prep (neon_all_site_prep.R)

