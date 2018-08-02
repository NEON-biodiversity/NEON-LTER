# Title: Modeling
# Author: Cameo Chilcutt
# Date: 10 July 2018
rm=(list=ls())
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

#You need to load all the workspace from the prep to be able to do any modeling.
load("neon_within_site_prep.RData")

#Correlation Tests this computes all pairwise correlations using Pearson's correlation test.
library(psych)
names(arc_plant)
pairs.panels(arc_plant[c(2:5,8,10:17)])

# This is the first linear model. First is the plant species richness data ~ all your predictor variables.
pm1<-lm(arc_plant$ln.richness~arc_plant$distance_m.buildings+
          arc_plant$distance_m.pipeline+
          arc_plant$distance_m.thermokarst+
          arc_plant$distance_m.roads+
          arc_plant$nlcdCls+
          arc_plant$elevatn+
          arc_plant$tool_ppt+
          arc_plant$tool_tmean)
summary(pm1)
#significant variables: roads and herbaceous 

#Follow this link if you need help interpreting your summary. https://www.quora.com/How-do-I-interpret-the-summary-of-a-linear-model-in-R

# We need to check the normality of this model so that we can ensure that everything is relatively normally distributed.
library(car)
qqPlot(pm1)
plot(pm1)

#Reduced Model 1 - remove pipeline
pm2<-lm(arc_plant$ln.richness~arc_plant$distance_m.buildings+
          arc_plant$distance_m.thermokarst+
          arc_plant$distance_m.roads+
          arc_plant$nlcdCls+
          arc_plant$elevatn+
          arc_plant$tool_ppt+
          arc_plant$tool_tmean)
summary(pm2)
#significant variables: roads and herbaceous 

#Reduced Model 2 - remove buildings
pm3<-lm(arc_plant$ln.richness~
          arc_plant$distance_m.thermokarst+
          arc_plant$distance_m.roads+
          arc_plant$nlcdCls+
          arc_plant$elevatn+
          arc_plant$tool_ppt+
          arc_plant$tool_tmean)
summary(pm3)
#significant variables: roads and herbaceous 

#Reduced Model 3 - remove thermokarst
pm4<-lm(arc_plant$ln.richness~
          arc_plant$distance_m.roads+
          arc_plant$nlcdCls+
          arc_plant$elevatn+
          arc_plant$tool_ppt+
          arc_plant$tool_tmean)
summary(pm4)
#significant variables: roads and herbaceous 

#Reduced Model 4: remove elevation, temperature, and precipitation
pm5<-lm(arc_plant$ln.richness~
          arc_plant$distance_m.roads+
          arc_plant$nlcdCls)
summary(pm5)
#significant variables: roads, herbaceous, intercept

#Reduced Model 5
pm6<-lm(arc_plant$ln.richness~
          arc_plant$distance_m.roads)
summary(pm6)
#significant variables: roads and intercept

#Reduced Model 6
pm7<-lm(arc_plant$ln.richness~
          arc_plant$nlcdCls)
summary(pm7)
#significant variables: intercept

#ANOVA
anova(pm1, pm2) #Not Significant
anova(pm1, pm3) #Not Significant
anova(pm1, pm4) #Not Significant
anova(pm1, pm5) #Not Significant
anova(pm1, pm6) #Not Significant
anova(pm1, pm7) #*****Significant******
anova(pm2, pm3) #Not Significant
anova(pm2, pm4) #Not Significant
anova(pm2, pm5) #Not Significant
anova(pm3, pm4) #Not Significant
anova(pm3, pm5) #Not Significant
anova(pm4, pm5) #Not Significant
anova(pm5, pm6) #*****Significant******
anova(pm5, pm7) #*****Significant******

# Remove outliers
#roads
plot(arc_plant$distance_m.roads, arc_plant$ln.richness)

identify(arc_plant$distance_m.roads, arc_plant$ln.richness, labels=row.names(arc_plant)) 
arc_plant$plotID
#Row name is 34 this is TOOL_041 plotID in plant.rich.max. We are going to remove this data point. And any other outliers we identify.
arc_plant<-subset(arc_plant,arc_plant$plotID != "TOOL_041")
arc_plant<-subset(arc_plant,arc_plant$plotID != "TOOL_042")
arc_plant<-subset(arc_plant,arc_plant$plotID != "TOOL_043")
plot(arc_plant$distance_m.roads, arc_plant$ln.richness)


# Plot the relationship between ln.richness and the distance_m.roads
rich.road<-ggplot(arc_plant, aes(x = distance_m.roads, y = ln.richness)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue")
rich.road+labs(title="Plants",
               x ="Distance to Road", y = "Species Richness")

# Since land cover is also significant we should plot the richness and look at the relationship between them.
plot(arc_plant$nlcdCls, arc_plant$ln.richness,main="Plants", 
     xlab="Landcover Type", ylab="Species Richness")

#There is an outlier, but we are going to leave it because it does not significantly skew the data. Plus if it is removed, it gets rid of the box shape on the shrubScrub variable.

#just for fun, let's look at the relationship between richness and precipitation
rich.precip<-ggplot(arc_plant, aes(x = tool_ppt, y = ln.richness)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "green")
rich.precip+labs(title="Plants",
               x ="Precipitation", y = "Species Richness")

#just for fun, let's look at the relationship between richness and temperature
rich.temp<-ggplot(arc_plant, aes(x = tool_tmean, y =ln.richness)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "green")
rich.temp+labs(title="Plants",
                 x ="Temperature", y = "Species Richness")
#These plots show no relationship as expected from our non significant p-values indicated in the models.
################################################################################
#Birds
#Correlation Tests this computes all pairwise correlations using Pearson's correlation test.
names(arc_bird)
pairs.panels(arc_bird[c(2:5,8,10:17)])

# This is the second linear model. First is the bird species richness data ~ all your predictor variables.
bm1<-lm(arc_bird$ln.richness~
          arc_bird$distance_m.buildings+
          arc_bird$distance_m.thermokarst+
          arc_bird$distance_m.pipeline+
          arc_bird$distance_m.roads+
          arc_bird$nlcdCls+
          arc_bird$elevatn+
          arc_bird$tool_ppt+
          arc_bird$tool_tmean)
summary(bm1)

# Normality of Residuals
qqPlot(bm1)
plot(bm1)

#Reduced model 1 - remove pipeline
bm2<-lm(arc_bird$ln.richness~
          arc_bird$distance_m.buildings+
          arc_bird$distance_m.thermokarst+
          arc_bird$distance_m.roads+
          arc_bird$nlcdCls+
          arc_bird$elevatn+
          arc_bird$tool_ppt+
          arc_bird$tool_tmean)
summary(bm2)
#Significant variables: roads

#Reduced model 2 - remove buildings and elevation
bm3<-lm(arc_bird$ln.richness~
          arc_bird$distance_m.thermokarst+
          arc_bird$distance_m.roads+
          arc_bird$nlcdCls+
          arc_bird$tool_ppt+
          arc_bird$tool_tmean)
summary(bm3)
#Significant variables: roads

#Reduced model 3 - remove thermokarst
bm4<-lm(arc_bird$ln.richness~
          arc_bird$distance_m.roads+
          arc_bird$nlcdCls+
          arc_bird$tool_ppt+
          arc_bird$tool_tmean)
summary(bm4)
#Significant variables: roads

#Reduced model 4 - remove land cover, temperature and precipitation
bm5<-lm(arc_bird$ln.richness~
          arc_bird$distance_m.roads)
summary(bm5)
#Significant variables: roads & intercept

#ANOVA
anova(bm1, bm2) #Not Significant
anova(bm1, bm3) #Not Significant
anova(bm1, bm4) #Not Significant
anova(bm1, bm5) #Not Significant
anova(bm2, bm3) #Not Significant
anova(bm2, bm4) #Not Significant
anova(bm2, bm5) #Not Significant
anova(bm3, bm4) #Not Significant
anova(bm3, bm5) #Not Significant
anova(bm4, bm5) #Not Significant

#plots
bird.rich.road<-ggplot(arc_bird, aes(x = distance_m.roads, y =ln.richness)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue")
bird.rich.road+labs(title="Birds",
               x ="Distance to Road", y = "Species Richness")


# Remove outliers
#roads
plot(arc_bird$distance_m.roads, arc_bird$ln.richness)

identify(arc_plant$distance_m.roads, arc_plant$ln.richness, labels=row.names(arc_plant)) 
arc_plant$plotID
#Row name is 34 this is TOOL_041 plotID in plant.rich.max. We are going to remove this data point. And any other outliers we identify.
arc_plant<-subset(arc_plant,arc_plant$plotID != "TOOL_041")
arc_plant<-subset(arc_plant,arc_plant$plotID != "TOOL_042")
arc_plant<-subset(arc_plant,arc_plant$plotID != "TOOL_043")
plot(arc_plant$distance_m.roads, arc_plant$ln.richness)


# Plot the relationship between ln.richness and the distance_m.roads
bird_rich_road<-ggplot(arc_bird, aes(x = distance_m.roads, y =ln.richness)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue")
rich.road+labs(title="Birds",
               x ="Distance to Road", y = "Species Richness")

#just for fun, let's look at the relationship between richness and precipitation
bird.rich.precip<-ggplot(arc_bird, aes(x = tool_ppt, y =ln.richness)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "green")
bird.rich.precip+labs(title="Birds",
                 x ="Precipitation", y = "Species Richness")

#just for fun, let's look at the relationship between richness and temperature
bird.rich.temp<-ggplot(arc_bird, aes(x = tool_tmean, y =ln.richness)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "green")
bird.rich.temp+labs(title="Birds",
               x ="Temperature", y = "Species Richness")
#These plots show no relationship as expected from our non significant p-values indicated in the models.


#Let's look at the relationship between bird richness and plant richness.
arc_bird$ln.richness.b<-arc_bird$ln.richness
arc_plant$ln.richness.p<-arc_plant$ln.richness
bird.plant<-merge(arc_bird, arc_plant, by=c("plotID", "nlcdCls", "elevatn","tool_ppt", "tool_tmean", "distance_m.roads"))
names(bird.plant)

#Remove unnecessary columns
bird.plant<-bird.plant[c(-(7:17))]
names(bird.plant)
bird.plant1<- bird.plant[c(-(8:18))]
names(bird.plant1)

#bird v plant model
bird.plant.model<-lm(bird.plant$ln.richness.b~bird.plant$ln.richness.p)
summary(bird.plant.model)

#plot
bird.plant.rich<-ggplot(bird.plant1, aes(x = ln.richness.p, y =ln.richness.b)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")
bird.plant.rich+labs(title="Richness", x ="plant richness", y = "bird richness")
#Here we can see a slightly positive correlation between bird and plant richness. Although the relationship is not significant, it can still play a role in the variation seen in bird richness.

#Now, we should look at a model that incorporates distance to roads since that was the significant variable for both pm and bm models.

bpm1<-lm(bird.plant1$ln.richness.b~bird.plant1$ln.richness.p+
          bird.plant1$distance_m.roads)
summary(bpm1)

bpm2<-lm(bird.plant1$ln.richness.b~bird.plant1$ln.richness.p*
           bird.plant1$distance_m.roads)
summary(bpm2)

anova(bpm1, bpm2)

#Correlation test
cor.test(bird.plant1$ln.richness.b,bird.plant1$ln.richness.p)

#Now that within site analysis is complete, we can move on to cross-site prep (neon_all_site_prep.R)
