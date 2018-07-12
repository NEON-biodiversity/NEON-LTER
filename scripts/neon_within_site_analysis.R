# Title: Modeling
# Author: Cameo Chilcutt
# Date: 10 July 2018

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
names(arc_plant)
pairs.panels(arc_plant[c(2:5,8,10:15)])

# This is the first linear model. First is the plant species richness data ~ all your predictor variables.
pm1<-lm(arc_plant$ln.richness~arc_plant$distance_m.buildings+
          arc_plant$distance_m.pipeline+
          arc_plant$distance_m.thermokarst+
          arc_plant$distance_m.roads+
          arc_plant$nlcdCls+
          arc_plant$elevatn)
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
          arc_plant$elevatn)
summary(pm2)
#significant variables: roads and herbaceous 

#Reduced Model 2 - remove buildings
pm3<-lm(arc_plant$ln.richness~
          arc_plant$distance_m.thermokarst+
          arc_plant$distance_m.roads+
          arc_plant$nlcdCls+
          arc_plant$elevatn)
summary(pm3)
#significant variables: roads and herbaceous 

#Reduced Model 3 - remove thermokarst
pm4<-lm(arc_plant$ln.richness~
          arc_plant$distance_m.roads+
          arc_plant$nlcdCls+
          arc_plant$elevatn)
summary(pm4)
#significant variables: roads and herbaceous 

#Reduced Model 4: remove elevation
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
anova(pm6, pm7) #No P-Value

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
rich.road<-ggplot(arc_plant, aes(x = arc_plant$distance_m.roads, y =arc_plant$ln.richness)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue")
rich.road+labs(title="Plants",
               x ="Distance to Road", y = "Species Richness")

# Since land cover is also significant we should plot the richness and look at the relationship between them.
plot(arc_plant$nlcdCls, arc_plant$ln.richness,main="Plants", 
     xlab="Landcover Type", ylab="Species Richness")

#There is an outlier, but we are going to leave it because it does not significantly skew the data. Plus if it is removed, it gets rid of the box shape on the shrubScrub variable.
################################################################################
#Correlation Tests this computes all pairwise correlations using Pearson's correlation test.
pairs.panels(bird.rich.max[c(5,8:13)])

# This is the second linear model. First is the bird species richness data ~ all your predictor variables.
bm1<-lm(bird.rich.max$ln.richness~
          bird.rich.max$distance_m.buildings+
          bird.rich.max$distance_m.thermokarst+
          bird.rich.max$distance_m.pipeline+
          bird.rich.max$distance_m.roads+
          bird.rich.max$nlcdCls+
          bird.rich.max$elevatn)
summary(bm1)
#Significant variables: roads

# Normality of Residuals
qqPlot(bm1)
plot(bm1)

#Reduced model 1 - remove pipeline
bm2<-lm(bird.rich.max$ln.richness~
          bird.rich.max$distance_m.buildings+
          bird.rich.max$distance_m.thermokarst+
          bird.rich.max$distance_m.roads+
          bird.rich.max$nlcdCls+
          bird.rich.max$elevatn)
summary(bm2)
#Significant variables: roads

#Reduced model 2 - remove buildings and elevation
bm3<-lm(bird.rich.max$ln.richness~
          bird.rich.max$distance_m.thermokarst+
          bird.rich.max$distance_m.roads+
          bird.rich.max$nlcdCls)
summary(bm3)
#Significant variables: roads & intercept

#Reduced model 3 - remove thermokarst
bm4<-lm(bird.rich.max$ln.richness~
          bird.rich.max$distance_m.roads+
          bird.rich.max$nlcdCls)
summary(bm4)
#Significant variables: roads & intercept

#Reduced model 4 - remove land cover
bm5<-lm(bird.rich.max$ln.richness~
          bird.rich.max$distance_m.roads)
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
anova(bm3, bm4) #**Significant**
anova(bm3, bm5) #Not Significant
anova(bm4, bm5) #Not Significant
# Check out these example scripts to assess normality, outliers, etc: https://www.statmethods.net/stats/rdiagnostics.html
# use qqplot() at least
# do data look normal? if not, then we will need to transform
