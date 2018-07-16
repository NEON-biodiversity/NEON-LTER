# Title: Cross-site Modeling
# Author: Cameo Chilcutt
# Date: 12 July 2018

# Set file paths
rm(list=ls())
setwd("G:\\My Drive\\NEON_LTER_2018\\data\\raw_data")

#Close graphics devices
graphics.off()

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
load("neon_all_site_prep.RData")

#Correlation Tests this computes all pairwise correlations using Pearson's correlation test
library(psych)
names(all_plant)
pairs.panels(all_plant[c(3:9)])

# This is the first linear model. First is the plant species richness data ~ all your predictor variables.
apm1<-lm(all_plant$richness~all_plant$bldgs_dist+
           all_plant$roads_dist+
           all_plant$nlcdCls+
           all_plant$elevatn)
summary(apm1)
#significant variables: deciduousForest, evergreenForest, grasslandHerbaceous, mixedForest, and woodyWetlands

#Reduced Model 2 - removed bldgs
apm2<-lm(all_plant$richness~
           all_plant$roads_dist+
           all_plant$nlcdCls+
           all_plant$elevatn)
summary(apm2)
#significant variables: deciduousForest, grasslandHerbaceous, mixedForest, and woodyWetlands

#Reduced Model 3 - removed roads
apm3<-lm(all_plant$richness~
           all_plant$nlcdCls+
           all_plant$elevatn)
summary(apm3)
#significant variables: deciduousForest, grasslandHerbaceous, and woodyWetlands

#Reduced Model 4 - removed elevation
apm4<-lm(all_plant$richness~
           all_plant$nlcdCls)
summary(apm4)
#significant variables: intercept, deciduousForest, evergreenForest grasslandHerbaceous, mixedForest, and woodyWetlands

anova(apm1,apm2)
anova(apm1,apm3)
anova(apm1,apm4)
anova(apm2,apm3)
anova(apm2,apm4)
anova(apm3,apm4)

#plots
all_plant_plot<-ggplot(all_plant, aes(x =nlcdCls , y =richness, fill=siteID)) + 
  geom_boxplot() +
  stat_smooth(method = "lm", col = "blue")
all_plant_plot+labs(title="Richness",
                     x ="Land Cover Type", y = "plant richness")


#Birds
abm1<-lm(all_bird$richness~all_bird$bldgs_dist+
           all_bird$roads_dist+
           all_bird$nlcdCls+
           all_bird$elevatn)
summary(abm1)
#significant variables: deciduousForest, evergreenForest, grasslandHerbaceous, mixedForest, and woodyWetlands

#Reduced Model - remove bldgs and roads
abm2<-lm(all_bird$richness~
           all_bird$nlcdCls+
           all_bird$elevatn)
summary(abm2)
#significant variables: deciduousForest, evergreenForest, grasslandHerbaceous, mixedForest, and woodyWetlands

abm3<- lm(all_bird$richness~all_bird$nlcdCls)
summary(abm3)
#significant variables: intercept, deciduousForest, evergreenForest, grasslandHerbaceous, mixedForest, and woodyWetlands

anova(abm1, abm2)
anova(abm1, abm3)
anova(abm2, abm3)

#All Data
head(united)
hist(united$richness)
united$ln.richness<-log(united$richness)
hist(united$ln.richness)

arm<-lm(united$ln.richness~
          united$nlcdCls+
          united$siteID+
          united$bldgs_dist+
          united$roads_dist+
          united$severe_dist)
summary(arm)

#Reduced Model - Remove bldgs and roads
arm1<-lm(united$ln.richness~
          united$nlcdCls+
          united$siteID+
          united$severe_dist)
summary(arm1)

#Reduced Model - Remove siteID
arm2<-lm(united$ln.richness~
           united$nlcdCls+
           united$severe_dist)
summary(arm2)

#Reduced Model - Remove severe_dist
arm3<-lm(united$ln.richness~
           united$nlcdCls)
summary(arm3)

anova(arm, arm1)
anova(arm, arm2)
anova(arm, arm3)
anova(arm1, arm2)
anova(arm1, arm3)
anova(arm2, arm3)

#plots
rich.land<-ggplot(united, aes(x = nlcdCls , y =ln.richness, fill=siteID)) + 
  geom_boxplot() +
  stat_smooth(method = "lm", col = "blue")
rich.land+labs(title="Richness",
              x ="Land Cover Type", y = "Richness")

##################################################################################
# include site as a function
head(united_wide)

#Correlation Tests this computes all pairwise correlations using Pearson's correlation test
names(united_wide)
pairs.panels(united_wide[c(2:13)])
head(united_wide)

# Models
pms1<-lm(united_wide$richness.bird~united_wide$richness.plant+
           united_wide$severe_dist*united_wide$siteID+
           united_wide$ln.bldgs_dist*united_wide$siteID+
           united_wide$ln.roads_dist*united_wide$siteID)
summary(pms1)

anova(pms1)
#significant variables: plant richness, distance to severe disturbance, and roads.
pms2<-lm(united_wide$richness.bird~united_wide$richness.plant+
           united_wide$severe_dist+
           united_wide$ln.bldgs_dist+
           united_wide$siteID+
           united_wide$ln.roads_dist)
summary(pms2)
#significant variables: roads
anova(pms2)
#significant variables: plant, severe, and roads
anova(pms1,pms2) #p-value 0.4

#plots
all_plot<-ggplot(united, aes(x = taxa , y =richness, fill=siteID)) + 
  geom_boxplot() +
  stat_smooth(method = "lm", col = "blue")
all_plot+labs(title="Richness",
                    x ="Taxa", y = "Richness")


sev_plot<-ggplot(united_wide, aes(x = siteID , y =severe_dist)) + 
  geom_boxplot() +
  stat_smooth(method = "lm", col = "blue") +
  scale_y_log10()
sev_plot+labs(title="Distance to severe disturbances per site",
              x ="Richness", y = "Distance to Severe Disturbance")

bird_richness<-ggplot(united_wide, aes(x = nlcdCls , y =richness.bird, fill=siteID)) + 
  geom_boxplot() +
  stat_smooth(method = "lm", col = "blue") +
  scale_y_log10()
bird_richness+labs(title="Richness",
              x ="Land Cover Type", y = "Bird Richness")

