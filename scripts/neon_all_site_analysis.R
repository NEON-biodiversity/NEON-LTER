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
load("G:\\My Drive\\NEON_LTER_2018\\data\\final_data\\neon\\neon_all_site_prep.RData")

#Correlation Tests this computes all pairwise correlations using Pearson's correlation test
library(psych)
names(all_plant)
pairs.panels(all_plant[c(3:10,14:15)])

# This is the first linear model. First is the plant species richness data ~ all your predictor variables.
apm1<-lm(all_plant$richness~all_plant$bldgs_dist+
           all_plant$roads_dist+
           all_plant$nlcdCls+
           all_plant$elevatn+
           all_plant$precipitation+
           all_plant$temperature)
summary(apm1)
#significant variables: roads, herbaceous, and temperature

#Reduced Model 2 - removed bldgs and elevatn
apm2<-lm(all_plant$richness~
           all_plant$roads_dist+
           all_plant$nlcdCls+
           all_plant$precipitation+
           all_plant$temperature)
summary(apm2)
#significant variables: roads, herbaceous, and temperature

#Reduced Model 3 - removed precipitation
apm3<-lm(all_plant$richness~
           all_plant$roads_dist+
           all_plant$nlcdCls+
           all_plant$temperature)
summary(apm3)
#significant variables: intercept, roads, sedgeHerbaceous, deciduousForest, evergreenForest, grasslandHerbaceous, mixedForest, woodyWetlands, and temperature

#Reduced Model 4 - model individual significant variables
apm4<-lm(all_plant$richness~
           all_plant$roads_dist)
summary(apm4)
#significant variables: intercept

#Reduced Model 5 - model individual significant variables
apm5<-lm(all_plant$richness~
           all_plant$nlcdCls+ all_plant$siteID)
summary(apm5)
#significant variables: intercept, deciduousForest, evergreenForest,  mixedForest, and woodyWetlands

#Reduced Model 6 - model individual significant variables
apm6<-lm(all_plant$richness~
           all_plant$temperature)
summary(apm6)
#significant variables: intercept and temperature

anova(apm1,apm2)#Not Significant
anova(apm1,apm3)#Not Significant
anova(apm1,apm4)#**Significant**
anova(apm1,apm5)#**Significant**
anova(apm1,apm6)#**Significant**
anova(apm2,apm3)#Not Significant
anova(apm2,apm4)#**Significant**
anova(apm2,apm5)#**Significant**
anova(apm2,apm6)#**Significant**
anova(apm3,apm4)#**Significant**
anova(apm3,apm5)#**Significant**
anova(apm3,apm6)#**Significant**
anova(apm4,apm5)#**Significant**
anova(apm5,apm6)#Not Significant

#plots
plant_landcover<-ggplot(all_plant, aes(x =nlcdCls , y =richness, fill=siteID)) +
  geom_boxplot() +
  stat_smooth(method = "lm", col = "blue")
plant_landcover+labs(title="Richness",
                     x ="Land Cover Type", y = "plant richness")

plant_temperature<-ggplot(all_plant, aes(x =temperature , y =richness, fill=siteID)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue")
plant_temperature+labs(title="Richness",
                     x ="Temperature", y = "plant richness")

###############################################################################
#Birds
abm1<-lm(all_bird$richness~all_bird$bldgs_dist+
           all_bird$roads_dist+
           all_bird$nlcdCls+
           all_bird$elevatn+
           all_bird$precipitation+
           all_bird$temperature)
summary(abm1)

#significant variables: deciduousForest, evergreenForest, mixedForest, and woodyWetlands

#Reduced Model - remove bldgs and roads
abm2<-lm(all_bird$richness~
           all_bird$nlcdCls+
           all_bird$elevatn+
           all_bird$precipitation+
           all_bird$temperature)
summary(abm2)
#significant variables: deciduousForest, evergreenForest, mixedForest, and woodyWetlands

#Reduced Model - remove elevatn
abm3<- lm(all_bird$richness~
            all_bird$nlcdCls+
            all_bird$precipitation+
            all_bird$temperature)
summary(abm3)
#significant variables: intercept, deciduousForest, evergreenForest, grasslandHerbaceous, mixedForest, and woodyWetlands

#Reduced Model - model only significant variable
abm4<- lm(all_bird$richness~
            all_bird$nlcdCls+all_bird$siteID)
summary(abm4)
#significant variables: intercept, deciduousForest, evergreenForest, grasslandHerbaceous, mixedForest, and woodyWetlands

anova(abm1, abm2)#Not Significant
anova(abm1, abm3)#Not Significant
anova(abm1, abm4)#Not Significant
anova(abm2, abm3)#Not Significant
anova(abm3, abm4)#Not Significant

################################################################################
#Correlation Tests
names(kth)
pairs.panels(kth[c(3:9,16:17)])

#All Data
hist(kth_final$richness)
kth_final$ln.richness<-log(kth_final$richness)
hist(kth_final$ln.richness)

#Complex Model
head(kth_final)
arm<-lm(kth_final$ln.richness~
          kth_final$nlcdCls+
          kth_final$siteID+
          kth_final$bldgs_dist+
          kth_final$roads_dist+
          kth_final$severe_dist+
          kth_final$temperature+
          kth_final$precipitation)
summary(arm)
#Significant variables: sedgeHerbaceous

#Reduced Model - Remove bldgs and roads
arm1<-lm(kth_final$ln.richness~
           kth_final$nlcdCls+
           kth_final$siteID+
           kth_final$severe_dist+
           kth_final$temperature+
           kth_final$precipitation)
summary(arm1)
#Significant variables: sedgeHerbaceous

#Reduced Model - Remove siteID
arm2<-lm(kth_final$ln.richness~
           kth_final$nlcdCls+
           kth_final$severe_dist+
           kth_final$temperature+
           kth_final$precipitation)
summary(arm2)
#Significant variables: sedgeHerbaceous

#Reduced Model - Remove severe_dist
arm3<-lm(kth_final$ln.richness~
           kth_final$nlcdCls+
           kth_final$temperature+
           kth_final$precipitation)
summary(arm3)
#Significant variables: sedgeHerbaceous

#Reduced Model - Remove precipitation
arm4<-lm(kth_final$ln.richness~
           kth_final$nlcdCls+
           kth_final$temperature)
summary(arm4)
#Significant variables: intercept and sedgeHerbaceous

#Reduced Model - remove temperature
arm5<-lm(kth_final$ln.richness~
           kth_final$nlcdCls)
summary(arm5)
#Significant variables: intercept, sedgeHerbaceous, deciduousForest, evergreenForest, grasslandHerbaceous, mixedForest, and woodyWetlands

#Reduced Model - remove ncldCls
arm6<-lm(kth_final$ln.richness~
           kth_final$temperature)
summary(arm6)
#Significant variables: intercept and temperature

anova(arm, arm1)#Not Significant
anova(arm, arm2)#Not Significant
anova(arm, arm3)#Not Significant
anova(arm, arm4)#Not Significant
anova(arm, arm5)#Not Significant
anova(arm, arm6)#Not Significant
anova(arm1, arm2)#Not Significant
anova(arm1, arm3)#Not Significant
anova(arm1, arm4)#Not Significant
anova(arm1, arm5)#Not Significant
anova(arm1, arm6)#Not Significant
anova(arm2, arm3)#Not Significant
anova(arm2, arm4)#Not Significant
anova(arm2, arm5)#Not Significant
anova(arm2, arm6)#Not Significant
anova(arm3, arm4)#Not Significant
anova(arm3, arm5)#Not Significant
anova(arm3, arm6)#Not Significant
anova(arm4, arm5)#Not Significant
anova(arm4, arm6)#**Significant**
anova(arm5, arm6)#Not Significant

#plots
rich.land<-ggplot(kth_final, aes(x = nlcdCls , y =ln.richness, fill=siteID)) + 
  geom_boxplot() +
  stat_smooth(method = "lm", col = "blue")
rich.land+labs(title="Richness",
              x ="Land Cover Type", y = "Richness")

rich.temp<-ggplot(kth_final, aes(x = temperature , y =ln.richness, fill=siteID)) + geom_point() +
  stat_smooth(method = "lm", col = "blue")
rich.temp+labs(title="Richness",
               x ="Temperature", y = "Richness")
################################################################################
# include site as a function
head(kth)

#Correlation Tests this computes all pairwise correlations using Pearson's correlation test
names(kth)
pairs.panels(kth[c(2:9,14:17)])
head(kth)

# Models
pms1<-lm(kth$richness.bird~kth$richness.plant+
           kth$severe_dist*kth$siteID+
           kth$ln.bldgs_dist*kth$siteID+
           kth$ln.roads_dist*kth$siteID+
           kth$temperature*kth$siteID+
           kth$precipitation*kth$siteID)
summary(pms1)
#Significant Variables: intercept, siteIDKONZ, temperature, siteIDKONZ*temp

#Reduced model - remove precipitation and bldgs
pms2<-lm(kth$richness.bird~kth$richness.plant+
           kth$severe_dist+
           kth$siteID+
           kth$ln.roads_dist+
           kth$temperature)
summary(pms2)
#significant variables: None

#Reduced model - only plants and roads
pms3<-lm(kth$richness.bird~kth$richness.plant+
           kth$ln.roads_dist)
summary(pms3)

anova(pms1,pms2) #p-value 0.16
anova(pms1, pms3)

#distance to severe disturbances by site
sev_plot<-ggplot(kth, aes(x = siteID , y =severe_dist)) + 
  geom_boxplot() +
  stat_smooth(method = "lm", col = "blue") +
  scale_y_log10()
sev_plot+labs(title="Distance to severe disturbances per site",
              x ="Richness", y = "Distance to Severe Disturbance")
#Toolik Lake has the most distance between the severe disturbance and the NEON plots. Whereas Harvard Forest and Konza Prairie have severe disturbances occuring very close or on top of the NEON plots.

#landcover and bird richness
bird_richness<-ggplot(kth, aes(x = nlcdCls , y =richness.bird, fill=siteID)) + 
  geom_boxplot() +
  stat_smooth(method = "lm", col = "blue") 
bird_richness+labs(title="Richness",
              x ="Land Cover Type", y = "Bird Richness")

#bird richness to plant richness by site
richness.bp<-ggplot(kth, aes(x = richness.plant , y =richness.bird)) + geom_point() + scale_x_log10()+
  stat_smooth(method = "lm", col = "red") 
richness.bp+labs(title="Richness",
                   x ="Land Cover Type", y = "Bird Richness")

#Correlation test
cor.test(kth$richness.bird,kth$richness.plant)

#Across sites birds have a positive correlation with plants. It is a weak correlation with a value or 0.189. 
