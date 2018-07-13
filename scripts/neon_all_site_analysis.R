# Title: Cross-site Modeling
# Author: Cameo Chilcutt
# Date: 12 July 2018

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

#You need to load all the workspace from the prep to be able to do any modeling.
load("neon_all_site_prep.RData")

#Correlation Tests this computes all pairwise correlations using Pearson's correlation test
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
all_plant_plot<-ggplot(all_plant, aes(x =nlcdCls , y =richness)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue")
all_plant_plot+labs(title="Richness",
                     x ="Land Cover Type", y = "plant richness")
