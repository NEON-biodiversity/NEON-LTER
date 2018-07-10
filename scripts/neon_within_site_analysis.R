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

# This is the first linear model. First is the plant species richness data ~ all your predictor variables.
pm1<-lm(plant.rich.max$ln.richness~plant.rich.max$ln.distance_m.burn+
          plant.rich.max$distance_m.buildings+
          plant.rich.max$distance_m.water.source+
          plant.rich.max$distance_m.roads+
          plant.rich.max$elevatn)
summary(pm1)
#Follow this link if you need help interpreting your summary. https://www.quora.com/How-do-I-interpret-the-summary-of-a-linear-model-in-R

# This is where we will start creating a reduced model to see if we can find more significant variables within the model. We will take out all variables with p-values above 0.4.
pm2<-lm(plant.rich.max$ln.richness~plant.rich.max$ln.distance_m.burn+
          plant.rich.max$distance_m.buildings+
          plant.rich.max$distance_m.roads+
          plant.rich.max$elevatn)
summary(pm2)

anova(pm1, pm2)

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
