# Title:        osbs Within Site Analysis
# Site:         osbs Forest
# Data Sources: 
# Authors:      Phoebe Zarnetske, Cameo Arnold, Huijie Wei
# Date:         23 Aug 2018

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


load("OSBS_within_site_prep.RData")
#############################################

#Now you're ready for modeling. 
library(psych)
names(osbs_plant)
pairs.panels(osbs_plant[c(2:5,7,9:10,12:13)])
ggsave(filename = 'plants_pair.pdf', width = 10, height = 10)

# This is the first linear model. First is the plant species richness data ~ all your predictor variables.

pm1<-lm(osbs_plant$ln.richness~osbs_plant$distance_m.building+
          osbs_plant$distance_m.roads+
          osbs_plant$nlcdCls+
          osbs_plant$elevatn+
          osbs_plant$Freq)
summary(pm1)
anova(pm1)
#significant variables: landcover(nlcdCls)

#Follow this link if you need help interpreting your summary. https://www.quora.com/How-do-I-interpret-the-summary-of-a-linear-model-in-R

# We need to check the normality of this model so that we can ensure that everything is relatively normally distributed.
library(car)
qqPlot(pm1)
plot(pm1)

#Reduced Model 1 - remove roads
pm2<-lm(osbs_plant$ln.richness~osbs_plant$distance_m.building+
          osbs_plant$nlcdCls+
          osbs_plant$elevatn+
          osbs_plant$Freq)
summary(pm2)

#significant variables:landcover(nlcdCls)

#Reduced Model 2 - remove building
pm3<-lm(osbs_plant$ln.richness~
          osbs_plant$nlcdCls+
          osbs_plant$elevatn+
          osbs_plant$Freq)
summary(pm3)
#significant variables:  landcover(nlcdCls)


#Reduced Model 3 - remove elevation
pm4<-lm(osbs_plant$ln.richness~
          osbs_plant$nlcdCls+
          osbs_plant$Freq)
summary(pm4)
#significant variables:  landcover(nlcdCls)

#Reduced Model 4 - remove frequency
pm5<-lm(osbs_plant$ln.richness~
          osbs_plant$nlcdCls)
summary(pm5)
#significant variables:  landcover(nlcdCls)


#ANOVA
anova(pm1, pm2) #Not Significant
anova(pm1, pm3) #Not Significant
anova(pm1, pm4) #Not Significant
anova(pm1, pm5) #Not Significant
anova(pm2, pm3) #Not Significant
anova(pm2, pm4) #Not Significant
anova(pm2, pm5) #Not Significant
anova(pm3, pm4) #Not Significant
anova(pm3, pm5) #Not Significant
anova(pm4, pm5) #Not Significant


# # Plot the relationship between ln.richness and land cover 
plot(osbs_plant$nlcdCls, osbs_plant$ln.richness,main="Plants", 
     xlab="Landcover Type", ylab="Species Richness")
ggsave(filename = 'landcovertype.pdf', width = 5, height = 5)


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
## so what's the relationship betweeen the evergreen forest data and fire frequency
anova(lm(evergr$Freq~evergr$richness))
## they are not correlate
# but lets see the plot
everg.burn<-ggplot(evergr, aes(x = evergr$Freq, y =evergr$richness)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue")
rich.burn+labs(title="EvergreenForst",
               x ="Fire frequency", y = "EvergreenForest richness")
ggsave(filename = 'EvergreenForst.pdf', width = 5, height = 5)


# just for fun,Plot the relationship between richness and the burn frequency
rich.burn<-ggplot(osbs_plant, aes(x = osbs_plant$Freq, y =osbs_plant$richness)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue")
rich.burn+labs(title="Plants",
               x ="Fire frequency", y = "Species Richness")
ggsave(filename = 'plants.pdf', width = 5, height = 5)


################################################################################
#Birds
#Correlation Tests this computes all pairwise correlations using Pearson's correlation test.
names(osbs_bird)
library(psych)
pairs.panels(osbs_bird[c(2:5,7,9:10,12:13)])
ggsave(filename = 'pairs.pdf', width = 10, height = 10)
# no correlation

# This is the second linear model. First is the bird species richness data ~ all your predictor variables.
names(osbs_bird)
bm1<-lm(osbs_bird$ln.richness~
          osbs_bird$distance_m.building+
          osbs_bird$distance_m.roads+
          osbs_bird$nlcdCls+
          osbs_bird$elevatn+
          osbs_bird$Freq)
summary(bm1)
anova(bm1)
#### no significant result at all

# Normality of Residuals
qqPlot(bm1)
plot(bm1)


##################################### mammals
# This is the second linear model. First is the bird species richness data ~ all your predictor variables.
names(osbs_mammal)
mm1<-lm(osbs_mammal$ln.richness~
          osbs_mammal$distance_m.building+
          osbs_mammal$distance_m.roads+
          osbs_mammal$nlcdCls+
          osbs_mammal$elevatn+
          osbs_mammal$Freq)
summary(mm1)
anova(mm1)
#### lack of data 

# Normality of Residuals
qqPlot(bm1)
plot(bm1)

#Let's look at the relationship between bird richness and plant richness.
osbs_bird$ln.richness<-osbs_bird$ln.richness
osbs_plant$ln.richness<-osbs_plant$ln.richness
bird.plant<-merge(osbs_bird, osbs_plant, by=c("plotID", "nlcdCls", "elevatn", "distance_m.roads")) ## why using the roads not the buildings
names(bird.plant)

#bird v plant model
head(bird.plant)
bird.plant.model<-lm(bird.plant$ln.richness.x~bird.plant$ln.richness.y)
summary(bird.plant.model)

#plot
bird.plant.rich<-ggplot(bird.plant, aes(x = ln.richness.x, y =ln.richness.y)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")
bird.plant.rich+labs(title="Richness", x ="bird richness", y = "plant richness")
ggsave(filename = 'richness_plant_bird.pdf', width = 5, height = 5)
#Here we can see a slightly positive correlation between bird and plant richness. 
#
# Although the relationship is not significant, it can still play a role in the variation seen in bird richness.


#Correlation test
cor.test(bird.plant$ln.richness.x,bird.plant$ln.richness.y)

#Now that within site analysis is complete, we can move on to cross-site prep (neon_all_site_prep.R)

