setwd("~/Documents/NEON_LTER_2018")
rm(list = ls())
library(sp)
library(raster)
library(rgdal)
library(ggplot2)
library (ggmap)
library (ggsn)
library(gridExtra)

# osmu burn frequncy  #import frequency
osmu_burn<- readOGR("./osmu_burn", "osmu_burn")
head(osmu_burn@data)

#plot osmu which includes fire frequency
# plot(osmu_burn, col=cm.colors(10), alpha=1, legend=F, main="fire frequency")
library(dplyr)

## code by daijiang
osmu_burn2 = ggplot2::fortify(osmu_burn)
sort(unique(osmu_burn2$id))
osmu_burn2<-left_join(osmu_burn2, dplyr::select(osmu_burn@data, id = ID, burns.time))### you have to put burns.time to the old data frame
# filter(osmu_burn2, id == 59)
head(osmu_burn2)
unique(osmu_burn2$id)
unique(as.data.frame(osmu_burn)$ID)

# create new column filled with default colour--- catagoried! don't used
# osmu_burn2$frequency1="middle"
# osmu_burn2$frequency1[osmu_burn$burns.time>=6]="high"
# osmu_burn2$frequency1[osmu_burn$burns.time<=3]="low"
# osmu_burn$burn=osmu_burn$burns.time

ggplot(osmu_burn2) +
  geom_point(aes(x=long,y=lat,color = burns.time),size = 3) +
  scale_color_gradient(low = 'blue', high = 'red') +
  theme_bw()