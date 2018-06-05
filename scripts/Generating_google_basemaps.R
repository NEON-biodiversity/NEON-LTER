##This script is to download an appropriate google map, add a shapefile, 
##and then include a scale bar and north arrow.
##By: Beth Gerstner

#Download packages ggplot2, ggsn, ggmap, rgdal

#Load in libraries
library(ggplot2)
library (ggmap)
library (ggsn)
library(rgdal)

##Extent of Toolik Field Station
##NewExtent<-extent(-149.600, -149.589, 68.625, 68.630)

#Obtain basemap of the Arctic field site through google maps 
#(can also generate map types "roadmap", "terrain", "satellite", "hybrid")

arctic_map <- get_map(location = c(lon = -150.5, lat = 69.3),
                      color = "color",
                      source = "google",
                      maptype = "terrain",
                      zoom = 7)

#set working directory to folder with appropriate shapefile
setwd("/Users/bethgerstner/Desktop/anaktuvuk_burn_perim$data")

#read in shapefile (in this case it is of burn perimeters of the field site)
burn_path <- "/Users/bethgerstner/Desktop/anaktuvuk_burn_perim$data"
burn_name <- "progression_perimeters_0822-0930"
burn_extent <- readOGR(burn_path, burn_name)

#Change projection of the data so that the shapefile can be placed on top of the google map
spTransform(burn_extent, CRS("+proj=longlat +datum=WGS84"))

#Visualize the shapefile and make sure it loaded in correctly
ggplot(data = burn_extent, aes(x = long, y = lat, group = group)) + geom_path()

#Use the function 'fortify' to turn this shapefile into a dataframe so it's easy to plot on map
burn_extent.f = fortify(burn_extent)

#Generate a full map with both the google map, polygon, north arrow and scale bar.
ggmap(arctic_map) + 
  geom_polygon(aes(x = long, y = lat, group = group), 
               data = burn_extent.f,
               alpha = 0.8, 
               color = "blue",
               size = 0.2) + xlab("Longitude")+ ylab("Latitude") + ggtitle("Toolik Field Station: Digitized Burn Perimeter")+ theme(plot.title = element_text(hjust = .5)) + scalebar(x.min= -153.3, x.max= -153.0, y.min= 68.35, y.max= 68.42, dist= 50, location= "bottomleft", dd2km = TRUE, st.size=4, st.dist = .6, height = 0.5, model="WGS84")
+ north2(arctic_map, x = 0.30, y = 0.31, scale = 0.15, symbol = 1)


