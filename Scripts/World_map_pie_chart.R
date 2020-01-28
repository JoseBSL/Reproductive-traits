#This is an example combining two other example of how to plot on a map
library(rworldmap)
library(ggplot2)
library(readxl)
library(scatterpie)
library(plotly)
library(dplyr)
library(maps)
library(maptools)   #mapping: plotting
library(rgeos)      #mapping: simplifying geometry
library(rgdal)      #mapping: projecting
library(spdep)      #Moran's I, Geary's C, poly2nb
library(rnaturalearth)  #natural earth API
library(ggmap)

#read data (still working on it)
setwd("~/Reproductive Traits/")
data <- read_excel("data/data_traits.xlsx",na="")



#Creat a base plot with gpplot2
p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")

#Add map to base plot
base_world <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                                     colour="light blue", fill="light blue")
base_world_messy

longitude <- c(3.296797, 10.216667, 10.233333, 
               -68.015892, 1.575532, 9.1, 18.5,
               -20.5, 99.63806 ,135.866667,148.266667, 
               57.443254,129.493741, -71.3,
               -52, 57.43, -90.600747)

latitude <- c(42.315336, 56.066667, 56.066667, 
              -32.008985, 52.762395, 56.1, 68.35,
              74.5, 27.90139,35.166667,-36.45,
              -20.452076, 28.377248, 81.816667,
              71, -20.25, -0.290164)

locality <- c("Spain", "Denmark", "Denmark", 
              "Argentina", "England", "Denmark", 
              "Sweden", "Greenland", "China", 
              "Japan", "Australia", "Mauritius",
              "Japan", "Canada", "Greenland", "Mauritius", "Galapagos")

id <- c("bartomeus_2008_bat1ca.csv", "beck_2006.csv", "bundgaard_2003.csv", 
        "chacoff_2011.csv", "dicks_2002_1.csv", "dupont_2009_denmark.csv",
        "elberling_1999.csv", "elberling_unpublished_data.csv","fang_huang_2008.csv",
        "Inoue_1990.csv", "inouye_1988.csv", "kaiser_bunbury_2010_1.csv",
        "kato_2000.csv", "kevan_1970.csv", "lundgren_2005.csv", "Mauritius_valerie_unpublished_data.csv",
        "mcmullen_1993.csv")

Plant_height_min_m <-c(rep(1,5),rep(3,5),rep(5,7))
Plant_height_max_m <- c(rep(1,5),rep(9,5),rep(5,7))
Plant_height_mean_m <- c(rep(6,5),rep(4,5),rep(2,7))

data <- data.frame(longitude, latitude, locality,
                   id,Plant_height_min_m,Plant_height_max_m,Plant_height_mean_m)


 unproj <- CRS("+proj=longlat +datum=WGS84") #default WGS84 projection
 
 countries <- ne_countries(scale=110)
 p <- ggplot() +  geom_polygon(data=countries, aes(x=long, y=lat, group=group),  color="white", lwd = .25)+
   geom_scatterpie(aes(x=longitude, y=latitude), 
                   data = fortify(data), cols = colnames(data[,c(5:7)]))
 
p



library(rgdal)
library(ggplot2)
library(rgeos)
library(maptools)
library(grid)
library(gridExtra)

map.det<- readOGR(dsn="c:/swissBOUNDARIES3D/V200/SHAPEFILE_LV03", layer="VECTOR200_KANTONSGEBIET")
map.kt <- map.det[map.det$ICC=="CH" & (map.det$OBJECTID %in% c(1:73)),]

# Merge polygons by ID
map.test <- unionSpatialPolygons(map.kt, map.kt@data$OBJECTID)

#get centroids
map.test.centroids <- gCentroid(map.test, byid=T)
map.test.centroids <- as.data.frame(map.test.centroids)
map.test.centroids$OBJECTID <- row.names(map.test.centroids)

#create df for ggplot
kt_geom <- fortify(map.kt, region="OBJECTID")

#Plot map
map.test <- ggplot(kt_geom)+
  geom_polygon(aes(long, lat, group=group), fill="white")+
  coord_fixed()+
  geom_path(color="gray48", mapping=aes(long, lat, group=group), size=0.2)+
  geom_point(data=map.test.centroids, aes(x=x, y=y), size=2, alpha=6/10)

map.test


