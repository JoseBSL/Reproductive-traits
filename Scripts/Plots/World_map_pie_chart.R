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


#read metadata with coordinates 
metadata <- read.csv("Data/Data_processing/metadata.csv", stringsAsFactors = F)
str(metadata)
metadata_sub <- metadata[,c(1:6)]

#Fix some coordinates manually, this locations have more than one
#fore representation purposes I will use just one
metadata_sub[6,3] <- "1.575532"
metadata_sub[6,4] <- "52.762395"
metadata_sub[7,3] <- "9.1"
metadata_sub[7,4] <- "56.1"
metadata_sub[29,3] <- "-6.16895"
metadata_sub[29,4] <- "37.234966"


#FIRST I TRY WITH A SUBSET (UNIQUE CASES)
#Now read networks
#read data (still working on it)
all <- read.csv("Data/Data_processing/all.csv", stringsAsFactors = F)

all <- all[,c(1,4,6,7,8,15)]
str(all)
library(reshape2)
library(dplyr)

#Group breeding system per metaweb or web
all_summary_breeding_system <- dcast(all,Id~Breeding_system,value.var = "Breeding_system")
#There is one NA network because Kaiser-Bunbury 2014 hasnt been done yet
#I still have to work with that network or remove it
#In the meanwhile I'll work with 28 networks

all_summary_breeding_system <- all_summary_breeding_system[-c(6,30),]

#Select what breeding system to show








#REMEMBER I STILL HAVE TO SUBSET THE UNQUE CASES OF THE LONG FORMAT DATA PER SITE!



library(ggplot2)
library(scatterpie)

set.seed(123)
long <- rnorm(50, sd=100)
lat <- rnorm(50, sd=50)
d <- data.frame(long=long, lat=lat)
d <- with(d, d[abs(long) < 150 & abs(lat) < 70,])
n <- nrow(d)
d$region <- factor(1:n)
d$A <- abs(rnorm(n, sd=1))
d$B <- abs(rnorm(n, sd=2))
d$C <- abs(rnorm(n, sd=3))
d$D <- abs(rnorm(n, sd=4))
d$radius <- 6 * abs(rnorm(n))
head(d)

##          long        lat region          A        B        C        D
## 1  -56.047565  12.665926      1 0.71040656 2.887786 1.309570 2.892264
## 2  -23.017749  -1.427338      2 0.25688371 1.403569 1.375096 4.945092
## 4    7.050839  68.430114      3 0.24669188 0.524395 3.189978 5.138863
## 5   12.928774 -11.288549      4 0.34754260 3.144288 3.789556 2.295894
## 8 -126.506123  29.230687      5 0.95161857 3.029335 1.048951 2.471943
## 9  -68.685285   6.192712      6 0.04502772 3.203072 2.596539 4.439393
##      radius
## 1 6.4847970
## 2 3.7845247
## 4 0.6818394
## 5 9.1974120
## 8 3.1267039
## 9 2.9392227

world <- map_data('world')
p <- ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill=NA, color="black") +
  coord_quickmap()
p + geom_scatterpie(aes(x=long, y=lat, group=region, r=radius),
                    data=d, cols=LETTERS[1:4], color=NA, alpha=.8) +
  geom_scatterpie_legend(d$radius, x=-160, y=-55)




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




library(ggplot2)
library(scatterpie)
set.seed(123)
long <- rnorm(50, sd=100)
lat <- rnorm(50, sd=50)
d <- data.frame(long=long, lat=lat)
d <- with(d, d[abs(long) < 150 & abs(lat) < 70,])
n <- nrow(d)
d$region <- factor(1:n)
d$A <- abs(rnorm(n, sd=1))
d$B <- abs(rnorm(n, sd=2))
d$C <- abs(rnorm(n, sd=3))
d$D <- abs(rnorm(n, sd=4))
d$radius <- 6 * abs(rnorm(n))
head(d)
world <- map_data('world')
p <- ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill=NA, color="black") +
  coord_quickmap()
p + geom_scatterpie(aes(x=long, y=lat, group=region, r=radius),
                    data=d, cols=LETTERS[1:4], color=NA, alpha=.8) +
  geom_scatterpie_legend(d$radius, x=-160, y=-55)

