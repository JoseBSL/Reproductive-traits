#Worldmap plot
#I will show graphically some summaries of our data
#The best that I find so far is with pie charts on a worldmap
#the old function subplot does not work anymore

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
metadata_sub <- metadata_sub[ , -c(1)]
metadata_sub <- metadata_sub[order(metadata_sub$Id_number),] 
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
colnames(all_summary_breeding_system)[1] <- "Id"
metadata_sub <- metadata_sub[-14,]
colnames(metadata_sub)[1] <- "Id"
levels(all_summary_breeding_system$Id)
all_summary_breeding_system$Id <- gsub("\\..*","",all_summary_breeding_system$Id)


#Merge Data 
data_merged<- merge(metadata_sub, all_summary_breeding_system)

data_merged[,c(3,4,6:12)] <- sapply(data_merged[,c(3,4,6:12)],as.numeric)
sapply(data_merged, class)
data_merged$Id <- as.factor(data_merged$Id)
str(data_merged)
#Prepare data for plotting
data_merged$Hermaphroditism <- data_merged$hermaphrodite
data_merged$Dioecy <- data_merged$androdioecious + data_merged$androgynodioecious + data_merged$dioecious+
  data_merged$gynodioecious + data_merged$monoecious_dioecious + data_merged$`polygamo-dioecious`+
  data_merged$subdioecious

data_merged$Monoecy <- data_merged$andromonoecious + data_merged$gynomonoecious + data_merged$monoecious
+ data_merged$submonoecious
  
data_merged_sub <- data_merged[, c(1:5, 19:21)]
#Plot data
world <- map_data('world')

p <- ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill=NA, color="black") +
  coord_quickmap()
p +  geom_scatterpie(aes(x=longitude, y=latitude,group=Id), 
                     data = data_merged_sub, cols = colnames(data_merged_sub[,c(6:8)]),alpha=.8)



#REMEMBER I STILL HAVE TO SUBSET THE UNQUE CASES OF THE LONG FORMAT DATA PER SITE!




#EXAMPLE

unproj <- CRS("+proj=longlat +datum=WGS84") #default WGS84 projection

countries <- ne_countries(scale=110)
p <- ggplot() +  geom_polygon(data=countries, aes(x=long, y=lat, group=group),  color="white", lwd = .25)+
  geom_scatterpie(aes(x=longitude, y=latitude), 
                  data = data, cols = colnames(data[,c(5:7)]))

p

str(data)
