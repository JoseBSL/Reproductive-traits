
#Load Libraries
library(readxl)
library(ggplot2)
library(dbplyr)
library(scatterpie)
library(reshape2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# READ LONG FORMAT DATA 
world <- ne_countries(returnclass = "sf")

Long_format_metawebs <- readRDS("OLD/Long_format_metawebs.RData")

#SUBSET UNIQUE CASES PER NETWORK
Long_format_subset <-  unique(Long_format_metawebs[,c("Plant_species","Id")])
#It seems to work fine
#and the total number of species makes sense, a bit more than the total unique cases for all the networks


#READ TRAIT DATA IN ORDER TO MERGE
data <- read.csv("Data/Data_processing/Pollinator_species_names/all.csv", stringsAsFactors = F)
data <- data[,c(1,4,6,7,8,51,27)]
levels(as.factor(data$Breeding_system))
#Filter data for columns of interest
data_filtered <- subset(data, Info_level=="flower"|Info_level=="capitulum"|Info_level=="inflorescence"|Info_level=="NA")
#Rename column to merge
colnames(data_filtered)[1] <- "Plant_species"
#Convert to factor 
data_filtered <- as.data.frame(data_filtered, stringsAsFactors = TRUE)

#Merge the two dataframes (trait data and the unique cases per network)
all_species_per_network <- merge(Long_format_subset, data_filtered,  by=c("Plant_species","Id"), all = T )

#Group breeding system per metaweb or web
all_summary_life_form <- dcast(all_species_per_network,Id~life_form,value.var = "life_form")
#Cleaning NA and one unfinished network
all_summary_life_form <- all_summary_life_form[-c(6,30),-c(6)]
#Removing ".csv" from the Id to megere with coordinates
all_summary_life_form$Id <- gsub("\\..*","",all_summary_life_form$Id)

#Now merge with metadata to add coordinates to plot it
metadata <- read.csv("Data/Data_processing/Metadata/metadata.csv", stringsAsFactors = F)
str(metadata)
metadata_sub <- metadata[,c(1:6,11)]
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
metadata_sub <- metadata_sub[-14,]
colnames(metadata_sub)[1] <- "Id"

str(metadata_sub)

metadata_sub$longitude <- as.numeric(metadata_sub$longitude)
metadata_sub$latitude <- as.numeric(metadata_sub$latitude)

library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)


ggplot(data = world) +
  geom_sf(fill="antiquewhite", color="grey49") +
  xlab("Longitude") + ylab("Latitude") +
  geom_jitter(data=metadata_sub, aes(longitude,latitude), 
  shape = 10, colour = "black", fill = "white",size = 2, 
  stroke = 1,width = 0.2, height = 3)+ 
  theme(panel.grid.major = element_line(color = gray(.5), 
 linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))





ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill="antiquewhite", color="grey49") +
  coord_quickmap()+ylab("Latitude")+ xlab("Longitude")+coord_equal() + 
  coord_sf( ylim = c(-60, 100), expand = FALSE)+
  geom_jitter(data=metadata_sub, aes(longitude,latitude), shape = 10, colour = "black", 
              fill = "white",size = 2, stroke = 1,width = 0.2, height = 3)+
  theme(panel.grid.major = element_line(color = gray(.5), 
linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))

ggplot() + 
  geom_sf(data = world, show.legend = F) 

ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen")+
  theme(panel.grid.major = element_line(color = gray(.5), 
                                                                                      linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))
