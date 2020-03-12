#WORLDPLOT WITH BREEDING SYSTEMS

#Load Libraries
library(readxl)


# READ LONG FORMAT DATA
Long_format_metawebs <- readRDS("Data/RData/Long_format_metawebs.RData")

#SUBSET UNIQUE CASES PER NETWORK
Long_format_subset <-  unique(Long_format_metawebs[,c("Plant_species","Id")])
#It seems to work fine
#and the total number of species makes sense, a bit more than the total unique cases for all the networks


#READ TRAIT DATA IN ORDER TO MERGE
data <- read.csv("Data/Data_processing/all.csv", stringsAsFactors = F)
data <- all[,c(1,4,6,7,8,15,27)]
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
all_summary_breeding_system <- dcast(all_species_per_network,Id~Breeding_system,value.var = "Breeding_system")

#Cleaning NA and one unfinished network
all_summary_breeding_system <- all_summary_breeding_system[-c(6,30),]

#Removing ".csv" from the Id to megere with coordinates
all_summary_breeding_system$Id <- gsub("\\..*","",all_summary_breeding_system$Id)

#Now merge with metadata to add coordinates to plot it
metadata <- read.csv("Data/Data_processing/metadata.csv", stringsAsFactors = F)
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

#Merge coordinates and data 
data_merged <- merge(metadata_sub, all_summary_breeding_system)

data_merged[,c(3,4,6:19)] <- sapply(data_merged[,c(3,4,6:19)],as.numeric)
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

data_merged_sub <- data_merged[, c(1:6, 20:22)]
#Plot data
world <- map_data('world')


#network size fix

data_merged_sub$network_size[data_merged_sub$network_size > 47000] <- 47000

p <- ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill="white", color="black") +
  coord_quickmap()+ylab("Latitude")+ xlab("Longitude")+coord_equal() + coord_sf( ylim = c(-60, 100), expand = FALSE)
p +  geom_scatterpie(aes(x=longitude, y=latitude,group=Id, r= scale(network_size)*7), 
                     data = data_merged_sub, cols = colnames(data_merged_sub[,c(7:9)]),alpha=.8)+ 
  scale_fill_manual(breaks = colnames(data_merged_sub[,c(7:9)]),
                       labels = c("Hermaphroditism", "Dioecy", "Monoecy"),
                       values = c("Hermaphroditism" = "#4DAF4A",
                                  "Monoecy" = "#984EA3",
                                  "Dioecy" = "orange")) +labs(title = "Breeding systems",subtitle = "",
  caption = "",fill = NULL) +
  theme(legend.position = c(0.19, 0.009),
        legend.justification = c(1, 0),
        axis.ticks = element_blank(),legend.key.size = unit(0.2, "cm"))

#At the moment I cannot see all the networks, think about correct for network size


#EXAMPLE


world <- map_data('world')
ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill="grey97", color="grey") +
  geom_scatterpie(data = data_merged_sub, 
                  aes(longitude, latitude),
                  cols = colnames(data_merged_sub[,c(7:9)]),
                  alpha = 0.5) +
  scale_fill_manual(
    breaks = colnames(data_merged_sub[,c(7:9)]),
    labels = c("Hermaphroditism", "Dioecy", "Monoecy"),
    values = c("Hermaphroditism" = "orange",
               "Monoecy" = "cyan",
               "Dioecy" = "black"),
    shape= c()) +
  labs(title = "Breeding systems",
       subtitle = "",
       caption = "28 networks",
       fill = NULL) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = c(1.20, 0.02),
        legend.justification = c(1, 0),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text=element_text(size=6))



