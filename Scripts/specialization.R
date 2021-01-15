

#LOAD LIBRARIES
library(StatMatch) #Mahalanobis distance
########################################################################################################################################################
#1) LOAD NETWORK DATA
########################################################################################################################################################
#Set working directory to read files
setwd("~/R_Projects/Reproductive traits/Data/Data_networks_quantitative") 

temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, function(i){read.csv(i, check.names=FALSE)})
#Add "id" to the list to the long format data
data_id_list <- lapply(seq_along(my_data), 
                       function(x) cbind(my_data[[x]], unique.id=my_files[x]))


#For loop to melt each data frame and merge
i <- NULL
all_long <- NULL

for (i in data_id_list){
  i <- melt(i)
  all_long <- rbind(all_long, i)
}

#Renaming columns
colnames(all_long) <- c("Plant_species", "Id", "Pollinator_species", "Interaction") 
########################################################################################################################################################
#2) READ TRAIT DATA
########################################################################################################################################################
#READ TRAIT DATA ALREADY IMPUTED
setwd("~/R_Projects/Reproductive traits") 
trait_data <- read.csv("Data/Csv/all_species_imputed_trait_data_forest_data.csv") 
colnames(trait_data)[1] <- "Plant_species"
########################################################################################################################################################
#3) MERGE DATA
########################################################################################################################################################

#prepare dataframe without duplicates
unique_plants <- all_long[!duplicated(all_long$Plant_species),]
#remove species that are not until species level
#remove cases that are not until species level
unique_plants$Plant_species <-  sub('sp.1$', 'sp.', unique_plants$Plant_species)
unique_plants$Plant_species <- sub('sp.2$', 'sp.', unique_plants$Plant_species)
unique_plants$Plant_species <- sub('sp$', 'sp.', unique_plants$Plant_species)
unique_plants$Plant_species <- sub("sp.$", "DELETE", unique_plants$Plant_species)
unique_plants <- unique_plants[- grep("DELETE",  unique_plants$Plant_species),] #remove species with "DELETE"

#Now merge network data and trait data
unique_plant_trait_data <- merge(uni,trait_data, by="Plant_species")
str(unique_plant_trait_data)
#prepare matrix of traits to calculate distances
#select just numeric variables (the ones of the ppca)

data_for_dist <- unique_plant_trait_data[,c("Autonomous_selfing_level_fruit_set","Flowers_per_plant","Corolla_diameter_mean","STYLE_IMPUTED","OVULES_IMPUTED",
                                           "IMPUTED_plant_height_mean_m")]


rownames(data_for_dist) <- unique_plant_trait_data$Species_all

########################################################################################################################################################
#4) CALCULATE DISTANCE
########################################################################################################################################################

#calculate distance
md1 <- mahalanobis.dist(data_for_dist)
#calculate pca
output_pcoa<- pcoa(md1)
#average rows of the 6 different axis
data_to_average <- data.frame(output_pcoa$vectors)
data_mean <- rowMeans(data_to_average)

########################################################################################################################################################
#4) NOW MERGE WITH THE 
########################################################################################################################################################


