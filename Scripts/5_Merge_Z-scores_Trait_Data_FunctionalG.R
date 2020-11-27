########################################################################################################################################################
#SCRIPT TO MERGE LONG DATA WITH Z-SCORES, TRAIT DATA AND FUNCTIONAL GROUPS

#1) LOAD DATA 

#3) MERGE WITH TRAIT DATA

#4) MERGE WITH FUNCTIONAL GROUPS

#5) SAVE DATA
########################################################################################################################################################

#LOAD LIBRARIES
library(data.table)
library(readxl) #read trait data

########################################################################################################################################################
#1) LOAD LONG FORMAT DATA WITH Z-SCORES, TRAIT DATA AND FUNCTIONAL GROUP DATA
########################################################################################################################################################
long_d_2 <- read.csv("Data/Csv/long_format_quantitative_networks_Z_scores.csv")
long_d_2_non_apis <- read.csv("Data/Csv/long_format_quantitative_networks_Z_scores_non_apis.csv")
t_data <- read_excel("Data/Trait_data_raw/Trait_data_final.xlsx")
hclust_d_5 <- read.csv("Data/Csv/imputed_trait_data_hclust_5_clusters.csv") #5 clusters
########################################################################################################################################################
#3) MERGE WITH TRAIT DATA
########################################################################################################################################################

#process trait data all species
t_data <- t_data[1:1701,]
#filter data, select species with flower level info and capitulum
t_data_filtered <- filter(t_data, Info_level == "flower" |  Info_level == "capitulum")
t_data_filtered <- as.data.frame(t_data_filtered)
colnames(t_data_filtered)[1] <- "Plant_species"
#select columns of interest
traits <- t_data_filtered %>% select(Species_geonet,Order_all,Family_all,Genus_all,Species_all,Breeding_system,IMPUTED_Compatibility,Autonomous_selfing_level,Autonomous_selfing_level_data_type,Autonomous_selfing_level_fruit_set,Flower_morphology,Flower_symmetry,Flowers_per_plant,Flowers_per_inflorescence,Floral_unit_width,Corolla_diameter_mean,Corolla_length_mean,STYLE_IMPUTED,OVULES_IMPUTED,life_form,lifespan,IMPUTED_plant_height_mean_m)
#Remove duplicated species
t <- traits[!duplicated(traits$Species_all), ]
colnames(t)[1] <- "Plant_species"

#MERGE NETWORK AND TRAIT DATA
long_format_trait_data <- merge(long_d_2, t, by = "Plant_species", all.x =T)
########################################################################################################################################################
#repeat fo dataset witout apis mellifera
long_format_trait_data_non_apis <- merge(long_d_2_non_apis, t, by = "Plant_species", all.x =T)
########################################################################################################################################################
#4) MERGE WITH FUNCTIONAL GROUPS
########################################################################################################################################################
#5 CLUSTERS
#Some data processing before merging
#colnames to merge by same id
colnames(hclust_d_5)[1] <-"Species_all"
#convert to chracters
hclust_d_5$Species_all <- as.character(hclust_d_5$Species_all)
long_format_trait_data$Species_all <- as.character(long_format_trait_data$Species_all)

#convert character NA's to NA's
long_format_trait_data$Species_all[long_format_trait_data$Species_all=="NA"]<- NA
hclust_d_5$Species_all[hclust_d_5$Species_all=="NA"]<- NA

#remove NA'S
long_format_trait_data <- long_format_trait_data[!is.na(long_format_trait_data$Species_all),]
hclust_d_5 <- hclust_d_5[!is.na(hclust_d_5$Species_all),]

#merge
quantitative_networks_Z_scores_with_traits_and_5_clusters <- merge(long_format_trait_data,hclust_d_5, by="Species_all", all.x  = T) #now data is ready for analysis
head(quantitative_networks_Z_scores_with_traits_and_5_clusters)
#remov 2 X'2 cloumn
quantitative_networks_Z_scores_with_traits_and_5_clusters <- quantitative_networks_Z_scores_with_traits_and_5_clusters[,-c(3,4)]
########################################################################################################################################################
#repeat fo dataset witout apis mellifera
#convert to chracters
long_format_trait_data_non_apis$Species_all <- as.character(long_format_trait_data_non_apis$Species_all)
#convert character NA's to NA's
long_format_trait_data_non_apis$Species_all[long_format_trait_data_non_apis$Species_all=="NA"]<- NA
#remove NA'S
long_format_trait_data_non_apis <- long_format_trait_data_non_apis[!is.na(long_format_trait_data_non_apis$Species_all),]
#merge
long_format_trait_data_non_apis_5_clusters <- merge(long_format_trait_data_non_apis,hclust_d_5, by="Species_all", all.x  = T) #now data is ready for analysis
#remov 2 X'2 cloumn
long_format_trait_data_non_apis_5_clusters <- long_format_trait_data_non_apis_5_clusters[,-c(3,4)]
########################################################################################################################################################
#5) SAVE DATA
########################################################################################################################################################

#SAVE 5 CLUSTERS
write.csv(quantitative_networks_Z_scores_with_traits_and_5_clusters, "Data/Csv/quantitative_networks_Z_scores_with_traits_and_5_clusters_hclust.csv")
#SAVE 5 CLUSTERS NON APIS
write.csv(long_format_trait_data_non_apis_5_clusters, "Data/Csv/long_format_trait_data_non_apis_5_clusters.csv")
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################


