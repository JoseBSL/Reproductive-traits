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
library(dplyr)
########################################################################################################################################################
#1) LOAD LONG FORMAT DATA WITH Z-SCORES, TRAIT DATA AND FUNCTIONAL GROUP DATA
########################################################################################################################################################
long_d_2 <- read.csv("Data/Csv/long_format_quantitative_networks_Z_scores.csv")
t_data <- read_excel("Data/Trait_data_raw/Trait_data_final.xlsx")

#Read one each time and run
#hclust_d_5 <- read.csv("Data/Csv/imputed_trait_data_hclust_5_clusters_famd.csv") #5 clusters
hclust_d_5 <- read.csv("Data/Csv/imputed_trait_data_hclust_5_clusters_forest_data.csv") #5 clusters

########################################################################################################################################################
#3) MERGE WITH TRAIT DATA
########################################################################################################################################################
#process trait data all species
t_data <- t_data[1:1712,]
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

levels(factor(long_d_2$Plant_species))
levels(factor(t$Plant_species))


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
final_quant_clusters <- quantitative_networks_Z_scores_with_traits_and_5_clusters[,-c(3,4)]

#Exclude rows without info to species level
final_quant_clusters <- final_quant_clusters[ grep("Arrabidaea sp.", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Carduus sp.", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Eupatorium sp.", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Gymnocalycium sp.", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Jatropa sp.", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Leontodon sp.", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Linum sp.", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Malpighiaceae sp.1", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Myrtaceae sp.1", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Oxalis sp.", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Pectis sp.", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Ranunculus sp.", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Retama sp.", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Senegalia sp.", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Sida sp.", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Tradescanta sp.", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Trifolium sp.1", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Vernonia sp.", final_quant_clusters$Plant_species, invert = TRUE) , ]
final_quant_clusters <- final_quant_clusters[ grep("Xyris sp", final_quant_clusters$Plant_species, invert = TRUE) , ]

#check for Na's
na <- final_quant_clusters[is.na(final_quant_clusters$Clusters),]


########################################################################################################################################################
#5) SAVE DATA
########################################################################################################################################################
#SAVE 5 CLUSTERS
setwd("~/R_Projects/Reproductive Traits")

#write.csv(quantitative_networks_Z_scores_with_traits_and_5_clusters, "Data/Csv/quantitative_networks_Z_scores_with_traits_and_5_clusters_hclust_famd.csv")
write.csv(final_quant_clusters, "Data/Csv/quantitative_networks_Z_scores_with_traits_and_5_clusters_hclust_forest_data.csv")
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################


