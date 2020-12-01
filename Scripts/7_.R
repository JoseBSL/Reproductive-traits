########################################################################################################################################################
#SCRIPT TO EXPLORE THE TRAIT DATA OF THE DIFFERENT CLUSTERS

#1)LOAD DATA 

#2)PHYLOGENETIC DISTANCE OF THE SPECIES -Add them as covariable-

#3)SAVE MODEL

#4)PLOT OUTPUT NICELY
########################################################################################################################################################
library(dplyr)


########################################################################################################################################################
#1) READ DATA
########################################################################################################################################################
#read trait data with clusters
hclust_d_5 <- read.csv("Data/Csv/imputed_trait_data_hclust_5_clusters.csv") #5 clusters


#read unscaled trait data in order to visualize better the clusters
trait_data <- read.csv("Data/Csv/all_species_imputed_trait_data.csv", row.names = "Species_all") #set spp names as rownames 
trait_data$Clusters
trait_data$Clusters <- hclust_d_5$Clusters 

#select columns of interest
t <- trait_data[c("Breeding_system","IMPUTED_Compatibility","Autonomous_selfing_level",
                        "Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant", "Flowers_per_inflorescence",
                        "Floral_unit_width", "Corolla_diameter_mean", "Corolla_length_mean", "STYLE_IMPUTED", "OVULES_IMPUTED", "life_form", "lifespan",
                        "IMPUTED_plant_height_mean_m", "Clusters")]



summ_clusters <- t  %>% group_by(Clusters) %>% do(the_summary = summary(.))
summ_clusters$the_summary


scale(t$Autonomous_selfing_level_fruit_set.x)
