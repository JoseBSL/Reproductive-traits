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
trait_data$Clusters <- as.factor(hclust_d_5$Clusters)

#select columns of interest
t <- trait_data[c("Breeding_system","IMPUTED_Compatibility","Autonomous_selfing_level",
                        "Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant", "Flowers_per_inflorescence",
                        "Floral_unit_width", "Corolla_diameter_mean", "Corolla_length_mean", "STYLE_IMPUTED", "OVULES_IMPUTED", "life_form", "lifespan",
                        "IMPUTED_plant_height_mean_m", "Clusters")]

t$Flowers_per_plant

boxplot(t$Flowers_per_plant)
min(t$Flowers_per_plant)


summ_clusters <- t  %>% group_by(Clusters) %>% do(the_summary = summary(.))
summ_clusters$the_summary


#lets try a mix heatmap

ap <- available.packages()

View(ap)
"CluMix" %in% rownames(ap)

t_1 <- trait_data[c(
                    "Autonomous_selfing_level_fruit_set",  "Flowers_per_plant", "Flowers_per_inflorescence",
                    "Floral_unit_width", "Corolla_diameter_mean", "Corolla_length_mean", "STYLE_IMPUTED", "OVULES_IMPUTED",
                    "IMPUTED_plant_height_mean_m")]



mix.heatmap(t, rowmar=7)

t <- hclust_d_5[c("Breeding_system","IMPUTED_Compatibility","Autonomous_selfing_level",
                  "Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant", "Flowers_per_inflorescence",
                  "Floral_unit_width", "Corolla_diameter_mean", "Corolla_length_mean", "STYLE_IMPUTED", "OVULES_IMPUTED", "life_form", "lifespan",
                  "IMPUTED_plant_height_mean_m", "Clusters")]

t_1 <- trait_data[c("Breeding_system","IMPUTED_Compatibility","Autonomous_selfing_level",
                  "Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant", "Flowers_per_inflorescence",
                  "Floral_unit_width", "Corolla_diameter_mean", "Corolla_length_mean", "STYLE_IMPUTED", "OVULES_IMPUTED", "life_form", "lifespan",
                  "IMPUTED_plant_height_mean_m")]




t$Clusters <- as.factor(t$Clusters)


data(mixdata)
str(t)
 mix.heatmap(mixdata, rowmar=7, legend.mat=TRUE)

 e.clust_5$height
 
a<-  dendro.subjects(g.dist, w)
b <- plot(e.clust_5, main = "Cluster dengrogram based on effect traits",cex = 0.08)
plot(a)

 ## with (random) color bars 
 colbar <- as.factor(t$Clusters)
 rowbar <- rep(c("darkorange","grey"), ncol(mixdata))
 mix.heatmap(mixdata, ColSideColors=colbar, RowSideColors=rowbar, 
              legend.colbar=c("1","2","3"), legend.rowbar=c("a","b"), rowmar=7)
 
  ## example with variable weights
  w <- rep(1:2, each=5)
 mix.heatmap(mixdata, varweights=w, rowmar=7)

rownames(t_1)<- e.clust_5$labels
 str(t_1)
 
 
  mix.heatmap(t,  ColSideColors=colbar,rowmar=7)
 

  e.clust_5$labels
  