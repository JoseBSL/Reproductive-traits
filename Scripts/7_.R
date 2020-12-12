########################################################################################################################################################
#SCRIPT TO EXPLORE THE TRAIT DATA OF THE DIFFERENT CLUSTERS

#1)LOAD DATA 

#2)PHYLOGENETIC DISTANCE OF THE SPECIES -Add them as covariable-

#3)SAVE MODEL

#4)PLOT OUTPUT NICELY
########################################################################################################################################################
library(dplyr)
library(ggplot2)
library(viridis)
########################################################################################################################################################
#1) READ DATA
########################################################################################################################################################
#read unscaled trait data in order to visualize better the clusters
d <- read.csv("Data/Csv/all_species_imputed_trait_data.csv")
#read trait data with clusters
#I'll select the column of clusters and include it on the non standardize trait data
hclust_d_5 <- read.csv("Data/Csv/imputed_trait_data_hclust_5_clusters.csv") #5 clusters
#No add the cluster columns (it has the same order)
d$Clusters <- as.factor(hclust_d_5$Clusters)

#select columns of interest
t <- d[c("Breeding_system","IMPUTED_Compatibility","Autonomous_selfing_level",
                        "Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant", "Flowers_per_inflorescence",
                        "Floral_unit_width", "Corolla_diameter_mean", "Corolla_length_mean", "STYLE_IMPUTED", "OVULES_IMPUTED", "life_form", "lifespan",
                        "IMPUTED_plant_height_mean_m", "Clusters")]


#Exploratory summary of the data
summ_clusters <- t  %>% group_by(Clusters) %>% do(the_summary = summary(.))
summ_clusters$the_summary


#I start visualizing the qualitative variables

########################################################################################################################################################
#Breeding system
df <- t  %>%
  group_by(Breeding_system, Clusters) %>%
  summarise(counts = n()) 

df$Breeding_system <- factor(df$Breeding_system, levels=c("Dioecious","Hermaphrodite", "Monoecious"))


library(tidyr)
df <- complete(df, Breeding_system, Clusters, fill = list(counts=0))




#option1
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = Breeding_system),
    stat = "identity", position = position_stack()) +scale_fill_viridis(discrete = T)+theme_classic()

#option2
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = Breeding_system),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  )+scale_fill_viridis(discrete = T,drop=FALSE)+theme_classic()
########################################################################################################################################################
#Compatibility
df <- t  %>%
  group_by(IMPUTED_Compatibility, Clusters) %>%
  summarise(counts = n()) 

#option1
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = IMPUTED_Compatibility),
    stat = "identity", position = position_stack()) +scale_fill_viridis(discrete = T)+theme_classic()

#option2
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = IMPUTED_Compatibility),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  )+scale_fill_viridis(discrete = T)+theme_classic()
########################################################################################################################################################
#Autonomous_selfing_level

str(t$Breeding_system)

df <- t  %>%
  group_by(Autonomous_selfing_level, Clusters) %>%
  summarise(counts = n()) 

#option1
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = Autonomous_selfing_level),
    stat = "identity", position = position_stack()) +scale_fill_viridis(discrete = T)+theme_classic()

#option2
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = Autonomous_selfing_level),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  )+scale_fill_viridis(discrete = T,drop=FALSE)+theme_classic() + scale_x_discrete(drop=FALSE)
########################################################################################################################################################
#Flower_morphology
df <- t  %>%
  group_by(Flower_morphology, Clusters) %>%
  summarise(counts = n()) 

#option1
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = Flower_morphology),
    stat = "identity", position = position_stack()) +scale_fill_viridis(discrete = T)+theme_classic()

#option2
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = Flower_morphology),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  )+scale_fill_viridis(discrete = T)+theme_classic()
########################################################################################################################################################
#Flower_symmetry
df <- t  %>%
  group_by(Flower_symmetry, Clusters) %>%
  summarise(counts = n()) 

#option1
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = Flower_symmetry),
    stat = "identity", position = position_stack()) +scale_fill_viridis(discrete = T)+theme_classic()

#option2
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = Flower_symmetry),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  )+scale_fill_viridis(discrete = T)+theme_classic()



########################################################################################################################################################
#lifespan
df <- t  %>%
  group_by(lifespan, Clusters) %>%
  summarise(counts = n()) 

#option1
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = lifespan),
    stat = "identity", position = position_stack()) +scale_fill_viridis(discrete = T)+theme_classic()

#option2
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = lifespan),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  )+scale_fill_viridis(discrete = T)+theme_classic()


########################################################################################################################################################
#life_form
df <- t  %>%
  group_by(life_form, Clusters) %>%
  summarise(counts = n()) 

#option1
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = life_form),
    stat = "identity", position = position_stack()) +scale_fill_viridis(discrete = T)+theme_classic()

#option2
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = life_form),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  )+scale_fill_viridis(discrete = T)+theme_classic()
