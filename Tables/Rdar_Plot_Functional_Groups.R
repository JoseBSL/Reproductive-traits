
#LOAD LIBRARIES
#devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
suppressPackageStartupMessages(library(dplyr))
library(scales)
library(tibble)
library(ggradar)
library(janitor)

#QUANTITATIVVE VARIABLES

#generate subset of quantitative variables for plotting
quant_median <- subset(Table_all_traits, Category=="median")

#delete col number 2
quant_median <- quant_median[,-2]

#transpose dataframe
final_df <- as.data.frame(t(quant_median))

#rownames to colnames (1st one)
final_df_1 <- final_df %>% row_to_names(row_number = 1)

#Convert all variables to numeric
final_df_2 <- sapply(final_df_1, function(x) as.numeric(as.character(x)))

#set back the missed rownames in the processing
rownames(final_df_2) <- rownames(final_df_1)

# Convert to dataframe
final_df_2 <- as.data.frame(final_df_2)

# Scale values and set rowname as column
final_df_3 <-final_df_2 %>% rownames_to_column( var = "group" ) %>%   mutate_at(vars(-group),funs(rescale)) 

#Plot Radar
ggradar(final_df_3) 


#QUALITATIVE VARIABLES

#LOAD DATA
#read unscaled trait data in order to visualize better the clusters
d <- read.csv("Data/Csv/all_species_imputed_trait_data_forest_data.csv")

dat_1 <- read.csv("Data/Csv/imputed_trait_data_hclust_5_clusters_forest_data.csv", row.names = "X") 

d$Clusters <- dat_1$Clusters

#select columns of interest
t <- d[c("Breeding_system","IMPUTED_Compatibility","Autonomous_selfing_level",
         "Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant", "Flowers_per_inflorescence",
         "Floral_unit_width", "Corolla_diameter_mean", "Corolla_length_mean", "STYLE_IMPUTED", "OVULES_IMPUTED", "life_form", "lifespan",
         "IMPUTED_plant_height_mean_m", "Nectar_presence_absence", "Clusters")]



##############################################################################################################################
#Prepare table qualitative variables
##############################################################################################################################

##############################################################################################################################
#Breeding system
t$Breeding_system <- factor(t$Breeding_system, levels=c("Dioecious", "Monoecious", "Hermaphrodite"))

#Summary to create table
breeding <- t %>%
  group_by(Clusters,Breeding_system) %>%
  summarise (n = n()) %>%
  mutate(Percentage = n / sum(n)*100)

breeding <- breeding[,-3]

breeding <- spread(breeding, Clusters, Percentage)

colnames(breeding)[1] <- "Category"

Trait <- data.frame(Trait= c(rep("Breeding system", 3)))

Breeding_system <- cbind(Trait, breeding)

##############################################################################################################################
#Compatibility
str(t)
t$IMPUTED_Compatibility <- as.character(t$IMPUTED_Compatibility)
t$IMPUTED_Compatibility[t$IMPUTED_Compatibility=="dioecious"] <- "Self incompatible"
t$IMPUTED_Compatibility[t$IMPUTED_Compatibility=="monoecious"] <- "Self incompatible"
t$IMPUTED_Compatibility[t$IMPUTED_Compatibility=="self_incompatible"] <- "Self incompatible"
t$IMPUTED_Compatibility[t$IMPUTED_Compatibility=="partially_self_compatible"] <- "Partially self compatible"
t$IMPUTED_Compatibility[t$IMPUTED_Compatibility=="self_compatible"] <- "Self compatible"

t$IMPUTED_Compatibility <- factor(t$IMPUTED_Compatibility, levels=c("Self incompatible", "Partially self compatible", "Self compatible"))
#Summary to create table
comp <- t %>%
  group_by(Clusters,IMPUTED_Compatibility) %>%
  summarise (n = n()) %>%
  mutate(Percentage = n / sum(n)*100)

comp <- comp[,-3]

comp <- spread(comp, Clusters, Percentage)
#Replace Na's with zeros
comp[is.na(comp)] <- 0
#Set call name os categories per trait
colnames(comp)[1] <- "Category"
#Create new column
Trait <- data.frame(Trait= c(rep("Compatibility system", 3)))
#Add column
Compatibility_system <- cbind(Trait, comp)
#Bind rows
bind_rows(Breeding_system, Compatibility_system)


############################################################################################################################################
#Life form
t$life_form <- as.character(t$life_form)
t$life_form[t$life_form =="vine"] <- "Shrub"
t$life_form[t$life_form =="tree"] <- "Tree"
t$life_form[t$life_form =="herb"] <- "Herb"
t$life_form[t$life_form =="shrub"] <- "Shrub"
#Order levels
t$life_form <- factor(t$life_form, levels=c("Tree", "Shrub", "Herb"))
#Summary to create table
life_form <- t %>%
  group_by(Clusters,life_form) %>%
  summarise (n = n()) %>%
  mutate(Percentage = n / sum(n)*100)
#delete column to convert to wide
life_form <- life_form[,-3]
#convert to wide
life_form <- spread(life_form, Clusters, Percentage)
#Set call name os categories per trait
colnames(life_form)[1] <- "Category"
#Create new column
Trait <- data.frame(Trait= c(rep("Life form", 3)))
#Add column
Life_form<- cbind(Trait, life_form)
#Bind rows
bind_rows(Breeding_system, Compatibility_system, Life_form)

############################################################################################################################################
t$lifespan <- factor(t$lifespan, levels=c("Perennial","Short lived"))
#Summary to create table
life_span <- t %>%
  group_by(Clusters,lifespan) %>%
  summarise (n = n()) %>%
  mutate(Percentage = n / sum(n)*100)
#delete column to convert to wide
life_span <- life_span[,-3]
#convert to wide
life_span <- spread(life_span, Clusters, Percentage)
#Set call name os categories per trait
colnames(life_span)[1] <- "Category"
#Create new column
Trait <- data.frame(Trait= c(rep("Life span", 2)))
#Add column
Life_span<- cbind(Trait, life_span)
#Bind rows
bind_rows(Breeding_system, Compatibility_system, Life_form, Life_span)

############################################################################################################################################
#t$Flower_morphology <- as.character(t$Flower_morphology)
#t$Flower_morphology[t$Flower_morphology=="Funnelform"] <- "Campanulate"
#t$Flower_morphology[t$Flower_morphology=="Spike"] <- "Brush"
#
#Flower_morphology <- t %>%
#  group_by(Clusters,Flower_morphology) %>%
#  summarise (n = n()) %>%
#  mutate(Percentage = n / sum(n)*100)
##delete column to convert to wide
#Flower_morphology <- Flower_morphology[,-3]
##convert to wide
#Flower_morphology <- spread(Flower_morphology, Clusters, Percentage)
##Set call name os categories per trait
#colnames(Flower_morphology)[1] <- "Category"
##Create new column
#Trait <- data.frame(Trait= c(rep("Flower shape", 6)))
##Add column
#Flower_morphology<- cbind(Trait, Flower_morphology)
##Replace Na's with zeros
#Flower_morphology[is.na(Flower_morphology)] <- 0
##Bind rows
#bind_rows(Breeding_system, Compatibility_system, Life_form, Life_span,Flower_morphology)
#
############################################################################################################################################
t$Flower_symmetry <- as.character(t$Flower_symmetry)
t$Flower_symmetry[t$Flower_symmetry =="actinomorphic"] <- "Actinomorphic"
t$Flower_symmetry[t$Flower_symmetry =="zygomorphic"] <- "Zygomorphic"

Flower_symmetry <- t %>%
  group_by(Clusters,Flower_symmetry) %>%
  summarise (n = n()) %>%
  mutate(Percentage = n / sum(n)*100)
#delete column to convert to wide
Flower_symmetry <- Flower_symmetry[,-3]
#convert to wide
Flower_symmetry <- spread(Flower_symmetry, Clusters, Percentage)
#Set call name os categories per trait
colnames(Flower_symmetry)[1] <- "Category"
#Create new column
Trait <- data.frame(Trait= c(rep("Flower symmetry", 2)))
#Add column
Flower_symmetry<- cbind(Trait, Flower_symmetry)
#Bind rows
bind_rows(Breeding_system, Compatibility_system, Life_form, Life_span,Flower_morphology,Flower_symmetry)
############################################################################################################################################
#t$Autonomous_selfing_level <- as.character(t$Autonomous_selfing_level)
#t$Autonomous_selfing_level[t$Autonomous_selfing_level =="none"] <- "None"
#t$Autonomous_selfing_level[t$Autonomous_selfing_level =="low"] <- "Low"
#t$Autonomous_selfing_level[t$Autonomous_selfing_level =="medium"] <- "Medium"
#t$Autonomous_selfing_level[t$Autonomous_selfing_level =="high"] <- "High"
#
##Order levels
#t$Autonomous_selfing_level <- factor(t$Autonomous_selfing_level, levels=c("High", "Medium", "Low", "None"))
#
#Selfing <- t %>%
#  group_by(Clusters,Autonomous_selfing_level) %>%
#  summarise (n = n()) %>%
#  mutate(Percentage = n / sum(n)*100)
#
##delete column to convert to wide
#Selfing <- Selfing[,-3]
##convert to wide
#Selfing <- spread(Selfing, Clusters, Percentage)
##Set call name os categories per trait
#colnames(Selfing)[1] <- "Category"
##Create new column
#Trait <- data.frame(Trait= c(rep("Selfing level", 2)))
##Replace Na's with zeros
#Selfing[is.na(Selfing)] <- 0
##Add column
#Selfing_1 <- cbind(Trait, Selfing)
##Bind rows
#bind_rows(Breeding_system, Compatibility_system, Life_form, Life_span,Flower_morphology, Selfing_1)
#
#############################################################################################################################################
t$Nectar_presence_absence <- as.character(t$Nectar_presence_absence)
t$Nectar_presence_absence[t$Nectar_presence_absence =="yes"] <- "Yes"
t$Nectar_presence_absence[t$Nectar_presence_absence =="no"] <- "No"

Nectar <- t %>%
  group_by(Clusters,Nectar_presence_absence) %>%
  summarise (n = n()) %>%
  mutate(Percentage = n / sum(n)*100)

#delete column to convert to wide
Nectar <- Nectar[,-3]
#convert to wide
Nectar <- spread(Nectar, Clusters, Percentage)
#Set call name os categories per trait
colnames(Nectar)[1] <- "Category"
#Create new column
Trait <- data.frame(Trait= c(rep("Nectar", 2)))
#Replace Na's with zeros
Nectar[is.na(Nectar)] <- 0
#Add column
Nectar <- cbind(Trait, Nectar)
#Bind rows
data <- bind_rows(Breeding_system, Compatibility_system, Life_form, Life_span, Flower_symmetry, Nectar)

#remove first col
data_1 <- data[,-1]
colnames(data_1) <- c("Category", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")

#transpose dataframe
data_2 <- as.data.frame(t(data_1))

#rownames to colnames (1st one)
data_3 <- data_2 %>% row_to_names(row_number = 1)

#Convert all variables to numeric
data_4 <- sapply(data_3, function(x) as.numeric(as.character(x)))

#set back the missed rownames in the processing
rownames(data_4) <- rownames(data_3)

# Convert to dataframe
data_4 <- as.data.frame(data_4)
str(data_4)

# Scale values and set rowname as column
data_5 <-data_4 %>% rownames_to_column( var = "group" ) %>%   mutate_at(vars(-group),funs(rescale)) 

#Plot Radar
ggradar(data_5) 


data_6 <- melt(data_5)

 ggplot(data_6, aes(x = variable, y = value,group=group,colour=group)) +
  geom_line(size=0.5) +
  geom_point(size = 1.5) + theme(axis.text.x = element_text(angle = 45,size = 5),legend.position="none")


cluster1 <- subset(data_6, group=="Cluster 1")

c1 <- ggplot(cluster1, aes(x = variable, y = value,group=group,colour=group)) +
  geom_line(size=0.5) +geom_bar(stat = "identity")+
  geom_point(size = 1.5) + theme(axis.text.x = element_text(angle = 45,size = 5),legend.position="none")

cluster2 <- subset(data_6, group=="Cluster 2")

c2 <- ggplot(cluster2, aes(x = variable, y = value,group=group,colour=group)) +
  geom_line(size=0.5) +geom_bar(stat = "identity")+
  geom_point(size = 1.5) + theme(axis.text.x = element_text(angle = 45,size = 5),legend.position="none")

cluster3 <- subset(data_6, group=="Cluster 3")

c3 <- ggplot(cluster3, aes(x = variable, y = value,group=group,colour=group)) +
  geom_line(size=0.5) +geom_bar(stat = "identity")+
  geom_point(size = 1.5) + theme(axis.text.x = element_text(angle = 45,size = 5),legend.position="none")

cluster4 <- subset(data_6, group=="Cluster 4")

c4 <- ggplot(cluster4, aes(x = variable, y = value,group=group,colour=group)) +
  geom_line(size=0.5) +geom_bar(stat = "identity")+
  geom_point(size = 1.5) + theme(axis.text.x = element_text(angle = 45,size = 5),legend.position="none")

cluster5 <- subset(data_6, group=="Cluster 5")

c5 <- ggplot(cluster5, aes(x = variable, y = value,group=group,colour=group)) +
  geom_line(size=0.5) + geom_bar(stat = "identity")+
  geom_point(size = 1.5) + theme(axis.text.x = element_text(angle = 45,size = 5),legend.position="none")

plot_grid(c1, c2, c3, c4, c5, ncol = 1,align="none")
