##########################################
#Visualization of plant functional groups#
##########################################

library(cowplot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)
library(data.table)
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
t$IMPUTED_Compatibility[t$IMPUTED_Compatibility=="dioecious"] <- "Unisexual flower"
t$IMPUTED_Compatibility[t$IMPUTED_Compatibility=="monoecious"] <- "Unisexual flower"
t$IMPUTED_Compatibility[t$IMPUTED_Compatibility=="self_incompatible"] <- "Self incompatible"
t$IMPUTED_Compatibility[t$IMPUTED_Compatibility=="partially_self_compatible"] <- "Partially self compatible"
t$IMPUTED_Compatibility[t$IMPUTED_Compatibility=="self_compatible"] <- "Self compatible"

t$IMPUTED_Compatibility <- factor(t$IMPUTED_Compatibility, levels=c("Unisexual flower", "Self incompatible", "Partially self compatible", "Self compatible"))
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
Trait <- data.frame(Trait= c(rep("Compatibility system", 4)))
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
t$Flower_morphology <- as.character(t$Flower_morphology)
t$Flower_morphology[t$Flower_morphology=="Funnelform"] <- "Campanulate"
t$Flower_morphology[t$Flower_morphology=="Spike"] <- "Brush"

Flower_morphology <- t %>%
  group_by(Clusters,Flower_morphology) %>%
  summarise (n = n()) %>%
  mutate(Percentage = n / sum(n)*100)
#delete column to convert to wide
Flower_morphology <- Flower_morphology[,-3]
#convert to wide
Flower_morphology <- spread(Flower_morphology, Clusters, Percentage)
#Set call name os categories per trait
colnames(Flower_morphology)[1] <- "Category"
#Create new column
Trait <- data.frame(Trait= c(rep("Flower shape", 6)))
#Add column
Flower_morphology<- cbind(Trait, Flower_morphology)
#Replace Na's with zeros
Flower_morphology[is.na(Flower_morphology)] <- 0
#Bind rows
bind_rows(Breeding_system, Compatibility_system, Life_form, Life_span,Flower_morphology)

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
t$Autonomous_selfing_level <- as.character(t$Autonomous_selfing_level)
t$Autonomous_selfing_level[t$Autonomous_selfing_level =="none"] <- "None"
t$Autonomous_selfing_level[t$Autonomous_selfing_level =="low"] <- "Low"
t$Autonomous_selfing_level[t$Autonomous_selfing_level =="medium"] <- "Medium"
t$Autonomous_selfing_level[t$Autonomous_selfing_level =="high"] <- "High"

#Order levels
t$Autonomous_selfing_level <- factor(t$Autonomous_selfing_level, levels=c("High", "Medium", "Low", "None"))

Selfing <- t %>%
  group_by(Clusters,Autonomous_selfing_level) %>%
  summarise (n = n()) %>%
  mutate(Percentage = n / sum(n)*100)

#delete column to convert to wide
Selfing <- Selfing[,-3]
#convert to wide
Selfing <- spread(Selfing, Clusters, Percentage)
#Set call name os categories per trait
colnames(Selfing)[1] <- "Category"
#Create new column
Trait <- data.frame(Trait= c(rep("Selfing level", 2)))
#Replace Na's with zeros
Selfing[is.na(Selfing)] <- 0
#Add column
Selfing_1 <- cbind(Trait, Selfing)
#Bind rows
bind_rows(Breeding_system, Compatibility_system, Life_form, Life_span,Flower_morphology, Selfing_1)

############################################################################################################################################
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
bind_rows(Breeding_system, Compatibility_system, Life_form, Life_span,Flower_morphology, Flower_symmetry,Selfing_1, Nectar)


##############################################################################################################################
#Prepare table quantitative variables
##############################################################################################################################

Selfing <-  t %>% group_by(Clusters)%>% 
  select(Autonomous_selfing_level_fruit_set) %>% # select variables to summarise
  summarise_each(funs(min = min,
                     # q25 = quantile(., 0.25), 
                      median = median, 
                      #q75 = quantile(., 0.75), 
                      max = max,
                      mean = mean, 
                    sd = sd))
  

s1 <- as.data.frame(t(Selfing))
#set colnames
colnames(s1) <- c("1","2","3","4","5")
#remove the cluster row
s1 <- s1[-1,]
#convert rownames to 1st columns
s1 <- setDT(s1, keep.rownames = "TRUE")[]
colnames(s1)[1] <- "Category"
#Create data frame to add grouping variable
Trait <- data.frame(Trait= c(rep("Autonomous selfing", 5)))
#Add column
Selfing <- cbind(Trait, s1)

##############################################################################################################################
flower_number <-  t %>% group_by(Clusters)%>% 
  select(Flowers_per_plant) %>% # select variables to summarise
  summarise_each(funs(min = min,
                      # q25 = quantile(., 0.25), 
                      median = median, 
                      #q75 = quantile(., 0.75), 
                      max = max,
                      mean = mean, 
                      sd = sd))


f1 <- as.data.frame(t(flower_number))
#set colnames
colnames(f1) <- c("1","2","3","4","5")
#remove the cluster row
f1 <- f1[-1,]
#convert rownames to 1st columns
f1 <- setDT(f1, keep.rownames = "TRUE")[]
colnames(f1)[1] <- "Category"
#Create data frame to add grouping variable
Trait <- data.frame(Trait= c(rep("Flower number", 5)))
#Add column
Flower_number <- cbind(Trait, f1)


##############################################################################################################################
Flowers_per_inflorescence <-  t %>% group_by(Clusters)%>% 
  select(Flowers_per_inflorescence) %>% # select variables to summarise
  summarise_each(funs(min = min,
                      # q25 = quantile(., 0.25), 
                      median = median, 
                      #q75 = quantile(., 0.75), 
                      max = max,
                      mean = mean, 
                      sd = sd))


f_i_1 <- as.data.frame(t(Flowers_per_inflorescence))
#set colnames
colnames(f_i_1) <- c("1","2","3","4","5")
#remove the cluster row
f_i_1 <- f_i_1[-1,]
#convert rownames to 1st columns
f_i_1 <- setDT(f_i_1, keep.rownames = "TRUE")[]
colnames(f_i_1)[1] <- "Category"
#Create data frame to add grouping variable
Trait <- data.frame(Trait= c(rep("Flowers per inflorescence", 5)))
#Add column
Flower_per_inflo <- cbind(Trait, f_i_1)

bind_rows(Selfing, Flower_number, Flower_per_inflo)

##############################################################################################################################
Floral_unit_width <-  t %>% group_by(Clusters)%>% 
  select(Floral_unit_width) %>% # select variables to summarise
  summarise_each(funs(min = min,
                      # q25 = quantile(., 0.25), 
                      median = median, 
                      #q75 = quantile(., 0.75), 
                      max = max,
                      mean = mean, 
                      sd = sd))


f_u_w_1 <- as.data.frame(t(Floral_unit_width))
#set colnames
colnames(f_u_w_1) <- c("1","2","3","4","5")
#remove the cluster row
f_u_w_1 <- f_u_w_1[-1,]
#convert rownames to 1st columns
f_u_w_1 <- setDT(f_u_w_1, keep.rownames = "TRUE")[]
colnames(f_u_w_1)[1] <- "Category"
#Create data frame to add grouping variable
Trait <- data.frame(Trait= c(rep("Inflorescence width", 5)))
#Add column
Inflo_width <- cbind(Trait, f_u_w_1)

bind_rows(Selfing, Flower_number, Flower_per_inflo, Inflo_width)

##############################################################################################################################
Corolla_diameter_mean <-  t %>% group_by(Clusters)%>% 
  select(Corolla_diameter_mean) %>% # select variables to summarise
  summarise_each(funs(min = min,
                      # q25 = quantile(., 0.25), 
                      median = median, 
                      #q75 = quantile(., 0.75), 
                      max = max,
                      mean = mean, 
                      sd = sd))


c_w_1 <- as.data.frame(t(Corolla_diameter_mean))
#set colnames
colnames(c_w_1) <- c("1","2","3","4","5")
#remove the cluster row
c_w_1 <- c_w_1[-1,]
#convert rownames to 1st columns
c_w_1 <- setDT(c_w_1, keep.rownames = "TRUE")[]
colnames(c_w_1)[1] <- "Category"
#Create data frame to add grouping variable
Trait <- data.frame(Trait= c(rep("Flower width", 5)))
#Add column
Flower_width <- cbind(Trait, c_w_1)

bind_rows(Selfing, Flower_number, Flower_per_inflo, Inflo_width, Flower_width)


##############################################################################################################################
Corolla_length_mean <-  t %>% group_by(Clusters)%>% 
  select(Corolla_length_mean) %>% # select variables to summarise
  summarise_each(funs(min = min,
                      # q25 = quantile(., 0.25), 
                      median = median, 
                      #q75 = quantile(., 0.75), 
                      max = max,
                      mean = mean, 
                      sd = sd))


c_l_1 <- as.data.frame(t(Corolla_length_mean))
#set colnames
colnames(c_l_1) <- c("1","2","3","4","5")
#remove the cluster row
c_l_1 <- c_l_1[-1,]
#convert rownames to 1st columns
c_l_1 <- setDT(c_l_1, keep.rownames = "TRUE")[]
colnames(c_l_1)[1] <- "Category"
#Create data frame to add grouping variable
Trait <- data.frame(Trait= c(rep("Flower length", 5)))
#Add column
Flower_length<- cbind(Trait, c_l_1)

bind_rows(Selfing, Flower_number, Flower_per_inflo, Inflo_width, Flower_width, Flower_length)

##############################################################################################################################
STYLE_IMPUTED <-  t %>% group_by(Clusters)%>% 
  select(STYLE_IMPUTED) %>% # select variables to summarise
  summarise_each(funs(min = min,
                      # q25 = quantile(., 0.25), 
                      median = median, 
                      #q75 = quantile(., 0.75), 
                      max = max,
                      mean = mean, 
                      sd = sd))


s_l_1 <- as.data.frame(t(STYLE_IMPUTED))
#set colnames
colnames(s_l_1) <- c("1","2","3","4","5")
#remove the cluster row
s_l_1 <- s_l_1[-1,]
#convert rownames to 1st columns
s_l_1 <- setDT(s_l_1, keep.rownames = "TRUE")[]
colnames(s_l_1)[1] <- "Category"
#Create data frame to add grouping variable
Trait <- data.frame(Trait= c(rep("Style length", 5)))
#Add column
Style_length<- cbind(Trait, s_l_1)

bind_rows(Selfing, Flower_number, Flower_per_inflo, Inflo_width, Flower_width, Flower_length, Style_length)

##############################################################################################################################

OVULES_IMPUTED <-  t %>% group_by(Clusters)%>% 
  select(OVULES_IMPUTED) %>% # select variables to summarise
  summarise_each(funs(min = min,
                      # q25 = quantile(., 0.25), 
                      median = median, 
                      #q75 = quantile(., 0.75), 
                      max = max,
                      mean = mean, 
                      sd = sd))


o_n_1 <- as.data.frame(t(OVULES_IMPUTED))
#set colnames
colnames(o_n_1) <- c("1","2","3","4","5")
#remove the cluster row
o_n_1 <- o_n_1[-1,]
#convert rownames to 1st columns
o_n_1 <- setDT(o_n_1, keep.rownames = "TRUE")[]
colnames(o_n_1)[1] <- "Category"
#Create data frame to add grouping variable
Trait <- data.frame(Trait= c(rep("Ovule number", 5)))
#Add column
Ovule_number <- cbind(Trait, o_n_1)

bind_rows(Selfing, Flower_number, Flower_per_inflo, Inflo_width, Flower_width, Flower_length, Ovule_number)


##############################################################################################################################

IMPUTED_plant_height_mean_m <-  t %>% group_by(Clusters)%>% 
  select(IMPUTED_plant_height_mean_m) %>% # select variables to summarise
  summarise_each(funs(min = min,
                      # q25 = quantile(., 0.25), 
                      median = median, 
                      #q75 = quantile(., 0.75), 
                      max = max,
                      mean = mean, 
                      sd = sd))


p_h_1 <- as.data.frame(t(IMPUTED_plant_height_mean_m))
#set colnames
colnames(p_h_1) <- c("1","2","3","4","5")
#remove the cluster row
p_h_1 <- p_h_1[-1,]
#convert rownames to 1st columns
p_h_1 <- setDT(p_h_1, keep.rownames = "TRUE")[]
colnames(p_h_1)[1] <- "Category"
#Create data frame to add grouping variable
Trait <- data.frame(Trait= c(rep("Plant height", 5)))
#Add column
Plant_height <- cbind(Trait, p_h_1)


#Bind quantitative and qualitative variables
Table_all_traits <- bind_rows(Breeding_system, Compatibility_system, Life_form, Life_span,Flower_morphology, Flower_symmetry,Selfing_1, Nectar,
          Selfing, Flower_number, Flower_per_inflo, Inflo_width, Flower_width, Flower_length, Style_length, Ovule_number, Plant_height)

#Select just two decimals
is.num <- sapply(Table_all_traits, is.numeric)
Table_all_traits[is.num] <- lapply(Table_all_traits[is.num], round, 2)

colnames(Table_all_traits) <- c("Trait", "Category", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5") 