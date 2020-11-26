########################################################################################################################################################
#SCRIPT TO CALCULATE FUNCTIONAL GROUPS ##(METHOD:HCLUST)##

#1)READ TAIT IMPUTED DATA FOR ALL SPECIES (created in 2_Trait_Data_Imputation)

#2)SCALE VARIABLES

#3)CALCULATE GOWER DISTANCE

#4)FIND OPTIMAL NUMBER OF FUNCTIONAL GROUPS (2 METHODS HCLUST AND PARTITIONING AROUND MEDIOIDS -PAM-) BUT IN THIS SCRIPT JUST HCLUST

#5)PLOT DENDROGRAMS WITH OPTIMAL NUMBER OF CLUSTERS 

#6)SAVE DATA 
########################################################################################################################################################

#LOAD LIBRARIES
library(cluster)
library(NbClust)
library(maptree)
library(FD)
library(bbmle)
library(spdep)
library(RANN)
library(missMDA)
library(Rtsne)
library(dplyr)

########################################################################################################################################################
#1)READ TRAIT DATA
########################################################################################################################################################

#load data
trait_data <- read.csv("Data/Csv/all_species_imputed_trait_data.csv", row.names = "Species_all") #set spp names as rownames 
#select columns to calculate Gower distance (remove genus, order and family)
trait_data <- trait_data[,-c(1:4)]
rownames(trait_data) <- gsub("Species_all_", "", rownames(trait_data))
str(trait_data) #check data structure

########################################################################################################################################################
#2)SCALE VARIABLES
########################################################################################################################################################
trait_data[,c(4,7:13,16)] <- scale(mutate_all(trait_data[,c(4,7:13,16)], function(x) as.numeric(as.character(x))))

########################################################################################################################################################
#3)CALCULATE GOWER DISTANCE
########################################################################################################################################################

#Give weights to the different traits
w <- c(0.1428,	#breeding system
       0.0476,	0.0476,	0.0476, #selfing/compatibility
       0.0714,	0.0714, #flower morphology/symmetry
       0.0285,0.0285,0.0285,0.0285,0.0285, #floral investment  	
       0.1428, #style length
       0.1428, #ovule number
       0.0476,0.0476,0.0476) #life form

#calculate gowers distance for all species
g.dist <- gowdis(trait_data)

########################################################################################################################################################
#4)FIND OPTIMAL NUMBER OF FUNCTIONAL GROUPS
########################################################################################################################################################
noclus <- hclust(g.dist, method="ward.D2")
b <- kgs(noclus,g.dist, maxclust=21)#5 clusters has lowest penalty score
plot(names (b), b, xlab="Number of Clusters", ylab="Penalty score")

########################################################################################################################################################
#5)PLOT DENDROGRAMS WITH OPTIMAL NUMBER OF CLUSTERS 
########################################################################################################################################################

#########
# HCLUST 5 clusters 
#########
e.clust_5 <- hclust(g.dist, method="ward.D2")
plot(e.clust_5, main = "Cluster dengrogram based on effect traits",cex = 0.08)
cut.g_5 <- readline("5")
cut.g_5 <- as.integer(cut.g_5)
e.gr_5 <- cutree(e.clust_5, k = 5)
e.gr_5_1 <- rect.hclust(e.clust_5, k = 5, border = "red")
#summary of clusters
#Check clusters
hclust_5 <- trait_data  %>%mutate(cluster = as.factor(e.gr_5)) %>% group_by(cluster) %>% do(the_summary = summary(.))
hclust_5$the_summary
#visualize clusters
tsne_obj <- Rtsne(g.dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%data.frame() %>%setNames(c("X", "Y")) %>%mutate(cluster = as.factor(e.gr_5))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster))




#########
# HCLUST 14 clusters 
#########

e.clust_14 <- hclust(g.dist, method="ward.D2")
plot(e.clust_14, main = "Cluster dengrogram based on effect traits",cex = 0.08)
cut.g_14 <- readline("14")
cut.g_14 <- as.integer(cut.g_14)
e.gr_14 <- cutree(e.clust_14, k = 14)
e.gr_14_1 <- rect.hclust(e.clust_14, k = 14, border = "red")
#summary of clusters
#Check clusters
hclust_14 <- trait_data  %>%mutate(cluster = as.factor(e.gr_14)) %>% group_by(cluster) %>% do(the_summary = summary(.))
hclust_14$the_summary
#visualize clusters
tsne_obj <- Rtsne(g.dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%data.frame() %>%setNames(c("X", "Y")) %>%mutate(cluster = as.factor(e.gr_14))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster))



#########################################################################################
#The optimal number of clusters is 5 but 14 clusters also divide the data evenly
#I'm going to save both, for the moment I'll work just with 5
#########################################################################################

########################################################################################################################################################
#6)SAVE DATA
########################################################################################################################################################

#SAVE 5 CLUSTERS
#The order is still the same (I have check it previously) so I can cbind the output and trait data
trait_data_5 <- cbind(trait_data, e.gr_5)
head(trait_data_5)
#change colname
names(trait_data_5)[names(trait_data_5) == "e.gr_5"] <- "Clusters"
#Write csv
write.csv(trait_data_5, "Data/Csv/imputed_trait_data_hclust_5_clusters.csv") 

#SAVE 14 CLUSTERS
#The order is still the same (I have check it previously) so I can cbind the output and trait data
trait_data_14 <- cbind(trait_data, e.gr_14)
head(trait_data_14)
#change colname
names(trait_data_14)[names(trait_data_14) == "e.gr_14"] <- "Clusters"
#Write csv
write.csv(trait_data_14, "Data/Csv/imputed_trait_data_hclust_14_clusters.csv") 

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

