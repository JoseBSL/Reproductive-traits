########################################################################################################################################################
#SCRIPT TO CALCULATE FUNCTIONAL GROUPS

#1)READ TAIT IMPUTED DATA FOR ALL SPECIES (created in 2_Trait_Data_Imputation)

#2)SCALE VARIABLES

#3)CALCULATE GOWER DISTANCE

#4)FIND OPTIMAL NUMBER OF FUNCTIONAL GROUPS (2 METHODS HCLUST AND PARTITIONING AROUND MEDIOIDS -PAM-)

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



#Now check with PAM (partitioning around medioids)
sil_width <- c(NA)
for(i in 2:20){  
  pam_fit <- pam(g.dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:20, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:20, sil_width)

#Seems that with PAM 14 clusters is the optimal number (more specific clusters)

e.clust <- hclust(g.dist, method="ward.D2")
plot(e.clust, main = "Cluster dengrogram based on effect traits",cex = 0.08)
cut.g <- readline("14")
cut.g <- as.integer(cut.g)
e.gr <- cutree(e.clust, k = 14)
e.gr2 <- rect.hclust(e.clust, k = 14, border = "red")


########################################################################################################################################################
#5)PLOT DENDROGRAMS WITH OPTIMAL NUMBER OF CLUSTERS 
########################################################################################################################################################

#########
#METHOD## 1) HCLUST 5 clusters optimal number
#########
e.clust <- hclust(g.dist, method="ward.D2")
plot(e.clust, main = "Cluster dengrogram based on effect traits",cex = 0.08)
cut.g <- readline("5")
cut.g <- as.integer(cut.g)
e.gr <- cutree(e.clust, k = 5)
e.gr2 <- rect.hclust(e.clust, k = 5, border = "red")

#Check clusters
hclust_results <- trait_data  %>%mutate(cluster = as.factor(e.gr)) %>% group_by(cluster) %>% do(the_summary = summary(.))
hclust_results$the_summary

#Visualize clusters with t-sne
tsne_obj <- Rtsne(g.dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%data.frame() %>%setNames(c("X", "Y")) %>%mutate(cluster = as.factor(e.gr))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster))



#########
#METHOD## 2) PAM 14 clusters optimal number
#########

#PLOT T-sne 14 clusters
tsne_obj <- Rtsne(g.dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%data.frame() %>%setNames(c("X", "Y")) %>%mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster))

#check summary "Partitioning Around Medoids"
pam_fit <- pam(g.dist, diss = TRUE, k = 14)
pam_results <- trait_data  %>%mutate(cluster = pam_fit$clustering) %>% group_by(cluster) %>% do(the_summary = summary(.))
pam_results$the_summary

#########################################################################################
##Although method 1 and 2 give different outputs, 5 and 14 clusters are visible with both
##I'm going to stick with 5 clusters with hierarchical clustering
##Theres is not a strong reason both seem fine
#########################################################################################

########################################################################################################################################################
#6)SAVE DATA
########################################################################################################################################################
#The order is still the same (I have check it previously) so I can cbind the output and trait data
trait_data <- cbind(trait_data, e.gr)
head(trait_data)

#change colname
names(trait_data)[names(trait_data) == "e.gr"] <- "Clusters"

#Write csv
write.csv(trait_data, "Data/Csv/imputed_trait_data_hclust_5_clusters.csv") 

