########################################################################################################################################################
#SCRIPT TO CALCULATE FUNCTIONAL GROUPS ##(METHOD:PAM)##

#1)READ TAIT IMPUTED DATA FOR ALL SPECIES (created in 2_Trait_Data_Imputation)

#2)SCALE VARIABLES

#3)CALCULATE GOWER DISTANCE

#4)FIND OPTIMAL NUMBER OF FUNCTIONAL GROUPS (2 METHODS HCLUST AND PARTITIONING AROUND MEDIOIDS -PAM-) HERE JUST PAM

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

#14 CLUSTERS IS THE OPTIMUM i'm going to divide in 5 and 14

########################################################################################################################################################
#5)PLOT DENDROGRAMS WITH OPTIMAL NUMBER OF CLUSTERS 
########################################################################################################################################################

#########
#1) PAM 5 
#########
#check summary "Partitioning Around Medoids"
pam_fit_5 <- pam(g.dist, diss = TRUE, k = 5)
pam_results_5 <- trait_data  %>%mutate(cluster = pam_fit_5$clustering) %>% group_by(cluster) %>% do(the_summary = summary(.))
pam_results_5$the_summary
#PLOT T-sne 5 clusters
tsne_obj <- Rtsne(g.dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%data.frame() %>%setNames(c("X", "Y")) %>%mutate(cluster = factor(pam_fit_5$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster))

#########
#2) PAM 14
#########
#check summary "Partitioning Around Medoids"
pam_fit_14 <- pam(g.dist, diss = TRUE, k = 14)
pam_results_14 <- trait_data  %>%mutate(cluster = pam_fit_14$clustering) %>% group_by(cluster) %>% do(the_summary = summary(.))
pam_results_14$the_summary
#PLOT T-sne 5 clusters
tsne_obj <- Rtsne(g.dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%data.frame() %>%setNames(c("X", "Y")) %>%mutate(cluster = factor(pam_fit_14$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster))
#########################################################################################
#The optimum is 14 clusters but again as we did with HCLUST we are going to consider both
#5 and 14 clusters
#########################################################################################

########################################################################################################################################################
#6)SAVE DATA
########################################################################################################################################################
#5 CLUSTERS
#The order is still the same (I have check it previously) so I can cbind the output and trait data
#Save PAM output
trait_data_pam_5 <- cbind(trait_data, pam_fit_5$clustering)
names(trait_data_pam_5)[names(trait_data_pam_5) == "pam_fit_5$clustering"] <- "Clusters"
levels(as.factor(trait_data_pam_5$Clusters))
#Write csv
write.csv(trait_data_pam_5, "Data/Csv/imputed_trait_data_pam_5_clusters.csv") 

#14 CLUSTERS
#The order is still the same (I have check it previously) so I can cbind the output and trait data
#Save PAM output
trait_data_pam_14 <- cbind(trait_data, pam_fit_14$clustering)
names(trait_data_pam_14)[names(trait_data_pam_14) == "pam_fit_14$clustering"] <- "Clusters"
levels(as.factor(trait_data_pam_14$Clusters))
#Write csv
write.csv(trait_data_pam_14, "Data/Csv/imputed_trait_data_pam_14_clusters.csv") 

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

