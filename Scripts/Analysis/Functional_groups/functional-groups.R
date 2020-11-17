
#This script assigns species to functional groups using weighted functional traits

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#load packages
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

require(cluster)
require(NbClust)
require(maptree)
require(FD)
require(bbmle)
require(spdep)
require(RANN)
library(missMDA)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#READ DATA
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

trait_data <- read.csv("Data/Csv/quantitative_networks_trait_data.csv")
str(trait_data)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#PREPARE DATA
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#select columns of interest
t <- trait_data[c("Order_all","Family_all","Genus_all","Species_all","Breeding_system","IMPUTED_Compatibility","Autonomous_selfing_level",
"Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant", "Flowers_per_inflorescence",
"Floral_unit_width", "Corolla_diameter_mean", "Corolla_length_mean", "STYLE_IMPUTED", "OVULES_IMPUTED", "life_form", "lifespan",
"IMPUTED_plant_height_mean_m")]

#Breeding_system
t$Breeding_system <- as.character(t$Breeding_system)
t$Breeding_system[t$Breeding_system=="androdioecious"] <- "dioecious"
t$Breeding_system[t$Breeding_system=="androgynodioecious"] <- "dioecious"
t$Breeding_system[t$Breeding_system=="andromonoecious"] <- "monoecious"
t$Breeding_system[t$Breeding_system=="gynodioecious"] <- "dioecious"
t$Breeding_system[t$Breeding_system=="gynoecious"] <- "monoecious"
t$Breeding_system[t$Breeding_system=="gynomonoecious"] <- "monoecious"
t$Breeding_system[t$Breeding_system=="gynomonoecious"] <- "monoecious"
t$Breeding_system[t$Breeding_system=="monoecious_dioecious"] <- "monoecious"
t$Breeding_system[t$Breeding_system=="polygamo-dioecious"] <- "dioecious"
t$Breeding_system[t$Breeding_system=="protandrous"] <- "hermaphrodite"
t$Breeding_system[t$Breeding_system=="protogynous"] <- "hermaphrodite"
t$Breeding_system[t$Breeding_system=="subdioecious"] <- "dioecious"
t$Breeding_system[t$Breeding_system=="submonoecious"] <- "monoecious"
#change to capital letters
t$Breeding_system[t$Breeding_system=="monoecious"] <- "Monoecious"
t$Breeding_system[t$Breeding_system=="dioecious"] <- "Dioecious"
t$Breeding_system[t$Breeding_system=="hermaphrodite"] <- "Hermaphrodite"
levels(as.factor(t$Breeding_system))
t$Breeding_system <- as.factor(t$Breeding_system)

#Now 3 levels

#Flower_morphology
t$Flower_morphology <- as.character(t$Flower_morphology)

t$Flower_morphology[t$Flower_morphology=="bowl"] <- "open"
t$Flower_morphology[t$Flower_morphology=="dish"] <- "open"
t$Flower_morphology[t$Flower_morphology=="exposed"] <- "open"
t$Flower_morphology[t$Flower_morphology=="spadix"] <- "spike"
t$Flower_morphology[t$Flower_morphology=="open"] <- "Open"
t$Flower_morphology[t$Flower_morphology=="brush"] <- "Brush"
t$Flower_morphology[t$Flower_morphology=="campanulate"] <- "Campanulate"
t$Flower_morphology[t$Flower_morphology=="capitulum"] <- "Capitulum"
t$Flower_morphology[t$Flower_morphology=="funnelform"] <- "Funnelform"
t$Flower_morphology[t$Flower_morphology=="papilionaceous"] <- "Papilionaceous"
t$Flower_morphology[t$Flower_morphology=="spike"] <- "Spike"
t$Flower_morphology[t$Flower_morphology=="tube"] <- "Tube"
levels(as.factor(t$Flower_morphology))
t$Flower_morphology <- as.factor(t$Flower_morphology)

#Life span
t$lifespan <- as.character(t$lifespan )
t$lifespan[t$lifespan=="annual"] <- "short_lived"
t$lifespan[t$lifespan=="biennial"] <- "short_lived"
t$lifespan[t$lifespan=="short_lived"] <- "Short lived"
t$lifespan[t$lifespan=="perennial"] <- "Perennial"
levels(as.factor(t$lifespan))
t$lifespan <- as.factor(t$lifespan )


#check for NA's in species column
t_1 <- t[!is.na(t$Species_all),]
t_2 <- t_1[!is.na(t_1$Genus_all),]
t_3 <- t_2[!is.na(t_2$Family_all),]
t_4 <- t_3[!is.na(t_3$Order_all),]
str(t_4)
t_4$Flowers_per_inflorescence <- as.integer(t_4$Flowers_per_inflorescence)

#Explore patterns of missing data
missing_data <- unlist(lapply(t_4, function(x) sum(is.na(x))))/nrow(t)
sort(missing_data[missing_data >= 0], decreasing=T)
#Just one variable is above 50%
#Conduct data imputation
t_imputed <- imputeFAMD(t_4, ncp=3,threshold = 1e-06) 

head(t_imputed$completeObs)
#looks that it has been done well
#I'm going to fix the other two columns

t_imputed$completeObs$Family_all <- gsub("Family_all_", "", t_imputed$completeObs$Family_all)
t_imputed$completeObs$Genus_all <- gsub("Genus_all_", "", t_imputed$completeObs$Genus_all)
head(t_imputed$completeObs)

#SAVE DATA
write.csv(t_imputed$completeObs, "Data/Csv/quantitative_networks_imputed_trait_data.csv")


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#create Gower dissimilarity matrix (with traits weighted)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

e.dist <- gowdis(trait_data)
e$Compatibility

e_1 <- e[c("Corolla_length_mean","Floral_unit_width","style_length_mm", "Autonomous_selfing_level_fruit_set","ovules_mean","plant_height_mean_m")]
library(dplyr)
e_1[,c(1:6)] <- scale(mutate_all(e_1[,c(1:6)], function(x) as.numeric(as.character(x))))
e_1 <- e_1[complete.cases(e_1), ]

str(e_1)
e.dist <- gowdis(e_1)
plot(noclus)
kgs[]
?kgs
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#find optimal number of functional groups
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
noclus <- agnes(e_1, method="ward")
noclus <- hclust(e_1, method="ward.D")
noclus <- hclust(e.dist, method="ward.D2")

noclus$diss
b <- kgs(noclus,e.dist, maxclust=21)#6 clusters has lowest penalty score
plot(names (b), b, xlab="Number of Clusters", ylab="Penalty score")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#silhouette plot
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pamx <- pam(e.dist, 7)
summary(pamx)
plot(pamx)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#plot dendrogram
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

e.clust <- hclust(e.dist, method="ward.D2")
plot(e.clust, main = "Cluster dengrogram based on effect traits")
cut.g <- readline("7")
cut.g <- as.integer(cut.g)
e.gr <- cutree(e.clust, k = 7)
e.gr2 <- rect.hclust(e.clust, k = 7, border = "red")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------