########################################################################################################################################################
#SCRIPT FOR DATA IMPUTATION
########################################################################################################################################################
#1) READ TRAIT DATA

#2) CLEAN DATA AND RENAME LEVELS

#3) EXPLORE PATTERNS OF MISSING DATA

#4) CALCULATE PHYLOGENETIC DISTANCE TO CORRECT WITH EIGENVALUES IN THE IMPUTATION

#5) ICLUDE EIGENVALUES IN RAWDATA TO IMPROVE IMPUTATION OURPUT

#6) IMPUTE DATA --> RANDOM FOREST

#7) SAVE IMPUTED DATA
########################################################################################################################################################
#LOAD LIBRARIES
library(readxl) #read excel file (trait data)
library(dplyr) #data manipulation
library(missMDA) #For missing values
library(FactoMineR) #Produce pca biplot with individuals and variables
library(factoextra)
library(ggpubr) 
library(missForest) #random forst imputation
library(rtrees) #for phylogenetic distancelibrary(MASS)
library(ape)
library(PVR) #calculate eigen vectors and add them as a column in the imputation, seems to improve output
library(visdat) #VISUALIZE MISSING DATA
########################################################################################################################################################
#1) READ TRAIT DATA
########################################################################################################################################################
#load data
trait_data <- read_excel("Data/Trait_data_raw/Trait_data_final.xlsx",na = "NA")
########################################################################################################################################################
#2) A) CLEAN DATA AND B) RENAME LEVELS FOR IMPUTATION 
########################################################################################################################################################
#####
#A) CLEAN DATA
#####
#select just filled rows
trait_data_1 <- trait_data[1:1712,]


#filter data, select species with flower level info and capitulum
trait_filtered <- filter(trait_data_1, Info_level == "flower" |  Info_level == "capitulum")
levels(as.factor(trait_filtered$Info_level)) #checking levels
#remove NA's and duplicated species
trait_filtered$Species_all[trait_filtered$Species_all=="NA"]<-NA
trait_filtered_1 <- trait_filtered[!is.na(trait_filtered$Species_all),]
trait_filtered_2 <- trait_filtered_1[!duplicated(trait_filtered_1$Species_all),]

#select columns of interest
t <- trait_filtered_2[c("Species_geonet","Order_all","Family_all","Genus_all","Species_all","Breeding_system","Compatibility_system",
                        "Autonomous_selfing_level","Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant", 
                        "Corolla_diameter_mean", "Corolla_length_mean", "Style_length", "Ovule_number", "life_form", "lifespan",
                        "Plant_height_mean_m","Nectar_presence_absence","Nectar_ul","Nectar_mg","Nectar_concentration")]
########################################################################################################################################################
#####
#B) RENAME LEVELS FOR IMPUTATION
#####
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

########################################################################################################################################################
#3) EXPLORE PATTERNS OF MISSING DATA
########################################################################################################################################################
#Explore patterns of missing data
missing_data <- unlist(lapply(t, function(x) sum(is.na(x))))/nrow(t)
sort(missing_data[missing_data >= 0], decreasing=T)
#nectar values all above 50% except presence/absence
#then, just one cololumn with 68% missing data, then one with 35% and then 20% and 10%. The rest under 5%.
#it is recommended to remove columns over 50% but we are particulary interested in having numeric levels of quantitative selfing 
#we are going to work just with presence/absence of nectar
#we will perform further analysis just with a subset with the quantitative values o nectar (very interesting data and diffciult to obtain)

#use ifelse base r does not work with these NA'S Even with work around. base r does not like the na option of read excel
t$Autonomous_selfing_level_fruit_set <- ifelse(t$Autonomous_selfing_level %in% c("high") & is.na(t$Autonomous_selfing_level_fruit_set), 88, t$Autonomous_selfing_level_fruit_set)
t$Autonomous_selfing_level_fruit_set <- ifelse(t$Autonomous_selfing_level %in% c("medium") & is.na(t$Autonomous_selfing_level_fruit_set), 50.5, t$Autonomous_selfing_level_fruit_set)
t$Autonomous_selfing_level_fruit_set <- ifelse(t$Autonomous_selfing_level %in% c("low") & is.na(t$Autonomous_selfing_level_fruit_set), 13, t$Autonomous_selfing_level_fruit_set)
t$Autonomous_selfing_level_fruit_set <- ifelse(t$Autonomous_selfing_level %in% c("none") & is.na(t$Autonomous_selfing_level_fruit_set), 0, t$Autonomous_selfing_level_fruit_set)


#Now we just select presence/absence of nectar
#select columns of interest
t <- t[c("Species_geonet","Order_all","Family_all","Genus_all","Species_all","Breeding_system","Compatibility_system","Autonomous_selfing_level",
         "Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant",
         "Corolla_diameter_mean", "Corolla_length_mean", "Style_length", "Ovule_number", "life_form", "lifespan",
         "Plant_height_mean_m","Nectar_presence_absence")]


#check missing data now
missing_data <- unlist(lapply(t, function(x) sum(is.na(x))))/nrow(t)*100
sort(missing_data[missing_data >= 0], decreasing=T)

#seems ok to impute, just 36% percent of missing data in this column now.

########################################################################################################################################################
#4) CALCULATE PHYLOGENETIC DISTANCE TO CORRECT WITH EIGENVALUES IN THE IMPUTATION
########################################################################################################################################################

#Convert to factor 
cols <- c("Order_all", "Family_all", "Genus_all", "Species_all", "Compatibility_system", "Autonomous_selfing_level", "Flower_morphology",
          "Flower_symmetry", "life_form", "lifespan","Nectar_presence_absence")
t[cols] <- lapply(t[cols], factor)  ## as.factor() could also be used
str(t)


#Impute data considering phylogenetic distance of species
#it improves quality of imputation
#https://doi.org/10.1111/2041-210X.12232 see ref for method comparisons with and without imputation

#Multiple imputation process with phylogeny --> Missforest
#https://doi.org/10.1111/2041-210X.12232
#althought it does not have an option we are going to add it as a column

#Before data imputation I need to calculate the phylo of the species
#remove not found species that do not run with get tree
cols.num <- c("Family_all","Genus_all","Species_all")
t[cols.num] <- sapply(t[cols.num],as.character)
t$Species_all <- gsub("Species_all_", "", t$Species_all)

t <- t[!t$Species_all == "Pinus luchuensis", ]   # remove gymnosperm species

#remove species that are not until species level
#ALL THE SPECIES WITH SP. ARE DELETD
t$Species_geonet <- gsub("M_PL_.*","",t$Species_geonet) #subsitute partial string for nothing
t$Species_geonet <- gsub(" $","", t$Species_geonet, perl=T) #remove trailing spaces
t$Species_geonet <-  sub('sp.1$', 'sp.', t$Species_geonet)#PREPARE ALL SP TO SP.
t$Species_geonet <- sub('sp.2$', 'sp.', t$Species_geonet)#PREPARE ALL SP TO SP.
t$Species_geonet <- sub('sp$', 'sp.', t$Species_geonet) #PREPARE ALL SP TO SP.
t$Species_geonet <- sub("sp.$", "DELETE", t$Species_geonet) #change all sp. for DELETE
t <- t[- grep("DELETE",  t$Species_geonet),] #remove species with "DELETE"
#CHECK LEVELS
levels(factor(t$Species_all))

#Make these NA's as NA
t$Species_all[t$Species_all=="NA"] <- NA
t$Family_all[t$Family_all=="NA"] <- NA
t$Genus_all[t$Genus_all=="NA"] <- NA
#remove NA's
t <- t[!is.na(t$Family_all),]
t <- t[!is.na(t$Species_all),]
t <- t[!is.na(t$Genus_all),]

#Select complete cases for each of these quantitative variables for PPCA without missing values
t_1 <- t[complete.cases(t$Flowers_per_plant),]
t_2 <- t_1[complete.cases(t_1$Plant_height_mean_m),]
t_3 <- t_2[complete.cases(t_2$Nectar_presence_absence),]
t_4 <- t_3[complete.cases(t_3$Style_length),]
t_5 <- t_4[complete.cases(t_4$Corolla_diameter_mean),]
t_6 <- t_5[complete.cases(t_5$Ovule_number),]
t_7 <- t_6[complete.cases(t_6$Autonomous_selfing_level_fruit_set),]
########################################################################################################################################################
#7) SAVE DATA
########################################################################################################################################################
write.csv(t_7, "Data/Csv/imputed_trait_data_complete.cases.csv")
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################




