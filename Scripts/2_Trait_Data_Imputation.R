########################################################################################################################################################
#SCRIPT FOR DATA IMPUTATION

#1)READ TRAIT DATA

#2)CLEAN DATA

#3)PREPARE LEVELS

#3)EXPLORE MISSING DATA

#4)IMPUTE DATA
########################################################################################################################################################

#LOAD LIBRARIES
library(readxl) #read excel file (trait data)
library(dplyr) #data manipulation
library(missMDA) #For missing values
library(FactoMineR) #Produce pca biplot with individuals and variables
library(factoextra)
library(ggpubr)

########################################################################################################################################################
#1) READ TRAIT DATA
########################################################################################################################################################

#load data
trait_data <- read_excel("Data/Trait_data_raw/Trait_data_final.xlsx",na = "NA")

########################################################################################################################################################
#2) CLEAN DATA
########################################################################################################################################################

#select just filled rows
trait_data_1 <- trait_data[1:1701,]
#filter data, select species with flower level info and capitulum
trait_filtered <- filter(trait_data_1, Info_level == "flower" |  Info_level == "capitulum")
levels(as.factor(trait_filtered$Info_level)) #checking levels
#remove NA's and duplicated species
trait_filtered$Species_all[trait_filtered$Species_all=="NA"]<-NA
trait_filtered_1 <- trait_filtered[!is.na(trait_filtered$Species_all),]
trait_filtered_2 <- trait_filtered_1[!duplicated(trait_filtered_1$Species_all),]


#select columns of interest
t <- trait_filtered_2[c("Order_all","Family_all","Genus_all","Species_all","Breeding_system","IMPUTED_Compatibility","Autonomous_selfing_level",
                  "Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant", "Flowers_per_inflorescence",
                  "Floral_unit_width", "Corolla_diameter_mean", "Corolla_length_mean", "STYLE_IMPUTED", "OVULES_IMPUTED", "life_form", "lifespan",
                  "IMPUTED_plant_height_mean_m")]


########################################################################################################################################################
#3)PREPARE LEVELS
########################################################################################################################################################

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
#3)EXPLORE MISSING DATA
########################################################################################################################################################

#Explore patterns of missing data
missing_data <- unlist(lapply(t, function(x) sum(is.na(x))))/nrow(t)
sort(missing_data[missing_data >= 0], decreasing=T)
#just one cololumn with 65% missing data, then one with 35% and then 20% and 10%. The rest under 5%.
#it is recommended to remove columns over 50% but we are particulary interested in having numeric levels of quantitative selfing 
#we are going to keep it due to the little missing data of the other columns
########################################################################################################################################################
#4)IMPUTE DATA
########################################################################################################################################################

#Convert to factor 
cols <- c("Order_all", "Family_all", "Genus_all", "Species_all", "IMPUTED_Compatibility", "Autonomous_selfing_level", "Flower_morphology",
          "Flower_symmetry", "life_form", "lifespan")
t[cols] <- lapply(t[cols], factor)  ## as.factor() could also be used
str(t)

#Just one variable is above 50%
#Conduct data imputation
t_imputed <- imputeFAMD(t, ncp=3,threshold = 1e-06) 
head(t_imputed$completeObs)
#looks that it has been done well
#I'm going to fix the other two columns

t_imputed$completeObs$Family_all <- gsub("Family_all_", "", t_imputed$completeObs$Family_all)
t_imputed$completeObs$Genus_all <- gsub("Genus_all_", "", t_imputed$completeObs$Genus_all)
head(t_imputed$completeObs)

write.csv(t_imputed$completeObs, "Data/Csv/all_species_imputed_trait_data.csv")

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
