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
library(missForest)
library(rtrees) #for phylogenetic distancelibrary(MASS)
library(ape)
library(PVR)
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
#just one cololumn with 68% missing data, then one with 35% and then 20% and 10%. The rest under 5%.
#it is recommended to remove columns over 50% but we are particulary interested in having numeric levels of quantitative selfing 
#we are going to keep it due to the little missing data of the other columns


is.na(t$Autonomous_selfing_level)
str(t_1$Autonomous_selfing_level)
is.na(t_1$Autonomous_selfing_level)

#use ifelse base r does not work with these NA'S Even with work around. base r does not like the na option of read excel
t$Autonomous_selfing_level_fruit_set <- ifelse(t$Autonomous_selfing_level %in% c("high") & is.na(t$Autonomous_selfing_level_fruit_set), 88, t$Autonomous_selfing_level_fruit_set)
t$Autonomous_selfing_level_fruit_set <- ifelse(t$Autonomous_selfing_level %in% c("medium") & is.na(t$Autonomous_selfing_level_fruit_set), 50.5, t$Autonomous_selfing_level_fruit_set)
t$Autonomous_selfing_level_fruit_set <- ifelse(t$Autonomous_selfing_level %in% c("low") & is.na(t$Autonomous_selfing_level_fruit_set), 13, t$Autonomous_selfing_level_fruit_set)
t$Autonomous_selfing_level_fruit_set <- ifelse(t$Autonomous_selfing_level %in% c("none") & is.na(t$Autonomous_selfing_level_fruit_set), 0, t$Autonomous_selfing_level_fruit_set)

#check missing data now
missing_data <- unlist(lapply(t, function(x) sum(is.na(x))))/nrow(t)*100
sort(missing_data[missing_data >= 0], decreasing=T)




#seems ok to impute, just 36% percent of missing data. 

########################################################################################################################################################
#4) PREPARE DATA FOR IMPUTATION--> INCLUDE PHYLOGENY AS A SINGLE COLUMN OF EIGENS FROM PVR
########################################################################################################################################################

#Convert to factor 
cols <- c("Order_all", "Family_all", "Genus_all", "Species_all", "IMPUTED_Compatibility", "Autonomous_selfing_level", "Flower_morphology",
          "Flower_symmetry", "life_form", "lifespan")
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
t <- t[!t$Species_all == "Diospyros seychellarum", ]
t <- t[!t$Species_all == "Memecylon eleagni", ]
t <- t[!t$Species_all == "Ocotea laevigata", ]
t <- t[!t$Species_all == "Soulamea terminaloides", ]
#Make these NA's as NA
t$Species_all[t$Species_all=="NA"] <- NA
t$Family_all[t$Family_all=="NA"] <- NA
t$Genus_all[t$Genus_all=="NA"] <- NA
#remove NA's
t <- t[!is.na(t$Family_all),]
t <- t[!is.na(t$Species_all),]
t <- t[!is.na(t$Genus_all),]

#calculate phylo 
phylo <- as.data.frame(cbind(t$Family_all, t$Genus_all, t$Species_all))
colnames(phylo) <-  c("family", "genus", "species")
#Select unique cases
#phylo_2 <- phylo[!duplicated(phylo$species),]
phylo_2 <- tibble(phylo)
#get phylo
phylo_output <- get_tree(sp_list = phylo, tree = tree_plant_otl, taxon = "plant")
str(phylo_output)
#Convert phylogenetic tree into matrix
A_5 <- vcv.phylo(phylo_output)
#Standardize to max value 1
A_5 <- A_5/max(A_5)
#Unify column names; remove underscore and remove asterik
rownames(A_5) <- gsub("\\*", "", rownames(A_5))
colnames(A_5) <- gsub("\\*", "", colnames(A_5))
colnames(A_5) <- gsub("_", " ", colnames(A_5))
rownames(A_5) <- gsub("_", " ", rownames(A_5))

#Decomposing phylogenetic distance matrix derived from tree into a set of orthogonal vectors
x <- PVRdecomp(phylo_output)
#create dataframe to merge with data and impute
phylo_impute <- data.frame(x@Eigen[1],x@phylo[2])
#Standardize to max value 1
phylo_impute$values <- phylo_impute$values/max(phylo_impute$values)
#fix colnames to merge
phylo_impute$tip.label <- gsub("_", " ", phylo_impute$tip.label)
#change colnames to merge
colnames(phylo_impute) <- c("Eigenval", "Species_all")
#merge
dat_phylo <- merge(t, phylo_impute, by="Species_all")

str(dat_phylo[5:21,])

dat_phylo <- dat_phylo[- grep("sp", dat_phylo$Species_all),]

#IMPUTE DATA
cols.num <- c("Family_all","Genus_all","Species_all")
dat_phylo[cols.num] <- sapply(dat_phylo[cols.num],as.factor)

rownames(dat_phylo) <- (dat_phylo$Species_all)

str(dat_phylo)




#kfold (k=200 for repeatable results)
res.ncp<-estim_ncpFAMD(dat_phylo[,c(5:21)],ncp.max=5,nbsim=200)
res.ncp
#Compare imputation methods
t_imputed <- imputeFAMD(dat_phylo[,c(5:21)], ncp=3,threshold = 1e-06) 

#fraxinus america...
#error for FAMD
library(missMDA)
library(missForest)
library(tcltk)
library(mvtnorm)
#source useful functions

source("estim_ncpFAMD.R")
source("criteria.r")
source("createdata.r")
source("MAR.r")
Error(ximp=t_imputed,xmis=Tips.na,xtrue=Tips)

imp_forest <- missForest(dat_phylo[,c(5:21)], maxiter = 10, ntree = 100,variablewise = TRUE,verbose = TRUE)







#Conduct data imputation
res.imp.famd<-imputeFAMD(Tips.na,ncp=ncp)

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
