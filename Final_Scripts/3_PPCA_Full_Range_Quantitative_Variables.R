########################################################################################################################################################
#SCRIPT TO CALCULATE THE PHLOGENETIC INFORMED PRINCIPAL COMPONENT ANALYSISWITH FULL SET OF SPECICES
#Not exluding numeric values out of the range 2.5-97.5
########################################################################################################################################################
#LOAD LIBRARIES
library(phytools) #ppca
library(ape) #for phylogenetic distance
library(dplyr) #data processing
library(rtrees) #for phylogenetic distancelibrary(MASS)
library(reshape2) #data processing
library(MASS) #I think I used it for the kernel density of the plotting function; no longer used but I leave in case its handy later on
library(ggplot2) #plotting
library(broman) #crayon colours
library(magick) #add images
########################################################################################################################################################
#1) LOAD DATA
########################################################################################################################################################
#read data with missing values filled by data imputation
dat <- read.csv("Data/Csv/all_species_imputed_trait_data_forest_data.csv", row.names = "X")
########################################################################################################################################################
#2) Tidy up data to get phylo distance and conduct PCA
########################################################################################################################################################
########################################################################################################################################################
#remove not found species, cannot do PCA with unequal numbers of rows
cols.num <- c("Family_all","Genus_all","Species_all")
dat[cols.num] <- sapply(dat[cols.num],as.character)
dat$Species_all <- gsub("Species_all_", "", dat$Species_all)
dat <- dat[!dat$Species_all == "Diospyros seychellarum", ]
dat <- dat[!dat$Species_all == "Memecylon eleagni", ]
dat <- dat[!dat$Species_all == "Ocotea laevigata", ]
dat <- dat[!dat$Species_all == "Soulamea terminaloides", ]
########################################################################################################################################################
#3) SELECT DATA OF INTEREST
########################################################################################################################################################
dat_cleaning <- dat[,c(2,3,4,8,11,12,14,15,18)]
#CHECK LEVELS
str(dat)

#LOG TRANSFORM AND SCALE DATA
#CHECK LEVELS
str(dat_cleaning)
dat_cleaning[,c(4:9)] <- log(dat_cleaning[,c(4:9)]+1)
dat_cleaning[,c(4:9)] <- scale(dat_cleaning[,c(4:9)], center = T, scale = T)


final_d <- dat_cleaning[,c(4:9)]
########################################################################################################################################################
#4) GET PHYLO
########################################################################################################################################################
#calculate phylo 
phylo <- as.data.frame(cbind(dat_cleaning$Family_all, dat_cleaning$Genus_all, dat_cleaning$Species_all))
colnames(phylo) <-  c("family", "genus", "species")
#Select unique cases
#phylo_2 <- phylo[!duplicated(phylo$species),]
phylo_2 <- tibble(phylo)
#get phylo
phylo_output <- get_tree(sp_list = phylo_2, tree = tree_plant_otl, taxon = "plant")
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

########################################################################################################################################################
#4) CALCULATE PPCA
########################################################################################################################################################
#set same rownames
final_d <-  dat_cleaning[,c(4:9)]

rownames(final_d) <- dat_cleaning$Species_all
#fix species names
rownames(final_d) <- gsub(" ", "_", rownames(final_d))

#Output saved not RUN
phyl_pca_all_numeric_values <- phyl.pca(phylo_output, final_d,method="lambda",mode="cov")

####
#SAVE PHYLO PCA OUTPUT
####
saveRDS(phyl_pca_all_numeric_values, "Data/RData/phyl_pca_all_numeric_values.rds")
#SAVE ALSO DATA TO PLOT IT IN RMD file
saveRDS(dat_cleaning, "Data/RData/data_pca_all_numeric_values.rds")
