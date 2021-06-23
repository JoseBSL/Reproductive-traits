########################################################################################################################################################
#SCRIPT TO CALCULATE PHYLOGENETIC SIGNAL OF TRAITS

#1 READ DATA

#2) CALCULATE PHYLO

########################################################################################################################################################
#LOAD LIBRARIES
library(rtrees) #for phylogenetic distancelibrary(MASS)
library(phytools) #to phylo signal
library(ape)
library(brms)
library(dplyr)
########################################################################################################################################################
#READ DATA
########################################################################################################################################################
forest_data <- read.csv("Data/Csv/all_species_imputed_for_phylo_signal.csv")
str(forest_data)
########################################################################################################################################################
#CALCULATE PHYLO
########################################################################################################################################################
#calculate phylo 
phylo <- as.data.frame(cbind(forest_data$Family_all, forest_data$Genus_all, forest_data$Species_all))
colnames(phylo) <-  c("family", "genus", "species")
#Select unique cases
#phylo_2 <- phylo[!duplicated(phylo$species),]
phylo_1 <- tibble(phylo)
#get phylo
phylo_output <- get_tree(sp_list = phylo_1, tree = tree_plant_otl, taxon = "plant")
#Convert phylogenetic tree into matrix
A_5 <- vcv.phylo(phylo_output)
#Standardize to max value 1
A_5 <- A_5/max(A_5)
#Unify column names; remove underscore and remove asterik
rownames(A_5) <- gsub("\\*", "", rownames(A_5))
colnames(A_5) <- gsub("\\*", "", colnames(A_5))
colnames(A_5) <- gsub("_", " ", colnames(A_5))
rownames(A_5) <- gsub("_", " ", rownames(A_5))

#Add phylo column to dataset
forest_data$phylo
forest_data$phylo <- forest_data$Species_all
########################################################################################################################################################
#CALCULATE PHYLO SIGNAL FOR QUANTITATIVE TRAITS (LAMBDA)
########################################################################################################################################################


#SUBSET NUMERICAL COLUMNS R
nums <- unlist(lapply(forest_data, is.numeric))  
d <- forest_data[ , nums]

str(forest_data)

rownames(d) <- forest_data$Species_all

#Remove underscore from phylo
phylo_output$tip.label <- gsub("_", " ", phylo_output$tip.label)


#SELFING
selfing <- phylosig(phylo_output,setNames(d[,"Autonomous_selfing_level_fruit_set"],rownames(d)),method="lambda",test=TRUE)
#FLOWER NUMBER
flower_n <- phylosig(phylo_output,setNames(d[,"Flowers_per_plant"],rownames(d)),method="lambda",test=TRUE)
#INFLO WIDTH
inflow_width <- phylosig(phylo_output,setNames(d[,"Floral_unit_width"],rownames(d)),method="lambda",test=TRUE)
#FLOWER WIDTH
flower_width <- phylosig(phylo_output,setNames(d[,"Corolla_diameter_mean"],rownames(d)),method="lambda",test=TRUE)
#FLOWER LENGTH
flower_length <- phylosig(phylo_output,setNames(d[,"Corolla_length_mean"],rownames(d)),method="lambda",test=TRUE)
#STYLE LENGTH
style_n <- phylosig(phylo_output,setNames(d[,"Style_length"],rownames(d)),method="lambda",test=TRUE)
#OVULE NUMBER
ovule_n <- phylosig(phylo_output,setNames(d[,"Ovule_number"],rownames(d)),method="lambda",test=TRUE)
#PLANT HEIGHT
plant_height <- phylosig(phylo_output,setNames(d[,"Plant_height_mean_m"],rownames(d)),method="lambda",test=TRUE)


#Save output to plot in an Rmd file
#SELFING
saveRDS(selfing, "Data/RData/selfing_phylo.rds")
#FLOWER NUMBER
saveRDS(flower_n, "Data/RData/flower_n_phylo.rds")
#INFLO WIDTH
saveRDS(inflow_width, "Data/RData/inflow_width_phylo.rds")
#FLOWER WIDTH
saveRDS(flower_width, "Data/RData/flower_width_phylo.rds")
#FLOWER LENGHT
saveRDS(flower_length, "Data/RData/flower_length_phylo.rds")
#STYLE LENGTH
saveRDS(style_n, "Data/RData/style_n_phylo.rds")
#OVULE NUMBER
saveRDS(ovule_n, "Data/RData/ovule_n_phylo.rds")
#PLANT HEIGHT
saveRDS(plant_height, "Data/RData/plant_height_phylo.rds")

#Select two decimals function
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
#LABELS
label <- c("Autonomous selfing", "Flower number", "Inflorescence width" ,"Flower width", "Flower length", "Style length", "Ovule number", "Plant height")
#LAMBDA
lambda <- c(selfing$lambda , flower_n$lambda, inflow_width$lambda, flower_width$lambda, flower_length$lambda, style_n$lambda, ovule_n$lambda, plant_height$lambda)
#P-VALUE
p_value <- c(selfing$P , flower_n$P, inflow_width$P,flower_width$P, flower_length$P, style_n$P, ovule_n$P, plant_height$P)

tabla <- data.frame(label, specify_decimal(lambda,2), specify_decimal(p_value,2))

colnames(tabla) <- c("Variable", "Lambda", "P value")


########################################################################################################################################################
#Load dataset with pollen and nectar and calculate phylo signal for the subset of species 
########################################################################################################################################################

forest_data_1 <- read.csv("Data/Csv/nectar_pollen_subset_imputed_trait_data.csv")
nrow(forest_data_1)
#calculate phylo 
phylo <- as.data.frame(cbind(forest_data_1$Family_all, forest_data_1$Genus_all, forest_data_1$Species_all))
colnames(phylo) <-  c("family", "genus", "species")
#Select unique cases
#phylo_2 <- phylo[!duplicated(phylo$species),]
phylo_1 <- tibble(phylo)
#get phylo
phylo_output_1 <- get_tree(sp_list = phylo_1, tree = tree_plant_otl, taxon = "plant")
#Convert phylogenetic tree into matrix
A_5 <- vcv.phylo(phylo_output)
#Standardize to max value 1
A_5 <- A_5/max(A_5)
#Unify column names; remove underscore and remove asterik
rownames(A_5) <- gsub("\\*", "", rownames(A_5))
colnames(A_5) <- gsub("\\*", "", colnames(A_5))
colnames(A_5) <- gsub("_", " ", colnames(A_5))
rownames(A_5) <- gsub("_", " ", rownames(A_5))

#Add phylo column to dataset
forest_data_1$phylo
forest_data_1$phylo <- forest_data$Species_all

#SUBSET NUMERICAL COLUMNS R
nums <- unlist(lapply(forest_data_1, is.numeric))  
d_1 <- forest_data_1[ , nums]

str(forest_data_1)

rownames(d_1) <- forest_data_1$Species_all

#Remove underscore from phylo
phylo_output_1$tip.label <- gsub("_", " ", phylo_output_1$tip.label)


#Pollen
pollen <- phylosig(phylo_output_1,setNames(d_1[,"Pollen_per_flower"],rownames(d_1)),method="lambda",test=TRUE)
saveRDS(pollen, "Data/RData/pollen_phylo.rds")

#Nectar
Nectar_ul <- phylosig(phylo_output_1,setNames(d_1[,"Nectar_ul"],rownames(d_1)),method="lambda",test=TRUE)
saveRDS(Nectar_ul, "Data/RData/Nectar_ul_phylo.rds")

#Not included at the end
#Nectar_mg <- phylosig(phylo_output_1,setNames(d_1[,"Nectar_mg"],rownames(d_1)),method="lambda",test=TRUE)
#saveRDS(Nectar_mg, "Data/RData/Nectar_mg_phylo.rds")

Nectar_concentration <- phylosig(phylo_output_1,setNames(d_1[,"Nectar_concentration"],rownames(d_1)),method="lambda",test=TRUE)
saveRDS(Nectar_concentration, "Data/RData/Nectar_concentration_phylo.rds")



