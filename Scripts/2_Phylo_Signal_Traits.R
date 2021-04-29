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
########################################################################################################################################################
#READ DATA
########################################################################################################################################################
forest_data <- read.csv("Data/Csv/all_species_imputed_trait_data_forest_data.csv")
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


#SELFING
selfing <- phylosig(tree=phylo_output,x=d$Autonomous_selfing_level_fruit_set,method="lambda",test=TRUE)
#FLOWER NUMBER
flower_n <- phylosig(tree=phylo_output,x=d$Flowers_per_plant,method="lambda",test=TRUE)
#FLOWER WIDTH
flower_width <- phylosig(tree=phylo_output,x=d$Corolla_diameter_mean,method="lambda",test=TRUE)
#FLOWER LENGTH
flower_length <- phylosig(tree=phylo_output,x=d$Corolla_length_mean,method="lambda",test=TRUE)
#STYLE LENGTH
style_n <- phylosig(tree=phylo_output,x=d$Style_length,method="lambda",test=TRUE)
#OVULE NUMBER
ovule_n <- phylosig(tree=phylo_output,x=d$Ovule_number,method="lambda",test=TRUE)
#PLANT HEIGHT
plant_height <- phylosig(tree=phylo_output,x=d$Plant_height_mean_m,method="lambda",test=TRUE)




str(forest_data)


forest_data[sapply(forest_data, is.character)] <- lapply(forest_data[sapply(forest_data, is.character)], as.factor)


nectar_signal <-  brm(Nectar_presence_absence~ 1 + (1|gr(phylo, cov = A)),
              data = forest_data, data2 = list(A = A_5), family  = bernoulli(), cores = 4,chains = 4, 
              sample_prior = TRUE, warmup = 1000, iter = 3000,
              control = list(adapt_delta = 0.99)) 

