##################################################
########
######
####
##
# Script to download the phylogeny from megatrees 
##
###
####
#####
#################################################


#Install package
#devtools::install_github("daijiang/rtrees")

#Load packages
library(rtrees)
library(ape)
library(readxl) #to read data in excel format
library(tibble)


#Load species data
t <- read_excel("Data/Data_raw/Trait_data_final.xlsx")
t_1 <- t[,c(7,8,9)]
colnames(t_1) <- c("family", "genus", "species")
#Select species unique cases
t_2 <- t_1[!duplicated(t_1[3]),]

#Following the github code https://github.com/daijiang/rtrees
species_list = tibble(species=t_2$species, genus=t_2$genus, family=t_2$family)


full_tree = get_tree(sp_list = species_list,
                     tree = tree_plant_otl, # either 
                     taxon = "plant", # or
                     scenario = "S1",
                     show_grafted = TRUE)

#plot(ladderize(full_tree), no.margin = T)
#plot(get_tree(sp_list = species_list, tree = full_tree, taxon = "plant",
              #show_grafted = T, tree_by_user = T), type = "fan")
class(full_tree)

write.nexus(full_tree, file="Data/Data_phylogeny/tree.nexus", translate = TRUE)

#Now download phylogeny just for the species of Beeefun
all_df <- readRDS("Data/RData/network_metrics.RData")
d <- all_df[,c(11,12,13)]
colnames(d) <- c("family", "genus", "species")
d_1 <- d[!duplicated(d[3]),]

bee_fun_tree = get_tree(sp_list = d_1,
                     tree = tree_plant_otl, # either 
                     taxon = "plant", # or
                     scenario = "S1",
                     show_grafted = TRUE)

write.nexus(bee_fun_tree, file="Data/Data_phylogeny/bee_fun_tree.nexus", translate = TRUE)
