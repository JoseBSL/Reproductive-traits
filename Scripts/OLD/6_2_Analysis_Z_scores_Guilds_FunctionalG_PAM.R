########################################################################################################################################################
#SCRIPT FOR ANALYSIS (Z-SCORES~FUNCTIONAL GROUP*GUILD) ##HCLUST##

#1) LOAD DATA 

#2) PHYLOGENETIC DISTANCE OF THE SPECIES -Add them as covariable-

#3) ANALYSIS

########################################################################################################################################################

#LOAD LIBRARIES
library(ape) #for phylogenetic distance
library(dplyr) #data processing
library(rtrees) #for phylogenetic distance
library(DHARMa)
library(brms)
library(cmdstanr)
library(ggplot2)
########################################################################################################################################################
#1) READ DATA
########################################################################################################################################################

d_5 <- read.csv("Data/Csv/quantitative_networks_Z_scores_with_traits_and_5_clusters_pam.csv", row.names = 1)
d_14 <- read.csv("Data/Csv/quantitative_networks_Z_scores_with_traits_and_14_clusters_pam.csv", row.names = 1)

########################################################################################################################################################
#2) PHYLOGENETIC DISTANCE OF THE SPECIES
########################################################################################################################################################

#5 CLUSTERS
#Prepare species, genus anD_5 family for calculating phylogenetic distance
d_5$Family_all <- as.character(d_5$Family_all)
d_5$Genus_all <- as.character(d_5$Genus_all)
d_5$Species_all <- as.character(d_5$Species_all)

#These species are not present for analysis in the matrix, remove or model cannot have phylo as covariable
d_5$Species_all[d_5$Species_all=="Diospyros seychellarum"] <- NA
d_5$Species_all[d_5$Species_all=="Memecylon eleagni"] <- NA
d_5$Species_all[d_5$Species_all=="Ocotea laevigata"] <- NA
d_5$Species_all[d_5$Species_all=="Soulamea terminaloides"] <- NA

#Make these NA's as NA
d_5$Species_all[d_5$Species_all=="NA"] <- NA
d_5$Family_all[d_5$Family_all=="NA"] <- NA
d_5$Genus_all[d_5$Genus_all=="NA"] <- NA
#remove NA's
d_5_1 <- d_5[!is.na(d_5$Family_all),]
d_5_1 <- d_5[!is.na(d_5$Species_all),]
d_5_1 <- d_5[!is.na(d_5$Genus_all),]

#prepare dataframe to calculate tree
phylo_5 <- as.data.frame(cbind(d_5_1$Family_all, d_5_1$Genus_all, d_5_1$Species_all))
colnames(phylo_5) <-  c("family", "genus", "species")

#Select unique cases
phylo_5_1 <- phylo_5[!duplicated(phylo_5$species),]
phylo_5_2 <- tibble(phylo_5_1)
str(phylo_5_2)

phylo_5_3 <- get_tree(sp_list = phylo_5_2, tree = tree_plant_otl, taxon = "plant")

#Convert phylogenetic tree into matrix
A_5 <- vcv.phylo(phylo_5_3)
#Standardize to max value 1
A_5 <- A_5/max(A_5)
#Unify column names; remove underscore and remove asterik
rownames(A_5) <- gsub("\\*", "", rownames(A_5))
colnames(A_5) <- gsub("\\*", "", colnames(A_5))
colnames(A_5) <- gsub("_", " ", colnames(A_5))
rownames(A_5) <- gsub("_", " ", rownames(A_5))

#Add phylo column to dataset
d_5_1$phylo
d_5_1$phylo <- d_5_1$Species_all
str(d_5_1)

d_5_1$Clusters <- as.character(d_5_1$Clusters)
d_5_1$Clusters[d_5_1$Clusters=="1"] <- "A"
d_5_1$Clusters[d_5_1$Clusters=="2"] <- "B"
d_5_1$Clusters[d_5_1$Clusters=="3"] <- "C"
d_5_1$Clusters[d_5_1$Clusters=="4"] <- "D"
d_5_1$Clusters[d_5_1$Clusters=="5"] <- "E"

d_5_1$Clusters <- as.factor(d_5_1$Clusters)
levels(as.factor(d_5_1$Clusters))

t_l <-d_5_1[d_5_1$Clusters=="D",]
levels(t_l$Breeding_system.y)


#14 CLUSTERS
#Prepare species, genus anD_5 family for calculating phylogenetic distance
d_14$Family_all <- as.character(d_14$Family_all)
d_14$Genus_all <- as.character(d_14$Genus_all)
d_14$Species_all <- as.character(d_14$Species_all)

#These species are not present for analysis in the matrix, remove or model cannot have phylo as covariable
d_14$Species_all[d_14$Species_all=="Diospyros seychellarum"] <- NA
d_14$Species_all[d_14$Species_all=="Memecylon eleagni"] <- NA
d_14$Species_all[d_14$Species_all=="Ocotea laevigata"] <- NA
d_14$Species_all[d_14$Species_all=="Soulamea terminaloides"] <- NA

#Make these NA's as NA
d_14$Species_all[d_14$Species_all=="NA"] <- NA
d_14$Family_all[d_14$Family_all=="NA"] <- NA
d_14$Genus_all[d_14$Genus_all=="NA"] <- NA
#remove NA's
d_14_1 <- d_14[!is.na(d_14$Family_all),]
d_14_1 <- d_14[!is.na(d_14$Species_all),]
d_14_1 <- d_14[!is.na(d_14$Genus_all),]

#prepare dataframe to calculate tree
phylo_14 <- as.data.frame(cbind(d_14_1$Family_all, d_14_1$Genus_all, d_14_1$Species_all))
colnames(phylo_14) <-  c("family", "genus", "species")

#Select unique cases
phylo_14_1 <- phylo_14[!duplicated(phylo_14$species),]
phylo_14_2 <- tibble(phylo_14_1)
str(phylo_14_2)

phylo_14_3 <- get_tree(sp_list = phylo_14_2, tree = tree_plant_otl, taxon = "plant")

#Convert phylogenetic tree into matrix
A_14 <- vcv.phylo(phylo_14_3)
#Standardize to max value 1
A_14 <- A_14/max(A_14)
#Unify column names; remove underscore and remove asterik
rownames(A_14) <- gsub("\\*", "", rownames(A_14))
colnames(A_14) <- gsub("\\*", "", colnames(A_14))
colnames(A_14) <- gsub("_", " ", colnames(A_14))
rownames(A_14) <- gsub("_", " ", rownames(A_14))

#Add phylo column to dataset
d_14_1$phylo
d_14_1$phylo <- d_14_1$Species_all
str(d_14_1)

#rename levels
d_14_1$Clusters <- as.character(d_14_1$Clusters)
d_14_1$Clusters[d_14_1$Clusters=="1"] <- "A"
d_14_1$Clusters[d_14_1$Clusters=="2"] <- "B"
d_14_1$Clusters[d_14_1$Clusters=="3"] <- "C"
d_14_1$Clusters[d_14_1$Clusters=="4"] <- "D"
d_14_1$Clusters[d_14_1$Clusters=="5"] <- "E"
d_14_1$Clusters[d_14_1$Clusters=="6"] <- "F"
d_14_1$Clusters[d_14_1$Clusters=="7"] <- "G"
d_14_1$Clusters[d_14_1$Clusters=="8"] <- "H"
d_14_1$Clusters[d_14_1$Clusters=="9"] <- "I"
d_14_1$Clusters[d_14_1$Clusters=="10"] <- "J"
d_14_1$Clusters[d_14_1$Clusters=="11"] <- "K"
d_14_1$Clusters[d_14_1$Clusters=="12"] <- "L"
d_14_1$Clusters[d_14_1$Clusters=="13"] <- "M"
d_14_1$Clusters[d_14_1$Clusters=="14"] <- "N"

#CONVERT BACK TO FACTOR
d_14_1$Clusters <- as.factor(d_14_1$Clusters)
#CHECK LEVELS
levels(as.factor(d_14_1$Clusters))

########################################################################################################################################################
#3) Analysis
########################################################################################################################################################

#STUDENT
#5 clusters hclust
m_5_clust_stu_pam <- brm(Z_scores ~ guild*Clusters + (1|Id) + (1|gr(phylo, cov = A)),
                            data = d_5_1, family  = student(),data2 = list(A = A_5), cores = 4,chains = 4, 
                            sample_prior = TRUE, warmup = 500, iter = 1500,
                            control = list(adapt_delta = 0.99)) 


marginal_effects(m_5_clust_stu_pam, effects = "Clusters:guild")
pp_check(m_5_clust_stu_pam) +xlim(-10,10)+ylim(0,3)
pp_check(m_5_clust_stu_pam, type='violin_grouped',group="Clusters")+ylim(-4,4)
pp_check(m_5_clust_stu_pam, type='violin_grouped',group="guild")+ylim(-4,4)

#SAVE MODEL
setwd("~/Dropbox/PhD/R") #DROPBOX, files too large for github
saveRDS(m_5_clust_stu_pam, "m_5_clust_stu_pam.RDS")


#14 clusters hclust
m_14_clust_stu_pam <- brm(Z_scores ~ guild*Clusters + (1|Id) + (1|gr(phylo, cov = A)),
                             data = d_14_1, family  = student(),data2 = list(A = A_14), cores = 4,chains = 4,  
                             backend = "cmdstanr",
                             sample_prior = TRUE, warmup = 500, iter = 1500,
                             control = list(adapt_delta = 0.99)) 

marginal_effects(m_14_clust_stu_pam, effects = "Clusters:guild")
pp_check(m_14_clust_stu_pam) +xlim(-10,10)+ylim(0,3)
pp_check(m_14_clust_stu_pam, type='violin_grouped',group="Clusters")+ylim(-4,4)
pp_check(m_14_clust_stu_pam, type='violin_grouped',group="guild")+ylim(-4,4)

#SAVE MODEL
saveRDS(m_14_clust_stu_pam, "m_14_clust_stu_pam.RDS")












#NEXT PART EXPLORE CLUSTERS sTABLE SUMMARY
#select unique species
d_2 <- d_1[!duplicated(d_1$Species_all),]
#select columns of interest
str(d_2)
#select columns of interest
t <- d_2[c("Order_all","Family_all","Genus_all","Species_all","Breeding_system.y","IMPUTED_Compatibility.y","Autonomous_selfing_level.y",
           "Autonomous_selfing_level_fruit_set.x", "Flower_morphology.y", "Flower_symmetry.y", "Flowers_per_plant.x", "Flowers_per_inflorescence.x",
           "Floral_unit_width.x", "Corolla_diameter_mean.x", "Corolla_length_mean.x", "STYLE_IMPUTED.x", "OVULES_IMPUTED.x", "life_form.y", "lifespan.y",
           "Clusters")]


unique_spp <- t  %>% group_by(Clusters) %>% do(the_summary = summary(.))
unique_spp$the_summary
#Prepare summary in order to see the different modes/means between groups


########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

devtools::install_github("daijiang/phyr", force=T)

install.packages("brms")
