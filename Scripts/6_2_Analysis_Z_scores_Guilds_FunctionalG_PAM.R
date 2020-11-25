########################################################################################################################################################
#SCRIPT FOR ANALYSIS (Z-SCORES~FUNCTIONAL GROUP*GUILD)

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

########################################################################################################################################################
#1) READ DATA
########################################################################################################################################################

d <- read.csv("Data/Csv/quantitative_networks_Z_scores_with_traits_and_14_clusters.csv", row.names = 1)

########################################################################################################################################################
#2) PHYLOGENETIC DISTANCE OF THE SPECIES
########################################################################################################################################################

#Prepare species, genus and family for calculating phylogenetic distance
d$Family_all <- as.character(d$Family_all)
d$Genus_all <- as.character(d$Genus_all)
d$Species_all <- as.character(d$Species_all)

#These species are not present for analysis in the matrix, remove or model cannot have phylo as covariable
d$Species_all[d$Species_all=="Diospyros seychellarum"] <- NA
d$Species_all[d$Species_all=="Memecylon eleagni"] <- NA
d$Species_all[d$Species_all=="Ocotea laevigata"] <- NA
d$Species_all[d$Species_all=="Soulamea terminaloides"] <- NA

#Make these NA's as NA
d$Species_all[d$Species_all=="NA"] <- NA
d$Family_all[d$Family_all=="NA"] <- NA
d$Genus_all[d$Genus_all=="NA"] <- NA
#remove NA's
d_1 <- d[!is.na(d$Family_all),]
d_1 <- d[!is.na(d$Species_all),]
d_1 <- d[!is.na(d$Genus_all),]

#prepare dataframe to calculate tree
phylo <- as.data.frame(cbind(d_1$Family_all, d_1$Genus_all, d_1$Species_all))
colnames(phylo) <-  c("family", "genus", "species")

#Select unique cases
phylo_1 <- phylo[!duplicated(phylo$species),]
phylo_2 <- tibble(phylo_1)
str(phylo_2)

phylo_3 <- get_tree(sp_list = phylo_2, tree = tree_plant_otl, taxon = "plant")

#Convert phylogenetic tree into matrix
A <- vcv.phylo(phylo_3)

#Standardize to max value 1
A <- A/max(A)

#Unify column names; remove underscore and remove asterik
rownames(A) <- gsub("\\*", "", rownames(A))
colnames(A) <- gsub("\\*", "", colnames(A))
colnames(A) <- gsub("_", " ", colnames(A))
rownames(A) <- gsub("_", " ", rownames(A))

#Add phylo column to dataset
d_1$phylo
d_1$phylo <- d_1$Species_all
str(d_1)

d_1$Clusters <- as.character(d_1$Clusters)
d_1$Clusters[d_1$Clusters=="1"] <- "A"
d_1$Clusters[d_1$Clusters=="2"] <- "B"
d_1$Clusters[d_1$Clusters=="3"] <- "C"
d_1$Clusters[d_1$Clusters=="4"] <- "D"
d_1$Clusters[d_1$Clusters=="5"] <- "E"
d_1$Clusters[d_1$Clusters=="6"] <- "F"
d_1$Clusters[d_1$Clusters=="7"] <- "G"
d_1$Clusters[d_1$Clusters=="8"] <- "H"
d_1$Clusters[d_1$Clusters=="9"] <- "I"
d_1$Clusters[d_1$Clusters=="10"] <- "J"
d_1$Clusters[d_1$Clusters=="11"] <- "K"
d_1$Clusters[d_1$Clusters=="12"] <- "L"
d_1$Clusters[d_1$Clusters=="13"] <- "M"
d_1$Clusters[d_1$Clusters=="14"] <- "N"

d_1$Clusters <- as.factor(d_1$Clusters)
levels(as.factor(d_1$Clusters))
str(d_1)
t_l <-d_1[d_1$Clusters=="D",]
levels(t_l$Breeding_system.y)
########################################################################################################################################################
#3) Analysis
########################################################################################################################################################
m1_PAM <- brm(Z_scores ~ guild*Clusters + (1|Id) + (1|gr(phylo, cov = A)),
          data = d_1, family  = gaussian(),data2 = list(A = A), cores = 4,
          sample_prior = TRUE, warmup = 500, iter = 1500,save_all_pars=T,
          control = list(adapt_delta = 0.99)) 


conditional_effects(m1_PAM, effects = "Clusters:guild")

summary(m1)
library(ggplot2)
pp_check(m1) +xlim(-10,10)+ylim(0,3)
c_e <- conditional_effects(m1) 
p1 <- plot(c_e, points=T,plot = FALSE)[[1]]
bayes_R2(m1)

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

