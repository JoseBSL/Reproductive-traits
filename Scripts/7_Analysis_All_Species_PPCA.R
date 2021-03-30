########################################################################################################################################################
#SCRIPT TO CALCULATE THE PCA

#1) LOAD DATA 

########################################################################################################################################################
#LOAD LIBRARIES
library(phytools)
library(ape) #for phylogenetic distance
library(dplyr) #data processing
library(rtrees) #for phylogenetic distancelibrary(MASS)
library(reshape2)
library(viridis) #COLOUR GGPLOT
library(MASS)
library(ggplot2)
library(broman) #crayon colours
library(tidyverse)

theme_ms <- function(base_size=12, base_family="Helvetica") {
  (theme_bw(base_size = base_size, base_family = base_family)+
     theme(text=element_text(color="black"),
           axis.title=element_text( size = rel(1.3)),
           axis.text=element_text(size = rel(1), color = "black"),
           legend.title=element_text(face="bold"),
           legend.text=element_text(),
           legend.background=element_rect(fill="transparent"),
           legend.key.size = unit(0.9, 'lines'),
           panel.border=element_rect(color="black",size=1),
           panel.grid.minor.x =element_blank(),
           panel.grid.minor.y= element_blank(),
           panel.grid.major= element_blank()
     ))
}

########################################################################################################################################################
#1) LOAD DATA
########################################################################################################################################################
#read data with missing values filled by data imputation
dat <- read.csv("Data/Csv/all_species_imputed_trait_data_forest_data.csv", row.names = "X")
dat_1 <- read.csv("Data/Csv/imputed_trait_data_hclust_5_clusters_forest_data.csv", row.names = "X") 

dat$Clusters <- dat_1$Clusters
########################################################################################################################################################
#2) Tidy up data to get phylo distance and conduct PCA
########################################################################################################################################################
########################################################################################################################################################
#remove not found species, cannot do PCA with unequal numbers of rows
cols.num <- c("Family_all","Genus_all","Species_all")
dat[cols.num] <- sapply(dat[cols.num],as.character)
dat$Species_all <- gsub("Species_all_", "", dat$Species_all)
#dat <- dat[!dat$Species_all == "Diospyros seychellarum", ]
#dat <- dat[!dat$Species_all == "Memecylon eleagni", ]
#dat <- dat[!dat$Species_all == "Ocotea laevigata", ]
#dat <- dat[!dat$Species_all == "Soulamea terminaloides", ]
########################################################################################################################################################
#3) REMOVE OUTLIERS, OUT OF 2.5-97.5 RANGE
########################################################################################################################################################
dat_cleaning <- dat[,c(2,3,4,8,11,14,16,17,20,22)]

#dat_cleaning_1 <- dat_cleaning %>%
#  filter(between(Flowers_per_plant, quantile(Flowers_per_plant, 0.025), quantile(Flowers_per_plant, 0.975)))
#
#dat_cleaning_2 <- dat_cleaning_1 %>%
#  filter(between(Corolla_diameter_mean, quantile(Corolla_diameter_mean, 0.025), quantile(Corolla_diameter_mean, 0.975)))
#
#dat_cleaning_3 <- dat_cleaning_2 %>%
#  filter(between(STYLE_IMPUTED, quantile(STYLE_IMPUTED, 0.025), quantile(STYLE_IMPUTED, 0.975)))
#
#dat_cleaning_4 <- dat_cleaning_3 %>%
#  filter(between(OVULES_IMPUTED, quantile(OVULES_IMPUTED, 0.025), quantile(OVULES_IMPUTED, 0.975)))
#
#dat_cleaning_5 <- dat_cleaning_4 %>%
#  filter(between(IMPUTED_plant_height_mean_m, quantile(IMPUTED_plant_height_mean_m, 0.025), quantile(IMPUTED_plant_height_mean_m, 0.975)))

#dat_cleaning_6 <- dat_cleaning_5 %>%
# filter(between(Autonomous_selfing_level_fruit_set, quantile(Autonomous_selfing_level_fruit_set, 0.025), quantile(Autonomous_selfing_level_fruit_set, 0.975)))


#LOG all columns, seems neccesary to standardize skewed data


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
#phyl_pca_famd_1 <- phyl.pca(phylo_output, final_d,method="lambda",mode="cov")
pca_all_species <- phyl.pca(phylo_output, final_d,method="lambda",mode="cov")



#REMEMBER TO SAVE OUTPUT TOMORROW


#Convert to dataframe
phylo_pca <- as.data.frame(pca_all_species$S)

#Convert rownames to colnames
phylo_pca_1 <-phylo_pca %>% rownames_to_column( var = "Species_all" )

#Remove underscore
phylo_pca_1$Species_all <- gsub("_", " ", phylo_pca_1$Species_all)


#Load trait data
d_5 <- read.csv("Data/Csv/quantitative_networks_Z_scores_with_traits_and_5_clusters_hclust_forest_data.csv", row.names = 1)
levels(factor(d_5$guild))
#I'm going to run the model just with the main poll. functional groups
#exclude other insects level
dat <- subset(d_5, guild!="Other_insects" & guild!="Lizards" & guild!="Birds")
levels(factor(dat$guild))
dat$guild <- as.character(dat$guild)
dat$guild[dat$guild=="Bee"] <- "Bees"
dat$guild <- factor(dat$guild, levels = c("Bees","Coleoptera", "Lepidoptera", "Non-bee-Hymenoptera",
                                          "Non-syrphids-diptera", "Syrphids"))


#Merge
dat_analysis <- merge(dat, phylo_pca_1, by="Species_all", all.x = T)


########################################################################################################################################################
#2) PHYLOGENETIC DISTANCE OF THE SPECIES
########################################################################################################################################################
#5 CLUSTERS
#Prepare species, genus anD_5 family for calculating phylogenetic distance
dat_analysis$Family_all  <- as.character(dat_analysis$Family_all)
dat_analysis$Genus_all   <- as.character(dat_analysis$Genus_all)
dat_analysis$Species_all <- as.character(dat_analysis$Species_all)


#prepare dataframe to calculate tree
phylo_5 <- as.data.frame(cbind(dat_analysis$Family_all, dat_analysis$Genus_all, dat_analysis$Species_all))
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
dat_analysis$phylo
dat_analysis$phylo <- dat_analysis$Species_all
str(dat)

dat_analysis$Clusters <- as.character(dat_analysis$Clusters)
dat_analysis$Clusters[dat_analysis$Clusters=="1"] <- "A"
dat_analysis$Clusters[dat_analysis$Clusters=="2"] <- "B"
dat_analysis$Clusters[dat_analysis$Clusters=="3"] <- "C"
dat_analysis$Clusters[dat_analysis$Clusters=="4"] <- "D"
dat_analysis$Clusters[dat_analysis$Clusters=="5"] <- "E"

dat_analysis$Clusters <- as.factor(dat_analysis$Clusters)
levels(as.factor(dat_analysis$Clusters))
levels(factor(dat_analysis$guild))

########################################################################################################################################################
#3.1)ANALYSIS
########################################################################################################################################################

#5 clusters hclust
model_1_all_spp <- brm((Interaction-1) ~ PC1*guild+PC2*guild + (1|Id) + (1|gr(phylo, cov = A)),
                         data = dat_analysis, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                         sample_prior = TRUE, warmup = 500, iter = 2000,
                         control = list(adapt_delta = 0.99))


bayes_R2(model_1_all_spp)
loo_R2(model_1_all_spp)

marginal_effects(model_1_all_spp, effects = "PC1:guild")
marginal_effects(model_1_all_spp, effects = "PC2:guild")

pp_check(model_1_all_spp) +xlim(-50,200)+ylim(0,0.1)


#Plot nicely PC1

ce_pc1 <- conditional_effects(model_1_all_spp, effects = "PC1:guild",points=T) 

ggplot(ce_pc1[[1]], aes(x = PC1, y = (estimate__+1), group=guild, colour=guild)) + geom_point(data = dat_analysis,
  aes(x = PC1, y = Interaction),size = 1.5, alpha=0.9) + geom_line(size=1.2) + 
  ylim(0,quantile(dat_analysis$Interaction, 0.95))  + ylab("Number of visits")+
  theme_ms()

#Plot nicely PC2
ce_pc2 <- conditional_effects(model_1_all_spp, effects = "PC2:guild",points=T) 

ggplot(ce_pc2[[1]], aes(x = PC2, y = (estimate__+1), group=guild, colour=guild)) + geom_point(data = dat_analysis,
  aes(x = PC2, y = Interaction),size = 1, alpha=0.75) + geom_line(size=1.2) + 
  ylim(0,quantile(dat_analysis$Interaction, 0.95)) + ylab("Number of visits")+
  theme_ms()

########################################################################################################################################################
#4 MAIN PC´S ALL SPECIES
########################################################################################################################################################

model_1_all_spp_4pcs <- brm((Interaction-1) ~ PC1*guild+PC2*guild+PC3*guild+PC4*guild+ (1|Id) + (1|gr(phylo, cov = A)),
                       data = dat_analysis, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                       sample_prior = TRUE, warmup = 500, iter = 2000,
                       control = list(adapt_delta = 0.99))

#Plot nicely PC1
ce_pc1 <- conditional_effects(model_1_all_spp_4pcs, effects = "PC1:guild",points=T) 

ggplot(ce_pc1[[1]], aes(x = PC1, y = (estimate__+1), group=guild, colour=guild)) + geom_point(data = dat_analysis,
  aes(x = PC1, y = Interaction),size = 1, alpha=0.75) + geom_line(size=1.2) + 
  ylim(0,quantile(dat_analysis$Interaction, 0.95)) + ylab("Number of visits")+
  theme_ms()

#Plot nicely PC2
ce_pc2 <- conditional_effects(model_1_all_spp_4pcs, effects = "PC2:guild",points=T) 

ggplot(ce_pc2[[1]], aes(x = PC2, y = (estimate__+1), group=guild, colour=guild)) + geom_point(data = dat_analysis,
   aes(x = PC2, y = Interaction),size = 1, alpha=0.75) + geom_line(size=1.2) + 
  ylim(0,quantile(dat_analysis$Interaction, 0.95)) + ylab("Number of visits")+
  theme_ms()


#Plot nicely PC2
ce_pc3 <- conditional_effects(model_1_all_spp_4pcs, effects = "PC3:guild",points=T) 

ggplot(ce_pc3[[1]], aes(x = PC3, y = (estimate__+1), group=guild, colour=guild)) + geom_point(data = dat_analysis,
  aes(x = PC3, y = Interaction),size = 1, alpha=0.75) + geom_line(size=1.2) + 
  ylim(0,quantile(dat_analysis$Interaction, 0.95)) + ylab("Number of visits")+
  theme_ms()


#Plot nicely PC2
ce_pc4 <- conditional_effects(model_1_all_spp_4pcs, effects = "PC4:guild",points=T) 

ggplot(ce_pc4[[1]], aes(x = PC4, y = (estimate__+1), group=guild, colour=guild)) + geom_point(data = dat_analysis,
  aes(x = PC4, y = Interaction),size = 1, alpha=0.75) + geom_line(size=1.2) + 
  ylim(0,quantile(dat_analysis$Interaction, 0.95)) + ylab("Number of visits")+
  theme_ms()

