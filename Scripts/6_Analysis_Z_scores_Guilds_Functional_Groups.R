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
library(phyr) #Analysis
library(DHARMa)

########################################################################################################################################################
#1) READ DATA
########################################################################################################################################################

d <- read.csv("Data/Csv/quantitative_networks_Z_scores_with_traits_and_clusters.csv", row.names = 1)

########################################################################################################################################################
#2) PHYLOGENETIC DISTANCE OF THE SPECIES
########################################################################################################################################################

#Prepare species, genus and family for calculating phylogenetic distance
d$Family_all <- as.character(d$Family_all)
d$Genus_all <- as.character(d$Genus_all)
d$Species_all <- as.character(d$Species_all)

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

########################################################################################################################################################
#1) PHYLOGENETIC DISTANCE OF THE SPECIES
########################################################################################################################################################
library("INLA")
library("phyr")
m1 <- pglmm(Z_scores ~ guild + (1 | phylo) +(1 | Id),
            data = d_1,cov_ranef = list(sp = A),bayes = T, prior = "pc.prior.auto")

summary(m1)
plot_bayes(m1, sort = TRUE)

resids <- DHARMa::simulateResiduals(m1, plot = FALSE)

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

install.packages("remotes")
remotes::install_github("daijiang/phyr")

