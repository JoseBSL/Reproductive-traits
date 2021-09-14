########################################################################################################################################################
#SCRIPT TO PREPARE DATA FOR ANALYSIS 1) VISITS~ POLL. FUNCTIONAL GROUP*PRINCIPAL COMPONENTS
########################################################################################################################################################
#LOAD LIBRARIES
library(data.table)
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
library(brms)
library(DHARMa)
library(cmdstanr)
library(emmeans)
library(tidybayes)
########################################################################################################################################################
#LOAD DATA
########################################################################################################################################################
long_d <- read.csv("Data/Csv/long_format_quantitative_networks.csv", row.names = 1) #quantitative network data|weighted by frequency of visits per plant species
phyl_pca <- readRDS("Data/RData/phyl_pca_forest.rds") #add PCA loadings for analysis
dat_cleaning <- readRDS("Data/RData/data_all_species_PPCA.rds") #data for PPCA
########################################################################################################################################################
#PREPARE DATA
########################################################################################################################################################
#Select data with interaction greater than 0
#long_d_1 <- long_d[long_d$Interaction>0,]
long_d_1 <- long_d

#Remove other orders/guilds that are not these ones
long_d_2 <- long_d_1[!is.na(long_d_1$guild),] #I do it by guild because just these 6 guilds are named in this column

#check levels
levels(factor(long_d_2$guild)) #9 DIFFERENT GUILDS|After I'll select the main fucntional poll. groups for analysis

#convert to dataframe the ppca data
phyl_pca_1 <- data.frame(phyl_pca$S)

#convert rownames to column
dat_cleaning_1 <- setDT(dat_cleaning, keep.rownames = TRUE)[]
colnames(dat_cleaning_1)[1] <- "Plant_species"

phyl_pca_1$Plant_species <- dat_cleaning_1$Plant_species
#ADD ALSO THESE OTHER 3 COLUMNS WITH SPP, GENUS AND FAMILY INFO NEEDED TO GET THE PHYLO FOR THE MODEL
phyl_pca_1$Family_all <- dat_cleaning_1$Family_all
phyl_pca_1$Genus_all <- dat_cleaning_1$Genus_all
phyl_pca_1$Species_all <- dat_cleaning_1$Species_all

#merge columns
dat_analysis <- merge(long_d_2, phyl_pca_1, by = "Plant_species")

dat_analysis$System <- dat_analysis$Id

dat_analysis$System[grepl("peralta_2006", dat_analysis$System)] <- "peralta_2006"
dat_analysis$System[grepl("small_1976", dat_analysis$System)] <- "small_1976"
dat_analysis$System[grepl("arroyo_correa_new_zealand", dat_analysis$System)] <- "arroyo_correa_2019"
dat_analysis$System[grepl("fang_2008", dat_analysis$System)] <- "fang_2008"
dat_analysis$System[grepl("kaiser-bunbury_2017", dat_analysis$System)] <- "kaiser-bunbury_2017"
dat_analysis$System[grepl("inouye_1988", dat_analysis$System)] <- "inouye_1988"
dat_analysis$System[grepl("kaiser-bunbury_2010", dat_analysis$System)] <- "kaiser-bunbury_2010"
dat_analysis$System[grepl("kaiser-bunbury_2011", dat_analysis$System)] <- "kaiser-bunbury_2011"
dat_analysis$System[grepl("burkle_usa_2013", dat_analysis$System)] <- "burkle_2013"
dat_analysis$System[grepl("dicks_2002", dat_analysis$System)] <- "dicks_2002"
dat_analysis$System[grepl("dupont_2009", dat_analysis$System)] <- "dupont_2009"
dat_analysis$System[grepl("bartomeus_spain_2008_medca", dat_analysis$System)] <- "bartomeus_2008"
dat_analysis$System[grepl("bartomeus_spain_2008_batca", dat_analysis$System)] <- "bartomeus_2008"
dat_analysis$System[grepl("bartomeus_spain_2008_selop", dat_analysis$System)] <- "bartomeus_2008"
dat_analysis$System[grepl("bartomeus_spain_2008_miqop", dat_analysis$System)] <- "bartomeus_2008"
dat_analysis$System[grepl("bartomeus_spain_2008_fraop", dat_analysis$System)] <- "bartomeus_2008"
dat_analysis$System[grepl("lundgren_2005", dat_analysis$System)] <- "lundgren_2005"
dat_analysis$System[grepl("olesen_2002_mauritius", dat_analysis$System)] <- "olesen_2002_mauritius"
dat_analysis$System[grepl("olesen_2002_azores", dat_analysis$System)] <- "olesen_2002_azores"
dat_analysis$System[grepl("bartomeus_spain_2008", dat_analysis$System)] <- "bartomeus_spain_2008"
dat_analysis$System[grepl("bundgaard_2003_denmark", dat_analysis$System)] <- "bundgaard_2003"
dat_analysis$System[grepl("elberling_sweeden_1999", dat_analysis$System)] <- "elberling_1999"
###############
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
str(dat_analysis)
###############








str(dat_analysis)
#convert to binary
dat_analysis$Interaction[dat_analysis$Interaction>0] <-1

diamond.glm <- glm(formula = Interaction ~ PC1*guild,
                   data = dat_analysis,
                   family = binomial)
summary(diamond.glm)
#Check r2
performance::r2(diamond.glm)

library(ggplot2)
ggplot(dat_analysis, aes(x=PC1, y=Interaction, color=guild)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))






#Prepare brms model like if it was for a final analysis
library(brms)
fit1 <- brm(Interaction ~ PC1*guild + (1|System), cores = 4,chains = 4, 
            data = dat_analysis[1:100], family = binomial())

summary(fit1)



