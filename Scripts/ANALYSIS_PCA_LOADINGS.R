########################################################################################################################################################
#COMPARISON OF 3 METHODS

#A  ---> PCA WITH SUBSET OF SPECIES

#B  ---> FULL PCA but some species are lost when filtering with the PCA

#C  ---> FULL PCA without spcies los (not preprocessing of PCA)

#NOTE: SINGLETONS HAVE BEEN REMOVED FOR ANALYSIS

#Seems that the most accurate methodology is to conduct the PCA with the right methology (option B), in the other way 
#the output of the PCA has more noise. Hence, the analysis


########################################################################################################################################################
#Load libraries
library(phytools)
library(ape) #for phylogenetic distance
library(dplyr) #data processing
library(rtrees) #for phylogenetic distancelibrary(MASS)
library(reshape2)
library(viridis) #COLOUR GGPLOT
library(MASS)
library(ggplot2)
library(data.table)
########################################################################################################################################################
#A  ---> PCA WITH SUBSET OF SPECIES
########################################################################################################################################################

data <- read.csv("Data/Csv/metric_analysis_data_3rd_question.csv", row.names = 1)
head(data)

#select unique cases
unique_sp <- data[!duplicated(data$Species),]

#calculate phylo 
phylo <- as.data.frame(cbind(unique_sp$Family, unique_sp$Genus, unique_sp$Species))
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


#log
unique_sp[c("Selfing_quantitative", "Ovule_number", "Plant_height", "Flower_width", "Style_length")] <- log(unique_sp[c("Selfing_quantitative", 
    "Ovule_number", "Plant_height", "Flower_width", "Style_length")]+1)
#scale
unique_sp[c("Selfing_quantitative", "Ovule_number", "Plant_height", "Flower_width", "Style_length")] <- scale(unique_sp[c("Selfing_quantitative", 
    "Ovule_number", "Plant_height", "Flower_width", "Style_length")], center = T, scale = T)


#create dataframe for analysis
final_d <- unique_sp[c("Selfing_quantitative", "Ovule_number", "Plant_height", "Flower_width", "Style_length")]
rownames(final_d) <- unique_sp$Species
rownames(final_d) <- gsub(" ", "_",rownames(final_d))
#calculatye PCA
phyl_pca<- phyl.pca(phylo_output, final_d,method="lambda",mode="cov")


#save loadings into a dataframe
pca_loadings <- data.frame(phyl_pca$S)
pca_loadings <- setDT(pca_loadings, keep.rownames = TRUE)[]
colnames(pca_loadings)[1] <- "Species"
pca_loadings$Species <- gsub("_", " ", pca_loadings$Species)
#merge with the large dataframe
data_analysis <- merge(data, pca_loadings, by="Species")
head(data_analysis)
str(data_analysis)
m1 <- lme4::lmer(d ~ PC1  +(1|Id), data=data_analysis)
summary(m1)
#visualizing the patterns
jtools::effect_plot(m1, pred = PC1, interval = TRUE, plot.points = TRUE)

########################################################################################################################################################
#B  ---> FULL PCA but some species are lost when filtering with the PCA
########################################################################################################################################################
#Now we obtain the loadings from the global PCA which is likely to define better in the trait space the species
phyl_pca_2 <- readRDS("Data/RData/phyl_pca_forest.rds")

pca_loadings_2 <- data.frame(phyl_pca_2$S)

rownames(pca_loadings_2) <- gsub("_", " ", rownames(pca_loadings_2))
#set same colnames for merging
pca_loadings_2 <- setDT(pca_loadings_2, keep.rownames = TRUE)[]

colnames(pca_loadings_2)[1] <- "Species"
#check data for merge
head(data)

#merge columns
data_analysis2 <- merge(data, pca_loadings_2, by="Species")

m1 <- lme4::lmer(d ~ PC1  +(1|Id), data=data_analysis2)
summary(m1)
#visualizing the patterns
jtools::effect_plot(m1, pred = PC1 , interval = TRUE, plot.points = TRUE)

percentage <- round(diag(phyl_pca_2$Eval) / sum(phyl_pca_2$Eval) * 100, 2) #calculate percentage

########################################################################################################################################################
#C  ---> FULL PCA without spcies los (not preprocessing of PCA)
########################################################################################################################################################
#dat <- read.csv("Data/Csv/all_species_imputed_trait_data_famd_data.csv", row.names = "X")
#
#cols.num <- c("Family_all","Genus_all","Species_all")
#dat[cols.num] <- sapply(dat[cols.num],as.character)
#dat$Species_all <- gsub("Species_all_", "", dat$Species_all)
#
#dat_1 <- dat[,c(2,3,4,8,11,14,16,17,20)]
#
#dat_1[,c(4:9)] <- log(dat_1[,c(4:9)]+1)
#dat_1[,c(4:9)] <- scale(dat_1[,c(4:9)], center = T, scale = T)
#
#final_d <- dat_1[,c(4:9)]
#
##calculate phylo 
#phylo <- as.data.frame(cbind(dat_1$Family_all, dat_1$Genus_all, dat_1$Species_all))
#colnames(phylo) <-  c("family", "genus", "species")
##Select unique cases
##phylo_2 <- phylo[!duplicated(phylo$species),]
#phylo_2 <- tibble(phylo)
##get phylo
#phylo_output <- get_tree(sp_list = phylo_2, tree = tree_plant_otl, taxon = "plant")
#str(phylo_output)
##Convert phylogenetic tree into matrix
#A_5 <- vcv.phylo(phylo_output)
##Standardize to max value 1
#A_5 <- A_5/max(A_5)
##Unify column names; remove underscore and remove asterik
#rownames(A_5) <- gsub("\\*", "", rownames(A_5))
#colnames(A_5) <- gsub("\\*", "", colnames(A_5))
#colnames(A_5) <- gsub("_", " ", colnames(A_5))
#rownames(A_5) <- gsub("_", " ", rownames(A_5))

#set same rownames
#final_d <-  dat_1[,c(4:9)]

#rownames(final_d) <- dat_1$Species_all
#fix species names
#rownames(final_d) <- gsub(" ", "_", rownames(final_d))
#calculate PCA
#phyl_pca__method_C<- phyl.pca(phylo_output, final_d,method="lambda",mode="cov")
#save output
#saveRDS(phyl_pca__method_C, "Data/RData/phyl_pca__method_C.rds")
phyl_pca__method_C <- readRDS("Data/RData/phyl_pca__method_C.rds")
phyl_pca__method_C$L
#convert to dataframe
phyl_pca <- data.frame(phyl_pca__method_C$S)
#set same rownames
rownames(phyl_pca) <- gsub("_", " ", rownames(phyl_pca))
#set same colnames for merging
phyl_pca <- setDT(phyl_pca, keep.rownames = TRUE)[]

colnames(phyl_pca)[1] <- "Species"
#check data for merge
data <- read.csv("Data/Csv/metric_analysis_data_3rd_question.csv", row.names = 1)

#merge columns
data_analysis2 <- merge(data, phyl_pca, by="Species")

m1 <- lme4::lmer(d ~ PC4  +(1|Id), data=data_analysis2)
summary(m1)
#visualizing the patterns
jtools::effect_plot(m1, pred = PC4 , interval = TRUE, plot.points = TRUE)


percentage <- round(diag(phyl_pca_2$Eval) / sum(phyl_pca_2$Eval) * 100, 2) #calculate percentage
