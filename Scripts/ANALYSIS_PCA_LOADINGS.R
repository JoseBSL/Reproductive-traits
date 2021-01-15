########################################################################################################################################################
#COMPARISON OF 3 METHODS

#A  ---> PCA WITH SUBSET OF SPECIES

#B  ---> FULL PCA but some species are lost when filtering with the PCA

#C  ---> FULL PCA without spcies lost

#NOTE: SINGLETONS HAVE BEEN REMOVED FOR ANALYSIS
########################################################################################################################################################
#Load libraries
library(rtrees) #for phylogenetic distancelibrary(MASS)
library(ape) #for phylogenetic distance
library(phytools) #phyl pca

#CALCULATE NEW PCA FOR THE SUBSET OF QUANTITATIVE SPECIES


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


m1 <- lme4::lmer(d ~ PC2  +(1|Id), data=data_analysis2)
summary(m1)
#visualizing the patterns
jtools::effect_plot(m1, pred = PC2 , interval = TRUE, plot.points = TRUE)

percentage <- round(diag(phyl_pca_2$Eval) / sum(phyl_pca_2$Eval) * 100, 2) #calculate percentage

