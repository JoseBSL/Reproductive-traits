########################################################################################################################################################
#SCRIPT TO PREPARE DATA FOR ANALYSIS 1) VISITS~ POLL. FUNCTIONAL GROUP*PRINCIPAL COMPONENTS
########################################################################################################################################################
#LOAD LIBRARIES
library(data.table)
########################################################################################################################################################
#LOAD DATA
########################################################################################################################################################
long_d <- read.csv("Data/Csv/long_format_quantitative_networks.csv", row.names = 1) #quantitative network data|weighted by frequency of visits per plant species
phyl_pca <- readRDS("Data/RData/phyl_pca_all_numeric_values.rds") #add PCA loadings for analysis
dat_cleaning <- readRDS("Data/RData/data_pca_all_numeric_values.rds") #data for PPCA
########################################################################################################################################################
#PREPARE DATA
########################################################################################################################################################
#Select data with interaction greater than 0
long_d_1 <- long_d[long_d$Interaction>0,]

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
data_analysis <- merge(long_d_2, phyl_pca_1, by = "Plant_species")
#Explore why some levels dropped
#data_analysis <- merge(long_d_2, phyl_pca_1, by = "Plant_species", all.x = T)
#They are just the 4 species that get_tree cannot find and then, the ones that are until genus level, and thus were not included on the ppca
########################################################################################################################################################
#SAVE DATA
########################################################################################################################################################
saveRDS(data_analysis, "Data/RData/data_analysis_1.rds") 





