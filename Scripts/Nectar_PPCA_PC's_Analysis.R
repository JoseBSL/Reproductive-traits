########################################################################################################################################################
#SCRIPT TO PREPARE DATA FOR ANALYSIS 1) WITH THE SUBSET OF SPECIES WITH QUANTITATIVE VALUES OF NECTAR
########################################################################################################################################################
#LOAD LIBRARIES
library(data.table)
########################################################################################################################################################
#LOAD DATA
########################################################################################################################################################
long_d <- read.csv("Data/Csv/long_format_quantitative_networks.csv", row.names = 1) #quantitative network data|weighted by frequency of visits per plant species
phyl_pca <- readRDS("Data/RData/phyl_pca_forest_nectar_all.rds") #add PCA loadings for analysis
nectar_data <- readRDS("Data/RData/data_subset_nectar.rds")

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
phyl_pca_1$Plant_species <- nectar_data$Species_all
phyl_pca_1$Genus_all  <- nectar_data$Genus_all
phyl_pca_1$Family_all  <- nectar_data$Family_all
phyl_pca_1$Family_all 


nectar_datos <- merge(phyl_pca_1,long_d_2, by = "Plant_species")


nectar_datos <- subset(nectar_datos, guild!="Other_insects" & guild!="Lizards" & guild!="Birds") #Select just the main 6 functional groups
levels(factor(nectar_datos$guild))
nectar_datos$guild <- as.character(nectar_datos$guild)
nectar_datos$guild[nectar_datos$guild=="Bee"] <- "Bees" #Adding an S to bee(S)
nectar_datos$guild <- factor(nectar_datos$guild, levels = c("Bees","Coleoptera", "Lepidoptera", "Non-bee-Hymenoptera",
                                                            "Non-syrphids-diptera", "Syrphids"))

#check number of levels
nectar_datos %>% 
  group_by(guild) %>%
  summarise(no_rows = length(guild))

########################################################################################################################################################
#SAVE DATA
########################################################################################################################################################
#saveRDS(data_analysis, "Data/RData/data_analysis_1_nectar.rds") 
########################################################################################################################################################
########################################################################################################################################################

nectar_datos$Family_all  <- as.character(nectar_datos$Family_all)
nectar_datos$Genus_all   <- as.character(nectar_datos$Genus_all)
nectar_datos$Plant_species <- as.character(nectar_datos$Plant_species)

#prepare dataframe to calculate tree
phylo_5 <- as.data.frame(cbind(nectar_datos$Family_all, nectar_datos$Genus_all, nectar_datos$Plant_species))
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
nectar_datos$phylo
nectar_datos$phylo <- nectar_datos$Plant_species

#prepare random factor
nectar_datos$System <- nectar_datos$Id

nectar_datos$System[grepl("peralta_2006", nectar_datos$System)] <- "peralta_2006"
nectar_datos$System[grepl("small_1976", nectar_datos$System)] <- "small_1976"
nectar_datos$System[grepl("arroyo_correa_new_zealand", nectar_datos$System)] <- "arroyo_correa_2019"
nectar_datos$System[grepl("fang_2008", nectar_datos$System)] <- "fang_2008"
nectar_datos$System[grepl("kaiser-bunbury_2017", nectar_datos$System)] <- "kaiser-bunbury_2017"
nectar_datos$System[grepl("inouye_1988", nectar_datos$System)] <- "inouye_1988"
nectar_datos$System[grepl("kaiser-bunbury_2010", nectar_datos$System)] <- "kaiser-bunbury_2010"
nectar_datos$System[grepl("kaiser-bunbury_2011", nectar_datos$System)] <- "kaiser-bunbury_2011"
nectar_datos$System[grepl("burkle_usa_2013", nectar_datos$System)] <- "burkle_2013"
nectar_datos$System[grepl("dicks_2002", nectar_datos$System)] <- "dicks_2002"
nectar_datos$System[grepl("dupont_2009", nectar_datos$System)] <- "dupont_2009"
nectar_datos$System[grepl("bartomeus_spain_2008_medca", nectar_datos$System)] <- "bartomeus_2008"
nectar_datos$System[grepl("bartomeus_spain_2008_batca", nectar_datos$System)] <- "bartomeus_2008"
nectar_datos$System[grepl("bartomeus_spain_2008_selop", nectar_datos$System)] <- "bartomeus_2008"
nectar_datos$System[grepl("bartomeus_spain_2008_miqop", nectar_datos$System)] <- "bartomeus_2008"
nectar_datos$System[grepl("bartomeus_spain_2008_fraop", nectar_datos$System)] <- "bartomeus_2008"
nectar_datos$System[grepl("lundgren_2005", nectar_datos$System)] <- "lundgren_2005"
nectar_datos$System[grepl("olesen_2002_mauritius", nectar_datos$System)] <- "olesen_2002_mauritius"
nectar_datos$System[grepl("olesen_2002_azores", nectar_datos$System)] <- "olesen_2002_azores"
nectar_datos$System[grepl("bartomeus_spain_2008", nectar_datos$System)] <- "bartomeus_spain_2008"
nectar_datos$System[grepl("bundgaard_2003_denmark", nectar_datos$System)] <- "bundgaard_2003"
nectar_datos$System[grepl("elberling_sweeden_1999", nectar_datos$System)] <- "elberling_1999"



#MODEL
nectar_model <- brm((Interaction-1) ~ PC1*guild + PC2*guild + PC3*guild +(1|System/Id) + (1|gr(phylo, cov = A)),
                  data = nectar_datos, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                  sample_prior = TRUE, warmup = 500, iter = 2000,
                  control = list(adapt_delta = 0.99))


#CHECKING MODEL!
summary(nectar_model)
performance::r2(nectar_model)
conditional_effects(nectar_model)
#It gives a similar output with and without nectar













