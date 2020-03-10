#HERE I MERGE LONG FORMAT DATA AND THE TRAIT DATA

#library
library(readxl)

Long_format_metawebs <- readRDS("Data/RData/Long_format_metawebs.RData")

#Checking unique cases of plant species
species <- unique(Long_format_metawebs$Plant_species)
species <- as.data.frame(species)
colnames(species)[1] <- "species"
species$species <- as.factor(species$species)

data <- read_excel("Data/Data_raw/Trait_data_final.xlsx")

data <- as.data.frame(data)
levels(as.factor(data$Info_level))


data_filtered <- subset(data, Info_level=="flower"|Info_level=="capitulum"|Info_level=="inflorescence"|Info_level=="NA")
data_filtered_species <- data_filtered[,c(1,2,3)]
data_filtered_species <- as.data.frame(data_filtered_species)
colnames(data_filtered_species)[1] <- "species"
data_filtered_species$species <- as.factor(data_filtered_species$species)

str(species)
str(data_filtered_species)
c <- bind_cols(species,data_filtered_species)
b <- merge(species, data_filtered_species, all=T, by="species")
