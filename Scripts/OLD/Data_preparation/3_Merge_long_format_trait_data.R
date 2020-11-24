#HERE I MERGE LONG FORMAT DATA AND THE TRAIT DATA

#library
library(readxl)
library(dplyr)
#setworkingdirectory
setwd("~/R_Projects/Reproductive traits") 


#1st
#Read long format data. Generated in 1_Long_format_data.R
Long_format_metawebs <- readRDS("Data/RData/Long_format_metawebs.RData")

#Selecting unique cases of plant species
Long_data_unique <- Long_format_metawebs[!duplicated(Long_format_metawebs[1]),]


#2nd
#Read data with traits
data <- read_excel("Data/Data_raw/Trait_data_final.xlsx")
data <- as.data.frame(data)
levels(as.factor(data$Info_level))
#Filter data, I have more information for the asteraceae species but we just want this for now
#Capitulum: Asteraceae flower or other structure that acts as a capitulum (capitulum like)
#Inflorescence: Flowers that are highly grouped together and were considered at inflorescence level
#Flower: Individual reproductive organ grouped or not with others, 
#that from a poll. perspective makes sense to be considered isolated
data_filtered <- subset(data, Info_level=="flower"|Info_level=="capitulum"|Info_level=="inflorescence"|Info_level=="NA")
colnames(data_filtered)[1] <- "Plant_species"

data_filtered$Species <- as.factor(data_filtered$Species)
data_filtered <- as.data.frame(data_filtered, stringsAsFactors = TRUE)


#NOW I MERGE BOTH  (THE LIST OF SPECIES AND THE TRAIT DATA)
str(Long_data)
str(Long_data_unique)

all <- merge(Long_data_unique, data_filtered,  by="Plant_species", all = T )
write.csv(all, "data/Data_processing/all.csv", row.names = F)
#There are still some gaps, but is good for now (150 spp)
#I'll add them after some preliminary analyses

