# Processing data from kaiser bunbury 2011 
# DOI: 10.1111/j.1365-2745.2010.01732.x
# Seychelles


#Load data

d <- read.csv("Data/Data_processing/Data_networks_processing/kaiser_bunbury_seychelles_2011/Kaiser_bunbury_seychelles.csv", header=T, stringsAsFactors=F)

species <- read.csv("Data/Data_processing/Data_networks_processing/kaiser_bunbury_seychelles_2011/Kaiser_bunbury_seychelles_species.csv", header=T, stringsAsFactors=F)

species_animal <-  subset(species, Kingdom == "Animal")
colnames(species_animal)[3] <- "Insect.code"

all <- merge(d, species_animal, by="Insect.code", all=T)

#FIX MERGE FIRST FOUR ROWS CONTINUE TOMORROW