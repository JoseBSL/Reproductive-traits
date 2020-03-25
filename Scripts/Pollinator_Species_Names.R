# GROUP POLLINATOR TAXA AND CHECK NAMES

# Load Libraries
library(readxl)
library(ggplot2)
library(dbplyr)
library(scatterpie)
library(reshape2)
library(taxize)
library(stringr)

# READ LONG FORMAT DATA
Long_format_metawebs <- readRDS("Data/RData/Long_format_metawebs.RData")

# SUBSET UNIQUE CASES PER NETWORK
Long_format_subset_poll <-  unique(Long_format_metawebs[,c("Pollinator_species","Id")])
str(Long_format_subset_poll$Pollinator_species)
# SELECT FIRST WORD OF THE SPECIES (GENUS), AND UNIQUE CASE
poll <- as.data.frame(unique(word(Long_format_subset_poll$Pollinator_species),1))
str(poll)

colnames(poll)[1] <- "Genus"

#RUN TAXSIZE
#I HAVE TO SPLIT THE DATAFRAME BECAUSE THE PACKAGE IS GIVING A LOT OF ERRORS WITH BIG DATASETS (FOR ME AT LEAST)

#I START WITH 50 SPP
poll_50 <- poll[c(1:50),]
poll_50 <- as.data.frame(poll_50)
colnames(poll_50)[1] <- "Genus"

poll_species=tax_name(query=poll_50$Genus,get=c("genus", "order", "family"),db="itis",division_filter = "Arthropoda",rank_query="genus")
poll_50 <- poll_species


poll_100 <- poll[c(51:100),]
poll_100 <- as.data.frame(poll_100)
colnames(poll_100)[1] <- "Genus"


poll_species_100=tax_name(query=poll_100$Genus,get=c("genus", "order", "family"),db="itis",division_filter = "Arthropoda",rank_query="genus")
poll_100 <- poll_species_100

poll_150 <- poll[c(101:150),]
poll_150 <- as.data.frame(poll_150)
colnames(poll_150)[1] <- "Genus"

poll_species_150=tax_name(query=poll_150$Genus,get=c("genus", "order", "family"),db="itis",division_filter = "Arthropoda",rank_query="genus")
poll_150 <- poll_species_150

poll_200 <- poll[c(151:200),]
poll_200 <- as.data.frame(poll_200)
colnames(poll_200)[1] <- "Genus"

poll_species_200=tax_name(query=poll_200$Genus,get=c("genus", "order", "family"),db="itis",division_filter = "Arthropoda",rank_query="genus")
poll_200 <- poll_species_200

poll_species_200 <- rbind(poll_50,poll_100,poll_150, poll_200)
saveRDS(poll_species_200, "Data/RData/poll_species_200.RData")
write.csv(poll_species_200, "Data/Data_processing/poll_200.csv")

poll_400 <- poll[c(201:400),]
poll_400 <- as.data.frame(poll_400)
colnames(poll_400)[1] <- "Genus"

poll_species_400=tax_name(query=poll_400$Genus,get=c("genus", "order", "family"),db="itis",division_filter = "Arthropoda",rank_query="genus")
poll_400 <- poll_species_400
write.csv(poll_species_400, "Data/Data_processing/poll_400.csv")


poll_800<- poll[c(401:800),]
poll_800 <- as.data.frame(poll_800)
colnames(poll_800)[1] <- "Genus"

poll_species_800=tax_name(query=poll800$Genus,get=c("genus", "order", "family"),db="itis",division_filter = "Arthropoda",rank_query="genus")
poll_800 <- poll_species_800
write.csv(poll_species_800, "Data/Data_processing/poll_800.csv")

poll_1400<- poll[c(801:1400),]
poll_1400 <- as.data.frame(poll_1400)
colnames(poll_1400)[1] <- "Genus"

poll_species_1400=tax_name(query=poll1400$Genus,get=c("genus", "order", "family"),db="itis",division_filter = "Arthropoda",rank_query="genus")
poll_1400 <- poll_species_1400
write.csv(poll_species_1400, "Data/Data_processing/poll_1400.csv")

poll_2065<- poll[c(1401:2065),]
poll_2065 <- as.data.frame(poll_2065)
colnames(poll_2065)[1] <- "Genus"

poll_species_2065=tax_name(query=poll2065$Genus,get=c("genus", "order", "family"),db="itis",division_filter = "Arthropoda",rank_query="genus")
poll_2065 <- poll_species_2065
write.csv(poll_species_2065, "Data/Data_processing/poll_2065.csv")


#NOW I HAVE ALL THE SPECIES
#BIND DATASETS

poll_200 <- read.csv("Data/Data_processing/poll_200.csv")
poll_400 <- read.csv("Data/Data_processing/poll_400.csv")
poll_800 <- read.csv("Data/Data_processing/poll_800.csv")
poll_1400 <- read.csv("Data/Data_processing/poll_1400.csv")
poll_2065 <- read.csv("Data/Data_processing/poll_2065.csv")

poll_spp <- rbind(poll_200, poll_400, poll_800, poll_1400, poll_2065)
poll_spp$genus_old <- poll$Genus
poll_spp <- poll_spp[,-1]
write.csv(poll_spp, "Data/Data_processing/poll_spp.csv")


#I have added manually the ORDERS, FAMILIES AND GENUSES
#By using https://www.catalogueoflife.org/ 
#or other addittional resources when required for specific cases

#READ POLLINATOR ORDERS, FAMILIES AND GENUSES (UNIQUE CASES)
poll_spp_names_corrected <- read.csv("Data/Data_processing/poll_spp_names_corrected.csv")

Long_format_metawebs$genus_old <- word(Long_format_metawebs$Pollinator_species)

all_long_format <- merge(Long_format_metawebs, poll_spp_names_corrected, by= "genus_old")

all_long_format <- all_long_format[,-c(1,6,7)]

colnames(all_long_format) <- c("Plant_species", "Pollinator_species", "Interaction", "Id", "Pollinator_order", "Pollinator_family",
                              "Pollinator_genus")
