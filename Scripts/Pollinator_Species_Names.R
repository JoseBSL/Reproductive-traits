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

# SELECT SUBSET
poll <- as.data.frame(unique(word(Long_format_subset_poll$Pollinator_species),1))
str(poll)

colnames(poll)[1] <- "Genus"

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

poll_250 <- poll[c(201:250),]
poll_250 <- as.data.frame(poll_250)
colnames(poll_250)[1] <- "Genus"

poll_species_250=tax_name(query=poll_250$Genus,get=c("genus", "order", "family"),db="itis",division_filter = "Arthropoda",rank_query="genus")
poll_250 <- poll_species_250
