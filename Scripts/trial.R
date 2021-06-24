########################################################################################################################################################
#SCRIPT TO CALCULATE THE PHLOGENETIC INFORMED PRINCIPAL COMPONENT ANALYSIS
########################################################################################################################################################
#LOAD LIBRARIES
library(phytools) #ppca
library(ape) #for phylogenetic distance
library(dplyr) #data processing
library(rtrees) #for phylogenetic distancelibrary(MASS)
library(reshape2) #data processing
library(MASS) #I think I used it for the kernel density of the plotting function; no longer used but I leave in case its handy later on
library(ggplot2) #plotting
library(broman) #crayon colours
library(magick) #add images
########################################################################################################################################################
#1) LOAD DATA
########################################################################################################################################################
#read data with missing values filled by data imputation
dat <- read.csv("Data/Csv/all_species_imputed_trait_data_forest_data.csv", row.names = "X")
########################################################################################################################################################
#2) Tidy up data to get phylo distance and conduct PCA
########################################################################################################################################################
########################################################################################################################################################
#remove not found species, cannot do PCA with unequal numbers of rows
cols.num <- c("Family_all","Genus_all","Species_all")
dat[cols.num] <- sapply(dat[cols.num],as.character)
dat$Species_all <- gsub("Species_all_", "", dat$Species_all)
########################################################################################################################################################
#3) REMOVE OUTLIERS, OUT OF 2.5-97.5 RANGE WHICH HELPS IMPUTATION PROCESS. SEE ARTICLE FOR REF.
########################################################################################################################################################
dat_cleaning <- dat[,c(2,3,4,8,11,12,14,15,18)]
#CHECK LEVELS
str(dat)

dat_cleaning_1 <- dat_cleaning %>%
  filter(between(Flowers_per_plant, quantile(Flowers_per_plant, 0.025), quantile(Flowers_per_plant, 0.975)))

dat_cleaning_2 <- dat_cleaning_1 %>%
  filter(between(Corolla_diameter_mean, quantile(Corolla_diameter_mean, 0.025), quantile(Corolla_diameter_mean, 0.975)))

dat_cleaning_3 <- dat_cleaning_2 %>%
  filter(between(Style_length, quantile(Style_length, 0.025), quantile(Style_length, 0.975)))

dat_cleaning_4 <- dat_cleaning_3 %>%
  filter(between(Ovule_number, quantile(Ovule_number, 0.025), quantile(Ovule_number, 0.975)))

dat_cleaning_5 <- dat_cleaning_4 %>%
  filter(between(Plant_height_mean_m, quantile(Plant_height_mean_m, 0.025), quantile(Plant_height_mean_m, 0.975)))

#ALL VALUES ARE BETWEEN 0 AND 100, THEREFORE WE DO NOT DISCARD OUTLIERS FOR THIS VARIABLE
#dat_cleaning_6 <- dat_cleaning_5 %>%
# filter(between(Autonomous_selfing_level_fruit_set, quantile(Autonomous_selfing_level_fruit_set, 0.025), quantile(Autonomous_selfing_level_fruit_set, 0.975)))

#LOG TRANSFORM AND SCALE DATA
#CHECK LEVELS
str(dat_cleaning_5)
dat_cleaning_5[,c(4:9)] <- log(dat_cleaning_5[,c(4:9)]+1)
dat_cleaning_5[,c(4:9)] <- scale(dat_cleaning_5[,c(4:9)], center = T, scale = T)


final_d <- dat_cleaning_5[,c(4:9)]
nrow(final_d)


df <- dat_cleaning[,c(4:9)]
str(df)

library(dplyr)

# Checking only for column a. Top 1% and bottom 1% is removed 
dat_cleaning_1_1 <- dat_cleaning %>% filter(between(Flowers_per_plant, quantile(Flowers_per_plant, .025), quantile(Flowers_per_plant, .975)))
nrow(dat_cleaning_1_1)
dat_cleaning_1 <- dat_cleaning %>%
  filter(between(Flowers_per_plant, quantile(Flowers_per_plant, 0.025), quantile(Flowers_per_plant, 0.975)))
nrow(dat_cleaning_1)
nrow(dat_cleaning_1)


all_vars[,c(1:5)]

dat_cleaning %>% select(Autonomous_selfing_level_fruit_set)

# Checking for column a & b. Top 1% and bottom 1% is removed
df_1 <- df %>% select(-"Autonomous_selfing_level_fruit_set") %>%filter_all(all_vars[,c(1:5)](between(., quantile(., 0.025), quantile(., 0.975))))
nrow(df_1)

boxplot(scale(df_1$Flowers_per_plant))
boxplot(scale(dat_cleaning_5[,c(5)]))

