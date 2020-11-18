##############################################################################
#PROCESSING NECTAR_POLEN DATA 19/11/20
##############################################################################

library(readxl)
setwd("~/R_Projects/Reproductive traits") 
#read data (we are currently working with it)
pollen_nectar <- read_excel("Data/Trait_data_raw/pollen_nectar_data_18_11_20.xlsx")
str(pollen_nectar)
pollen_nectar$Species_geonet <- as.character(pollen_nectar$Species_geonet)
pollen_nectar <- as.data.frame(pollen_nectar)
sub <- pollen_nectar[,c(2,8)]


#read trait data, I'm going to priorize species from the quantitative networks)

quantitative_net_species <- read.csv("Data/Csv/quantitative_networks_trait_data.csv")
no_dup <- quantitative_net_species[!duplicated(quantitative_net_species$Plant_species),]
str(no_dup)
#create a data frame with just species names to merge
d <- as.data.frame(no_dup$Plant_species)
colnames(d) <- "Species_geonet"
#add col with yes 
str(d)
d$Species_geonet <- as.character(pollen_nectar$Species_geonet)
d$yes <- "Yes"
#now merge data
merged <- merge(pollen_nectar, d, by= "Species_geonet", all.x=T )
write.csv(merged, "Data/Data_processing/pollen_nectar_data_which_species_to_give_priority.csv")
