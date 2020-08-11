# data preparation of the networks of Kaiser_bunbury 2009
#Key data from the paper to convert to visitation
#We observed pollinators for a total of 858.5 hours (site 1 = 471 h, site 2 = 387.5 h)


#site 1 control
d <- read.csv("Data/Data_processing/Data_networks_processing/Kaiser_Bunbury_2017.csv", header=T, stringsAsFactors=F)

plants <- read.csv("Data/Data_processing/Data_networks_processing/Kaiser_Bunbury_2017_plant_species.csv", header=T,stringsAsFactors=F )

pollinator <- read.csv("Data/Data_processing/Data_networks_processing/Kaiser_Bunbury_2017_pollinator_species.csv", header=T)

str(d)
str(plants)

merge_d_plants <- merge(d, plants, by="Plant.species.ID", all = T)

merge_all <- merge(merge_d_plants, pollinator, by="Pollinator.species.ID", all = T)

library(reshape2)
pollinator_1 <- pollinator[,c(2,3)]

a <- reshape(pollinator_1, idvar = "Pollinator.species.name", timevar = "Pollinator.species.ID", direction = "wide")

library(tidyr)
a <- spread(pollinator_1, key = Pollinator.species.ID, value = Pollinator.species.name)

merge_all_1 <- merge_d_plants[,7:50]

