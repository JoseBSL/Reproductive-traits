#Check for species, family and order in taxsize 
#After I will look for traits

library(taxize)
library(TR8)

data<- read.csv("data/species_list.csv")
head(data)
str(data)
data_list<- data$species_geonet
#for(i in data_list)

plant_species=tax_name(query=data$species_geonet,
                     get=c("species"),db="ncbi", 
                     division_filter = "Plantae",rank_query="Species")

write.csv(plant_species,"data/plant_species.csv")

plant_family=tax_name(query=data$species_geonet,
                       get=c("family"),db="ncbi", 
                       division_filter = "Plantae",rank_query="Species")

write.csv(plant_family,"plant_family.csv")

plant_order=tax_name(query=data$species_geonet,
                      get=c("order"),db="ncbi", 
                      division_filter = "Plantae",rank_query="Species")

write.csv(plant_order,"plant_order.csv")

plant_genus=tax_name(query=data$species_geonet,
                     get=c("genus"),db="ncbi", 
                     division_filter = "Plantae",rank_query="Species")
write.csv(plant_genus,"plant_genus.csv")

data_species <- cbind(plant_order,plant_family, plant_species)
write.csv(data_species,"data_species.csv")

#clean NCBI columns and query  number
data_species=data_species[,-c(1,2,4,5,7,8)]
write.csv(data_species,"taxsize_output.csv")



#First I'm going to try to download info just from biolflor
#load my species list






data<- read.csv("data/species_list.csv")
head(data)
str(data)
data_list<- data$species_geonet
for(i in data_list)
My_traits<-tr8(species_list=i,gui_config=TRUE, synonyms = TRUE,download_list=c("list_of_traits_Biolflor"))
tr8(species_list=c("Salix alba","Populus nigra"),download_list=c("list_of_traits_Biolflor"))
{print(i)}


plant_order=tax_name(query=unique(head(plant_family$family)),
                     get=c("order"),db="ncbi", 
                     division_filter = "Plantae",rank_query="Family")

