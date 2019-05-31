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
data_species=data_species[complete.cases(data_species),]
data_list<- data_species$species
#Check what traits are available
available_tr8

b <- c("h_max", "h_min","li_form","reprod_meth","propag","growth_form","life_span","woodiness")
            #,"li_form_B","li_span",
            #"reprod_B","strategy","self-st","Breeding_sys","apomixis","life_form_P","inflorescence_fr","poll_vect_fr",
            #"fruit_type_fr","flower_colour_fr","li_form_fr","max_height_cal","Growth.Habit","Flower.Color",
            #"Growth.Form","Growth.Rate","Height..Mature","Lifespan","Bloom.Period","Vegetative.Spread.Rate")


for(i in data_list)
available_tr8
My_traits<-tr8(species_list=data_list,download_list=b)


