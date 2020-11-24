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


spp <- read.csv("data/spp.csv")
#First I'm going to try to download info just from biolflor
#load my species list
spp$Species_all<-as.character(spp$Species_all)
spp_list <- spp[,c(4)]
spp_list<- as.data.frame(spp_list)
spp_list[spp_list==""]<-NA
spp_complete=spp_list[complete.cases(spp_list),]
spp_complete<- as.data.frame(spp_complete)
species <- spp_complete$spp_complete
species <- as.character(species)
str(species)

#data_species=spp[complete.cases(spp),4]
#data_species<-as.data.frame(data_species)
#data_list<- data_species$species
#Check what traits are available
#available_tr8


a <- c("h_max", "h_min","li_form")

b <- c("reprod_meth","propag","growth_form","life_span","woodiness")

c <- c("li_form_B","li_span","reprod_B","strategy","self-st","Breeding_sys")

d <- c("apomixis")

d_1 <- c("apomixis","life_form_P","inflorescence_fr","poll_vect_fr",
       "fruit_type_fr","flower_colour_fr","li_form_fr")

e <- c("apomixis","life_form_P","inflorescence_fr","poll_vect_fr",
       "fruit_type_fr","flower_colour_fr","li_form_fr")

f <- c("max_height_cal","Growth.Habit","Flower.Color",
       "Growth.Form","Growth.Rate","Height.Mature","Lifespan",
       "Bloom.Period","Vegetative.Spread.Rate")

traits<-c("h_max", "h_min","li_form","reprod_meth","propag","growth_form","life_span","woodiness",
          "li_form_B","li_span","reprod_B","strategy","self-st","Breeding_sys","apomixis","life_form_P","inflorescence_fr","poll_vect_fr",
     "fruit_type_fr","flower_colour_fr","li_form_fr","max_height_cal","Growth.Habit","Flower.Color",
     "Growth.Form","Growth.Rate","Height.Mature","Lifespan","Bloom.Period","Vegetative.Spread.Rate")

#available_tr8
#retreved_traits<-tr8(species_list=data_list,download_list=b)


retreved_traits<-tr8(species_list=species,download_list=a)
retreved_traits_1<-tr8(species_list=species,download_list=b)
retreved_traits_2<-tr8(species_list=species,download_list=c)
retreved_traits_3_1<-tr8(species_list=species,download_list=d)
retreved_traits_3_2<-tr8(species_list=species,download_list=d_1)

#I haven't been able to run 3 gives an error
retreved_traits_4<-tr8(species_list=species,download_list=e)
#I haven't been able to run 4 either
retreved_traits_5<-tr8(species_list=species,download_list=f)


#life_form<-tr8(species_list=data_list,download_list="li_form")
#reprod_meth<-tr8(species_list=data_list,download_list="reprod_meth")

save.image("evn.Rdata")
#Way to proceed for a merge
#str(reprod_meth)
#df_reprod_meth <- as.data.frame(reprod_meth@results)
