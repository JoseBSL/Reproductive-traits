#Network organization

#CRITERIA
#Networks are going to be organized in metawebs, individual networks will be merged
#When communities are homgeneous within the same study


#FIRST I start with Bartomeus, vila & Santamaria 2008 from Oecologia

#BAT,MED,FAR is the Carprobrotus metaweb
#SEL,FRA,MIQ is the Opuntia metaweb

setwd("~/R_projects/Reproductive Traits") 

#Carpobrutus metaweb
bat <- read.csv("Data/Data_networks/bartomeus_2008_bat1ca.csv")
med <- read.csv("Data/Data_networks/bartomeus_2008_med1ca.csv")
far <- read.csv("Data/Data_networks/bartomeus_2008_far1ca.csv")

bat <- melt(bat)
med <- melt(med)
far <- melt(far)

colnames(bat) <- c("Plant_species", "Pollinator_species", "Interaction")
colnames(med) <- c("Plant_species", "Pollinator_species", "Interaction")
colnames(far) <- c("Plant_species", "Pollinator_species", "Interaction")


carpobrotus_metaweb_Bartomeus_2008 <- rbind(bat,med,far)
carpobrotus_metaweb_Bartomeus_2008$Pollinator_species=gsub("\\."," ",carpobrotus_metaweb_Bartomeus_2008$Pollinator_species)

carpobrotus_metaweb_Bartomeus_2008 <- acast(carpobrotus_metaweb_Bartomeus_2008, Plant_species ~ Pollinator_species , value.var='Interaction', 
                                            fun.aggregate=sum)

write.csv(carpobrotus_metaweb_Bartomeus_2008, "Data_networks_metawebs/1_carpobrotus_metaweb_Bartomeus_2008.csv")

#Opuntia metaweb
sel <- read.csv("Data/Data_networks/bartomeus_2008_sel1op.csv")
fra <- read.csv("Data/Data_networks/bartomeus_2008_fra1op.csv")
miq <- read.csv("Data/Data_networks/bartomeus_2008_miq1op.csv")

sel <- melt(sel)
fra <- melt(fra)
miq <- melt(miq)

colnames(sel) <- c("Plant_species", "Pollinator_species", "Interaction")
colnames(fra) <- c("Plant_species", "Pollinator_species", "Interaction")
colnames(miq) <- c("Plant_species", "Pollinator_species", "Interaction")

opuntia_metaweb_Bartomeus_2008 <- rbind(sel,fra,miq)
opuntia_metaweb_Bartomeus_2008$Pollinator_species=gsub("\\."," ",opuntia_metaweb_Bartomeus_2008$Pollinator_species)

opuntia_metaweb_Bartomeus_2008 <- acast(opuntia_metaweb_Bartomeus_2008, Plant_species ~ Pollinator_species , value.var='Interaction', 
                                        fun.aggregate=sum)

write.csv(opuntia_metaweb_Bartomeus_2008, "Data_networks_metawebs/2_opuntia_metaweb_Bartomeus_2008.csv")


csv_file_name <- c("1_carpobrotus_metaweb_Bartomeus_2008", "2_opuntia_metaweb_Bartomeus_2008.csv")

longitude <- c(3.296797,3.296797)

latitude <- c(42.315336,42.315336)

country <- c("Spain", "Spain")

location <- c("Natural Park of Cap de Creus (Catalonia, northeastern Spain))",
              "Natural Park of Cap de Creus (Catalonia, northeastern Spain))")

duration <- c("1 season", "1 season")

experiment_year <- c(2005, 2005)

unique_networks <-c(3,3)

plant_species <- c(18, 13)

pollinator_species <- c(37, 37)  

network_size <- c(666, 481)

metadata <- data.frame(csv_file_name, longitude, latitude, country, location,duration, 
                       experiment_year,unique_networks,plant_species, pollinator_species, 
                       network_size)

#SECOND  Bek PhD thesis 2006
#Not much info about this network, collected in one season in Denmark and coordinates

csv_file_name <- c("3_bek_2006.csv")

longitude <- c(56.066667)

latitude <- c(10.216667)

country <- c("Denmark")

location <- c(NA)

duration <- c(NA)

experiment_year <- c(NA)

unique_networks <-c(NA)

plant_species <- c(37)

pollinator_species <- c(225)  

network_size <- c(8325)

metadata_1 <- data.frame(csv_file_name, longitude, latitude, country, location,duration, 
                       experiment_year,unique_networks,plant_species, pollinator_species, 
                       network_size)

 metadata <- rbind(metadata, metadata_1)
 
 #3RDI Bundgaard 2003
 #Not much info about this network, collected in one season in Denmark and coordinates
 
 csv_file_name <- c("4_bundgaard_2003.csv")
 
 longitude <- c(56.066667)
 
 latitude <- c(10.233333)
 
 country <- c("Denmark")
 
 location <- c(NA)
 
 duration <- c(NA)
 
 experiment_year <- c(NA)
 
 unique_networks <-c(NA)
 
 plant_species <- c(16)
 
 pollinator_species <- c(44)  
 
 network_size <- c(704)
 
 metadata_2 <- data.frame(csv_file_name, longitude, latitude, country, location,duration, 
                          experiment_year,unique_networks,plant_species, pollinator_species, 
                          network_size)
 
 metadata <- rbind(metadata, metadata_2)
 
 
 #4th Chacoff 2011

 csv_file_name <- c("5_chacoff_2011.csv")
 
 longitude <- c(-68.015892)
 
 latitude <- c(-32.008985)
 
 country <- c("Argentina")
 
 location <- c("Central Monte desert biome in Mendoza")
 
 duration <- c("4 seasons")
 
 experiment_year <- c("2006-2009")
 
 unique_networks <-c(4)
 
 plant_species <- c(59)
 
 pollinator_species <- c(196)  
 
 network_size <- c(11564)
 
 metadata_3 <- data.frame(csv_file_name, longitude, latitude, country, location,duration, 
                          experiment_year,unique_networks,plant_species, pollinator_species, 
                          network_size)
 
 metadata <- rbind(metadata, metadata_3)
 
 
 #6th  Dicks, Corbet & pywell 

 
 #"Shelfanger, Norfolk", "Hickling Broad National Nature Reserve"
dicks_1 <- read.csv("Data_networks/6_dicks_2002_1.csv")
dicks_2 <- read.csv("Data_networks/6_dicks_2002_2.csv")

dicks_1 <- melt(dicks_1)
dicks_2 <- melt(dicks_2)

 colnames(dicks_1) <- c("Plant_species", "Pollinator_species", "Interaction")
 colnames(dicks_2) <- c("Plant_species", "Pollinator_species", "Interaction")

 
metaweb_dicks_2002 <- rbind(dicks_1,dicks_2)
metaweb_dicks_2002$Pollinator_species=gsub("\\."," ",metaweb_dicks_2002$Pollinator_species)
 
metaweb_dicks_2002 <- acast(metaweb_dicks_2002, Plant_species ~ Pollinator_species , value.var='Interaction', 
                                             fun.aggregate=sum)
 
 write.csv(metaweb_dicks_2002, "Data_networks_metawebs/6_metaweb_dicks_2002.csv")
 
 
 
 csv_file_name <- c("6_dicks_2002.csv")
 
 longitude <- c("1.575532; 1.097873")
 
 latitude <- c("52.762395; 52.413173")
 
 country <- c("England")
 
 location <- c("Shelfanger, Norfolk; Hickling Broad
National Nature Reserve")
 
 duration <- c("1 season")
 
 experiment_year <- c("2001?")
 
 unique_networks <-c(2)
 
 plant_species <- c(23)
 
 pollinator_species <- c(80)  
 
 network_size <- c(1840)
 
 metadata_4 <- data.frame(csv_file_name, longitude, latitude, country, location,duration, 
                          experiment_year,unique_networks,plant_species, pollinator_species, 
                          network_size)
 
 metadata <- rbind(metadata, metadata_4)
 
 
 #7th  Dupont & Olesen 2009
 
 #Although locations are separated, the communities are similar and will be integrated in asingle web
 #There is another network but I do not have access to it, so I will consider just 2 that are 
 #openly available
 
 
 dupont_1 <- read.csv("Data_networks/7_dupont_2009_denmark.csv")
 dupont_2 <- read.csv("Data_networks/7_dupont_2009_isenbjerg.csv")
 
 dupont_1 <- melt(dupont_1)
 dupont_2 <- melt( dupont_2)
 
 colnames(dupont_1) <- c("Plant_species", "Pollinator_species", "Interaction")
 colnames(dupont_2) <- c("Plant_species", "Pollinator_species", "Interaction")
 
 
 metaweb_dupont_2009 <- rbind(dupont_1,dupont_2)
 metaweb_dupont_2009$Pollinator_species=gsub("\\."," ",metaweb_dupont_2009$Pollinator_species)
 
 metaweb_dupont_2009 <- acast(metaweb_dupont_2009, Plant_species ~ Pollinator_species , value.var='Interaction', 
                             fun.aggregate=sum)
 
 write.csv(metaweb_dupont_2009, "Data_networks_metawebs/7_metaweb_dupont_2009.csv")
 
 
 csv_file_name <- c("7_dupont_2009.csv")
 
 longitude <- c("9.1; 9.266667")
 
 latitude <- c("56.1; 56.066667")
 
 country <- c("Denmark")
 
 location <- c("Fields near Ikast; Isen bjerg")
 
 duration <- c("1 season")
 
 experiment_year <- c("2005")
 
 unique_networks <-c(2)
 
 plant_species <- c(31)
 
 pollinator_species <- c(329)  
 
 network_size <- c(10199)
 
 metadata_5 <- data.frame(csv_file_name, longitude, latitude, country, location,duration, 
                          experiment_year,unique_networks,plant_species, pollinator_species, 
                          network_size)
 
 metadata <- rbind(metadata, metadata_5)
 
 
 #8th  Elberling 1999
 
#Unique network
 
 
 csv_file_name <- c("8_elberling_1999.csv")
 
 longitude <- c(18.5)
 
 latitude <- c(68.35)
 
 country <- c("Sweden")
 
 location <- c("Latnjajaure, Abisko")
 
 duration <- c("1 season")
 
 experiment_year <- c("1994")
 
 unique_networks <-c(1)
 
 plant_species <- c(24)
 
 pollinator_species <- c(118)  
 
 network_size <- c(2832)
 
 metadata_6 <- data.frame(csv_file_name, longitude, latitude, country, location,duration, 
                          experiment_year,unique_networks,plant_species, pollinator_species, 
                          network_size)
 
 metadata <- rbind(metadata, metadata_6)
 
 
 #9th  Elberling unpublished data
 
 
 csv_file_name <- c("9_elberling_unpublished_data.csv")
 
 longitude <- c(-20.5)
 
 latitude <- c(74.5)
 
 country <- c("Greenland")
 
 location <- c("Zackenberg")
 
 duration <- c(NA)
 
 experiment_year <- c(NA)
 
 unique_networks <-c(NA)
 
 plant_species <- c(31)
 
 pollinator_species <- c(76)  
 
 network_size <- c(2356)
 
 metadata_7 <- data.frame(csv_file_name, longitude, latitude, country, location,duration, 
                          experiment_year,unique_networks,plant_species, pollinator_species, 
                          network_size)
 
 metadata <- rbind(metadata, metadata_7)
 
 
 #10th  Fang & Huang 2012
 
 
fang_huang_1 <- read.csv("Data_networks/10_fang_huang_2008.csv")
fang_huang_2 <- read.csv("Data_networks/10_fang_huang_2009.csv")
fang_huang_3 <- read.csv("Data_networks/10_fang_huang_2010.csv")
 
fang_huang_1 <- melt(fang_huang_1)
fang_huang_2 <- melt(fang_huang_2)
fang_huang_3 <- melt(fang_huang_3)
 
 colnames(fang_huang_1) <- c("Plant_species", "Pollinator_species", "Interaction")
 colnames(fang_huang_2) <- c("Plant_species", "Pollinator_species", "Interaction")
 colnames(fang_huang_3) <- c("Plant_species", "Pollinator_species", "Interaction")
 
 
 metaweb_fang_huang_2012 <- rbind(fang_huang_1,fang_huang_2,fang_huang_3)
 metaweb_fang_huang_2012$Pollinator_species=gsub("\\."," ",metaweb_fang_huang_2012$Pollinator_species)
 
 metaweb_fang_huang_2012 <- acast(metaweb_fang_huang_2012, Plant_species ~ Pollinator_species , value.var='Interaction', 
                              fun.aggregate=sum)
 
 write.csv(metaweb_fang_huang_2012, "Data_networks_metawebs/10_metaweb_fang_huang_2012.csv")
 

  csv_file_name <- c("10_metaweb_fang_huang_2012.csv")
 
 longitude <- c(99.63806)
 
 latitude <- c(27.90139)
 
 country <- c("China")
 
 location <- c("Shangri-La Alpine Botanical Garden, Yunnan")
 
 duration <- c("3 seasons")
 
 experiment_year <- c("2008-2010")
 
 unique_networks <-c(3)
 
 plant_species <- c(130)
 
 pollinator_species <- c(247)  
 
 network_size <- c(32110)
 
 metadata_8 <- data.frame(csv_file_name, longitude, latitude, country, location,duration, 
                          experiment_year,unique_networks,plant_species, pollinator_species, 
                          network_size)
 
 metadata <- rbind(metadata, metadata_8)
 
 
 #11th  Inoue et al., 1990
 
csv_file_name <- c("11_metaweb_inoue_1990.csv")

longitude <- c(99.63806)

latitude <- c(27.90139)

country <- c("China")

location <- c("Shangri-La Alpine Botanical Garden, Yunnan")

duration <- c("3 seasons")

experiment_year <- c("2008-2010")

unique_networks <-c(3)

plant_species <- c(130)

pollinator_species <- c(247)  

network_size <- c(32110)

metadata_8 <- data.frame(csv_file_name, longitude, latitude, country, location,duration, 
                         experiment_year,unique_networks,plant_species, pollinator_species, 
                         network_size)

metadata <- rbind(metadata, metadata_8)