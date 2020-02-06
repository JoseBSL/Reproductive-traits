#Network organization

#CRITERIA
#Networks are going to be organized in metawebs, individual networks will be merged
#When communities are homgeneous within the same study


#FIRST I start with Bartomeus, vila & Santamaria 2008 from Oecologia

#BAT,MED,FAR is the Carprobrotus metaweb
#SEL,FRA,MIQ is the Opuntia metaweb
library(reshape2)
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

write.csv(carpobrotus_metaweb_Bartomeus_2008, "Data_networks_metawebs/1_metaweb_carpobrotus_Bartomeus_2008.csv")

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

write.csv(opuntia_metaweb_Bartomeus_2008, "Data_networks_metawebs/2_metaweb_opuntia_Bartomeus_2008.csv")


csv_file_name <- c("1_metaweb_carpobrotus_Bartomeus_2008", "2_metaweb_opuntia_Bartomeus_2008.csv")

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
 
 
#11th  Inouye et al., 1990
 
csv_file_name <- c("11_metaweb_inouye_1990.csv")

longitude <- c(135.866667)

latitude <- c(35.166667)

country <- c("Japan")

location <- c("Kibune, Kyoto")

duration <- c("4 seasons")

experiment_year <- c("1984-1987")

unique_networks <-c(4)

plant_species <- c(114)

pollinator_species <- c(883)  

network_size <- c(100662)

metadata_9 <- data.frame(csv_file_name, longitude, latitude, country, location,duration, 
                         experiment_year,unique_networks,plant_species, pollinator_species, 
                         network_size)

metadata <- rbind(metadata, metadata_9)


#12th  Inouye et al., 1988

inouye_1988 <- read.csv("Data_networks/12_inouye_1988.csv", row.names = 1)

csv_file_name <- c("12_inoue_1988.csv")

longitude <- c(148.266667)

latitude <- c(-36.45)

country <- c("Australia")

location <- c("Snowy Mountains")

duration <- c("1 season")

experiment_year <- c("1983-1984")

unique_networks <-c(1)

plant_species <- c(as.numeric(nrow(inouye_1988)))

pollinator_species <- c(as.numeric(ncol(inouye_1988)))  

network_size <- c(as.numeric(nrow(inouye_1988))*as.numeric(ncol(inouye_1988)))

metadata_10 <- data.frame(csv_file_name, longitude, latitude, country, location,duration, 
                         experiment_year,unique_networks,plant_species, pollinator_species, 
                         network_size)

metadata <- rbind(metadata, metadata_10)

#13th  Kaiser-Bunbury, memmott & muller 2009
#Two sites in Mauritius, homogeneus plant species
#Convert into metaweb for analysis

#Carpobrutus metaweb

control_kaiser_bunbury_2009 <- read.csv("Data_networks/13_control_kaiser_bunbury_2009.csv")
restored_kaiser_bunbury_2009 <- read.csv("Data_networks/13_restored_kaiser_bunbury_2009.csv")

control_kaiser_bunbury_2009 <- melt(control_kaiser_bunbury_2009)
restored_kaiser_bunbury_2009 <- melt(restored_kaiser_bunbury_2009)

colnames(control_kaiser_bunbury_2009) <- c("Plant_species", "Pollinator_species", "Interaction")
colnames(restored_kaiser_bunbury_2009) <- c("Plant_species", "Pollinator_species", "Interaction")
metaweb_kaiser_bunbury_2009<- rbind(control_kaiser_bunbury_2009,med,restored_kaiser_bunbury_2009)

metaweb_kaiser_bunbury_2009$Pollinator_species=gsub("\\."," ",metaweb_kaiser_bunbury_2009$Pollinator_species)

metaweb_kaiser_bunbury_2009 <- acast(metaweb_kaiser_bunbury_2009, Plant_species ~ Pollinator_species , value.var='Interaction', 
                                            fun.aggregate=sum)

write.csv(metaweb_kaiser_bunbury_2009, "Data_networks_metawebs/13_metaweb_kaiser_bunbury_2009.csv")


kaiser_bunbury_2009 <- read.csv("Data_networks_metawebs/13_metaweb_kaiser_bunbury_2009.csv", row.names = 1)

csv_file_name <- c("13_metaweb_kaiser_bunbury_2009.csv")

longitude <- c(57.443254)

latitude <- c(-20.452076)

country <- c("Mauritius")

location <- c("Black River Gorges National Park")

duration <- c("1 season")

experiment_year <- c("2003-2004")

unique_networks <-c(2)

plant_species <- c(as.numeric(nrow(kaiser_bunbury_2009)))

pollinator_species <- c(as.numeric(ncol(kaiser_bunbury_2009)))  

network_size <- c(as.numeric(nrow(kaiser_bunbury_2009))*as.numeric(ncol(kaiser_bunbury_2009)))

metadata_11 <- data.frame(csv_file_name, longitude, latitude, country, location,duration, 
                          experiment_year,unique_networks,plant_species, pollinator_species, 
                          network_size)

metadata <- rbind(metadata, metadata_11)



#14th  Kaiser-Bunbury et al., 2014
#https://doi.org/10.1890/14-0024.1

#Not able to find the individual networks so I will consider directly the csv's
#as a single metaweb, the criteria of division of this files
#could per site but I'm not sure

#I will have to read it with a loop, too many files
setwd("~/R_projects/Reproductive Traits/Data/Data_networks_processing/kaiser_bunbury_2014")


#Workflow found on stackoverflow to read all the files in a list
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, read.csv)

#For loop to melt each data frame and merge
i <- NULL
kaiser_bunbury_2014 <- NULL

for (i in data_id_list){
  i <- melt(i)
  kaiser_bunbury_2014 <- rbind(kaiser_bunbury_2014, i)
}

kaiser_bunbury_2014 <- kaiser_bunbury_2014[,-2]
colnames(kaiser_bunbury_2014) <- c("Plant_Species", "Pollinator_Species", "Interaction")
kaiser_bunbury_2014$Pollinator_Species <- gsub("\\.", " ", kaiser_bunbury_2014$Pollinator_Species)
kaiser_bunbury_2014 <- acast(kaiser_bunbury_2014, Plant_Species ~ Pollinator_Species, value.var='Interaction', 
      fun.aggregate=sum)
setwd("~/R_projects/Reproductive Traits")

write.csv(kaiser_bunbury_2014, "Data_networks/14_metaweb_kaiser_bunbury_2014.csv")
