#Network organization

#CRITERIA
#Networks are going to be organized in metawebs, individual networks will be merged
#When communities are homgeneous within the same study


#FIRST I start with Bartomeus, vila & Santamaria 2008 from Oecologia

#BAT,MED,FAR is the Carprobrotus metaweb
#SEL,FRA,MIQ is the Opuntia metaweb


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

write.csv(carpobrotus_metaweb_Bartomeus_2008, "Data_networks_metawebs/carpobrotus_metaweb_Bartomeus_2008.csv")

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

write.csv(opuntia_metaweb_Bartomeus_2008, "Data_networks_metawebs/opuntia_metaweb_Bartomeus_2008.csv")

