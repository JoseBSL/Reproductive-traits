# Processing data from kaiser bunbury 2017 
# DOI: 10.1111/j.1365-2745.2010.01732.x
# Seychelles
# IMPORTANT NOTE: THE SITES WERE SAMPLED 1 SEASON BETWEEN 2007 AND 2008
# I'm going to prepare here the unique networks per site and year and the metaweb

#Load library
library(tidyr)
library(reshape2)

#load data
d <- read.csv("Data/Data_processing/Data_networks_processing/kaiser_bunbury_seychelles_2017/kaiser_Bunbury_2017.csv", header=T, stringsAsFactors=F)
plants <- read.csv("Data/Data_processing/Data_networks_processing/kaiser_bunbury_seychelles_2017/kaiser_Bunbury_2017_plant_species.csv", header=T,stringsAsFactors=F )
pollinators <- read.csv("Data/Data_processing/Data_networks_processing/kaiser_bunbury_seychelles_2017/kaiser_Bunbury_2017_pollinator_species.csv", header=T)

#check data str
str(d)
str(plants)
str(pollinators)

#Merge both dataframes network data plant species 
merge_d_plants <- merge(d, plants, by="Plant.species.ID", all = T)
#check output
head(merge_d_plants)
#convert to long format to merge with pollinator id
all_long <- melt(merge_d_plants, id.vars=c("Plant.species.name", "Site","Treatment"), variable.name="Pollinator.species.ID",
 measure.vars=colnames(merge_d_plants[c(7:150)]))
head(all_long)


merge_all <- merge(all_long, pollinators, by="Pollinator.species.ID", all = T)
head(merge_all)
abc<- merge_all[,c(1,5,7)]
levels(as.factor(merge_all$Site))

#Now subset by site (8 in total)


#
##
###
####
##### 8 sites in total
####
###
##
#


###############################
#Site 1 Rosebelle
###############################
Rosebelle <- subset(merge_all, Site=="Rosebelle")
str(Rosebelle)
Rosebelle <- as.data.frame(Rosebelle, stringsAsFactors=T)
Rosebelle <- acast(Rosebelle, Rosebelle$Plant.species.name ~ Rosebelle$Pollinator.species.name , value.var='value', 
      fun.aggregate=sum, margins=F)
rowSums(as.matrix(Rosebelle))
#remove species with 0 visits
Rosebelle <- Rosebelle[rowSums(Rosebelle[, -1] > 0) != 0, ]
rowSums(as.matrix(Rosebelle))

#removing colsums
colSums(as.matrix(Rosebelle))
Rosebelle <- Rosebelle[, colSums(Rosebelle != 0) > 0]
colSums(as.matrix(Rosebelle))

write.csv(Rosebelle, "Data/Data_processing/Data_networks_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_rosebelle.csv")



###############################
#Site 2 Bernica
###############################
Bernica <- subset(merge_all, Site=="Bernica")
Bernica <- as.data.frame(Bernica, stringsAsFactors=T)
Bernica <- acast(Bernica, Bernica$Plant.species.name ~ Bernica$Pollinator.species.name , value.var='value', 
                   fun.aggregate=sum, margins=F)
rowSums(as.matrix(Bernica))
#remove species with 0 visits
Bernica <- Bernica[rowSums(Bernica[, -1] > 0) != 0, ]
rowSums(as.matrix(Bernica))

#removing colsums
colSums(as.matrix(Bernica))
Bernica <- Bernica[, colSums(Bernica != 0) > 0]
colSums(as.matrix(Bernica))

write.csv(Bernica, "Data/Data_processing/Data_networks_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_bernica.csv")



###############################
#Site 3 Casse dent
###############################
Casse_Dent <- subset(merge_all, Site=="Casse Dent")
str(Casse_Dent)
Casse_Dent <- as.data.frame(Casse_Dent, stringsAsFactors=T)
Casse_Dent <- acast(Casse_Dent, Casse_Dent$Plant.species.name ~ Casse_Dent$Pollinator.species.name , value.var='value', 
                 fun.aggregate=sum, margins=F)
rowSums(as.matrix(Casse_Dent))
#remove species with 0 visits
Casse_Dent <- Casse_Dent[rowSums(Casse_Dent[, -1] > 0) != 0, ]
rowSums(as.matrix(Casse_Dent))

#removing colsums
colSums(as.matrix(Casse_Dent))
Casse_Dent <- Casse_Dent[, colSums(Casse_Dent != 0) > 0]
colSums(as.matrix(Casse_Dent))

write.csv(Casse_Dent, "Data/Data_processing/Data_networks_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_casse_dent.csv")



###############################
#Site 4 Copolia
###############################
Copolia <- subset(merge_all, Site=="Copolia")
str(Copolia)
Copolia <- as.data.frame(Copolia, stringsAsFactors=T)
Copolia <- acast(Copolia, Copolia$Plant.species.name ~ Copolia$Pollinator.species.name , value.var='value', 
                    fun.aggregate=sum, margins=F)
rowSums(as.matrix(Copolia))
#remove species with 0 visits
Copolia <- Copolia[rowSums(Copolia[, -1] > 0) != 0, ]
rowSums(as.matrix(Copolia))

#removing colsums
colSums(as.matrix(Copolia))
Copolia <- Copolia[, colSums(Copolia != 0) > 0]
colSums(as.matrix(Copolia))

write.csv(Copolia, "Data/Data_processing/Data_networks_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_copolia.csv")



###############################
#Site 5 La reserve
###############################
reserve <- subset(merge_all, Site=="Reserve, La")
str(reserve)
reserve <- as.data.frame(reserve, stringsAsFactors=T)
reserve <- acast(reserve, reserve$Plant.species.name ~ reserve$Pollinator.species.name , value.var='value', 
                 fun.aggregate=sum, margins=F)
rowSums(as.matrix(reserve))
#remove species with 0 visits
reserve <- reserve[rowSums(reserve[, -1] > 0) != 0, ]
rowSums(as.matrix(reserve))

#removing colsums
colSums(as.matrix(reserve))
reserve <- reserve[, colSums(reserve != 0) > 0]
colSums(as.matrix(reserve))

write.csv(reserve, "Data/Data_processing/Data_networks_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_reserve.csv")


###############################
#Site 6 Salazie
###############################
Salazie <- subset(merge_all, Site=="Salazie")
str(Salazie)
Salazie <- as.data.frame(Salazie, stringsAsFactors=T)
Salazie <- acast(Salazie, Salazie$Plant.species.name ~ Salazie$Pollinator.species.name , value.var='value', 
                 fun.aggregate=sum, margins=F)
rowSums(as.matrix(Salazie))
#remove species with 0 visits
Salazie <- Salazie[rowSums(Salazie[, -1] > 0) != 0, ]
rowSums(as.matrix(Salazie))

#removing colsums
colSums(as.matrix(Salazie))
Salazie <- Salazie[, colSums(Salazie != 0) > 0]
colSums(as.matrix(Salazie))

write.csv(Salazie, "Data/Data_processing/Data_networks_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_salazie.csv")



###############################
#Site 7 Trois ferres
###############################
Trois_freres <- subset(merge_all, Site=="Trois Freres")
str(Salazie)
Trois_freres <- as.data.frame(Trois_freres, stringsAsFactors=T)
Trois_freres <- acast(Trois_freres, Trois_freres$Plant.species.name ~ Trois_freres$Pollinator.species.name , value.var='value', 
                 fun.aggregate=sum, margins=F)
rowSums(as.matrix(Trois_freres))
#remove species with 0 visits
Trois_freres <- Trois_freres[rowSums(Trois_freres[, -1] > 0) != 0, ]
rowSums(as.matrix(Trois_freres))

#removing colsums
colSums(as.matrix(Trois_freres))
Trois_freres <- Trois_freres[, colSums(Trois_freres != 0) > 0]
colSums(as.matrix(Trois_freres))

write.csv(Trois_freres, "Data/Data_processing/Data_networks_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_trois_feres.csv")



###############################
#Site 8 Tea plantation
###############################
tea_plantation <- subset(merge_all, Site=="Tea Plantation")
str(tea_plantation)
tea_plantation <- as.data.frame(tea_plantation, stringsAsFactors=T)
tea_plantation <- acast(tea_plantation, tea_plantation$Plant.species.name ~ tea_plantation$Pollinator.species.name , value.var='value', 
                      fun.aggregate=sum, margins=F)
rowSums(as.matrix(tea_plantation))
#remove species with 0 visits
tea_plantation <- tea_plantation[rowSums(tea_plantation[, -1] > 0) != 0, ]
rowSums(as.matrix(tea_plantation))

#removing colsums
colSums(as.matrix(tea_plantation))
tea_plantation <- tea_plantation[, colSums(tea_plantation != 0) > 0]
colSums(as.matrix(tea_plantation))

write.csv(tea_plantation, "Data/Data_processing/Data_networks_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_tea_plantation.csv")


############################################
#NOW PREPARE METAWEB WITH ALL SITES TOGETHER
############################################
#checking for NA'S
merge_all$value[is.na(merge_all$value)]
#no NA's
#create othe data.frame to dont mess on the merge_all
all_long_for_meta <- merge_all
#create metawen
metaweb <- acast(all_long_for_meta, all_long_for_meta$Plant.species.name ~ all_long_for_meta$Pollinator.species.name , value.var='value', 
                        fun.aggregate=sum, margins=F)

#same remove plant species with no visits
rowSums(as.matrix(metaweb))
metaweb_1 <- metaweb[rowSums(metaweb[, -1] > 0) != 0, ]
rowSums(as.matrix(metaweb_1))

#same remove poll species with no visits
colSums(as.matrix(metaweb_1))
metaweb_2 <- metaweb_1[, colSums(metaweb_1 != 0) > 0]
colSums(as.matrix(metaweb_2))

#save metaweb
write.csv(metaweb_2, "Data/Data_processing/Data_networks_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_metaweb.csv")
