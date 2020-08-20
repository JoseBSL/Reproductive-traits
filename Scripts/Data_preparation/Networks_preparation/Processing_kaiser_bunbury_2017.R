# Processing data from kaiser bunbury 2017 
# DOI: 10.1111/j.1365-2745.2010.01732.x
# Seychelles
# Note: Sampled 1 season between 2007 and 2008
# I'm going to prepare here the unique networks per site and year and the metaweb

#Load library
library(tidyr)
library(reshape2)

#load data
d <- read.csv("Data/Data_processing/kaiser_bunbury_seychelles_2017/kaiser_Bunbury_2017.csv", header=T, stringsAsFactors=F)
plants <- read.csv("Data/Data_processing/kaiser_bunbury_seychelles_2017/kaiser_Bunbury_2017_plant_species.csv", header=T,stringsAsFactors=F )
pollinators <- read.csv("Data/Data_processing/kaiser_bunbury_seychelles_2017/kaiser_Bunbury_2017_pollinator_species.csv", header=T)

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


#Check levels
#plants
levels(as.factor(row.names(Rosebelle)))
#seems ok
#insect species
levels(as.factor(colnames(Rosebelle)))
colnames(Rosebelle) <- gsub("sp", "sp.", colnames(Rosebelle), fixed=TRUE)
colnames(Rosebelle)[colnames(Rosebelle)=="Forcipomyia (Euprojoannisia) sp.1"] <- "Forcipomyia sp.1"
colnames(Rosebelle)[colnames(Rosebelle)=="Forcipomyia (Euprojoannisia) sp.1"] <- "Forcipomyia amieuensis sp.1"
colnames(Rosebelle)[colnames(Rosebelle)=="Forcipomyia (Euprojoannisia) sp.1"] <- "Forcipomyia litoraurea sp.1"
colnames(Rosebelle)[colnames(Rosebelle)=="Endotricha mesenterialis mahensis"] <- "Endotricha mesenterialis"
colnames(Rosebelle)[colnames(Rosebelle)=="Scaptodrosophila undet sp.1"] <- "Scaptodrosophila sp.1"
colnames(Rosebelle)[colnames(Rosebelle)=="Scaptodrosophila undet sp.1"] <- "Scaptodrosophila sp.1"
colnames(Rosebelle)[colnames(Rosebelle)=="Forcipomyia (Thyridomyia) litoraurea"] <- "Forcipomyia litoraurea sp.2"
colnames(Rosebelle)[colnames(Rosebelle)=="Forcipomyia (Microhelea) amieuensis"] <- "Forcipomyia amieuensis sp.2"
colnames(Rosebelle)[colnames(Rosebelle)=="Scaptodrosophila undet sp.2"] <- "Scaptodrosophila sp.2"


write.csv(Rosebelle, "Data/Data_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_rosebelle.csv")



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


#Check levels
#plants
levels(as.factor(row.names(Bernica)))
#seems ok
#insect species
colnames(Bernica) <- gsub("sp", "sp.", colnames(Bernica), fixed=TRUE)
levels(as.factor(colnames(Bernica)))
colnames(Bernica)[colnames(Bernica)=="Amblypsilopus (?) nr. simplex"] <- "Amblypsilopus simplex"
colnames(Bernica)[colnames(Bernica)=="Amblypsilopus (?) sp.. indet."] <- "Amblypsilopus sp."
colnames(Bernica)[colnames(Bernica)=="Forcipomyia (Euprojoannisia) sp.1"] <- "Euprojoannisia sp.1"
colnames(Bernica)[colnames(Bernica)=="Forcipomyia (Microhelea) amieuensis"] <- "Forcipomyia amieuensis"
colnames(Bernica)[colnames(Bernica)=="Stenomorda near disp.arilis"] <- "Stenomorda sp."
colnames(Bernica)[colnames(Bernica)=="Scaptodrosophila undet sp.1"] <- "Scaptodrosophila sp."
colnames(Bernica)[colnames(Bernica)=="Liosarcophaga sp.ilargyra"] <- "Sarcophaga sp."

write.csv(Bernica, "Data/Data_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_bernica.csv")



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


#Check levels
#plants
levels(as.factor(row.names(Casse_Dent)))
#seems ok
#insect species
colnames(Casse_Dent) <- gsub("sp", "sp.", colnames(Casse_Dent), fixed=TRUE)
levels(as.factor(colnames(Casse_Dent)))
colnames(Casse_Dent)[colnames(Casse_Dent)=="Amblypsilopus (?) nr. simplex"] <- "Amblypsilopus simplex"
colnames(Casse_Dent)[colnames(Casse_Dent)=="Forcipomyia (Euprojoannisia) sp.1"] <- "Forcipomyia sp.1"
colnames(Casse_Dent)[colnames(Casse_Dent)=="Forcipomyia (Microhelea) amieuensis"] <- "Forcipomyia amieuensis"
colnames(Casse_Dent)[colnames(Casse_Dent)=="Forcipomyia (Thyridomyia) litoraurea"] <- "Forcipomyia litoraurea"
colnames(Casse_Dent)[colnames(Casse_Dent)=="Drosophila sp.inipes"] <- "Drosophila spinipes"
colnames(Casse_Dent)[colnames(Casse_Dent)=="Scaptodrosophila undet sp.1"] <- "Scaptodrosophila sp.1"
colnames(Casse_Dent)[colnames(Casse_Dent)=="Scaptodrosophila undet sp.2"] <- "Scaptodrosophila sp.2"
colnames(Casse_Dent)[colnames(Casse_Dent)=="near Eurycratus sp.1 "] <- "Eurycratus sp.1"

write.csv(Casse_Dent, "Data/Data_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_casse_dent.csv")



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


#Check levels
#plants
levels(as.factor(row.names(Copolia)))
#seems ok
#insect species
colnames(Copolia) <- gsub("sp", "sp.", colnames(Copolia), fixed=TRUE)
levels(as.factor(colnames(Copolia)))
colnames(Copolia)[colnames(Copolia)=="Amblypsilopus (?) nr. simplex"] <- "Amblypsilopus simplex"
colnames(Copolia)[colnames(Copolia)=="Cryptophleps ? nigrihalteratus ?"] <- "Cryptophleps nigrihalteratus"
colnames(Copolia)[colnames(Copolia)=="Scaptodrosophila undet sp.1"] <- "Scaptodrosophila sp.1"
colnames(Copolia)[colnames(Copolia)=="Forcipomyia (Thyridomyia) litoraurea"] <- "Forcipomyia litoraurea"
colnames(Copolia)[colnames(Copolia)=="Forcipomyia (Euprojoannisia) sp.1"] <- "Forcipomyia sp.1"
colnames(Copolia)[colnames(Copolia)=="Forcipomyia (Microhelea) amieuensis"] <- "Forcipomyia amieuensis"
colnames(Copolia)[colnames(Copolia)=="near Eurycratus sp.1 "] <- "Eurycratus sp.1"


write.csv(Copolia, "Data/Data_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_copolia.csv")



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


#Check levels
#plants
levels(as.factor(row.names(reserve)))
#seems ok
#insect species
colnames(reserve) <- gsub("sp", "sp.", colnames(reserve), fixed=TRUE)
levels(as.factor(colnames(reserve)))
colnames(reserve)[colnames(reserve)=="Amblypsilopus (?) nr. simplex"] <- "Amblypsilopus simplex"
colnames(reserve)[colnames(reserve)=="Forcipomyia (Thyridomyia) litoraurea"] <- "Forcipomyia litoraurea"
colnames(reserve)[colnames(reserve)=="Forcipomyia (Euprojoannisia) sp.1"] <- "Forcipomyia sp.1"
colnames(reserve)[colnames(reserve)=="Forcipomyia (Microhelea) amieuensis"] <- "Forcipomyia amieuensis"
colnames(reserve)[colnames(reserve)=="Scaptodrosophila undet sp.1"] <- "Scaptodrosophila sp.1"
colnames(reserve)[colnames(reserve)=="Liosarcophaga sp.ilargyra"] <- "Liosarcophaga spilargyra"
colnames(reserve)[colnames(reserve)=="near Eurycratus sp.1 "] <- "Eurycratus sp.1"


write.csv(reserve, "Data/Data_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_reserve.csv")


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


#Check levels
#plants
levels(as.factor(row.names(Salazie)))
#seems ok
#insect species
colnames(Salazie) <- gsub("sp", "sp.", colnames(Salazie), fixed=TRUE)
levels(as.factor(colnames(Salazie)))
colnames(Salazie)[colnames(Salazie)=="Amblypsilopus (?) nr. simplex"] <- "Amblypsilopus simplex"
colnames(Salazie)[colnames(Salazie)=="Scaptodrosophila undet sp.2"] <- "Scaptodrosophila sp.2"
colnames(Salazie)[colnames(Salazie)=="Scaptodrosophila undet sp.1"] <- "Scaptodrosophila sp.1"
colnames(Salazie)[colnames(Salazie)=="Forcipomyia (Euprojoannisia) sp.1"] <- "Forcipomyia sp.1"
colnames(Salazie)[colnames(Salazie)=="Forcipomyia (Thyridomyia) litoraurea"] <- "Forcipomyia litoraurea"
colnames(Salazie)[colnames(Salazie)=="Endotricha mesenterialis mahensis"] <- "Endotricha mesenterialis"



write.csv(Salazie, "Data/Data_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_salazie.csv")



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


#Check levels
#plants
levels(as.factor(row.names(Trois_freres)))
#seems ok
#insect species
colnames(Trois_freres) <- gsub("sp", "sp.", colnames(Trois_freres), fixed=TRUE)
levels(as.factor(colnames(Trois_freres)))
colnames(Trois_freres)[colnames(Trois_freres)=="Forcipomyia (Thyridomyia) litoraurea"] <- "Forcipomyia litoraurea"
colnames(Trois_freres)[colnames(Trois_freres)=="Forcipomyia (Thyridomyia) litoraurea"] <- "Forcipomyia litoraurea"
colnames(Trois_freres)[colnames(Trois_freres)=="Scaptodrosophila undet sp.2"] <- "Scaptodrosophila sp.2"
colnames(Trois_freres)[colnames(Trois_freres)=="Scaptodrosophila undet sp.1"] <- "Scaptodrosophila sp.1"
colnames(Trois_freres)[colnames(Trois_freres)=="Forcipomyia (Euprojoannisia) sp.1"] <- "Forcipomyia sp.1"
colnames(Trois_freres)[colnames(Trois_freres)=="Forcipomyia (Microhelea) amieuensis"] <- "Forcipomyia amieuensis"
colnames(Trois_freres)[colnames(Trois_freres)=="Inostemma close to soederlundi"] <- "Inostemma sp."

#save data
write.csv(Trois_freres, "Data/Data_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_trois_feres.csv")



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



#Check levels
#plants
levels(as.factor(row.names(tea_plantation)))
#seems ok
#insect species
colnames(tea_plantation) <- gsub("sp", "sp.", colnames(tea_plantation), fixed=TRUE)
levels(as.factor(colnames(tea_plantation)))
colnames(tea_plantation)[colnames(tea_plantation)=="Amblypsilopus (?) nr. simplex"] <- "Amblypsilopus simplex"
colnames(tea_plantation)[colnames(tea_plantation)=="Forcipomyia (Microhelea) amieuensis"] <- "Forcipomyia amieuensis"
colnames(tea_plantation)[colnames(tea_plantation)=="Forcipomyia (Thyridomyia) litoraurea"] <- "Forcipomyia litoraurea"
colnames(tea_plantation)[colnames(tea_plantation)=="Forcipomyia (Euprojoannisia) sp.1"] <- "Forcipomyia sp.1"
colnames(tea_plantation)[colnames(tea_plantation)=="Scaptodrosophila undet sp.1"] <- "Scaptodrosophila sp.1"
colnames(tea_plantation)[colnames(tea_plantation)=="near Eurycratus sp.1 "] <- "Eurycratus sp.1"




write.csv(tea_plantation, "Data/Data_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_tea_plantation.csv")


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

#Check levels
#plants
levels(as.factor(row.names(metaweb_2)))
#seems ok
#insect species
colnames(metaweb_2) <- gsub("sp", "sp.", colnames(metaweb_2), fixed=TRUE)
levels(as.factor(colnames(metaweb_2)))
colnames(metaweb_2)[colnames(metaweb_2)=="Amblypsilopus (?) nr. simplex"] <- "Amblypsilopus simplex"
colnames(metaweb_2)[colnames(metaweb_2)=="Amblypsilopus (?) sp.. indet."] <- "Amblypsilopus sp."
colnames(metaweb_2)[colnames(metaweb_2)=="Endotricha mesenterialis mahensis"] <- "Endotricha mesenterialis"
colnames(metaweb_2)[colnames(metaweb_2)=="Stenomorda near disp.arilis"] <- "Stenomorda disparilis"
colnames(metaweb_2)[colnames(metaweb_2)=="Forcipomyia (Microhelea) amieuensis"] <- "Forcipomyia amieuensis"
colnames(metaweb_2)[colnames(metaweb_2)=="Forcipomyia (Thyridomyia) litoraurea"] <- "Forcipomyia litoraurea"
colnames(metaweb_2)[colnames(metaweb_2)=="Scaptodrosophila undet sp.1"] <- "Scaptodrosophila sp.1"
colnames(metaweb_2)[colnames(metaweb_2)=="Drosophila sp.inipes"] <- "Drosophila spinipes"
colnames(metaweb_2)[colnames(metaweb_2)=="Liosarcophaga sp.ilargyra"] <- "Liosarcophaga spilargyra"
colnames(metaweb_2)[colnames(metaweb_2)=="Cryptophleps ? nigrihalteratus ?"] <- "Cryptophleps nigrihalteratus"
colnames(metaweb_2)[colnames(metaweb_2)=="Forcipomyia (Euprojoannisia) sp.1"] <- "Forcipomyia sp.1"
colnames(metaweb_2)[colnames(metaweb_2)=="Scaptodrosophila undet sp.2"] <- "Scaptodrosophila sp.2"
colnames(metaweb_2)[colnames(metaweb_2)=="Inostemma close to soederlundi"] <- "Inostemma sp."



#save metaweb
write.csv(metaweb_2, "Data/Data_processing/kaiser_bunbury_seychelles_2017/kaiser_bunbury_2017_seychelles_metaweb.csv")
