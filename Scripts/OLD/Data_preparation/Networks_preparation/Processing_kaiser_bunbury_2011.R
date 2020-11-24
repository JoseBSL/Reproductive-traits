# Processing data from kaiser bunbury 2011 
# DOI: 10.1111/j.1365-2745.2010.01732.x
# Seychelles
# Note: Sampled 1 season between 2007 and 2008
# I'm going to prepare here the unique networks per site and year and the metaweb

#Load library
library(reshape2)

#Load data
d <- read.csv("Data/Data_processing/kaiser_bunbury_seychelles_2011/Kaiser_bunbury_seychelles.csv", header=T, stringsAsFactors=F)
species <- read.csv("Data/Data_processing/kaiser_bunbury_seychelles_2011/Kaiser_bunbury_seychelles_species.csv", header=T, stringsAsFactors=F)

#Fix some species names for merging
species[species=="bee hawkmoth"] <- "bee_hawkmoth"
species[species=="bee sp1"] <- "bee_sp1"
species[species=="Unknown 5"] <- "Unknown_5"
species[species=="Unknown 4"] <- "Unknown_4"


#Subset just animal species for adding poll names
species_animal <-  subset(species, Kingdom == "Animal")
colnames(species_animal)[3] <- "Insect.code"

#Join the two dataframes
all <- merge(d, species_animal, by="Insect.code", all=T)

colnames(all)[45] <- "Pollinator.species.ID"
all <- all[,-c(1,2,4,43,44,46,47)]
#Convert to long format
all_long <- melt(all, id.vars=c("Pollinator.species.ID", "Site"), variable.name="Plant.species.ID")
#remove dots in species names and convert to space
all_long$Plant.species.ID <- gsub("\\.", " ", all_long$Plant.species.ID)

#Now subset by site in order to prepare network by year and site
levels(as.factor(all_long$Site))


#
##
###
####
##### 6 sites in total
####
###
##
#

###############################
#First site TROIS FERES ("Tro")
###############################
levels(as.factor(all_long$Site))
tro <- subset(all_long, Site== "Tro")
#convert NA's to zeros
tro$value[is.na(tro$value)] <- 0

trois_ferres <- acast(tro, tro$Plant.species.ID ~ tro$Pollinator.species.ID , value.var='value', 
                   fun.aggregate=sum, margins=F)
#Now clean network with just the interactions that take place
#plant species or pollinators with 0 interactions are going to be removed
rowSums(as.matrix(trois_ferres))
trois_ferres_1 <- trois_ferres[rowSums(trois_ferres[, -1] > 0) != 0, ]
#Check if it worked
rowSums(as.matrix(trois_ferres_1))
#Seems ok
#Now for pollinators
colSums(as.matrix(trois_ferres_1))
trois_ferres_2 <- trois_ferres_1[, colSums(trois_ferres_1 != 0) > 0]
colSums(as.matrix(trois_ferres_2))
#seems ok
#plant species
rownames(trois_ferres_2)
#insect species
colnames(trois_ferres_2)
colnames(trois_ferres_2) <- gsub("Fly", "Diptera", colnames(trois_ferres_2))
colnames(trois_ferres_2) <- gsub(" sp", " sp.", colnames(trois_ferres_2))
colnames(trois_ferres_2)[colnames(trois_ferres_2)=="Forcipomyia (Euprojoannisia) sp.1"] <- "Forcipomyia sp1"
colnames(trois_ferres_2)[colnames(trois_ferres_2)=="Scaptodrosophila undet sp.1"] <- "Scaptodrosophila sp1"

#save network
write.csv(trois_ferres_2,"Data/Data_processing/kaiser_bunbury_seychelles_2011/kaiser_bunbury_2011_seychelles_trois_ferres.csv")


################################
#2nd site Tea plantation ("Tea")
################################
levels(as.factor(all_long$Site))
tea <- subset(all_long, Site== "Tea")
#convert NA's to zeros
tea$value[is.na(tea$value)] <- 0
tea_plantation <- acast(tea, tea$Plant.species.ID ~ tea$Pollinator.species.ID , value.var='value', 
                      fun.aggregate=sum, margins=F)
#Now clean network with just the interactions that take place
#plant species or pollinators with 0 interactions are going to be removed
rowSums(as.matrix(tea_plantation))
tea_plantation_1 <- tea_plantation[rowSums(tea_plantation[, -1] > 0) != 0, ]
#Check if it worked
rowSums(as.matrix(tea_plantation_1))
#Seems ok
#Now for pollinators
colSums(as.matrix(tea_plantation_1))
tea_plantation_2 <- tea_plantation_1[, colSums(tea_plantation_1 != 0) > 0]
colSums(as.matrix(tea_plantation_2))
#seems ok
#plant species
rownames(tea_plantation_2)
#insect species
colnames(tea_plantation_2)
colnames(tea_plantation_2) <- gsub("Fly", "Diptera", colnames(tea_plantation_2))
colnames(tea_plantation_2) <- gsub(" sp", " sp.", colnames(tea_plantation_2))
colnames(tea_plantation_2)[colnames(tea_plantation_2)=="Forcipomyia (Euprojoannisia) sp.1"] <- "Forcipomyia sp1"
colnames(tea_plantation_2)[colnames(tea_plantation_2)=="Forcipomyia (Thyridomyia) litoraurea"] <- "Forcipomyia litoraurea"
colnames(tea_plantation_2)[colnames(tea_plantation_2)=="Hemipyrellia sp.."] <- "Hemipyrellia sp."
colnames(tea_plantation_2)[colnames(tea_plantation_2)=="Scaptodrosophila undet sp.1"] <- "Scaptodrosophila sp.1"


#save network
write.csv(tea_plantation_2,"Data/Data_processing/kaiser_bunbury_seychelles_2011/kaiser_bunbury_2011_seychelles_tea_plantation.csv")


################################
#3rd site La reserve ("Res")
################################
levels(as.factor(all_long$Site))
res <- subset(all_long, Site== "Res")
#convert NA's to zeros
res$value[is.na(res$value)] <- 0
la_reserve <- acast(res, res$Plant.species.ID ~ res$Pollinator.species.ID , value.var='value', 
                        fun.aggregate=sum, margins=F)
#Now clean network with just the interactions that take place
#plant species or pollinators with 0 interactions are going to be removed
rowSums(as.matrix(la_reserve))
la_reserve_1 <- la_reserve[rowSums(la_reserve[, -1] > 0) != 0, ]
#Check if it worked
rowSums(as.matrix(la_reserve_1))
#Seems ok
#Now for pollinators
colSums(as.matrix(la_reserve_1))
la_reserve_2 <- la_reserve_1[, colSums(la_reserve_1 != 0) > 0]
colSums(as.matrix(la_reserve_2))
#seems ok
#plant species
rownames(la_reserve_2)
#insect species
colnames(la_reserve_2)
colnames(la_reserve_2) <- gsub("Fly", "Diptera", colnames(la_reserve_2))
colnames(la_reserve_2) <- gsub(" sp", " sp.", colnames(la_reserve_2))
colnames(la_reserve_2)[colnames(la_reserve_2)=="Hemipyrellia sp.."] <- "Hemipyrellia sp."
colnames(la_reserve_2)[colnames(la_reserve_2)=="Phalanta phalantha aethiopica"] <- "Phalanta phalantha"

#save network
write.csv(la_reserve_2,"Data/Data_processing/kaiser_bunbury_seychelles_2011/kaiser_bunbury_2011_seychelles_la_reserve.csv")


################################
#4th site Copolia ("Cop")
################################
levels(as.factor(all_long$Site))
cop <- subset(all_long, Site== "Cop")
#convert NA's to zeros
cop$value[is.na(cop$value)] <- 0
copolia <- acast(cop, cop$Plant.species.ID ~ cop$Pollinator.species.ID , value.var='value', 
                    fun.aggregate=sum, margins=F)
#Now clean network with just the interactions that take place
#plant species or pollinators with 0 interactions are going to be removed
rowSums(as.matrix(copolia))
copolia_1 <- copolia[rowSums(copolia[, -1] > 0) != 0, ]
#Check if it worked
rowSums(as.matrix(copolia_1))
#Seems ok
#Now for pollinators
colSums(as.matrix(copolia_1))
copolia_2 <- copolia_1[, colSums(copolia_1 != 0) > 0]
colSums(as.matrix(copolia_2))
#seems ok

#plant species
rownames(copolia_2)
#insect species
colnames(copolia_2)
colnames(copolia_2) <- gsub("Fly", "Diptera", colnames(copolia_2))
colnames(copolia_2) <- gsub(" sp", " sp.", colnames(copolia_2))
colnames(copolia_2)[colnames(copolia_2)=="Hemipyrellia sp.."] <- "Hemipyrellia sp."
colnames(copolia_2)[colnames(copolia_2)=="Forcipomyia (Euprojoannisia) sp.1"] <- "Forcipomyia sp.1"

#seems ok
#save network
write.csv(copolia_2,"Data/Data_processing/kaiser_bunbury_seychelles_2011/kaiser_bunbury_2011_seychelles_copolia.csv")


################################
#5th site Casse dent ("Cas")
################################
levels(as.factor(all_long$Site))
cas <- subset(all_long, Site== "Cas")
#convert NA's to zeros
cas$value[is.na(cas$value)] <- 0
casse_dent <- acast(cas, cas$Plant.species.ID ~ cas$Pollinator.species.ID , value.var='value', 
                 fun.aggregate=sum, margins=F)
#Now clean network with just the interactions that take place
#plant species or pollinators with 0 interactions are going to be removed
rowSums(as.matrix(casse_dent))
casse_dent_1 <- casse_dent[rowSums(casse_dent[, -1] > 0) != 0, ]
#Check if it worked
rowSums(as.matrix(casse_dent_1))
#Seems ok
#Now for pollinators
colSums(as.matrix(casse_dent_1))
casse_dent_2 <- casse_dent_1[, colSums(casse_dent_1 != 0) > 0]
colSums(as.matrix(casse_dent_2))
#seems ok

#plant species
rownames(casse_dent_2)
#insect species
colnames(casse_dent_2)
colnames(casse_dent_2) <- gsub("Fly", "Diptera", colnames(casse_dent_2))
colnames(casse_dent_2) <- gsub(" sp", " sp.", colnames(casse_dent_2))
colnames(casse_dent_2)[colnames(casse_dent_2)=="Amblypsilopus (?) nr. simplex"] <- "Amblypsilopus simplex"
colnames(casse_dent_2)[colnames(casse_dent_2)=="Forcipomyia (Euprojoannisia) sp.1"] <- "Forcipomyia sp.1"
colnames(casse_dent_2)[colnames(casse_dent_2)=="Scaptodrosophila undet sp.1"] <- "Scaptodrosophila sp.1"
colnames(casse_dent_2)[colnames(casse_dent_2)=="Hemipyrellia sp.."] <- "Hemipyrellia sp."

#save network
write.csv(casse_dent_2,"Data/Data_processing/kaiser_bunbury_seychelles_2011/kaiser_bunbury_2011_seychelles_casse_dent.csv")

################################
#6th site Bernica ("Ber")
################################
levels(as.factor(all_long$Site))
ber <- subset(all_long, Site== "Ber")
#convert NA's to zeros
ber$value[is.na(ber$value)] <- 0
bernica <- acast(ber, ber$Plant.species.ID ~ ber$Pollinator.species.ID , value.var='value', 
                    fun.aggregate=sum, margins=F)
#Now clean network with just the interactions that take place
#plant species or pollinators with 0 interactions are going to be removed
rowSums(as.matrix(bernica))
bernica_1 <- bernica[rowSums(bernica[, -1] > 0) != 0, ]
#Check if it worked
rowSums(as.matrix(bernica_1))
#Seems ok
#Now for pollinators
colSums(as.matrix(bernica_1))
bernica_2 <- bernica_1[, colSums(bernica_1 != 0) > 0]
colSums(as.matrix(bernica_2))
#seems ok
#plant species
rownames(bernica_2)
#insect species
colnames(bernica_2)
colnames(bernica_2) <- gsub("Fly", "Diptera", colnames(bernica_2))
colnames(bernica_2) <- gsub(" sp", " sp.", colnames(bernica_2))
colnames(bernica_2)[colnames(bernica_2)=="Apis mellifera adansonii"] <- "Apis mellifera"
colnames(bernica_2)[colnames(bernica_2)=="Scaptodrosophila undet sp.1"] <- "Scaptodrosophila sp.1"
colnames(bernica_2)[colnames(bernica_2)=="Hemipyrellia sp.."] <- "Hemipyrellia sp."
colnames(bernica_2)[colnames(bernica_2)=="Eagris sabadius maheta"] <- "Eagris sabadius"

#save network
write.csv(bernica_2,"Data/Data_processing/kaiser_bunbury_seychelles_2011/kaiser_bunbury_2011_seychelles_bernica.csv")



############################################
#NOW PREPARE METAWEB WITH ALL SITES TOGETHER
############################################

#Finally prepare metaweb with the different all the different sites together per year
all_long_for_meta <- all_long
all_long_for_meta$value[is.na(all_long_for_meta$value)] <- 0

#Convert to matrix
metaweb <- acast(all_long_for_meta, all_long_for_meta$Plant.species.ID ~ all_long_for_meta$Pollinator.species.ID , value.var='value', 
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
write.csv(metaweb_2,"Data/Data_processing/kaiser_bunbury_seychelles_2011/kaiser_bunbury_2011_seychelles_metaweb.csv")

