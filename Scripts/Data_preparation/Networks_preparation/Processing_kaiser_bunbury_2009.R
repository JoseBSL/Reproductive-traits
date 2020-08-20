# Processing data from kaiser bunbury 2011 
# DOI: 10.1111/j.1461-0248.2009.01437.x
# Mauritius
# Note: TSites sampled between 2007 and 2008
# I'm going to prepare here the unique networks per site and year and the metaweb

#Load library
library(reshape2)

#Load data
d <- read.csv("Data/Data_processing/kaiser_bunbury_mauritius_2010/Kaiser_bunbury_mauritius_2010.csv", header=T, stringsAsFactors=F)
species <- read.csv("Data/Data_processing/kaiser_bunbury_mauritius_2010/Kaiser_bunbury_mauritius_2010_species.csv", header=T, stringsAsFactors=F)


#subset species dataset to merge
animal_species <- subset(species, Kingdom == "Animal")
#change colname to merge data
colnames(d)[4]  <- "Reference.code"
#merge data
all <- merge(d, animal_species, by = "Reference.code", all= T)
all <- subset(all, Reference.code!="no visitors")


#select columns of interest
all_1 <- all[,-c(1,2,4,91,92,94:100)]
colnames(all_1)[88] <- "Pollinator.species.ID"
#Convert to long format
all_long <- melt(all_1, id.vars=c("Pollinator.species.ID", "Site"), variable.name="Plant.species.ID")
#leave just pollinator species names on id
all_long$Pollinator.species.ID <- sub("^(\\S*\\s+\\S+).*", "\\1", all_long$Pollinator.species.ID)


#Nos subset for plant sepcies in order to merge
plant_species <- subset(species, Kingdom == "Plants")
#now merge  by reference code
#but vefore remove dots and change for space
all_long$Plant.species.ID <- gsub("\\.", " ", all_long$Plant.species.ID)
#change colnames for merging
colnames(all_long)[3] <- "Reference.code"
#Remove varieties and just let species in order to merge with trait data
colnames(plant_species)[4] <- "Plant.species.ID" 

plant_species$Plant.species.ID <- sub("^(\\S*\\s+\\S+).*", "\\1", plant_species$Plant.species.ID)
#Now merge


#Some NAS appearing in plant species fix 
unique(levels(as.factor(plant_species$Reference.code)))
unique(levels(as.factor(all_long$Reference.code)))
#there is one extra level in the plant species dataframe, likely a mistake
#I have to call "Ta pe"  "Ta per" and solved
plant_species$Reference.code[plant_species$Reference.code=="Ta pe"] <- "Ta per"
plant_species$Reference.code[plant_species$Reference.code=="Psa te"] <- "Ps te"
plant_species$Reference.code[plant_species$Reference.code=="Psa te"] <- "Ps te"
plant_species$Reference.code[plant_species$Reference.code=="Psa ca"] <- "Psi ca"
all_long$Reference.code[all_long$Reference.code=="Ps ca"] <- "Psi ca"


#ALL THIS IS CODE WAS TO KNOW WHERE THE ISSUE OF THE MERGE WAS
#still one more lets check the issue
#merge unique cases to see issue
a <- as.data.frame(unique(levels(as.factor(plant_species$Reference.code))))
a_1 <- as.data.frame(unique(levels(as.factor(plant_species$Reference.code))))
a_2 <- cbind(a,a_1)
b <- as.data.frame(unique(levels(as.factor(all_long$Reference.code))))
colnames(a_2)[1] <- "ab"
colnames(b)[1] <- "ab"
ab <- merge(a_2,b, by="ab", all = T)



all_2 <- merge(all_long, plant_species, by = "Reference.code", all=T)
sum(is.na(all_2$Plant.species.ID))
unique(levels(as.factor(all_2$Reference.code)))


#NOW SUBSET BY SITE

#
##
###
####
##### 2 sites in total
####
###
##
#

###############################
#First site Control
###############################
control    <- subset(all_2, Site == "Control")
control$value[is.na(control$value)] <- 0
#convert to network
control_1 <- acast(control, control$Plant.species.ID ~ control$Pollinator.species.ID , value.var='value', 
                      fun.aggregate=sum, margins=F)

levels(as.factor(rownames(control_1)))
levels(as.factor(control$Plant.species.ID))

#remove non-existent interactions plant species
rowSums(as.matrix(control_1))
control_2 <- control_1[rowSums(control_1[, -1] > 0) != 0, ]
rowSums(as.matrix(control_2))
#remove non-existent interactions pollinator species
colSums(as.matrix(control_2))
control_3 <- control_2[, colSums(control_2 != 0) > 0]
colSums(as.matrix(control_3))
#seems ok

row.names(control_3)[row.names(control_3)=="NA"] <- "Sp."


#save network
write.csv(control_3,"Data/Data_processing/kaiser_bunbury_mauritius_2010/kaiser_bunbury_2010_mauritius_control.csv")



###############################
#Second site Restored
###############################
cma <- subset(all_2, Site == "CMA")
cma$value[is.na(cma$value)] <- 0
#convert to network
cma_1 <- acast(cma, cma$Plant.species.ID ~ cma$Pollinator.species.ID , value.var='value', 
                   fun.aggregate=sum, margins=F)
#remove non-existent interactions plant species
rowSums(as.matrix(cma_1))
cma_2 <- cma_1[rowSums(cma_1[, -1] > 0) != 0, ]
rowSums(as.matrix(cma_2))
#remove non-existent interactions pollinator species
colSums(as.matrix(cma_2))
cma_3 <- cma_2[, colSums(cma_2 != 0) > 0]
colSums(as.matrix(cma_3))
#seems ok
#save network
write.csv(cma_3,"Data/Data_processing/kaiser_bunbury_mauritius_2010/kaiser_bunbury_2010_mauritius_restored.csv")


############################################
#NOW PREPARE METAWEB WITH ALL SITES TOGETHER
############################################
#Finally prepare metaweb with the different all the different sites together per year
all_long_for_meta <- all_2
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
write.csv(metaweb_2,"Data/Data_processing/kaiser_bunbury_mauritius_2010/kaiser_bunbury_2010_mauritius_metaweb.csv")





