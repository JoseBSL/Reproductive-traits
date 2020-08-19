# Processing data from olesen 2002
# DOI: https://doi.org/10.1046/j.1472-4642.2002.00148.x
# Islands: azores (2000) and mauritius (1998-1999)
# IMPORTANT NOTE: 1 season 

#LOAD DATA

##########
#MAURITIUS
##########
mauritius <- read.csv("Data/Data_processing/olesen_islands_2002/olesen_mauritius.csv", row.names = 1)


#check levels
#plant species
levels(as.factor(row.names(mauritius)))

#insect species
levels(as.factor(colnames(mauritius)))
colnames(mauritius) <- gsub(".", " ", colnames(mauritius), fixed=TRUE)
colnames(mauritius) <- gsub(" Mauritius", "", colnames(mauritius), fixed=TRUE)
colnames(mauritius) <- gsub("sp", "sp.", colnames(mauritius), fixed=TRUE)
#seems ok

#save data
write.csv(mauritius, "Data/Data_processing/olesen_islands_2002/olesen_mauritius.csv")



##########
#AZORES
##########
azores <- read.csv("Data/Data_processing/olesen_islands_2002/olesen_azores.csv", row.names = 1)

#check levels
#plant species
levels(as.factor(row.names(azores)))


#insect species
levels(as.factor(colnames(azores)))
colnames(azores) <- gsub(".", " ", colnames(azores), fixed=TRUE)
colnames(azores) <- gsub(" Azores", "", colnames(azores), fixed=TRUE)
colnames(azores) <- gsub("sp", "sp.", colnames(azores), fixed=TRUE)

#save data 
write.csv(azores, "Data/Data_processing/olesen_islands_2002/olesen_2002_azores.csv")


