
#Read Kaiser-bunbury 2009 Networks
#DOI: https://doi.org/10.1016/j.ppees.2009.04.001
#Prepare individual networks and metaweb

library(reshape2)

#Read the two sites from Mauritius
restored_kaiser_bunbury_2009 <- read.csv("Data/Data_networks_processing/restored_kaiser_bunbury_2009.csv", row.names = 1)
control_kaiser_bunbury_2009 <- read.csv("Data/Data_networks_processing/control_kaiser_bunbury_2009.csv")


#First I work with the restored site (change position of rows for columns and convert to binary)
str(restored_kaiser_bunbury_2009)
#Convert to matrix
restored_kaiser_bunbury_2009 <- as.matrix(restored_kaiser_bunbury_2009)
restored_kaiser_bunbury_2009 <- melt(restored_kaiser_bunbury_2009)
#Columns names
colnames(restored_kaiser_bunbury_2009) <- c("Pollinator_species","Plant_species", "Interaction")
#Remove dot from species names
restored_kaiser_bunbury_2009$Plant_species=gsub("\\."," ",restored_kaiser_bunbury_2009$Plant_species)
#Add 1 and 0 when required Criteria df[,]>0 == 1; ifelse does the job
restored_kaiser_bunbury_2009$Interaction=ifelse(as.numeric(restored_kaiser_bunbury_2009$Interaction) > 0, 1, 0)
#Reorganize to individual matrix
restored_kaiser_bunbury_2009 <- acast(restored_kaiser_bunbury_2009, Plant_species ~ Pollinator_species , value.var='Interaction', 
                fun.aggregate=sum)

write.csv(restored_kaiser_bunbury_2009, "Data_networks/13_restored_kaiser_bunbury_2009.csv")

#Second control Kaiser bunbury 
control_kaiser_bunbury_2009 <- read.csv("Data/Data_networks_processing/control_kaiser_bunbury_2009.csv", row.names = 1)

str(control_kaiser_bunbury_2009)
#Convert to matrix
control_kaiser_bunbury_2009 <- as.matrix(control_kaiser_bunbury_2009)
control_kaiser_bunbury_2009 <- melt(control_kaiser_bunbury_2009)
#Columns names
colnames(control_kaiser_bunbury_2009) <- c("Pollinator_species","Plant_species", "Interaction")
#Remove dot from species names
control_kaiser_bunbury_2009$Plant_species=gsub("\\."," ",control_kaiser_bunbury_2009$Plant_species)
#Add 1 and 0 when required Criteria df[,]>0 == 1; ifelse does the job
control_kaiser_bunbury_2009$Interaction=ifelse(as.numeric(control_kaiser_bunbury_2009$Interaction) > 0, 1, 0)
#Reorganize to individual matrix
control_kaiser_bunbury_2009 <- acast(control_kaiser_bunbury_2009, Plant_species ~ Pollinator_species , value.var='Interaction', 
                                      fun.aggregate=sum)

write.csv(control_kaiser_bunbury_2009, "Data_networks/13_control_kaiser_bunbury_2009.csv")
