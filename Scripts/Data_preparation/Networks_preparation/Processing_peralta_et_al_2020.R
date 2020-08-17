# Processing data from peralta 2020
# DOI: 10.1111/j.1365-2745.2010.01732.x
# Mendoza, Argentina
# IMPORTANT NOTE: THE SITES WERE SAMPLED BETWEEN 2006-2011
# I'm going to prepare here the unique networks per site and year and the metaweb

#load library
library(reshape2)

#load data
plant_names <- read.csv("Data/Data_processing/peralta_argentina_2020/plant_names.csv", header=T, stringsAsFactors=F)
colnames(plant_names)[1] <- "Plant.species.ID"
insect_names <- read.csv("Data/Data_processing/peralta_argentina_2020/insect_names.csv", header=T, stringsAsFactors=F)
colnames(insect_names)[1] <- "Pollinator.species.ID"

#load functions
#this function is to convert first element of a string from lower to uppercasefirstup <- function(x) {
firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

###############################
#Site 1 2006
###############################
#load data
site_1_2006 <- read.csv("Data/Data_processing/peralta_argentina_2020/peralta_2006_site_1.csv", header=T, stringsAsFactors=F)
colnames(site_1_2006)[1] <- "Plant.species.ID"
#convert to long format
site_1_2006_long <- melt(site_1_2006, id.vars=c("Plant.species.ID"), variable.name="Pollinator.species.ID")
#merge with plant names
all_2006_1 <- merge(site_1_2006_long, plant_names, by = "Plant.species.ID", all = T)
all_2006_1_a <- merge(all_2006_1, insect_names, by = "Pollinator.species.ID", all = T)
colnames(all_2006_1_a)[4:5] <- c("Plant.species","Pollinator.species")

#remove underscore
all_2006_1_a$Plant.species <- gsub("_" , " ", fixed=TRUE, all_2006_1_a$Plant.species)
all_2006_1_a$Pollinator.species <- gsub("_" , " ", fixed=TRUE, all_2006_1_a$Pollinator.species)

#convert first element of the string to uppercase
all_2006_1_a$Plant.species <- firstup(all_2006_1_a$Plant.species)
#Convert to matrix
all_2006_1_b <- acast(all_2006_1_a, all_2006_1_a$Plant.species ~ all_2006_1_a$Pollinator.species , value.var='value', 
                 fun.aggregate=sum, margins=F)

#same remove plant species with no visits
rowSums(as.matrix(all_2006_1_b))
all_2006_1_c <- all_2006_1_b[rowSums(all_2006_1_b[, -1] > 0) != 0, ]
rowSums(as.matrix(all_2006_1_c))

#same remove poll species with no visits
colSums(as.matrix(all_2006_1_c))
all_2006_1_d <- all_2006_1_c[, colSums(all_2006_1_c != 0) > 0]
colSums(as.matrix(all_2006_1_d))

#save network
write.csv(all_2006_1_d,"Data/Data_processing/peralta_argentina_2020/peralta_2006_mendoza_site_1.csv")

###############################
#Site 2 2006
###############################
#load data
site_2_2006 <- read.csv("Data/Data_processing/peralta_argentina_2020/peralta_2006_site_2.csv", header=T, stringsAsFactors=F)
colnames(site_2_2006)[1] <- "Plant.species.ID"
#convert to long format
site_2_2006_long <- melt(site_2_2006, id.vars=c("Plant.species.ID"), variable.name="Pollinator.species.ID")
#merge with plant names
all_2006_2 <- merge(site_2_2006_long, plant_names, by = "Plant.species.ID", all = T)
all_2006_2_a <- merge(all_2006_2, insect_names, by = "Pollinator.species.ID", all = T)
colnames(all_2006_2_a)[4:5] <- c("Plant.species","Pollinator.species")

#remove underscore
all_2006_2_a$Plant.species <- gsub("_" , " ", fixed=TRUE, all_2006_2_a$Plant.species)
all_2006_2_a$Pollinator.species <- gsub("_" , " ", fixed=TRUE, all_2006_2_a$Pollinator.species)

#convert first element of the string to uppercase
all_2006_2_a$Plant.species <- firstup(all_2006_2_a$Plant.species)
#Convert to matrix
all_2006_2_b <- acast(all_2006_2_a, all_2006_2_a$Plant.species ~ all_2006_2_a$Pollinator.species , value.var='value', 
                      fun.aggregate=sum, margins=F)

#same remove plant species with no visits
rowSums(as.matrix(all_2006_2_b))
all_2006_2_c <- all_2006_2_b[rowSums(all_2006_2_b[, -1] > 0) != 0, ]
rowSums(as.matrix(all_2006_2_c))

#same remove poll species with no visits
colSums(as.matrix(all_2006_2_c))
all_2006_2_d <- all_2006_2_c[, colSums(all_2006_2_c != 0) > 0]
colSums(as.matrix(all_2006_2_d))

#save network
write.csv(all_2006_2_d,"Data/Data_processing/peralta_argentina_2020/peralta_2006_mendoza_site_2.csv")

###############################
#Site 3 2006
###############################
#load data
site_3_2006 <- read.csv("Data/Data_processing/peralta_argentina_2020/peralta_2006_site_3.csv", header=T, stringsAsFactors=F)
colnames(site_3_2006)[1] <- "Plant.species.ID"
#convert to long format
site_3_2006_long <- melt(site_3_2006, id.vars=c("Plant.species.ID"), variable.name="Pollinator.species.ID")

#merge with plant names
all_2006_3 <- merge(site_3_2006_long, plant_names, by = "Plant.species.ID", all = T)
all_2006_3_a <- merge(all_2006_3, insect_names, by = "Pollinator.species.ID", all = T)
colnames(all_2006_3_a)[4:5] <- c("Plant.species","Pollinator.species")

#remove underscore
all_2006_3_a$Plant.species <- gsub("_" , " ", fixed=TRUE, all_2006_3_a$Plant.species)
all_2006_3_a$Pollinator.species <- gsub("_" , " ", fixed=TRUE, all_2006_3_a$Pollinator.species)

#convert first element of the string to uppercase
all_2006_3_a$Plant.species <- firstup(all_2006_3_a$Plant.species)
#Convert to matrix
all_2006_3_b <- acast(all_2006_3_a, all_2006_3_a$Plant.species ~ all_2006_3_a$Pollinator.species , value.var='value', 
                      fun.aggregate=sum, margins=F)

#same remove plant species with no visits
rowSums(as.matrix(all_2006_3_b))
all_2006_3_c <- all_2006_3_b[rowSums(all_2006_3_b[, -1] > 0) != 0, ]
rowSums(as.matrix(all_2006_3_c))

#same remove poll species with no visits
colSums(as.matrix(all_2006_3_c))
all_2006_3_d <- all_2006_3_c[, colSums(all_2006_3_c != 0) > 0]
colSums(as.matrix(all_2006_3_d))

#save network
write.csv(all_2006_3_d,"Data/Data_processing/peralta_argentina_2020/peralta_2006_mendoza_site_3.csv")

###############################
#Site 4 2006
###############################
#load data
site_4_2006 <- read.csv("Data/Data_processing/peralta_argentina_2020/peralta_2006_site_4.csv", header=T, stringsAsFactors=F)
colnames(site_4_2006)[1] <- "Plant.species.ID"
#convert to long format
site_4_2006_long <- melt(site_4_2006, id.vars=c("Plant.species.ID"), variable.name="Pollinator.species.ID")

#merge with plant names
all_2006_4 <- merge(site_4_2006_long, plant_names, by = "Plant.species.ID", all = T)
all_2006_4_a <- merge(all_2006_4, insect_names, by = "Pollinator.species.ID", all = T)
colnames(all_2006_4_a)[4:5] <- c("Plant.species","Pollinator.species")

#remove underscore
all_2006_4_a$Plant.species <- gsub("_" , " ", fixed=TRUE, all_2006_4_a$Plant.species)
all_2006_4_a$Pollinator.species <- gsub("_" , " ", fixed=TRUE, all_2006_4_a$Pollinator.species)

#convert first element of the string to uppercase
all_2006_4_a$Plant.species <- firstup(all_2006_4_a$Plant.species)
#Convert to matrix
all_2006_4_b <- acast(all_2006_4_a, all_2006_4_a$Plant.species ~ all_2006_4_a$Pollinator.species , value.var='value', 
                      fun.aggregate=sum, margins=F)

#same remove plant species with no visits
rowSums(as.matrix(all_2006_4_b))
all_2006_4_c <- all_2006_4_b[rowSums(all_2006_4_b[, -1] > 0) != 0, ]
rowSums(as.matrix(all_2006_4_c))

#same remove poll species with no visits
colSums(as.matrix(all_2006_4_c))
all_2006_4_d <- all_2006_4_c[, colSums(all_2006_4_c != 0) > 0]
colSums(as.matrix(all_2006_4_d))

#save network
write.csv(all_2006_4_d,"Data/Data_processing/peralta_argentina_2020/peralta_2006_mendoza_site_4.csv")


################################
#Metaweb 2006
###############################

metaweb_long <- rbind(all_2006_1_a, all_2006_2_a, all_2006_3_a, all_2006_4_a)
#Convert to matrix
metaweb_net <- acast(metaweb_long, metaweb_long$Plant.species ~ metaweb_long$Pollinator.species , value.var='value', 
                      fun.aggregate=sum, margins=F)

#same remove plant species with no visits
rowSums(as.matrix(metaweb_net))
metaweb_net_a <- metaweb_net[rowSums(metaweb_net[, -1] > 0) != 0, ]
rowSums(as.matrix(metaweb_net_a))

#same remove poll species with no visits
colSums(as.matrix(metaweb_net_a))
metaweb_net_b <- metaweb_net_a[, colSums(metaweb_net_a != 0) > 0]
colSums(as.matrix(metaweb_net_b))

#save network
write.csv(metaweb_net_b,"Data/Data_processing/peralta_argentina_2020/peralta_2006_mendoza_metaweb.csv")


###############################
#Site 1 2007
###############################


#load data
site_1_2007 <- read.csv("Data/Data_processing/peralta_argentina_2020/peralta_2007.csv", header=T, stringsAsFactors=F)
colnames(site_1_2007)[1] <- "Plant.species.ID"
#convert to long format
site_1_2007_long <- melt(site_1_2007, id.vars=c("Plant.species.ID"), variable.name="Pollinator.species.ID")

#merge with plant names
all_2007_1 <- merge(site_1_2007_long, plant_names, by = "Plant.species.ID", all = T)
all_2007_1_a <- merge(all_2007_1, insect_names, by = "Pollinator.species.ID", all = T)
colnames(all_2007_1_a)[4:5] <- c("Plant.species","Pollinator.species")

#remove underscore
all_2007_1_a$Plant.species <- gsub("_" , " ", fixed=TRUE, all_2007_1_a$Plant.species)
all_2007_1_a$Pollinator.species <- gsub("_" , " ", fixed=TRUE, all_2007_1_a$Pollinator.species)

#convert first element of the string to uppercase
all_2007_1_a$Plant.species <- firstup(all_2007_1_a$Plant.species)
#Convert to matrix
all_2007_1_b <- acast(all_2007_1_a, all_2007_1_a$Plant.species ~ all_2007_1_a$Pollinator.species , value.var='value', 
                      fun.aggregate=sum, margins=F)

#same remove plant species with no visits
rowSums(as.matrix(all_2007_1_b))
all_2007_1_c <- all_2007_1_b[rowSums(all_2007_1_b[, -1] > 0) != 0, ]
rowSums(as.matrix(all_2007_1_c))

#same remove poll species with no visits
colSums(as.matrix(all_2007_1_c))
all_2007_1_d <- all_2007_1_c[, colSums(all_2007_1_c != 0) > 0]
colSums(as.matrix(all_2007_1_d))

#save network
write.csv(all_2007_1_d,"Data/Data_processing/peralta_argentina_2020/peralta_2007_mendoza.csv")


###############################
#Site 1 2008
###############################


#load data
site_1_2008 <- read.csv("Data/Data_processing/peralta_argentina_2020/peralta_2008.csv", header=T, stringsAsFactors=F)
colnames(site_1_2008)[1] <- "Plant.species.ID"
#convert to long format
site_1_2008_long <- melt(site_1_2008, id.vars=c("Plant.species.ID"), variable.name="Pollinator.species.ID")

#merge with plant names
all_2008_1 <- merge(site_1_2008_long, plant_names, by = "Plant.species.ID", all = T)
all_2008_1_a <- merge(all_2008_1, insect_names, by = "Pollinator.species.ID", all = T)
colnames(all_2008_1_a)[4:5] <- c("Plant.species","Pollinator.species")

#remove underscore
all_2008_1_a$Plant.species <- gsub("_" , " ", fixed=TRUE, all_2008_1_a$Plant.species)
all_2008_1_a$Pollinator.species <- gsub("_" , " ", fixed=TRUE, all_2008_1_a$Pollinator.species)

#convert first element of the string to uppercase
all_2008_1_a$Plant.species <- firstup(all_2008_1_a$Plant.species)
#Convert to matrix
all_2008_1_b <- acast(all_2008_1_a, all_2008_1_a$Plant.species ~ all_2008_1_a$Pollinator.species , value.var='value', 
                      fun.aggregate=sum, margins=F)

#same remove plant species with no visits
rowSums(as.matrix(all_2008_1_b))
all_2008_1_c <- all_2008_1_b[rowSums(all_2008_1_b[, -1] > 0) != 0, ]
rowSums(as.matrix(all_2008_1_c))

#same remove poll species with no visits
colSums(as.matrix(all_2008_1_c))
all_2008_1_d <- all_2008_1_c[, colSums(all_2008_1_c != 0) > 0]
colSums(as.matrix(all_2008_1_d))

#save network
write.csv(all_2008_1_d,"Data/Data_processing/peralta_argentina_2020/peralta_2008_mendoza.csv")



###############################
#Site 1 2009
###############################


#load data
site_1_2009 <- read.csv("Data/Data_processing/peralta_argentina_2020/peralta_2009.csv", header=T, stringsAsFactors=F)
colnames(site_1_2009)[1] <- "Plant.species.ID"
#convert to long format
site_1_2009_long <- melt(site_1_2009, id.vars=c("Plant.species.ID"), variable.name="Pollinator.species.ID")

#merge with plant names
all_2009_1 <- merge(site_1_2009_long, plant_names, by = "Plant.species.ID", all = T)
all_2009_1_a <- merge(all_2009_1, insect_names, by = "Pollinator.species.ID", all = T)
colnames(all_2009_1_a)[4:5] <- c("Plant.species","Pollinator.species")

#remove underscore
all_2009_1_a$Plant.species <- gsub("_" , " ", fixed=TRUE, all_2009_1_a$Plant.species)
all_2009_1_a$Pollinator.species <- gsub("_" , " ", fixed=TRUE, all_2009_1_a$Pollinator.species)

#convert first element of the string to uppercase
all_2009_1_a$Plant.species <- firstup(all_2009_1_a$Plant.species)
#Convert to matrix
all_2009_1_b <- acast(all_2009_1_a, all_2009_1_a$Plant.species ~ all_2009_1_a$Pollinator.species , value.var='value', 
                      fun.aggregate=sum, margins=F)

#same remove plant species with no visits
rowSums(as.matrix(all_2009_1_b))
all_2009_1_c <- all_2009_1_b[rowSums(all_2009_1_b[, -1] > 0) != 0, ]
rowSums(as.matrix(all_2009_1_c))

#same remove poll species with no visits
colSums(as.matrix(all_2009_1_c))
all_2009_1_d <- all_2009_1_c[, colSums(all_2009_1_c != 0) > 0]
colSums(as.matrix(all_2009_1_d))

#save network
write.csv(all_2009_1_d,"Data/Data_processing/peralta_argentina_2020/peralta_2009_mendoza.csv")


###############################
#Site 1 2010
###############################


#load data
site_1_2010 <- read.csv("Data/Data_processing/peralta_argentina_2020/peralta_2010.csv", header=T, stringsAsFactors=F)
colnames(site_1_2010)[1] <- "Plant.species.ID"
#convert to long format
site_1_2010_long <- melt(site_1_2010, id.vars=c("Plant.species.ID"), variable.name="Pollinator.species.ID")

#merge with plant names
all_2010_1 <- merge(site_1_2010_long, plant_names, by = "Plant.species.ID", all = T)
all_2010_1_a <- merge(all_2010_1, insect_names, by = "Pollinator.species.ID", all = T)
colnames(all_2010_1_a)[4:5] <- c("Plant.species","Pollinator.species")

#remove underscore
all_2010_1_a$Plant.species <- gsub("_" , " ", fixed=TRUE, all_2010_1_a$Plant.species)
all_2010_1_a$Pollinator.species <- gsub("_" , " ", fixed=TRUE, all_2010_1_a$Pollinator.species)

#convert first element of the string to uppercase
all_2010_1_a$Plant.species <- firstup(all_2010_1_a$Plant.species)
#Convert to matrix
all_2010_1_b <- acast(all_2010_1_a, all_2010_1_a$Plant.species ~ all_2010_1_a$Pollinator.species , value.var='value', 
                      fun.aggregate=sum, margins=F)

#same remove plant species with no visits
rowSums(as.matrix(all_2010_1_b))
all_2010_1_c <- all_2010_1_b[rowSums(all_2010_1_b[, -1] > 0) != 0, ]
rowSums(as.matrix(all_2010_1_c))

#same remove poll species with no visits
colSums(as.matrix(all_2010_1_c))
all_2010_1_d <- all_2010_1_c[, colSums(all_2010_1_c != 0) > 0]
colSums(as.matrix(all_2010_1_d))

#save network
write.csv(all_2010_1_d,"Data/Data_processing/peralta_argentina_2020/peralta_2010_mendoza.csv")

###############################
#Site 1 2011
###############################


#load data
site_1_2011 <- read.csv("Data/Data_processing/peralta_argentina_2020/peralta_2011.csv", header=T, stringsAsFactors=F)
colnames(site_1_2011)[1] <- "Plant.species.ID"
#convert to long format
site_1_2011_long <- melt(site_1_2011, id.vars=c("Plant.species.ID"), variable.name="Pollinator.species.ID")

#merge with plant names
all_2011_1 <- merge(site_1_2011_long, plant_names, by = "Plant.species.ID", all = T)
all_2011_1_a <- merge(all_2011_1, insect_names, by = "Pollinator.species.ID", all = T)
colnames(all_2011_1_a)[4:5] <- c("Plant.species","Pollinator.species")

#remove underscore
all_2011_1_a$Plant.species <- gsub("_" , " ", fixed=TRUE, all_2011_1_a$Plant.species)
all_2011_1_a$Pollinator.species <- gsub("_" , " ", fixed=TRUE, all_2011_1_a$Pollinator.species)

#convert first element of the string to uppercase
all_2011_1_a$Plant.species <- firstup(all_2011_1_a$Plant.species)
#Convert to matrix
all_2011_1_b <- acast(all_2011_1_a, all_2011_1_a$Plant.species ~ all_2011_1_a$Pollinator.species , value.var='value', 
                      fun.aggregate=sum, margins=F)

#same remove plant species with no visits
rowSums(as.matrix(all_2011_1_b))
all_2011_1_c <- all_2011_1_b[rowSums(all_2011_1_b[, -1] > 0) != 0, ]
rowSums(as.matrix(all_2011_1_c))

#same remove poll species with no visits
colSums(as.matrix(all_2011_1_c))
all_2011_1_d <- all_2011_1_c[, colSums(all_2011_1_c != 0) > 0]
colSums(as.matrix(all_2011_1_d))

#save network
write.csv(all_2011_1_d,"Data/Data_processing/peralta_argentina_2020/peralta_2011_mendoza.csv")
