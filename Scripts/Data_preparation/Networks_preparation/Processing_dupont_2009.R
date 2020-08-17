# Processing data from Dupont 2009
# DOI: https://doi.org/10.1111/j.1365-2656.2008.01501.x
# Denmark
# IMPORTANT NOTE: THE SITES WERE SAMPLED 1 SEASON BETWEEN 2004 AND 2005
# I'm going to prepare here the unique networks per site and year and the metaweb

#load library
library(reshape2)


##################
#Site IB
#################

#now read file
dupont_2009_ib<- read.csv("Data/Data_processing/Dupont_&_Olesen_2009/dupont_2009_ib.csv")
dupont_2009_ib$Species <- sub("^(\\S*\\s+\\S+).*", "\\1", dupont_2009_ib$Species)
colnames(dupont_2009_ib)[1] <- "Plants"
colnames(dupont_2009_ib)[4] <- "Insects"
colnames(dupont_2009_ib)[6] <- "Visits"

#convert to matrix format
dupont_2009_ib_a <- acast(dupont_2009_ib, Plants ~ Insects , value.var='Visits', fun.aggregate=sum, margins=F)
dupont_2009_ib_a[is.na(dupont_2009_ib_a)] <- 0

#save data
write.csv(dupont_2009_ib_a, "Data/Data_processing/Dupont_&_Olesen_2009/dupont_2009_denmark_ib.csv")


##################
#Site SO
#################

#now read file
dupont_2009_so<- read.csv("Data/Data_processing/Dupont_&_Olesen_2009/dupont_2009_so.csv")
dupont_2009_so$Species <- sub("^(\\S*\\s+\\S+).*", "\\1", dupont_2009_so$Species)
colnames(dupont_2009_so)[1] <- "Plants"
colnames(dupont_2009_so)[4] <- "Insects"
colnames(dupont_2009_so)[6] <- "Visits"


#convert to matrix format
dupont_2009_so_a <- acast(dupont_2009_so, Plants ~ Insects , value.var='Visits', fun.aggregate=sum, margins=F)
dupont_2009_so_a[is.na(dupont_2009_so_a)] <- 0

#save data
write.csv(dupont_2009_so_a, "Data/Data_processing/Dupont_&_Olesen_2009/dupont_2009_denmark_so.csv")

##################
#Site HL
##################

#now read file
dupont_2009_hl<- read.csv("Data/Data_processing/Dupont_&_Olesen_2009/dupont_2009_hl.csv")
dupont_2009_hl$Insect.species <- sub("^(\\S*\\s+\\S+).*", "\\1", dupont_2009_hl$Insect.species)
colnames(dupont_2009_hl)[1] <- "Plants"
colnames(dupont_2009_hl)[4] <- "Insects"
colnames(dupont_2009_hl)[6] <- "Visits"

#convert to matrix format
dupont_2009_hl_a <- acast(dupont_2009_hl, Plants ~ Insects , value.var='Visits', fun.aggregate=sum, margins=F)
dupont_2009_hl_a[is.na(dupont_2009_hl_a)] <- 0

#save data
write.csv(dupont_2009_hl_a, "Data/Data_processing/Dupont_&_Olesen_2009/dupont_2009_denmark_hl.csv")



##################
#Metaweb
##################

metaweb <- rbind(dupont_2009_ib, dupont_2009_so, dupont_2009_hl)
metaweb_a <- acast(metaweb, Plants ~ Insects , value.var='Visits', 
                                fun.aggregate=sum, margins=F)

write.csv(metaweb_a, "Data/Data_processing/Dupont_&_Olesen_2009/dupont_2009_denmark_metaweb.csv")
