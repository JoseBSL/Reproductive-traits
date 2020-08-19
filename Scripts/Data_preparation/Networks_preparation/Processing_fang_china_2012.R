# Processing data from fang 2012
# DOI: :10.1371/journal.pone.0032663
# CHINA
# IMPORTANT NOTE: 2007,2008,2009,2010
# I'm going to prepare here the unique networks per site and year and the metaweb


#LOAD DATA

##########
#year 2008
##########

fang_2008 <- read.csv("Data/Data_processing/fang_china_2012/fang_huang_2008.csv",row.names = 1)

#plant species
levels(as.factor(rownames(fang_2008)))
#removing one extra word in one sp.
rownames(fang_2008) <- sub("^(\\S*\\s+\\S+).*", "\\1", rownames(fang_2008))
#removing double spaces, just in case, I saw one in the 2009 file
rownames(fang_2008) <- gsub("  ", " ", rownames(fang_2008), fixed=TRUE)

levels(as.factor(rownames(fang_2008)))
#seems ok now

#insect species
levels(as.factor(colnames(fang_2008)))
colnames(fang_2008) <- gsub(".", " ", colnames(fang_2008), fixed=TRUE)
colnames(fang_2008) <- gsub("  ", " ", colnames(fang_2008), fixed=TRUE)
colnames(fang_2008) <- gsub("sp", "sp.", colnames(fang_2008), fixed=TRUE)
colnames(fang_2008) <- gsub("sp. ", "sp.", colnames(fang_2008), fixed=TRUE)
colnames(fang_2008) <- sub("^(\\S*\\s+\\S+).*", "\\1", colnames(fang_2008))
colnames(fang_2008) <- gsub("Bombyliidae", "Bombyliidae sp.1", colnames(fang_2008), fixed=TRUE)
colnames(fang_2008) <- gsub("Bombyliidae sp.1 1", "Bombyliidae sp.2", colnames(fang_2008), fixed=TRUE)


#save network
write.csv(fang_2008, "Data/Data_processing/fang_china_2012/fang_2008_china.csv")


##########
#year 2009
##########


fang_2009 <- read.csv("Data/Data_processing/fang_china_2012/fang_huang_2009.csv",row.names = 1)

#plant species
levels(as.factor(rownames(fang_2009)))
#removing some double spaces
rownames(fang_2009) <- gsub("  ", " ", rownames(fang_2009), fixed=TRUE)

#insect species
levels(as.factor(colnames(fang_2009)))
colnames(fang_2009) <- gsub(".", " ", colnames(fang_2009), fixed=TRUE)
colnames(fang_2009) <- gsub("sp", "sp.", colnames(fang_2009), fixed=TRUE)
colnames(fang_2009) <- gsub("sp. ", "sp.", colnames(fang_2009), fixed=TRUE)
colnames(fang_2009) <- gsub("sp. ", "sp.", colnames(fang_2009), fixed=TRUE)
colnames(fang_2009) <- gsub("Lycaenidae Celastrina", "Celastrina", colnames(fang_2009), fixed=TRUE)
colnames(fang_2009) <- gsub("Bombyliidae", "Bombyliidae sp.", colnames(fang_2009), fixed=TRUE)
colnames(fang_2009) <- gsub("Bombyliidae sp. ", "Bombyliidae sp.", colnames(fang_2009), fixed=TRUE)
colnames(fang_2009) <- gsub("Bombyliidae sp.$", "Bombyliidae sp.4", colnames(fang_2009))
colnames(fang_2009) <- gsub("  ", " ", colnames(fang_2009), fixed=TRUE)
colnames(fang_2009) <- gsub("  ", " ", colnames(fang_2009), fixed=TRUE)
#now seems ok

#save network
write.csv(fang_2009, "Data/Data_processing/fang_china_2012/fang_2009_china.csv")

##########
#year 2010
##########

fang_2010 <- read.csv("Data/Data_processing/fang_china_2012/fang_huang_2010.csv",row.names = 1)

#plant species
levels(as.factor(rownames(fang_2010)))
rownames(fang_2010) <- sub("^(\\S*\\s+\\S+).*", "\\1", rownames(fang_2010))
rownames(fang_2010) <- gsub("  ", " ", rownames(fang_2010), fixed=TRUE)
#seems ok

#insect species
levels(as.factor(colnames(fang_2010)))
colnames(fang_2010) <- gsub(".", " ", colnames(fang_2010), fixed=TRUE)
colnames(fang_2010) <- gsub("sp", "sp.", colnames(fang_2010), fixed=TRUE)
colnames(fang_2010) <- gsub("sp. ", "sp.", colnames(fang_2010), fixed=TRUE)
colnames(fang_2010) <- gsub("Lycaenidae Celastrina", "Celastrina", colnames(fang_2010), fixed=TRUE)
colnames(fang_2010)[colnames(fang_2010)=="Bombyliidae"] <- "Bombyliidae sp.4"
colnames(fang_2010)[colnames(fang_2010)=="Bombyliidae 1"] <- "Bombyliidae sp.5"
colnames(fang_2010)[colnames(fang_2010)=="Bombyliidae sp."] <- "Bombyliidae sp.6"
colnames(fang_2010) <- gsub("  ", " ", colnames(fang_2010), fixed=TRUE)
colnames(fang_2010) <- gsub("  ", " ", colnames(fang_2010), fixed=TRUE)
#now seems ok

#save network
write.csv(fang_2010, "Data/Data_processing/fang_china_2012/fang_2010_china.csv")
