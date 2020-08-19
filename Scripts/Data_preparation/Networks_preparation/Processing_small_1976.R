# Processing data from small 1976
# DOI: no doi bu here 
# small 1973
# IMPORTANT NOTE: 1 season 


#LOAD DATA

small <- read.csv("Data/Data_processing/small_canada_1976/small_1976.csv", row.names = 1)


#check levels
#plant species
levels(as.factor(row.names(small)))

#insect species
levels(as.factor(colnames(small)))
colnames(small) <- gsub(".", " ", colnames(small), fixed=TRUE)
colnames(small) <- gsub(" M_PL_033", "", colnames(small), fixed=TRUE)
colnames(small) <- gsub("sp", "sp.", colnames(small), fixed=TRUE)


#save data
write.csv(small, "Data/Data_processing/small_canada_1976/small_1976_canada.csv")
