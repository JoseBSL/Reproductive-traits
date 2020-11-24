# Processing data from inouye 1988
# DOI: https://doi.org/10.1111/j.1442-9993.1988.tb00968.x
# Australia
# 1983-1984


#LOAD DATA
inouye <- read.csv("Data/Data_processing/inouye_australia_1988/inouye_1988.csv", row.names = 1)


#check levels
#plant species
levels(as.factor(row.names(inouye)))
#looks good


#insect species
levels(as.factor(colnames(inouye)))
colnames(inouye) <- gsub(".", " ", colnames(inouye), fixed=TRUE)
colnames(inouye) <- gsub(" M_PL_019", "", colnames(inouye), fixed=TRUE)
colnames(inouye) <- gsub("sp", "sp.", colnames(inouye), fixed=TRUE)
#looks ok

write.csv(inouye, "Data/Data_processing/inouye_australia_1988/inouye_1988_australia.csv")
