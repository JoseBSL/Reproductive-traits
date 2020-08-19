# Processing data from lundgren 2004
# DOI: https://doi.org/10.1657/1523-0430(2005)037[0514:TDAHCW]2.0.CO;2
# Greenland
# IMPORTANT NOTE: 1 season 2002


lundgren <- read.csv("Data/Data_processing/lundgren_greenland_2005/lundgren_2005.csv", row.names = 1)

#plant species
levels(as.factor(row.names(lundgren)))


#insect species
levels(as.factor(colnames(lundgren)))
#remove dots
colnames(lundgren) <- gsub(".", " ", colnames(lundgren), fixed=TRUE)
colnames(lundgren) <- gsub(" M_PL_045", "", colnames(lundgren), fixed=TRUE)
colnames(lundgren) <- gsub("sp", "sp.", colnames(lundgren), fixed=TRUE)

#seems ok

#save data
write.csv(lundgren, "Data/Data_processing/lundgren_greenland_2005/lundgren_2005_greenland.csv")
