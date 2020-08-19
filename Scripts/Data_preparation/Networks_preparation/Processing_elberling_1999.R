# Processing data from elberling 1999
# DOI: https://doi.org/10.1111/j.1600-0587.1999.tb00507.x
# Sweeden
# IMPORTANT NOTE: THE SITES WERE SAMPLED 1 SEASON IN 1994


#LOAD DATA
elberling <- read.csv("Data/Data_processing/elberling_sweeden_1999/elberling_1999.csv", row.names = 1)

#plant species
levels(as.factor(row.names(elberling)))

#insect species
levels(as.factor(colnames(elberling)))
colnames(elberling) <- gsub(".", " ", colnames(elberling), fixed=TRUE)
colnames(elberling)[colnames(elberling)=="Limnophyes schnelli 1"] <- "Limnophyes schnelli sp1"
colnames(elberling) <- gsub("sp", "sp.", colnames(elberling), fixed=TRUE)
colnames(elberling) <- gsub("  ", " ", colnames(elberling), fixed=TRUE)

#write.csv
write.csv(elberling, "Data/Data_processing/elberling_sweeden_1999/elberling_sweeden_1999.csv")
