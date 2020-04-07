#Metadata of the networks

#load package to extract references from bib.file
#install.packages("bib2df")

library(bib2df)
library(reshape2)

#Read metadata generated in networks preparation
metadata <- read.csv("Data/Data_processing/Data_networks_processing/metadata_process.csv")
#Read bibtex file with references
df <- bib2df("scripts/references.bib")
#Select column of interest
df <- df[,-c(3,4,6:12,14:16,20,22,24,28:30)] 

df$Id_number <- seq.int(nrow(df))
#Merge two dataframes
d <- merge(df, metadata, by="BIBTEXKEY")
str(d)
d <- apply(d,2,as.character)
#select columns of interest
d_new <- d[,c(1,14,16:18,21:28)]
write.csv(d_new, "Data/Data_processing/metadata.csv")


