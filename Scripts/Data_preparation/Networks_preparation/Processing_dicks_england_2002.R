# Processing data from dicks 2002 england
# DOI: https://doi.org/10.1046/j.0021-8790.2001.00572.x
# ENGLAND
# IMPORTANT NOTE: THE SITES WERE SAMPLED 1 SEASON YEAR?
# I'm going to prepare here the unique networks per site and year and the metaweb

#LOAD DATA 

######
#SITE 1
######


site_1 <-read.csv("Data/Data_processing/dicks_england_2002/dicks_2002_1.csv", row.names = 1)
#check levels of both plant and insect species and fix if needed.

#plant species
levels(as.factor(rownames(site_1)))
#looks ok
#insect species
levels(as.factor(colnames(site_1)))
#ok lets do a bit of cleaning
#REMOVE .M_PL_006
colnames(site_1) <- gsub(".M_PL_006", "", colnames(site_1))
#leave just pollinator species names on id
colnames(site_1) <- sub("^(\\S*\\s+\\S+).*", "\\1", colnames(site_1))
#Change dot for space
colnames(site_1) <- gsub(".", " ", colnames(site_1), fixed=TRUE)
#change sp1 for sp.
colnames(site_1) <- gsub("1", ".", colnames(site_1), fixed=TRUE)

#Save network 
write.csv(site_1, "Data/Data_processing/dicks_england_2002/dicks_2002_england_shelfanger.csv")


######
#SITE 2
######


site_2 <-read.csv("Data/Data_processing/dicks_england_2002/dicks_2002_2.csv", row.names = 1)
#check levels of both plant and insect species and fix if needed.

#plant species
levels(as.factor(rownames(site_2)))
#looks ok
#insect species
levels(as.factor(colnames(site_2)))
#ok lets do a bit of cleaning
#REMOVE .M_PL_006
colnames(site_2) <- gsub(".M_PL_007", "", colnames(site_2))
#leave just pollinator species names on id
colnames(site_2) <- sub("^(\\S*\\s+\\S+).*", "\\1", colnames(site_2))
#Change dot for space
colnames(site_2) <- gsub(".", " ", colnames(site_2), fixed=TRUE)
#change sp1 for sp.
colnames(site_2) <- gsub("1", ".", colnames(site_2), fixed=TRUE)

#Save network 
write.csv(site_2, "Data/Data_processing/dicks_england_2002/dicks_2002_england_hicklingbroad.csv")
 