# Processing data from SOUZA 2017
# DOI: 10.1111/1365-2745.12978
# Chaco, Brazil
# IMPORTANT NOTE: THE SITES WERE SAMPLED BETWEEN 2009-2010
# I'm going to prepare here the unique networks per site and year and the metaweb
# NOTE:
# There are many networks available but they have many plant species
# We have selected just one network

#Load data
chaco <- read.csv("Data/Data_processing/souza_brazil_2017/souza_2017_brazil_chaco_data.csv", row.names = 1)


#I need to format the rownames without underscore
#gsub will do the job
rownames(chaco) <- gsub("_", " ", rownames(chaco))
rownames(chaco) <- gsub("cf. ", "", rownames(chaco))

#Also with colnames
colnames(chaco) <- gsub("_", " ", colnames(chaco))

#save data
write.csv(chaco, "Data/Data_processing/souza_brazil_2017/souza_2017_brazil_chaco.csv")


