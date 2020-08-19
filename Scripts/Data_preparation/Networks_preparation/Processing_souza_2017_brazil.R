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
#Some specific editing of species names
levels(as.factor(colnames(chaco)))
colnames(chaco)[colnames(chaco)=="Ceratina .Crewella. morrensis"] <- "Ceratina morrensis"
colnames(chaco)[colnames(chaco)=="Ceratina .Rhysoceratina. próx. volintans"] <- "Ceratina volintans"
colnames(chaco)[colnames(chaco)=="Glutophrissa drusilla drusilla"] <- "Glutophrissa drusilla"
colnames(chaco)[colnames(chaco)=="Anthanassa frisia herma"] <- "Anthanassa frisia"
colnames(chaco)[colnames(chaco)=="Agraulis vanillae maculosa"] <- "Agraulis vanillae"
colnames(chaco)[colnames(chaco)=="Larocanthidium próx. nigrilum"] <- "Larocanthidium nigrilum"
colnames(chaco)[colnames(chaco)=="Megachile .latu sensu. sp.4"] <- "Megachile sp.4"
colnames(chaco)[colnames(chaco)=="Megachile .sensu latu. sp.1"] <- "Megachile sp.1"
colnames(chaco)[colnames(chaco)=="Megachile .sensu latu. sp.2"] <- "Megachile sp.2"
colnames(chaco)[colnames(chaco)=="Megachile .sensu latu. sp.3"] <- "Megachile sp.3"
colnames(chaco)[colnames(chaco)=="Xylocopa aff. suspecta"] <- "Xylocopa suspecta"
colnames(chaco) <- gsub("  ", "", colnames(chaco))
#seems ok now

#save data
write.csv(chaco, "Data/Data_processing/souza_brazil_2017/souza_2017_brazil_chaco.csv")


