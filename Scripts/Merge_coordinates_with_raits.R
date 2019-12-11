#Step 2
#Read data generated in "Spp_network_id"

#I'm going to merge here the trait data and the coordinates (project geonet Saunders et al 2019)

#load data
traits <- read.csv("data/Output_processing_data/all_dat_id_unique.csv",na="")
coordinates_and_id <- read.csv("data/Process_data/coordinates_idref.csv")

head(traits)
head(coordinates_and_id)



#To merge first I have to clean the data


#decide what is best if modify mine or use the way of geonet