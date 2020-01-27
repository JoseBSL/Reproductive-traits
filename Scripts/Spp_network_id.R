#Processing data


#Step 1 Combine dataframe with traits and networks

#Here I'm going to read all the networks, create a list of spp
#and associate each spp to a network id

#load libraries
library(dplyr)
library(readxl)
library(tidyverse)


#To read all the csv's I setwd on the specific folder

#load data
setwd("~/Reproductive-Traits/")
data <- read_excel("data/Process_data/traits_2019_3.xlsx")

data <- read_excel("Data/Traits_Data.xlsx")

#checking data structure
#str(data)
#I convert to data.frame
data <- as.data.frame(data)
#str(data)

#Fixing some species for merging the two dataframes
#I corrected them earlier...
#So now not all match... I have to keep the typos of the networks to merge
#Fixing that here
data[26:29,6]<- "Achillea alpina "
data[417,6]<- "Bertiera zaluzania "
data[487,6]<- "Buddleia davidii"
data[360,6]<- "Asteraceae sp. 2"
data[361,6]<- "Asteraceae sp. 4"
data[362,6]<- "Asteraceae sp. 5"
data[494,6]<- "Bursera simaruba "
data[542,6]<- "Caltha palustris "
data[555,6]<- "Campanula punctata "
data[678,6]<- "Chaetanthera apiculata "
data[823,6]<- "Coffea arabica "
data[948,6]<- "Cypripedium guttatum "
data[980,6]<- "Dendropanax arboreus "
data[1045,6]<- "Dodonaea viscosa "
data[1119,6]<- "Epilobium nivale "
data[1361,6]<- "Geranium eriostemon "
data[1378,6]<- "Geum rossii "
data[1584,6]<- "Hypochoeris montana "
data[1666,6]<- "Jaborosa laciniata "
data[1778,6]<- "Leuceria candidissima "
data[1833,6]<- "Loasa incurva "
data[1844,6]<- "Lonicera alpigena "
data[1952,6]<- "Menonvillea hookeri "
data[2020,6]<- "Montiopsis gilliesii "
data[2054,6]<- "Myrcia_bella"
data[2128,6]<- "Opuntia sp1 M_PL_026"
data[2129,6]<- "Opuntia sp2 M_PL_026"
data[2119,6]<- "Opuntia echios"
data[2169,6]<- "Oxybaphus_obatus"
data[2172,6]<- "Oxytropis arctica "
data[2182,6]<- "Palicourea crocea "
data[2218,6]<- "Pavonia_sidifolia"
data[2235,6]<- "Pedicularis resupinata "
data[2272,6]<- "Petasites frigidus "
data[2324,6]<- "Picris  hieracioides"
data[2353,6]<- "Pleurostylia leucocarpa "
data[2432,6]<- "Potentilla vahliana "
data[2439,6]<- "Praxelis_clematidea"
data[2440,6]<- "Praxelis_kleinioides"
data[2460,6]<- "Proustia_cuneifolia"
data[2560,6]<- "Rhabdocaulon_stenodontum"
data[2617,6]<- "Roussea simplex "
data[2634,6]<- "Rubus pungens "
data[2815,6]<- "Senecio looseri "
data[2829,6]<- "Senecio tricephalus "
data[2984,6]<- "Sphagneticola_sp"
data[3088,6]<- "Tarasa humilis "
data[3188,6]<- "Trifolium repens Linn. "
data[3305,6]<- "Viburnum opulus "
data[3306,6]<- "Viburnum opulus "
data[1826,6]<- "Lippia_turbinata"



#setwd to read the csv files
setwd("~/Reproductive-Traits/Data/Data_networks")
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])))

my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, read.csv)

data_id_list <- lapply(seq_along(my_data), 
       function(x) cbind(my_data[[x]], unique.id=my_files[x]))

my_names = c("X", "unique.id")
result = lapply(data_id_list, "[",  my_names)
all_dat_id <- bind_rows(result, .id = "column_label")
nrow(all_dat_id)
#all species 4902
all_dat_id_unique <- all_dat_id[!duplicated(all_dat_id[2]),]
nrow(all_dat_id_unique)
#3142
head(all_dat_id_unique)

#rename column to merge
colnames(all_dat_id_unique)[2] <- "species_geonet"
head(data)

# merege species from the netwroks and my data with traits
# still working on the data
# but creating workflow now

data_all_id <-  merge(all_dat_id_unique, data, by="species_geonet",all = TRUE)
nrow(data_all_id)
#3344
nrow(data)
#3822
nrow(all_dat_id_unique)
#3142

# setwd to write csv on data folder
setwd("~/Reproductive Traits/")
write.csv(data_all_id, "data/Output_processing_data/all_dat_id_unique_1.csv",na="")








