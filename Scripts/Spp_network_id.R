#Here I'm going to read all the networks, create a list of spp
#and associate each spp to a network id

#load libraries
library(dplyr)
library(readxl)
library(tidyverse)


#To read all the csv's I setwd on the specific folder
#Then

#read data
data <- read_excel("Data/bionet_last.xls")
#str(data)
data <- as.data.frame(data)
#str(data)

setwd("Data/Data_networks")
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])))

my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, read.csv)

data_id_list <- lapply(seq_along(my_data), 
       function(x) cbind(my_data[[x]], unique.id=my_files[x]))

my_names = c("X", "unique.id")
result = lapply(data_id_list, "[",  my_names)
all_dat_id <- bind_rows(result, .id = "column_label")
all_dat_id_unique <- all_dat_id[!duplicated(all_dat_id[2]),]
nrow(all_dat_id_unique)

head(all_dat_id_unique)
colnames(all_dat_id_unique)[2] <- "species_geonet"
head(data)

# Does not work quite well, I lost rows
data_all_id <-  merge(all_dat_id_unique, data, by="species_geonet" )
nrow(data_all_id)
nrow(data)

b<- merge(all_dat_id_unique, data)
nrow(b)
#Let's try other example
#Going back to original Working Directory
setwd("~/Reproductive Traits")
data_all_id_1 <- full_join(all_dat_id_unique, data, by = "species_geonet") 
nrow(data_all_id_1)
write.csv(data_all_id_1, "data/data_all_id_1.csv")

