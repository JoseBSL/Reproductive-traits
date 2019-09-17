#Here I'm going to read all the networks, create a list of spp
#and associate each spp to a network id

#load library

library(dplyr)


#To read all the csv's I setwd on the specific folder
#Then
setwd("Data/Data_networks")
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])))

my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, read.csv)

data_id_list <- lapply(seq_along(my_data), 
       function(x) cbind(my_data[[x]], unique.id=temp[i]))

my_names = c("X", "unique.id")
result = lapply(data_id_list, "[",  my_names)

all_dat_id <- bind_rows(result, .id = "column_label")


all_dat_id_unique <- unique(all_dat_id[c("X","unique.id")])
