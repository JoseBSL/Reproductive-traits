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
setwd("~/Reproductive Traits/")
data <- read_excel("data/Process_data/traits_2019_2.xlsx")

#checking data structure
#str(data)
#I convert to data.frame
data <- as.data.frame(data)
#str(data)

#setwd to read the csv files
setwd("~/Reproductive Traits/data/Data_networks")
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
write.csv(data_all_id, "data/Output_processing_data/all_dat_id_unique.csv",na="")








