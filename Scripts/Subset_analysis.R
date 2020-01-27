#Here I will load just the networks that I have fill almost all the
#the plant traits in order to work with a subset

#load libraries

library(tidyr)
library(reshape2)
#Now I create a folder with the networks that I have used


#This is the subset of networks so far, I will keep adding... 
#id <- c("bartomeus_2008_bat1ca.csv", "beck_2006.csv", "bundgaard_2003.csv", 
        #"chacoff_2011.csv", "dicks_2002_1.csv", "dupont_2009_denmark.csv",
        #"elberling_1999.csv", "elberling_unpublished_data.csv","fang_huang_2008.csv",
        #"Inoue_1990.csv", "inouye_1988.csv", "kaiser_bunbury_2010_1.csv",
        #"kato_2000.csv", "kevan_1970.csv", "lundgren_2005.csv", "Mauritius_valerie_unpublished_data.csv",
        #"mcmullen_1993.csv")


setwd("~/Reproductive-Traits/Data/Data_networks_subset")
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

library(reshape2)
melt(bartomeus_2008_bat1ca.csv)

