# DATA PREPARATION FOR LONG FORMAT


#load libraries

library(tidyr)
library(reshape2)
library(dplyr)
library(plyr)


#Set working directory to read files
setwd("~/Reproductive Traits/data/Data_networks_subset") 
#It is call subset because I do not use all the networks
#In total between 20 and 30, still adding... Do not know exact number yet

#Workflow found on stackoverflow to read all the files in a list
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, read.csv)
#Add id to the list to the long format data
data_id_list <- lapply(seq_along(my_data), 
                       function(x) cbind(my_data[[x]], unique.id=my_files[x]))

#For loop 
i <- NULL
y <- NULL

for (i in data_id_list){
  i <- melt(i)
  y <- rbind(y, i)
}

