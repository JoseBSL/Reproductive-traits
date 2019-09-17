#Here I'm going to read all the networks, create a list of spp
#and associate each spp to a network id

#To read all the csv's I setwd on the specific folder
#Then
setwd("Data/Data_networks")
temp <- list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))





