#Here I'm going to read all the networks, create a list of spp
#and associate each spp to a network id


#Prueba
d <- read.csv("Data/Data_networks/Benadi_2014_1.csv")

d_1 <-d$X
d1=as.data.frame(d_1)
d1$id <- rep("benadi_2014_1" ,length(d1))

d_1 <-d$X
d1=as.data.frame(d_1)
d1$id <- rep("bartomeus_2008_bat1ca" ,length(d1))
setwd("Data/Data_networks")


temp = list.files( pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))
