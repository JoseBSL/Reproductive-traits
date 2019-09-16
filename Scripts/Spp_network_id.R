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


temp = list.files(path="Data/Data_networks", pattern="*.csv")
myfiles = lapply(temp, read.delim)


temp = list.files(path="Data/Data_networks/", pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

# Get the files names
files = list.files(path="Data/Data_networks",pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
