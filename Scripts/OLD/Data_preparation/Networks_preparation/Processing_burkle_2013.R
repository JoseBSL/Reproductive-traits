# Processing data from burkle 2013
# DOI: :10.1126/science.1232728
# CHINA
# Note: Same place as robertson networks but sampled in 2009-2010
# 13 sites within the same woodland

#LOAD LIBRARY
library(reshape2)

#LOAD DATA
burkle <- read.csv("Data/Data_processing/burkle_2013/burkle_2013.csv")

#remove underscore in species names
burkle$plant <- gsub("_", " ", burkle$plant)
burkle$bee<- gsub("_", " ", burkle$bee)
#check levels
levels(as.factor(burkle$plant))
levels(as.factor(burkle$bee))
#I´m going to remove the varieties and just keep the spp
#otherwise looks ok to me
burkle$plant <- sub("^(\\S*\\s+\\S+).*", "\\1", burkle$plant)
burkle$bee <- sub("^(\\S*\\s+\\S+).*", "\\1", burkle$bee)

#Now convert to network format with acast

burkle_net <- acast(burkle, burkle$plant ~ burkle$bee , value.var='sum.ints', 
               fun.aggregate=sum, margins=F)

write.csv(burkle_net, "Data/Data_processing/burkle_2013/burkle_usa_2013.csv")

