
#I have already converted the file from https://doi.org/10.1111/j.1365-2656.2008.01501.x
#to quantitative data in a csv manually

#now read file
d <- read.csv("Data/Data_processing/Dupont_&_Olesen_2009/Dupont_&_Olesen_2009_IB.csv")

#convert to matrix format

library(reshape2)
d_1 <- acast(d , Plant~Species, value.var="Visits")
d_1[is.na(d_1)] <- 0
str(d_1)
write.csv(d_1, "Data/Data_networks_quantitative/4_1_dupont_2009.csv")
