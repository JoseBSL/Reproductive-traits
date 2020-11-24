# Processing data from Bundgaard 2003
# DOI: Unpublished worked, conducted by a student of Jens Olesen
# Denmark
# Note: sampled 1 season 2003

#Load library
library(reshape2)
#Load data
bund_1 <- read.csv("Data/Data_processing/bundgaard_olesen_denmark_2003/bund_1.csv", check.names = F)
bund_2 <- read.csv("Data/Data_processing/bundgaard_olesen_denmark_2003/bund_2.csv", check.names = F)
bund_3 <- read.csv("Data/Data_processing/bundgaard_olesen_denmark_2003/bund_3.csv", check.names = F)
bund_4 <- read.csv("Data/Data_processing/bundgaard_olesen_denmark_2003/bund_4.csv", check.names = F)
bund_5 <- read.csv("Data/Data_processing/bundgaard_olesen_denmark_2003/bund_5.csv", check.names = F)


#Convert data to long format
long_1 <- reshape2::melt(bund_1, value.name = "Count", variable.name=c("Plant.species.ID"), na.rm = TRUE)
colnames(long_1)[1] <- "Pollinator.species.ID"               
long_2 <- reshape2::melt(bund_2, value.name = "Count", variable.name=c("Plant.species.ID"), na.rm = TRUE)
colnames(long_2)[1] <- "Pollinator.species.ID"
long_3 <- reshape2::melt(bund_3, value.name = "Count", variable.name=c("Plant.species.ID"), na.rm = TRUE)
colnames(long_3)[1] <- "Pollinator.species.ID" 
long_4 <- reshape2::melt(bund_4, value.name = "Count", variable.name=c("Plant.species.ID"), na.rm = TRUE)
colnames(long_4)[1] <- "Pollinator.species.ID" 
long_5 <- reshape2::melt(bund_5, value.name = "Count", variable.name=c("Plant.species.ID"), na.rm = TRUE)
colnames(long_5)[1] <- "Pollinator.species.ID" 

all_long <- rbind(long_1,long_2,long_3,long_4,long_5)

#Now convert to matrix 

matrix_all <- acast(all_long, all_long$Plant.species.ID ~ all_long$Pollinator.species.ID , value.var='Count', 
                fun.aggregate=sum, margins=F)
matrix_all_1 <- matrix_all[rowSums(matrix_all[, -1] > 0) != 0, ]
rowSums(as.matrix(matrix_all_1))
matrix_all_2 <- matrix_all_1[, colSums(matrix_all_1 != 0) > 0]
colSums(as.matrix(matrix_all_2))

write.csv(matrix_all_2, "Data/Data_processing/bundgaard_olesen_denmark_2003/bundgaard_denmark_2003.csv")
