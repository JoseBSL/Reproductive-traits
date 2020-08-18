#16 different networks
#I just select transects

#If this does not work, load data manually
install.packages("devtools")
require(devtools)
library(devtools)
install_github("BeeFunData", "ibartomeus")
library(BeeFunData)
library(reshape2)

#Seems that is not working (do not know why)
#I load data manually


#Filter for the same methodology
all_interactions <- subset(all_interactions, Out=="transect")

#check unique levels
levels(unique(all_interactions$Site_ID))

#Create a list with all the dataframes by Id
my_list <- split(all_interactions , f = all_interactions$Site_ID )

#Convert each dataframe to a matrix

my_list_matrices <- lapply(my_list, function(x) acast(x, Plant_gen_sp ~ Pollinator_gen_sp , value.var='Frequency', 
                                         fun.aggregate=sum, margins=F) )


#convert NA'S to zeros
my_list_matrices_1 <- rapply( my_list_matrices, f=function(x) ifelse(is.na(x),0,x), how="replace" )


#select row sums different of zero
my_list_matrices_2 <- lapply(my_list_matrices_1, function(x) (x[rowSums(x[, -1] > 0) != 0, ]))

#select colsums different of zero
my_list_matrices_3 <- lapply(my_list_matrices_2, function(x) (x[, colSums(x != 0) > 0]))


setwd("~/R_Projects/Reproductive traits") 
#Save each matrix individually
for(i in names(my_list_matrices_3)){
  write.csv(my_list_matrices_3[[i]], paste0("Data/Data_processing/bartomeus_donana_unpublished/29_Bartomeus_", i,"_unp.csv"))
}


