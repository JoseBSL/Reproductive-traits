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

#Save each matrix individually
for(i in names(my_list_matrices)){
  write.csv(my_list_matrices[[i]], paste0("Data/Data_processing/bartomeus_beefun/29_Bartomeus_", i,"_unp.csv"))
}


