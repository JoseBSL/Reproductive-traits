

#If this does not work, load data manually
install.packages("devtools")
require(devtools)
install_github("BeeFunData", "ibartomeus")
library(BeeFunData)
library(reshape2)

#Filter for the same methodology
all_interactions <- subset(all_interactions, Out="transect")
#Create matrix
matrix <- acast(all_interactions, Plant_gen_sp ~ Pollinator_gen_sp , value.var='Frequency', 
      fun.aggregate=sum, margins=TRUE)
#Save csv and export with the other networks

write.csv(matrix, "data/matrix/Data_networks_subset/bartomeus_Donana.csv")
