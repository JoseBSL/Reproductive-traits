#Load library

library(bipartite)
library(plyr)
library(data.table)

#####################################
###
####CODE TO CALCULATE NETWORK METRICS
###
#####################################


#First read all networks
#Set working directory to read files
setwd("~/R_Projects/Reproductive traits/Data/Data_processing/Bartomeus_BeeFun") 

#Workflow found on stackoverflow to read all the files in a list
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, function(i){read.csv(i, row.names = 1)})

#Load trait data



#Select output of metrics and prepare for model
#I will make a dataframe wi
library(dplyr)


# Function to sum the humber of visits per row
str(my_data[[1]])
rs_NA <- function(x){
z <- as.data.frame(rowSums(x))
z <- setDT(z, keep.rownames = TRUE)[]
z <- z[complete.cases(z),]
colnames(z) <- c("Species","Visits")
return(z)
}
#Perform sum number of visits 
visits <- rs_NA(my_data[[1]])
#degree
degree <- specieslevel(my_data[[1]], index="degree", level="lower")
#normalise degree
n_degree <- specieslevel(my_data[[1]], index="normalised degree", level="lower")
#specialization
d <- specieslevel(my_data[[1]], index="d", level="lower")

#combine metrics in a unique data frame
metrics <- cbind(visits, degree, n_degree, d)





