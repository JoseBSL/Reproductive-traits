#Load library

library(bipartite)
library(plyr)
library(data.table)
library(tidyverse) 
library(lubridate)
library(tibble)
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
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i],stringsAsFactors = FALSE)))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, function(i){read.csv(i, row.names = 1)})


#Load trait data


# Function to sum the humber of visits per row
#It converts NA'S to 0's
#
str(my_data[[1]])
rs_NA <- function(x){
z <- as.data.frame(x)
z[is.na(z)] <- 0
z <- as.data.frame(rowSums(z))
z <- setDT(z, keep.rownames = TRUE)[]
colnames(z) <- c("Species","Visits")
z[is.na(z)] <- 0
z[z$Species=="NA NA"]<-NA
z <- z[complete.cases(z$Species),]
z <- z[apply(z!=0, 1, all),]

return(z)
}

rs_NA(my_data[[2]])

#Function to prepare data for bipartite analysis
clean <- function(x){
  w <- x
row.names.remove <- "NA NA"
w <- w[!(row.names(w) %in% row.names.remove), ]
w[is.na(w)] <- 0
w <- as.data.frame(w, stringsAsFactors = TRUE)
w[, c(1:ncol(w))] <- sapply(w[, c(1:ncol(w))], as.numeric)

return(w)
}

B =clean(my_data[[2]])
z <- as.data.frame(rowSums(my_data[[2]]))

#it works


#Function to calculate all metrics and bind on dataframe
met <- function(x){
visits <- rs_NA(x)
#degree
degree <- specieslevel(clean(x), index="degree", level="lower")
#normalise degree
n_degree <- specieslevel(clean(x), index="normalised degree", level="lower")
#specialization
d <- specieslevel(clean(x), index="d", level="lower")
#combine metrics in a unique data frame
metrics <- cbind(visits, degree, n_degree, d)
return(metrics)
}

met(my_data[[1]])


