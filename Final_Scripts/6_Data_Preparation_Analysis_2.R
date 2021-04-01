########################################################################################################################################################
#SCRIPT TO PREPARE DATA FOR ANALYSIS 2) VISITS/SPECIALIZATION ~ PRINCIPAL COMPONENTS
########################################################################################################################################################
#LOAD LIBRARIES
library(data.table) # operate with df
library(bipartite) #calculate metrics
library(dplyr) #data manipulation 
library(stringr) #remove string
########################################################################################################################################################
#1) LOAD NETWORK DATA
########################################################################################################################################################
#Set working directory to read files
setwd("~/R_Projects/Reproductive traits/Data/Data_networks_quantitative") 

#read csv files
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, function(i){read.csv(i,  row.names = 1)})
# Add id to dataframe
names(my_data) <- stringr::str_replace(my_files, pattern = ".csv", replacement = "")
########################################################################################################################################################

[KEEP WORKING FROM HERE]

