# DATA PREPARATION FOR LONG FORMAT
#FOR THAT I HAVE A FOLDER WITH ALL THE NETWORKS THAT I´M GOING TO USE
#THEN I CONVERT FROM MATRIX TO LONG FORMAT



#load libraries

library(tidyr)
library(reshape2)
library(dplyr)
library(plyr)
library(stringr)
library(taxize)


#Set working directory to read files
setwd("~/R_Projects/Reproductive traits/Data/Data_networks_metawebs") 

#It is call subset because I do not use all the networks
#In total between 20 and 30, still adding... Do not know exact number yet

#Workflow found on stackoverflow to read all the files in a list
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, read.csv)
#Add "id" to the list to the long format data
data_id_list <- lapply(seq_along(my_data), 
                       function(x) cbind(my_data[[x]], unique.id=my_files[x]))

#For loop to melt each data frame and merge
i <- NULL
y <- NULL

for (i in data_id_list){
  i <- melt(i)
  y <- rbind(y, i)
}
#Output seems right

#Renaming columns
colnames(y) <- c("Plant_species", "Id", "Pollinator_species", "Interaction") 
#Reordering columns in a way that makes more sense for me
Long_data <- select(y, "Plant_species", "Pollinator_species", "Interaction", "Id") 
#Changing dot for white space
Long_data$Pollinator_species=gsub("\\."," ",Long_data$Pollinator_species)
setwd("~/R_projects/Reproductive Traits") 

#Now save long format data
saveRDS(Long_data, "Data/RData/Long_format_metawebs.RData")

#Pollinator_species <- as.data.frame(unique(word(Long_data$Pollinator_species),1))
#poll_famord=tax_name(query=Pollinator_species[,1],get=c("family","order"),division_filter=c("Arthropoda"),
#db="itis",rank_query="genus")
