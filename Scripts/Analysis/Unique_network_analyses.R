##################################################
########
######
####
##
# NETWORK ANALYSIS
##
###
####
#####
#################################################

#LOAD LIBRARIES
library(brms)
library(ape)
library(tidybayes)
library(ggplot2)
library(reshape2)
library(data.table)
library(dplyr)
library(bipartite)
library(readxl)

##########
#LOAD DATA
###########


#LOAD NETWORK DATA
#Set working directory to read files
setwd("~/R_Projects/Reproductive traits/Data/Data_networks_quantitative") 

#Workflow found on stackoverflow to read all the files in a list
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i],stringsAsFactors = FALSE)))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, function(i){read.csv(i, row.names = 1)})

names(my_data) <- my_files

data_id_list <- lapply(seq_along(my_data), 
                       function(x) cbind(my_data[[x]], unique.id=my_files[x]))


##########################
#CALCULATE NETWORK METRICS
##########################
#FIRST PREPARE SOME FUNCTIONS
#and prepare the data

#Function to sum the humber of visits per row
#It converts NA'S to 0's
#
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
#Function to calculate all metrics and bind on dataframe
met <- function(x){
  visits <- rs_NA(x)
  #degree
  degree <- specieslevel(clean(x), index="degree", level="lower")
  #normalise degree
  n_degree <- specieslevel(clean(x), index="normalised degree", level="lower")
  #specialization
  d <- specieslevel(clean(x), index="d", level="lower")
  #closeness
  closeness <- specieslevel(clean(x), index="closeness", level="lower")
  #betweenness
  betweenness <- specieslevel(clean(x), index="betweenness", level="lower")
  
  #combine metrics in a unique data frame
  metrics <- cbind(visits, degree, n_degree, d, closeness, betweenness)
  return(metrics)
}


#Loop to create a list of dataframes with the 16 networks
#and the network metrics for each plant species
i <- NULL
data <- NULL
metrics_list <- list()
for (i in names(my_data)){
  metrics_list[[i]] <- met(my_data[[i]])
}


#Now create a unique dataframe with bind rows from dplyr
metrics_all <- bind_rows(metrics_list, .id = "column_label")


##############################################
#NOW MERGE TRAIT DATA WITH THE NETWORK SPECIES
##############################################

#LOAD TRAIT DATA
setwd("~/R_Projects/Reproductive traits") 
trait_data <- read_excel("Data/Trait_data_raw/Trait_data_final.xlsx")
trait_data <- as.data.frame(trait_data)
levels(as.factor(trait_data$Info_level))
trait_data_filtered <- subset(trait_data, Info_level=="flower"|Info_level=="capitulum"|Info_level=="inflorescence"|Info_level=="NA")
#change plant species colnames for merging
colnames(trait_data_filtered)[1] <- "Plant_species"

#CONVERT NETWORKS TO LONG FORMAT
#Set working directory to read files
setwd("~/R_Projects/Reproductive traits/Data/Data_networks_quantitative") 

#It is call subset because I do not use all the networks
#In total between 20 and 30, still adding... Do not know exact number yet

#Workflow found on stackoverflow to read all the files in a list
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, function(i){read.csv(i, check.names=FALSE)})
#Add "id" to the list to the long format data
data_id_list <- lapply(seq_along(my_data), 
                       function(x) cbind(my_data[[x]], unique.id=my_files[x]))


#For loop to melt each data frame and merge
i <- NULL
all_long <- NULL

for (i in data_id_list){
  i <- melt(i)
  all_long <- rbind(all_long, i)
}

#Renaming columns
colnames(all_long) <- c("Plant_species", "Id", "Pollinator_species", "Interaction") 

#select unique cases for merging data
plants <- all_long[!duplicated(all_long$Plant_species),]

df <- merge(plants, trait_data_filtered, by = "Plant_species", all=T)
setwd("~/R_Projects/Reproductive traits")
write.csv(df, "Data/Trait_data_raw/trial.csv")



