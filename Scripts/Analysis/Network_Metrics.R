#####################################
###
####CODE TO CALCULATE NETWORK METRICS
###
#####################################

#Load libraries
library(readxl)
library(bipartite)
library(tibble)
library(data.table)
#First read all networks
#Set working directory to read files
setwd("~/R_Projects/Reproductive traits/Data/Data_processing/Bartomeus_BeeFun") 

#Workflow found on stackoverflow to read all the files in a list
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i],stringsAsFactors = FALSE)))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, function(i){read.csv(i, row.names = 1)})

names(my_data) <- my_files

data_id_list <- lapply(seq_along(my_data), 
                       function(x) cbind(my_data[[x]], unique.id=my_files[x]))
#Load trait data

setwd("~/R_Projects/Reproductive traits") 

t_data <- read_excel("Data/Data_raw/Trait_data_final.xlsx")
t_data <- as.data.frame(t_data)
colnames(t_data)[1] <- "Plant_species"
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
  #closeness
  closeness <- specieslevel(clean(x), index="closeness", level="lower")
  #betweenness
  betweenness <- specieslevel(clean(x), index="betweenness", level="lower")
  
  #combine metrics in a unique data frame
  metrics <- cbind(visits, degree, n_degree, d, closeness, betweenness)
  return(metrics)
}

met(my_data[[1]])

#loop to create a list of dataframes with the 16 networks
#and the network metrics for each plant species
i <- NULL
data <- NULL
metrics_list <- list()
for (i in names(my_data)){
  metrics_list[[i]] <- met(my_data[[i]])
}

#Now I have to merge the lsit of dataframe metrics with the traits
#First subset by traits of interest (just compatibility at the moment)

compat <- t_data[,c(1,14:19)]
colnames(compat)[1] <- "Species"
metrics_list[[1]]

#Loop to create the list with all the metrics and traits--> MERGE
i <- NULL
all_list <- list()
for (i in names(metrics_list)){
  all_list[[i]] <- merge(metrics_list[[i]], compat, by="Species")
  
}

#ADD Id to all trhe dataframes as column
#this will help to convert everything to unique dataframe easily
#Add "id" to the list to the long format data
all_list <- lapply(seq_along(all_list), 
                   function(x) cbind(all_list[[x]], unique.id=my_files[x]))
#Now merge all the data frames 
all_df <- bind_rows(all_list, .id = "unique.id")

#saveRDS(all_df, "Data/RData/network_metrics.RData")



#Prepare now data  flower size

#Now I have to merge the lsit of dataframe metrics with the traits
#First subset by traits of interest (just compatibility at the moment)

compat <- t_data[,c(1,14:19,29,31,34)]
colnames(compat)[1] <- "Species"
metrics_list[[1]]

#Loop to create the list with all the metrics and traits--> MERGE
i <- NULL
all_list <- list()
for (i in names(metrics_list)){
  all_list[[i]] <- merge(metrics_list[[i]], compat, by="Species")
  
}

#ADD Id to all trhe dataframes as column
#this will help to convert everything to unique dataframe easily
#Add "id" to the list to the long format data
all_list <- lapply(seq_along(all_list), 
                   function(x) cbind(all_list[[x]], unique.id=my_files[x]))
#Now merge all the data frames 
all_df <- bind_rows(all_list, .id = "unique.id")

#saveRDS(all_df, "Data/RData/network_metrics_flower_size_and_shape.RData")


#Prepare new data main traits

#Now I have to merge the lsit of dataframe metrics with the traits
#First subset by traits of interest (just compatibility at the moment)

compat <- t_data[,c(1,12,14:19,29,30,31,34,38,43,48,49,52)]
colnames(compat)[1] <- "Species"
metrics_list[[1]]

#Loop to create the list with all the metrics and traits--> MERGE
i <- NULL
all_list <- list()
for (i in names(metrics_list)){
  all_list[[i]] <- merge(metrics_list[[i]], compat, by="Species")
  
}

#ADD Id to all trhe dataframes as column
#this will help to convert everything to unique dataframe easily
#Add "id" to the list to the long format data
all_list <- lapply(seq_along(all_list), 
                   function(x) cbind(all_list[[x]], unique.id=my_files[x]))
#Now merge all the data frames 
all_df <- bind_rows(all_list, .id = "unique.id")

saveRDS(all_df, "Data/RData/network_metrics_main_traits.RData")
