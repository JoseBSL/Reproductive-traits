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
#2) CALCULATE METRICS 
########################################################################################################################################################

#Function to sum visits per row and convert NA'S to 0's in the case there is any
rs_NA <- function(x){
  z <- as.data.frame(x)
  z[is.na(z)] <- 0  #convert NA'S to 0's
  z <- as.data.frame(rowSums(z)) #sum rows
  z <- setDT(z, keep.rownames = TRUE)[] 
  colnames(z) <- c("Species","Visits")
  return(z)
}


#Calculate all metrics at once with bipartite
met <- function(x){
  Visits_Sum <- rs_NA(x) #previous function to sum rows 
  #degree
  #degree <- specieslevel(x, index="degree", level="lower")
  #normalise degree
  n_degree <- specieslevel(x, index="normalised degree", level="lower")
  #specialization
  d_Plant <- specieslevel(x, index="d", level="lower")   # specialization from BLUTHGEN 2006
  #closeness
  #closeness <- specieslevel(x, index="closeness", level="lower")
  #betweenness
  #betweenness <- specieslevel(x, index="betweenness", level="lower")
  #combine metrics in a unique data frame
  metrics <- cbind(Visits_Sum,n_degree,d_Plant) #degree, n_degree, d, closeness, betweenness)
  return(metrics)
}

##workaround to remove singletones
#i <- NULL
#data <- NULL
#metrics_list <- list()
#for (i in names(my_data)){
#  metrics_list[[i]] <- my_data[[i]][apply(my_data[[i]][,-1], 1, function(x) !all(x<2)),]
#}

#calculate network metrics with for loop
i <- NULL
data <- NULL
metrics_list_1 <- list()
for (i in names(metrics_list)){
  metrics_list_1[[i]] <- met(metrics_list[[i]])
}

#add id as a row name
all_list <- lapply(seq_along(metrics_list_1),function(x) cbind(metrics_list_1[[x]], 
                                                               unique.id=str_replace(my_files[x], pattern = ".csv", replacement = "")))

#Now merge all the data frames 
all_df <- bind_rows(all_list)

#some editing
#this species are giving trouble when I run the phylogeny, delete them
#all_df <- all_df[!all_df$Species == "Diospyros seychellarum", ]
#all_df <- all_df[!all_df$Species == "Ocotea laevigata", ]
#all_df <- all_df[!all_df$Species == "Soulamea terminaloides", ]
#t <- t[!t$Species_all == "Pinus luchuensis", ]   # remove gymnosperm species

#check
#all_df_unique_sp <- all_df[!duplicated(all_df$Species),]

#remove species that are not until species level
#ALL THE SPECIES WITH SP. ARE DELETD
all_df$Species <-  sub('sp.1$', 'sp.', all_df$Species)
all_df$Species <- sub('sp.2$', 'sp.', all_df$Species)
all_df$Species <- sub('sp$', 'sp.', all_df$Species)
all_df$Species <- sub("sp.$", "DELETE", all_df$Species)

all_df <- all_df[- grep("DELETE",  all_df$Species),] #remove species with "DELETE"

colnames(all_df)[1] <- "Species_all"

########################################################################################################################################################
#MERGE SPECIES LEVEL METRICS WITH THE SPACE COORDINATES FROM THE PPCA (almost there)
########################################################################################################################################################
#SET NORMAL WORKING DIRECTORY
setwd("~/R_Projects/Reproductive traits") 

#LOAD PPCA DATA FOR MERGE
phyl_pca <- readRDS("Data/RData/phyl_pca_forest_all_values.rds") #add PCA loadings for analysis
dat_cleaning <- readRDS("Data/RData/data_all_species_PPCA.rds") #data for PPCA

#CONVERT TO DATAFRAME
phyl_pca_1 <- data.frame(phyl_pca$S)

#CONVERT TORWS TO COLNAMES
dat_cleaning_1 <- setDT(dat_cleaning, keep.rownames = TRUE)[]
colnames(dat_cleaning_1)[1] <- "Species_all"

phyl_pca_1$Species_all <- dat_cleaning_1$Species_all
#ADD ALSO THESE OTHER 3 COLUMNS WITH SPP, GENUS AND FAMILY INFO NEEDED TO GET THE PHYLO FOR THE MODEL
phyl_pca_1$Family_all <- dat_cleaning_1$Family_all
phyl_pca_1$Genus_all <- dat_cleaning_1$Genus_all
phyl_pca_1$Species_all <- dat_cleaning_1$Species_all

#MERGE DATAFRAMES
data_analysis <- merge(all_df, phyl_pca_1, by = "Species_all")

#NOTE
#data_analysis_VALUES_DROPPED <- merge(all_df, phyl_pca_1, by = "Species_all", all.x = T)
#see that some levels are dropped but to calculate the position of the trait space is recommended 
#to delete values out of the range 2.5-97.5 percentile
#AND THIS IS WHY WE DO LOSS SOME SPP
########################################################################################################################################################
#SAVE DATA
########################################################################################################################################################
saveRDS(data_analysis, "Data/RData/data_analysis_2_all_values.rds") 
########################################################################################################################################################
########################################################################################################################################################







