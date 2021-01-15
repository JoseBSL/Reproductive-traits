########################################################################################################################################################
#SCRIPT FOR DATA PREPARATION
########################################################################################################################################################
####SUMMARY OF WHAT IS DONE HERE###

#1) LOAD NETWORK DATA

#2) CALCULATE METRICS 

#3) MERGE WITH TRAIT DATA

#4) SAVE DATA


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
 #n_degree <- specieslevel(x, index="normalised degree", level="lower")
 #specialization
 d_Plant <- specieslevel(x, index="d", level="lower")   # specialization from BLUTHGEN 2006
 #closeness
 #closeness <- specieslevel(x, index="closeness", level="lower")
 #betweenness
 #betweenness <- specieslevel(x, index="betweenness", level="lower")
 #combine metrics in a unique data frame
 metrics <- cbind(Visits_Sum,d_Plant) #degree, n_degree, d, closeness, betweenness)
 return(metrics)
}

#workaround to remove singletones
i <- NULL
data <- NULL
metrics_list <- list()
for (i in names(my_data)){
  metrics_list[[i]] <- my_data[[i]][apply(my_data[[i]][,-1], 1, function(x) !all(x<2)),]
}

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
all_df <- all_df[!all_df$Species == "Ocotea laevigata", ]
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



########################################################################################################################################################
#3) MERGE WITH TRAIT DATA
########################################################################################################################################################
setwd("~/R_Projects/Reproductive traits") 

#READ TRAIT DATA ALREADY IMPUTED
trait_data <- read.csv("Data/Csv/all_species_imputed_trait_data_forest_data.csv") 
colnames(trait_data)[1] <- "Original_spp_names"
#trait_data <- read.csv("Data/Csv/all_species_imputed_trait_data_famd_data.csv", row.names="X") 

#CHANGE COL NAME FOR MERGING 
setnames(all_df, old = c("Species"), new = c("Original_spp_names"))

#MERGE
merg1  <- merge(all_df[!duplicated(all_df$Original_spp_names)], trait_data, by="Original_spp_names")
merg2  <- merge(all_df[!duplicated(all_df$Original_spp_names)], trait_data, by="Original_spp_names", all.x = T)

#This was done because some species were not merging, now is fixed
nrow(merg1) - nrow(merg2)
#Fix manually
merg2 <- data.frame(merg2)
merg2[is.na(merg2$Order_all),1]


#Save final merge after confirming that everything works fine
df_final  <- merge(all_df, trait_data, by="Original_spp_names")

########################################################################################################################################################
#4) SAVE DATA
########################################################################################################################################################
#Change col names
colnames(df_final) <- c("Original_names", "Visits", "d", "Id", "Order", "Family", "Genus", "Species", "Breeding_system", "Compatibility", "Selfing", "Selfing_quantitative",
                        "Flower_shape", "Flower_symmetry", "Flower_number", "Flower_inflorescence", "Floral_unit_width", "Flower_width", "Flower_length", "Style_length",
                        "Ovule_number", "Life_form", "Life_span", "Plant_height")

#save data
write.csv(df_final, "Data/Csv/metric_analysis_data_3rd_question.csv")
########################################################################################################################################################
