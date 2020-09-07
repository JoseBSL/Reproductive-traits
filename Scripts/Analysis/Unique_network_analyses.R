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
library(rtrees)
library(ape)

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
# 1 CALCULATE NETWORK METRICS
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
colnames(metrics_all)[2] <- "Plant_species"


##############################################
#2 NOW MERGE TRAIT DATA WITH THE NETWORK SPECIES
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

#Remove duplicates
trait_data_filtered_1 <- trait_data_filtered[!duplicated(trait_data_filtered$Plant_species),]
df <- merge(plants, trait_data_filtered_1, by = "Plant_species", all=T)

#Here I will have to select the columnd of ID that has no NA's
#In order to subset by the species of these specific networks
net <- df[!is.na(df$Id),]
net[] <- lapply(net, function(x) if(is.factor(x)) factor(x) else x)

##########################################################
#3 NOW MERGE WITH THE NETWORK METRICS CREATED IN SECTION 1
##########################################################

all_df <- merge(net, metrics_all, by = "Plant_species", all=T)
colnames(all_df)[10:12] <- c("family", "genus", "species")
#Fixing colnames
colnames(all_df)[17] <- "Autonomous_selfing_level"
colnames(all_df)[17] <- "Autonomous_selfing_level"
colnames(all_df)[76] <- "Net_ID"

#Clean dataframe
all_df_1 <- all_df[,-c(2,3,6,7,8,13,16,63:75)]
#set all columns names to lower case
setnames(all_df_1, tolower(names(all_df_1)))
#dding col named phylo for analysis
all_df_1$phylo <- all_df_1$species
##########################################################
#4 CALCULATE PHYLOGENETIC DISTANCE
##########################################################

#FROM NOW ON REMEMBER TO USE THE CORRECTED SPECIES NAMES
#Set these species as NA, the tree cannot find these species ad they are giving issues
#if I leave them as NA
all_df_1$species[all_df_1$species=="Diospyros seychellarum"] <- NA
all_df_1$species[all_df_1$species=="Memecylon eleagni"] <- NA
all_df_1$species[all_df_1$species=="Ocotea laevigata"] <- NA
all_df_1$species[all_df_1$species=="Soulamea terminaloides"] <- NA

#Make these NA's as NA
all_df_1$species[all_df_1$species=="NA"] <- NA
all_df_1$species[all_df_1$genus=="NA"] <- NA
all_df_1$species[all_df_1$fmily=="NA"] <- NA

#REMOVE NA's for calculating distance
all_df_2 <- all_df_1[!is.na(all_df_1$species),]

#Prepare species, genus and family for calculating tree
phylo <- as.data.frame(cbind(all_df_2$family, all_df_2$genus, all_df_2$species))
colnames(phylo) <-  c("family", "genus", "species")

#Select unique cases
phylo_1 <- phylo[!duplicated(phylo$species),]
phylo_2 <- tibble(phylo_1)
phylo_3 <- get_tree(sp_list = phylo_2, tree = tree_plant_otl, taxon = "plant")

#Convert phylogenetic tree into matrix
A <- vcv.phylo(phylo_3)
#Standardize to max value 1
A <- A/max(A)
#Unify column names; remove underscore and remove asterik
rownames(A) <- gsub("\\*", "", rownames(A))
colnames(A) <- gsub("\\*", "", colnames(A))
colnames(A) <- gsub("_", " ", colnames(A))
rownames(A) <- gsub("_", " ", rownames(A))

#Run model
m1 <- brm(visits ~ autonomous_selfing_level + (1|net_id) + (1|gr(phylo, cov = A)),
  data = all_df_2, family = negbinomial(),data2 = list(A = A),
  sample_prior = TRUE, warmup = 500, iter = 1500,save_all_pars=T,
  control = list(adapt_delta = 0.99))


save.image(file='myEnvironment.RData')
conditional_effects