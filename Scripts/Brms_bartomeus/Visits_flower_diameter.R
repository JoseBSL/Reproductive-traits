##################################################
########
######
####
##
# NETWORK ANALYSIS BRMS BARTOMEUS 2008
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
library(dplyr)
library(tidyverse)



#LOAD NETWORK DATA
#Set working directory to read files
setwd("~/R_Projects/Reproductive traits/Data/Data_networks_quantitative") 

#Workflow found on stackoverflow to read all the files in a list
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i],stringsAsFactors = FALSE)))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, function(i){read.csv(i, row.names = 1)})

my_data <- my_data[1:6]
names(my_data) <- my_files[1:6]

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

i <- NULL
metrics_list_1 <- metrics_list
for (i in names(my_data)){
  metrics_list_1[[i]]$Z_score_sum <- scale(metrics_list_1[[i]]$Visits,center = TRUE, scale = TRUE)
}


#Now create a unique dataframe with bind rows from dplyr
metrics_all <- bind_rows(metrics_list_1, .id = "column_label")
colnames(metrics_all)[1] <- "Net_ID"
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


my_data <- my_data[1:6]
names(my_data) <- my_files[1:6]

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


#####################################################
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


#Prepare species to calcute phylogenetic distance
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

##########################################################
#4 Model1 (m1) VISITS~AUTONOMOUS SELFING LEVEL
##########################################################

#TWO DISTRIBUTIONS FIT THE DATA:
# 1 NEGATIVE BINOMIAL
# 2 GAUSSIAN/SKEW NORMAL

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


#Convert all NA'S to same type of NA's
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
all_df_2$compatibility <- make.true.NA(all_df_2$compatibility)
all_df_2 <- all_df_2[complete.cases(all_df_2$compatibility),]
colnames(all_df_2) <- make.unique(names(all_df_2))

levels(as.factor(all_df_2$compatibility))


all_df_3 <-  subset(all_df_2, compatibility=="self_compatible"|compatibility=="partially_self_compatible"|compatibility=="self_incompatible"|compatibility=="monoecious"|compatibility=="dioecious")

#Prepare example with selfing level
all_df_3 <- all_df_3 %>%mutate(compatibility = fct_relevel(compatibility, levels=c("self_compatible", "partially_self_compatible", "self_incompatible","monoecious", "dioecious")))

all_df_3$compatibility <- as.factor(all_df_3$compatibility)

levels(as.factor(all_df_3$compatibility))

all_df_2 <- all_df_1[!is.na(all_df_1$species),]
all_df_3$corolla_diameter_mean <- as.numeric(all_df_3$corolla_diameter_mean)

bart_2008_dat <- all_df_3
#TRY FIRST THE (1) NEGATIVE BINOMIAL DISTRIBUTION
bart_2008 <- brm(visits ~ corolla_diameter_mean + (1|net_id) + (1|gr(phylo, cov = A)),
          data = all_df_3, family = student(),data2 = list(A = A), cores = 4,
          sample_prior = TRUE, warmup = 500, iter = 1500,save_all_pars=T,
          control = list(adapt_delta = 0.99))
pp_check(bart_2008)
performance::r2_bayes(bart_2008)
c_e_bart_2008 <- conditional_effects(bart_2008)
plot(c_e_bart_2008, points=T,plot = FALSE)[[1]]






#LOAD NETWORK DATA
#Set working directory to read files
setwd("~/R_Projects/Reproductive traits/Data/Data_networks_quantitative") 

#Workflow found on stackoverflow to read all the files in a list
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i],stringsAsFactors = FALSE)))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, function(i){read.csv(i, row.names = 1)})

my_data <- my_data[16:31]
names(my_data) <- my_files[16:31]

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

i <- NULL
metrics_list_1 <- metrics_list
for (i in names(my_data)){
  metrics_list_1[[i]]$Z_score_sum <- scale(metrics_list_1[[i]]$Visits,center = TRUE, scale = TRUE)
}


#Now create a unique dataframe with bind rows from dplyr
metrics_all <- bind_rows(metrics_list_1, .id = "column_label")
colnames(metrics_all)[1] <- "Net_ID"
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


my_data <- my_data[16:31]
names(my_data) <- my_files[16:31]

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


#####################################################
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


#Prepare species to calcute phylogenetic distance
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

##########################################################
#4 Model1 (m1) VISITS~AUTONOMOUS SELFING LEVEL
##########################################################

#TWO DISTRIBUTIONS FIT THE DATA:
# 1 NEGATIVE BINOMIAL
# 2 GAUSSIAN/SKEW NORMAL

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


#Convert all NA'S to same type of NA's
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
all_df_2$compatibility <- make.true.NA(all_df_2$compatibility)
all_df_2 <- all_df_2[complete.cases(all_df_2$compatibility),]
colnames(all_df_2) <- make.unique(names(all_df_2))

levels(as.factor(all_df_2$compatibility))


all_df_3 <-  subset(all_df_2, compatibility=="self_compatible"|compatibility=="partially_self_compatible"|compatibility=="self_incompatible"|compatibility=="monoecious"|compatibility=="dioecious")

#Prepare example with selfing level
all_df_3 <- all_df_3 %>%mutate(compatibility = fct_relevel(compatibility, levels=c("self_compatible", "partially_self_compatible", "self_incompatible","monoecious", "dioecious")))

all_df_3$compatibility <- as.factor(all_df_3$compatibility)

levels(as.factor(all_df_3$compatibility))

all_df_2 <- all_df_1[!is.na(all_df_1$species),]
all_df_3$corolla_diameter_mean <- as.numeric(all_df_3$corolla_diameter_mean)

bart_unp_dat <- all_df_3

#TRY FIRST THE (1) NEGATIVE BINOMIAL DISTRIBUTION
bart_unp <- brm(visits ~ corolla_diameter_mean + (1|net_id) + (1|gr(phylo, cov = A)),
                data = all_df_3, family = student(),data2 = list(A = A), cores = 4,
                sample_prior = TRUE, warmup = 500, iter = 1500,save_all_pars=T,
                control = list(adapt_delta = 0.99))
pp_check(bart_unp)
performance::r2_bayes(bart_unp)
c_e_bart_unp <- conditional_effects(bart_unp)
plot(c_e_bart_unp, points=T,plot = FALSE)[[1]]

bart_studies <- rbind(c_e_bart_unp[[1]],c_e_bart_2008[[1]])
bart_studies_dat <- rbind(bart_unp_dat,bart_2008_dat)


ggplot(data=bart_studies, aes(x = corolla_diameter_mean, y = visits)) +
  geom_point(data = bart_unp_dat,alpha = 1/4,color = "#E31A1C") + geom_point(data = bart_2008_dat,alpha = 1/4, color = "#1F78B4",)+
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2") + theme_bw() + geom_smooth(data = c_e_bart_unp[[1]],
  aes(y = (estimate__), ymin = lower__, ymax = (upper__)),stat = "identity", color = "#E31A1C", alpha = 0.1, size = 1/2)+ 
  geom_smooth(data = c_e_bart_2008[[1]],aes(y = (estimate__), ymin = lower__, ymax = (upper__)),stat = "identity", color = "#A6CEE3", alpha = 0.1, size = 1/2)


library(RColorBrewer)
display.brewer.all()
cols <- brewer.pal(6, "Paired")






#kaiser-bunbury
#LOAD NETWORK DATA
#Set working directory to read files
setwd("~/R_Projects/Reproductive traits/Data/Data_networks_quantitative") 

#Workflow found on stackoverflow to read all the files in a list
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i],stringsAsFactors = FALSE)))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, function(i){read.csv(i, row.names = 1)})

my_data <- my_data[8:15]
names(my_data) <- my_files[8:15]

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

i <- NULL
metrics_list_1 <- metrics_list
for (i in names(my_data)){
  metrics_list_1[[i]]$Z_score_sum <- scale(metrics_list_1[[i]]$Visits,center = TRUE, scale = TRUE)
}


#Now create a unique dataframe with bind rows from dplyr
metrics_all <- bind_rows(metrics_list_1, .id = "column_label")
colnames(metrics_all)[1] <- "Net_ID"
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


my_data <- my_data[8:15]
names(my_data) <- my_files[8:15]

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


#####################################################
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


#Prepare species to calcute phylogenetic distance
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

##########################################################
#4 Model1 (m1) VISITS~AUTONOMOUS SELFING LEVEL
##########################################################

#TWO DISTRIBUTIONS FIT THE DATA:
# 1 NEGATIVE BINOMIAL
# 2 GAUSSIAN/SKEW NORMAL

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


#Convert all NA'S to same type of NA's
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
all_df_2$compatibility <- make.true.NA(all_df_2$compatibility)
all_df_2 <- all_df_2[complete.cases(all_df_2$compatibility),]
colnames(all_df_2) <- make.unique(names(all_df_2))

levels(as.factor(all_df_2$compatibility))


all_df_3 <-  subset(all_df_2, compatibility=="self_compatible"|compatibility=="partially_self_compatible"|compatibility=="self_incompatible"|compatibility=="monoecious"|compatibility=="dioecious")

#Prepare example with selfing level
all_df_3 <- all_df_3 %>%mutate(compatibility = fct_relevel(compatibility, levels=c("self_compatible", "partially_self_compatible", "self_incompatible","monoecious", "dioecious")))

all_df_3$compatibility <- as.factor(all_df_3$compatibility)

levels(as.factor(all_df_3$compatibility))

all_df_2 <- all_df_1[!is.na(all_df_1$species),]
all_df_3$corolla_diameter_mean <- as.numeric(all_df_3$corolla_diameter_mean)
bun_2017_dat <- all_df_3
#TRY FIRST THE (1) NEGATIVE BINOMIAL DISTRIBUTION
bun_2017 <- brm(visits ~ corolla_diameter_mean + (1|net_id) + (1|gr(phylo, cov = A)),
                data = all_df_3, family = student(),data2 = list(A = A), cores = 4,
                sample_prior = TRUE, warmup = 500, iter = 1500,save_all_pars=T,
                control = list(adapt_delta = 0.99))
pp_check(bun_2017)
performance::r2_bayes(bun_2017)
c_e_bun_2017 <- conditional_effects(bun_2017)
plot(c_e_bun_2017, points=T,plot = FALSE)[[1]]


studies <- rbind(c_e_bart_unp[[1]],c_e_bart_2008[[1]], c_e_bun_2017[[1]])
studies_dat <- rbind(bart_unp_dat,bart_2008_dat,bun_2017_dat)

max(bun_2017_dat$visits)
ggplot(data=studies, aes(x = corolla_diameter_mean, y = visits)) +
  geom_point(data = bart_unp_dat,alpha = 1/4,color = "#E31A1C") + geom_point(data = bart_2008_dat,alpha = 1/4, color = "#1F78B4",)+
  geom_point(data = bun_2017_dat,alpha = 1/4, color = "#33A02C") + scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2") + theme_bw() + geom_smooth(data = c_e_bart_unp[[1]],
   aes(y = (estimate__), ymin = lower__, ymax = (upper__)),stat = "identity", color = "#E31A1C", alpha = 0.1, size = 1/2)+ 
  geom_smooth(data = c_e_bart_2008[[1]],aes(y = (estimate__), ymin = lower__, ymax = (upper__)),stat = "identity", color = "#A6CEE3", alpha = 0.1, size = 1/2)+
  geom_smooth(data = c_e_bun_2017[[1]],aes(y = (estimate__), ymin = lower__, ymax = (upper__)),stat = "identity", color = "#33A02C", alpha = 0.1, size = 1/2)+
  ylim(0,400)


library(RColorBrewer)
display.brewer.all()
cols <- brewer.pal(6, "Paired")











#kaiser-bunbury
#LOAD NETWORK DATA
#Set working directory to read files
setwd("~/R_Projects/Reproductive traits/Data/Data_networks_quantitative") 

#Workflow found on stackoverflow to read all the files in a list
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i],stringsAsFactors = FALSE)))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, function(i){read.csv(i, row.names = 1)})

my_data <- my_data[32:39]
names(my_data) <- my_files[32:39]

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

i <- NULL
metrics_list_1 <- metrics_list
for (i in names(my_data)){
  metrics_list_1[[i]]$Z_score_sum <- scale(metrics_list_1[[i]]$Visits,center = TRUE, scale = TRUE)
}


#Now create a unique dataframe with bind rows from dplyr
metrics_all <- bind_rows(metrics_list_1, .id = "column_label")
colnames(metrics_all)[1] <- "Net_ID"
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


my_data <- my_data[32:39]
names(my_data) <- my_files[32:39]

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


#####################################################
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


#Prepare species to calcute phylogenetic distance
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

##########################################################
#4 Model1 (m1) VISITS~AUTONOMOUS SELFING LEVEL
##########################################################

#TWO DISTRIBUTIONS FIT THE DATA:
# 1 NEGATIVE BINOMIAL
# 2 GAUSSIAN/SKEW NORMAL

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


#Convert all NA'S to same type of NA's
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
all_df_2$compatibility <- make.true.NA(all_df_2$compatibility)
all_df_2 <- all_df_2[complete.cases(all_df_2$compatibility),]
colnames(all_df_2) <- make.unique(names(all_df_2))

levels(as.factor(all_df_2$compatibility))


all_df_3 <-  subset(all_df_2, compatibility=="self_compatible"|compatibility=="partially_self_compatible"|compatibility=="self_incompatible"|compatibility=="monoecious"|compatibility=="dioecious")

#Prepare example with selfing level
all_df_3 <- all_df_3 %>%mutate(compatibility = fct_relevel(compatibility, levels=c("self_compatible", "partially_self_compatible", "self_incompatible","monoecious", "dioecious")))

all_df_3$compatibility <- as.factor(all_df_3$compatibility)

levels(as.factor(all_df_3$compatibility))

all_df_2 <- all_df_1[!is.na(all_df_1$species),]
all_df_3$corolla_diameter_mean <- as.numeric(all_df_3$corolla_diameter_mean)
bun_2011_dat <- all_df_3
#TRY FIRST THE (1) NEGATIVE BINOMIAL DISTRIBUTION
bun_2011 <- brm(visits ~ corolla_diameter_mean + (1|net_id) + (1|gr(phylo, cov = A)),
                data = all_df_3, family = student(),data2 = list(A = A), cores = 4,
                sample_prior = TRUE, warmup = 500, iter = 1500,save_all_pars=T,
                control = list(adapt_delta = 0.99))
pp_check(bun_2011)
performance::r2_bayes(bun_2011)
c_e_bun_2011 <- conditional_effects(bun_2011)
plot(c_e_bun_2011, points=T,plot = FALSE)[[1]]


studies <- rbind(c_e_bart_unp[[1]],c_e_bart_2008[[1]], c_e_bun_2017[[1]],c_e_bun_2011[[1]])
studies_dat <- rbind(bart_unp_dat,bart_2008_dat,bun_2017_dat,bun_2011_dat)

ggplot(data=studies, aes(x = corolla_diameter_mean, y = visits)) +
  geom_point(data = bart_unp_dat,alpha = 1/4,color = "#E31A1C") + geom_point(data = bart_2008_dat,alpha = 1/4, color = "#1F78B4",)+
  geom_point(data = bun_2017_dat,aes(corolla_diameter_mean, visits),alpha = 1/4, color = "#33A02C") + geom_point(data = bun_2011_dat,alpha = 1/4, color = "#6A3D9A")+
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2") + theme_bw() + geom_smooth(data = c_e_bart_unp[[1]],
  aes(y = (estimate__), ymin = lower__, ymax = (upper__)),stat = "identity", color = "#E31A1C", alpha = 0.1, size = 1/2)+ 
  geom_smooth(data = c_e_bart_2008[[1]],aes(y = (estimate__), ymin = lower__, ymax = (upper__)),stat = "identity", color = "#A6CEE3", alpha = 0.05, size = 1/2)+
  geom_smooth(data = c_e_bun_2017[[1]],aes(y = (estimate__), ymin = lower__, ymax = (upper__)),stat = "identity", color = "#33A02C", alpha = 0.05, size = 1/2)+
  geom_smooth(data = c_e_bun_2011[[1]],aes(y = (estimate__), ymin = lower__, ymax = (upper__)),stat = "identity", color = "#6A3D9A", alpha = 0.05, size = 1/2)

d <- c_e_bun_2017
str(d)
d_1 <- d[c("corolla_diameter_mean","visits","plant_species")]


library(RColorBrewer)
display.brewer.all()
cols <- brewer.pal(10, "Paired")

