########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
##REGRESSION TREE FOR THE SUBSET OF SPECIES WITH INFO OF POLLEN AND NECTAR
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

#HERE I TRY TO SELECT THE MAIN EXPLANATORY TRAITS FOR EACH TRAIT 

#LOAD LIBRARIES
library(data.table) # operate with df
library(bipartite) #calculate metrics
library(dplyr) #data manipulation 
library(stringr) #remove string
library(brms)
library(projpred)
library(dplyr)
library(ggplot2)
library(bayesplot)
library(ape)
library(rtrees)
library(cowplot)
library(emmeans)
library(tidybayes)
library(multcomp)
library(rpart)
library(rpart.plot)
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
  degree <- specieslevel(x, index="degree", level="lower")
  #normalise degree
  n_degree <- specieslevel(x, index="normalised degree", level="lower")
  #specialization
  d_Plant <- specieslevel(x, index="d", level="lower")   # specialization from BLUTHGEN 2006
  #closeness
  closeness <- specieslevel(x, index="closeness", level="lower")
  #betweenness
  betweenness <- specieslevel(x, index="betweenness", level="lower")
  #combine metrics in a unique data frame
  metrics <- cbind(Visits_Sum,n_degree,d_Plant, closeness, betweenness, degree) #degree, n_degree, d, closeness, betweenness)
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
########################################################################################################################################################
########################################################################################################################################################

#SET NORMAL WORKING DIRECTORY
setwd("~/R_Projects/Reproductive traits") 

dat <- read.csv("Data/Csv/nectar_pollen_subset_imputed_trait_data.csv", row.names = "X")

#The correct species names are the row names, convert to 1st col to merge
#convert row to col
setDT(dat, keep.rownames = TRUE)[]
#set a colname
colnames(dat)[1] <- "Plants"
#merge
datitos <- merge(all_df, dat, by.x =  "Species_all", by.y= "Plants")


#New colname with log of visits

datitos_1 <- datitos
#FIRST QUALITATIVE VARIABLES
#Check levels before running models and perform cross validation
levels(factor(datitos_1$Breeding_system)) # ok
levels(factor(datitos_1$Compatibility_system)) #Fix
datitos_1$Compatibility_system[datitos_1$Compatibility_system=="dioecious"] <- "Unisexual flowers"
datitos_1$Compatibility_system[datitos_1$Compatibility_system=="monoecious"] <- "Unisexual flowers"
datitos_1$Compatibility_system[datitos_1$Compatibility_system=="self_incompatible"] <- "Self incompatible"
datitos_1$Compatibility_system[datitos_1$Compatibility_system=="self_compatible"] <- "Self compatible"
datitos_1$Compatibility_system[datitos_1$Compatibility_system=="partially_self_compatible"] <- "Partially self compatible"
#Check again
levels(factor(datitos_1$Compatibility_system)) #NOW OK
levels(factor(datitos_1$Autonomous_selfing_level)) 
datitos_1$Autonomous_selfing_level[datitos_1$Autonomous_selfing_level=="high"] <- "High-Medium"
datitos_1$Autonomous_selfing_level[datitos_1$Autonomous_selfing_level=="medium"] <- "High-Medium"
datitos_1$Autonomous_selfing_level[datitos_1$Autonomous_selfing_level=="low"] <- "Low-None"
datitos_1$Autonomous_selfing_level[datitos_1$Autonomous_selfing_level=="none"] <- "Low-None"
levels(factor(datitos_1$Autonomous_selfing_level)) #Ok 
levels(factor(datitos_1$Flower_morphology)) 
datitos_1$Flower_morphology[datitos_1$Flower_morphology=="Funnelform"] <- "Campanulate"
datitos_1$Flower_morphology[datitos_1$Flower_morphology=="Spike"] <- "Brush"
levels(factor(datitos_1$Flower_morphology)) #OK
levels(factor(datitos_1$Flower_symmetry)) 
datitos_1$Flower_symmetry[datitos_1$Flower_symmetry=="actinomorphic"] <- "Actinomorphic"
datitos_1$Flower_symmetry[datitos_1$Flower_symmetry=="zygomorphic"] <- "Zygomorphic"
levels(factor(datitos_1$Flower_symmetry)) #OK
levels(factor(datitos_1$life_form)) 
datitos_1$life_form[datitos_1$life_form=="herb"] <- "Herb"
datitos_1$life_form[datitos_1$life_form=="shrub"] <- "Shrub"
datitos_1$life_form[datitos_1$life_form=="tree"] <- "Tree"
datitos_1$life_form[datitos_1$life_form=="vine"] <- "Shrub"
levels(factor(datitos_1$life_form)) #OK
levels(factor(datitos_1$lifespan)) #OK
levels(factor(datitos_1$Nectar_presence_absence))
datitos_1$Nectar_presence_absence[datitos_1$Nectar_presence_absence=="yes"] <- "Yes"
datitos_1$Nectar_presence_absence[datitos_1$Nectar_presence_absence=="no"] <- "No"
levels(factor(datitos_1$Nectar_presence_absence)) #OK


df <- as.data.frame(datitos_1)

#Now quantitative variables
########################################################################################################################################################
#Visits quantitative 
########################################################################################################################################################
v_df <- df[c("Visits", "Autonomous_selfing_level_fruit_set","Flowers_per_plant","Corolla_diameter_mean","Style_length","Ovule_number",
           "Plant_height_mean_m","Nectar_ul","Nectar_concentration", "Pollen_per_flower")]


colnames(v_df) <- c("Visits", "Aut. selfing", "Flowers per plant", "Flower width", "Style length (mm)",
                     "Ovule number","Plant height (m)", "Microlitres of nectar", "Nectar concentration (%)","Pollen grains per flower")


v_df$Visits <- log10(v_df$Visits+1)
set.seed(2)
tree <- rpart(Visits~., data=v_df, cp=0.009,minbucket = 10) 


printcp(tree)
plotcp(tree) 

set.seed(2)
tree1 <- rpart(Visits~., data=v_df, cp=0.0095256,minbucket = 50) #7 splits is lower than the best tree plus on sd (rule of thumb for selecting trees)
rpart.plot(tree1, box.palette="GnOr")

printcp(tree1)
plotcp(tree1)

########################################################################################################################################################
#Normalize degree
########################################################################################################################################################
nd_df <- df[c("normalised.degree", "Autonomous_selfing_level_fruit_set","Flowers_per_plant","Corolla_diameter_mean","Style_length","Ovule_number",
                  "Plant_height_mean_m","Nectar_ul","Nectar_concentration", "Pollen_per_flower")]

colnames(nd_df) <- c("Normalized degree", "Aut. selfing", "Flowers per plant", "Flower width", "Style length (mm)",
                      "Ovule number","Plant height (m)",  "Microlitres of nectar", "Nectar concentration (%)","Pollen grains per flower")

set.seed(2)
tree <- rpart(`Normalized degree`~., data=nd_df, cp=0.005) #6node divisons
printcp(tree)
plotcp(tree)

set.seed(2)
tree2 <- rpart(`Normalized degree`~., data=nd_df, cp=0.0125459) #6node divisons it is within on sd + error of best tree
rpart.plot(tree2, box.palette="GnOr")


printcp(tree2)


#Again low node error, good predictive power of the tree

########################################################################################################################################################
#Specialization
########################################################################################################################################################
d_df <- df[c("d", "Autonomous_selfing_level_fruit_set","Flowers_per_plant","Corolla_diameter_mean","Style_length","Ovule_number",
                  "Plant_height_mean_m","Nectar_ul","Nectar_concentration", "Pollen_per_flower")]

colnames(d_df) <- c("Specialization", "Aut. selfing", "Flowers per plant", "Flower width", "Style length (mm)",
                     "Ovule number","Plant height (m)",  "Microlitres of nectar", "Nectar concentration (%)","Pollen grains per flower")


set.seed(2)
tree <- rpart(Specialization~., data=d_df, cp=0.005) #6node divisons
printcp(tree)
plotcp(tree)

set.seed(2)
tree3 <- rpart(Specialization~., data=d_df, cp=0.0171878) #7node divisons within the best xerror plus sd
rpart.plot(tree3, box.palette="GnOr")


#Save data to plot it in an rmd file
saveRDS(v_df, "Data/RData/log_visits_subset_nectar_pollen_tree.rds")
saveRDS(nd_df, "Data/RData/normalized_degree_subset_nectar_pollen_tree.rds")
saveRDS(d_df, "Data/RData/specialization_subset_nectar_pollen_tree.rds")


rpart.plot(tree1, box.palette="GnOr", main="Interaction frequency", cex.main=2) 
rpart.plot(tree2, box.palette="GnOr", main="Normalized degree", cex.main=2)
rpart.plot(tree3, box.palette="GnOr", main="Specialization", cex.main=2)


