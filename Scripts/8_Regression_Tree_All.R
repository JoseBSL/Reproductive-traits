########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
##REGRESSION TREE FOR ALL THE SPECIES
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

dat <- read.csv("Data/Csv/all_species_imputed_trait_data_forest_data.csv", row.names = "X")

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


datitos_2 <- as.data.frame(datitos_1)



#Now quantitative variables
########################################################################################################################################################
#VISITS REGRESSION TREE
########################################################################################################################################################
v_df_all <- datitos_2[c("Visits", "Autonomous_selfing_level_fruit_set","Flowers_per_plant","Corolla_diameter_mean", "Style_length","Ovule_number",
                  "Plant_height_mean_m")]


colnames(v_df_all) <- c("Visits", "Aut. selfing","Flowers per plant","Flower width", "Style length (mm)","Ovule number",
                                  "Plant height (m)")



v_df_all$Visits <- log10(v_df_all$Visits + 1)

set.seed(1)
tree <- rpart(Visits~., data=v_df_all, cp=0.001)
printcp(tree)
plotcp(tree)

set.seed(1)
tree_1 <- rpart(Visits~., data=v_df_all, cp=0.0089891)
rpart.plot(tree_1, box.palette="GnOr")

########################################################################################################################################################
#NORMALIZED DEGREE REGRESSION TREE
########################################################################################################################################################
nd_df_all <- datitos_2[c("normalised.degree", "Autonomous_selfing_level_fruit_set","Flowers_per_plant","Corolla_diameter_mean", "Style_length","Ovule_number",
                        "Plant_height_mean_m")]


colnames(nd_df_all) <- c("Normalized degree", "Aut. selfing","Flowers per plant","Flower width", "Style length (mm)","Ovule number",
                        "Plant height (m)")


set.seed(1)
tree <- rpart(`Normalized degree`~., data=nd_df_all, cp=0.001)
printcp(tree)
plotcp(tree)

set.seed(1)
tree_2 <- rpart(`Normalized degree`~., data=nd_df_all, cp=0.0159034) #7
rpart.plot(tree_2, box.palette="GnOr")

########################################################################################################################################################
#SPECIALIZATION REGRESSION TREE
########################################################################################################################################################
d_df_all <- datitos_2[c("d", "Autonomous_selfing_level_fruit_set","Flowers_per_plant","Corolla_diameter_mean", "Style_length","Ovule_number",
                         "Plant_height_mean_m")]


colnames(d_df_all) <- c("Specialization", "Aut. selfing","Flowers per plant","Flower width", "Style length (mm)","Ovule number",
                         "Plant height (m)")


set.seed(1)
tree_3 <- rpart(Specialization~., data=d_df_all, cp=0.001)
printcp(tree_3)
plotcp(tree_3)

set.seed(1)
tree_3 <- rpart(Specialization~., data=d_df_all, cp=0.0118611) #10
rpart.plot(tree_3, box.palette="GnOr")

#Save data to plot it in an rmd file
saveRDS(v_df_all, "Data/RData/log_visits_all_tree.rds")
saveRDS(nd_df_all, "Data/RData/normalized_degree_all_tree.rds")
saveRDS(d_df_all, "Data/RData/specialization_all_tree.rds")


t1 <- rpart.plot(tree_1, box.palette="GnOr", main="Interaction frequency", cex.main=2) 
t2 <- rpart.plot(tree_2, box.palette="GnOr", main="Normalized degree", cex.main=2)
t3 <- rpart.plot(tree_3, box.palette="GnOr", main="Specialization", cex.main=2)


cowplot::plot_grid(rpart.plot(tree_1, box.palette="GnOr", main="Interaction frequency", cex.main=2) +rpart.plot(tree_2, box.palette="GnOr", main="Normalized degree", cex.main=2))

par(mfrow=c(2,2))


