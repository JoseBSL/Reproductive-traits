########################################################################################################################################################
#SCRIPT TO CALCULATE THE REGRESSION TREE OF NECTAR AND PLOT IT
########################################################################################################################################################

library(rpart)
library(rpart.plot)

#READ NETWORK VISITATION DATA (VISITS AGGREGATED PER PLANT SPECIES)
long_d <- read.csv("Data/Csv/long_format_quantitative_networks.csv", row.names = 1) #quantitative network data|weighted by frequency of visits per plant species

#Select data with interaction greater than 0
long_d_1 <- long_d[long_d$Interaction>0,]

#Remove NA'S
long_d_2 <- long_d_1[!is.na(long_d_1$guild),] #I do it by guild because just these 6 guilds are named in this column

#check levels
levels(factor(long_d_2$guild))
########################################################################################################################################################
#NOW READ NECTAR DATA
forest_data <- read.csv("Data/Csv/nectar_pollen_subset_imputed_trait_data.csv")
str(forest_data)
nrow(forest_data)
colnames(forest_data)[1] <- "Plant_species"
########################################################################################################################################################
#MERGE DATA
data <- merge(long_d_2, forest_data, by="Plant_species")
nrow(data)


########################################################################################################################################################
#VISITATION DATA REGRESSION TREE
########################################################################################################################################################
data_1 <- data[c("Interaction","Autonomous_selfing_level_fruit_set","Corolla_diameter_mean","Corolla_length_mean",
                 "Flowers_per_plant","Style_length","Ovule_number","Plant_height_mean_m","Nectar_concentration",
                 "Nectar_mg","Nectar_ul","Pollen_per_flower")]

#Set colnames for plotting
colnames(data_1) <- c("Interactions", "Aut. selfing", "Flower width","Flower length" , "Flowers per plant","Style length (mm)",
                      "Ovule number","Plant height (m)", "Nectar_concentration","Mg of nectar","Microlitres of nectar","Pollen_per_flower")


#Visitation data regression tree
data_1$Interactions <- log10(data_1$Interactions+1) #Log transform to be able to run the tree with low error, it doesnt like non-normal dist

# Create the decision tree model
tree <- rpart(Interactions~., data=data_1, cp=.001)

#check number of splits and error
printcp(tree)
plotcp(tree)

#select optimal value for plotting cp=0.0043638, 10 splits
tree <- rpart(Interactions~., data=data_1, cp=0.0043638)

# Visualize the decision tree with green-orange pattern of color with rpart.plot
rpart.plot(tree, box.palette="GnOr")
########################################################################################################################################################
#NORMALIZED DEGREE REGRESSION TREE
data_1 <- data[c("Interaction","Autonomous_selfing_level_fruit_set","Corolla_diameter_mean","Corolla_length_mean",
                 "Flowers_per_plant","Style_length","Ovule_number","Plant_height_mean_m","Nectar_concentration",
                 "Nectar_mg","Nectar_ul","Pollen_per_flower")]

#Set colnames for plotting
colnames(data_1) <- c("Interactions", "Aut. selfing", "Flower width","Flower length" , "Flowers per plant","Style length (mm)",
                      "Ovule number","Plant height (m)", "Nectar_concentration","Mg of nectar","Microlitres of nectar","Pollen_per_flower")





