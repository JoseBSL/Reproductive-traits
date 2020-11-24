########################################################################################################################################################
#SCRIPT TO CALCULATE Z-SCORES, MERGE WITH TRAIT DATA AND MERGE WITH FUNCTIONAL GROUPS (JUST SUBSET OF QUANTITATIVE NETWORKS)

#1) LOAD LONG FORMAT QUANTITATIVE NETWORKS WITH POLL GUILDS

#2) CALCULATE Z-SCORES

#3) SAVE DATA
########################################################################################################################################################

#LOAD LIBRARIES
library(data.table)

########################################################################################################################################################
#1) LOAD LONG FORMAT DATA, TRAIT DATA AND FUNCTIONAL GROUP DATA
########################################################################################################################################################
long_d <- read.csv("Data/Csv/long_format_quantitative_networks.csv")
t_data <- read_excel("Data/Trait_data_raw/Trait_data_final.xlsx")
hclust_d <- read.csv("Data/Csv/imputed_trait_data_hclust_5_clusters.csv") 

########################################################################################################################################################
#2) CALCULATE Z-SCORES
########################################################################################################################################################

#Select data with interaction greater than 0
long_d_1 <- long_d[long_d$Interaction>0,]

#Calculate Z-scores BY NETWORK (ID)!!
long_d_1 <- data.table(long_d_1)
long_d_1[, Z_scores := scale(Interaction,center = TRUE, scale = TRUE), by = Id]

#Remove other orders/guilds that are not these ones
long_d_2 <- long_d_1[!is.na(long_d_1$guild),] #I do it by guild because just these 6 guilds are named in this column
#check levels
levels(long_d_2$guild) #6 DIFFERENT GUILDS

########################################################################################################################################################
#3) MERGE WITH TRAIT DATA
########################################################################################################################################################
write.csv(long_d_2, "Data/Csv/long_format_quantitative_networks_Z_scores.csv")

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################


