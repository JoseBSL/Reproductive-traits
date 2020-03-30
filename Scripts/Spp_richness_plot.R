#Here I'm going to show graphically the main taxa per network

library(dplyr)
library(tidyr)
library(plyr)


#Load data
data <- read.csv("Data/Data_processing/Long_format_metawebs_poll_taxa_added.csv")

#Unique species per network
data_network <-  unique(data[,c("Pollinator_species","Id","Pollinator_family", "Pollinator_order")])
#There are some mistakes in the families
#Not very relevant at the moment, fix later

#Sum unique species per network (Spp richness)
data_network_sum <- dcast(data_network,Id~Pollinator_order,value.var = "Pollinator_order")


data_network_main_orders <- select(data_network_sum, "Id", "Coleoptera","Lepidoptera","Hymenoptera", "Diptera") 
str(data_network_main_orders)

data_network_main_orders <- data_network_main_orders[,-1]
str(data_network_main_orders)
clrs <- c("darkblue", "blue", "yellow", "cornsilk")

data_long <- gather(data_network_main_orders, orders, measurement, Coleoptera:Diptera, factor_key=TRUE)

ggplot(data_long, aes(fill=orders, y=measurement, x=Id)) + 
  geom_bar(position="stack", stat="identity")




barplot(data_long, beside = FALSE, ylab = "", cex.names = 0.8
        , las = 2, col = clrs)

barplot(data_network_main_orders, main="Car Distribution by Gears and VS",
        xlab="Number of Gears",
        legend = rownames(counts))


# Stacked Bar Plot with Colors and Legend
counts <- table(mtcars$vs, mtcars$gear)
str(counts)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts))





library(ggplot2)

# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

# Stacked
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity")

library(tidyr)

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
data_long <- gather(data_network_main_orders, orders, measurement, Coleoptera:Diptera, factor_key=TRUE)

