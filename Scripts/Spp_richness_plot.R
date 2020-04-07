#Here I'm going to show graphically the main taxa per network

library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(reshape2)

#Load data
data <- read.csv("Data/Data_processing/Long_format_metawebs_poll_taxa_added.csv")

#Unique species per network
data_network <-  unique(data[,c("Pollinator_species","Id","Pollinator_family", "Pollinator_order")])
#There are some mistakes in the families
#Not very relevant at the moment, fix later

#Sum unique species per network (Spp richness)
data_network_sum <- dcast(data_network,Id~Pollinator_order,value.var = "Pollinator_order")

data_network_main_orders <- select(data_network_sum, "Id", "Hymenoptera","Coleoptera","Lepidoptera", "Diptera", "Hemiptera") 
str(data_network_main_orders)

#Adding the summatory per row
data_network_main_orders$total_col <- rowSums(data_network_main_orders[,-1])
data_network_main_orders[,c(2:6)] <- data.frame(sapply(data_network_main_orders[,c(2:6)], function(x) as.numeric(as.character(x))))

#Checking if it worked
str(data_network_main_orders)
#Calculate percentage of main orders
data_network_main_orders[,c(2:6)] <- sapply(data_network_main_orders[,c(2:6)], function(x) {x /data_network_main_orders$total_col*100})

#Convert to long format for plotting
data_long <- gather(data_network_main_orders, orders, measurement, Hymenoptera:Hemiptera, factor_key=TRUE)

#Convert again to wideformat for plotting with barplot/for ggplot the last format is enough
data_long <- data_long[,c(-2)]
data_wide <- spread(data_long, Id, measurement)
#Select colours from a Palette that Nacho made for http://www.pnas.org/content/early/2015/11/24/1517092112.long
clrs <- c("darkblue", "blue", "yellow", "cornsilk", "goldenrod",
          "orange")
data2 <- data_wide[,c(2:31)]
data2 <- data2[,order(data2[1,], decreasing=T)]
data2 <- as.matrix(data2)
colnames(data2) <- c("Ramirez 1989", "Ramirez 1992", "Bartomeus unp. 2015", "Chacoff 2011","Bartomeus 2008 1", "Bartomeus 2008 2", "Robertson 1929", "Small 1976",
                     "Inouye 1990", "Souza 2018", "Bundgaard 2003", "Inouye 1988", "Dupont 2009 1", "Olesen 2002 1", "Fang huang 2012", "mcmullen 1993", "Olesen 2002 2", "Dicks 2002",
                     "Elberling 1999", "Primack 1983 1", "Primack 1983 2", "Kato 2000", "Kaiser-Bunbunry 2009", "Traveset 2013", "Kevan 1970", "Bek 2006",
                     "Primack 1983 3", "Kaiser-Bunbury 2014", "Dupont 2009 2", "Lundgren 2005")

saveRDS(data2, "Data/RData/Spp_richness_by_order.RData")

par(mar = c(16,6.1,4.1,8))
barplot(data2, beside = FALSE, ylab = "% visits", cex.names = 0.5
        ,las = 2, col = clrs)
abline (h = 0)
par(xpd=TRUE)
leg <- c("Hymenoptera", "Coleoptera", "Lepidoptera", "Diptera", "Hemiptera")
legend(x = 35, y = 100, legend = rev(leg), cex = 0.4, fill = rev(clrs))
par(mar = c(5,1.1,4.1,2.1))
par(xpd=FALSE)


#VISITATION BAR PLOT

#Now I'm going to repeat the process but with visitation
#For that I will subset the networks for the ones with visitation data (around 20 in total)

m <- read.csv("Data/Data_processing/metadata.csv")
data <- read.csv("Data/Data_processing/Long_format_metawebs_poll_taxa_added.csv")

#Removing .csv for merging the two datasets
data$Id <- gsub("\\..*","",data$Id)

#select columns of interest before merging

m <- m[,c(2,14)]
colnames(m)[1] <- "Id"

#merge two dataframes
data_m <- merge(m, data, by="Id")

#Now I have just the networks with visitation data
quantitative_net <- subset(data_m, Data_type=="Quantitative")

#In this case I do something different
#I sum the visits per order, we are using numerical data, before we were using the species 

net_sum <- dcast(quantitative_net,Id~Pollinator_order,value.var = "Interaction")

#Subset orders of interest
net_sum_subset <- select(net_sum, "Id", "Hymenoptera","Coleoptera","Lepidoptera", "Diptera", "Hemiptera") 

#Adding the summatory per row
net_sum_subset$total_col <- rowSums(net_sum_subset[,-1])

#Calculate percentage of visits from main orders

net_sum_subset[,c(2:6)] <- sapply(net_sum_subset[,c(2:6)], function(x) {x /net_sum_subset$total_col*100})

#Convert to long format for plotting
net_long <- gather(net_sum_subset, orders, measurement, Hymenoptera:Hemiptera, factor_key=TRUE)

#Convert again to wideformat for plotting with barplot/for ggplot the last format is enough
net_long <- net_long[,c(-2)]
net_wide <- spread(net_long, Id, measurement)
net_wide <- net_wide[,c(2:20)]
net_wide <- net_wide[,order(net_wide[1,], decreasing=T)]
net_wide2 <- as.matrix(net_wide)

#Select colours from a Palette that Nacho made for http://www.pnas.org/content/early/2015/11/24/1517092112.long
clrs <- c("darkblue", "blue", "yellow", "cornsilk", "goldenrod",
          "orange")

colnames(net_wide2) <- c("Bartomeus unp. 2015", "Chacoff 2011","Bartomeus 2008 1", "Bartomeus 2008 2","Small 1976",
                     "Inouye 1990", "Souza 2018", "Inouye 1988", "Dupont 2009 1", "Olesen 2002 1", "Fang huang 2012", "Olesen 2002 2", "Dicks 2002",
                     "Elberling 1999","Kato 2000", "Kaiser-Bunbunry 2009", "Traveset 2013", "Kaiser-Bunbury 2014","Lundgren 2005")
par(mar = c(16,6.1,4.1,8))
barplot(net_wide2, beside = FALSE, ylab = "% visits", cex.names = 0.5
        ,las = 2, col = clrs)
abline (h = 0)
par(xpd=TRUE)
leg <- c("Hymenoptera", "Coleoptera", "Lepidoptera", "Diptera", "Hemiptera")
legend(x = 35, y = 100, legend = rev(leg), cex = 0.4, fill = rev(clrs))
par(mar = c(5,1.1,4.1,2.1))
par(xpd=FALSE)


saveRDS(net_wide2, "Data/RData/Spp_visitation_per_order.RData")

#Try plot with ggplot to see wich one looks better



