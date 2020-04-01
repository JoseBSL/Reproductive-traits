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
data2 <- data_wide[,c(2:29)]
data2 <- data2[,order(data2[1,], decreasing=T)]
data2 <- as.matrix(data2)
colnames(data2) <- c("Ramirez 1989", "Ramirez 1992", "Bartomeus unp. 2015", "Chacoff 2011","Bartomeus 2008", "Bartomeus 2008", "Robertson 1929", "Small 1976",
                     "Inouye 1990", "Souza 2018", "Bundgaard 2003", "Inouye 1988", "Dupont 2009", "Fang huang 2012", "mcmullen 1993", "Trivellone Unp.", "Dicks 2002",
                     "Elberling 1999", "Primack 1983", "Primack 1983", "Kato 2000", "Kaiser-Bunbunry 2009", "Traveset 2013", "Kevan 1970", "Bek 2006",
                     "Primack 1983", "Kaiser-Bunbury 2014", "Lundgren 2005")

saveRDS(data2, "Data/RData/Spp_richness_by_order.RData")

par(mar = c(11,6.1,4.1,8))
barplot(data2, beside = FALSE, ylab = "% visits", cex.names = 0.5
        ,las = 2, col = clrs)
abline (h = 0)
par(xpd=TRUE)
leg <- c("Hymenoptera", "Coleoptera", "Lepidoptera", "Diptera", "Hemiptera")
legend(x = 35, y = 100, legend = rev(leg), cex = 0.4, fill = rev(clrs))
par(mar = c(5,1.1,4.1,2.1))
par(xpd=FALSE)


