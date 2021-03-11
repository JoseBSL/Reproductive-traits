########################################################################################################################################################
#SCRIPT TO CALCULATE FUNCTIONAL GROUPS ##(METHOD:HCLUST)##

#1)READ TAIT IMPUTED DATA FOR ALL SPECIES (created in 2_Trait_Data_Imputation)

#2)SCALE VARIABLES

#3)CALCULATE GOWER DISTANCE

#4)FIND OPTIMAL NUMBER OF FUNCTIONAL GROUPS 

#5)PLOT DENDROGRAMS WITH OPTIMAL NUMBER OF CLUSTERS 

#6)SAVE DATA 
########################################################################################################################################################

#LOAD LIBRARIES
library(cluster)
library(NbClust)
library(maptree) #calculate clusters
library(FD)
library(bbmle)
library(spdep)
library(RANN)
library(missMDA)
library(Rtsne)
library(dplyr)
library(ggplot2)
library(dendextend)
########################################################################################################################################################
#1)READ TRAIT DATA
########################################################################################################################################################

#load data
trait_data <- read.csv("Data/Csv/all_species_imputed_trait_data_forest_data.csv", row.names = "Species_all") #set spp names as rownames 
#trait_data <- read.csv("Data/Csv/all_species_imputed_trait_data_famd_data.csv", row.names = "Species_all") #set spp names as rownames 

#select columns to calculate Gower distance (remove genus, order and family)
trait_data <- trait_data[,-c(1:4)]
rownames(trait_data) <- gsub("Species_all_", "", rownames(trait_data))
str(trait_data) #check data structure

########################################################################################################################################################
#2)SCALE VARIABLES
########################################################################################################################################################
trait_data[,c(4,7:13,16)] <- scale(mutate_all(trait_data[,c(4,7:13,16)], function(x) as.numeric(as.character(x))))

########################################################################################################################################################
#3)CALCULATE GOWER DISTANCE
########################################################################################################################################################

#Give weights to the different traits
w <- c(0.125,	#breeding system
       0.042,	0.042,0.042, #selfing/compatibility
       0.063,	0.063, #flower morphology/symmetry
       0.025,0.025,0.025,0.025,0.025, #floral investment  	
       0.125, #style length
       0.125, #ovule number
       0.042,0.042,0.042, #life form
       0.125) #nectar

#calculate gowers distance for all species
g.dist <- gowdis(trait_data)

########################################################################################################################################################
#4)FIND OPTIMAL NUMBER OF FUNCTIONAL GROUPS
########################################################################################################################################################
noclus <- hclust(g.dist, method="ward.D2")
b <- kgs(noclus,g.dist, maxclust=21)#5 clusters has lowest penalty score
plot(names (b), b, xlab="Number of Clusters", ylab="Penalty score")

########################################################################################################################################################
#5)PLOT DENDROGRAMS WITH OPTIMAL NUMBER OF CLUSTERS 
########################################################################################################################################################

#########
# HCLUST 5 clusters 
#########
e.clust_5 <- hclust(g.dist, method="ward.D2")
plot(e.clust_5, main = "Cluster dengrogram based on effect traits",cex = 0.08)
cut.g_5 <- readline("5")
cut.g_5 <- as.integer(cut.g_5)
e.gr_5 <- cutree(e.clust_5, k = 5)
e.gr_5_1 <- rect.hclust(e.clust_5, k = 5, border = "red")
#summary of clusters
#Check clusters
hclust_5 <- trait_data  %>%mutate(cluster = as.factor(e.gr_5)) %>% group_by(cluster) %>% do(the_summary = summary(.))
hclust_5$the_summary
#visualize clusters
tsne_obj <- Rtsne(g.dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%data.frame() %>%setNames(c("X", "Y")) %>%mutate(cluster = as.factor(e.gr_5))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster))
########################################################################################################################################################
#Plot dendrogram
########################################################################################################################################################
newggplot.ggdend <- function (data, segments = TRUE, labels = TRUE, nodes = TRUE, 
                              horiz = FALSE, theme = theme_dendro(), offset_labels = 0, ...) {
  data <- prepare.ggdend(data)
  #angle <- ifelse(horiz, 0, 90)
  #hjust <- ifelse(horiz, 0, 1)
  p <- ggplot()
  if (segments) {
    p <- p + geom_segment(data = data$segments, aes_string(x = "x", y = "y", xend = "xend", yend = "yend", colour = "col", linetype = "lty", size = "lwd"), lineend = "square") + 
      guides(linetype = FALSE, col = FALSE) + scale_colour_identity() + 
      scale_size_identity() + scale_linetype_identity()
  }
  if (nodes) {
    p <- p + geom_point(data = data$nodes, aes_string(x = "x", y = "y", colour = "col", shape = "pch", size = "cex")) + 
      guides(shape = FALSE, col = FALSE, size = FALSE) + 
      scale_shape_identity()
  }
  if (labels) {
    data$labels$cex <- 5 * data$labels$cex
    data$labels$y <- data$labels$y + offset_labels
    p <- p + geom_text(data = data$labels, aes_string(x = "x", y = "y", label = "label", colour = "col", size = "cex", angle = "angle", hjust = "hjust", vjust = "vjust"))#edited
  }
  if (horiz) {
    p <- p + coord_flip() + scale_y_reverse(expand = c(0.2, 0))
  }
  if (!is.null(theme)) {
    p <- p + theme
  }
  p
}

assignInNamespace(x = "ggplot.ggdend", ns = "dendextend", value = newggplot.ggdend)

dendro <- as.dendrogram(e.clust_5)

gdend <- dendextend::as.ggdend(dendro %>%
                                 set('branches_k_color', k = 5) %>%
                                 set('branches_lwd', 0.25) %>%
                                 set('labels_colors', k = 5) %>%
                                 set('labels_cex', 0.037),
                               theme = theme_minimal(),
                               horiz = TRUE)
gdend$labels$angle <- seq(90, -270, length = nrow(gdend$labels))
gdend$labels$vjust <- cos(gdend$labels$angle * pi) / (180)
gdend$labels$hjust <- sin(gdend$labels$angle * pi) / (180)



ggplot(gdend,offset_labels=-0.05) + theme(panel.grid.major = element_blank(),
                                          axis.text = element_blank(),
                                          axis.title = element_blank())+ coord_polar(theta = 'x') +  scale_y_reverse(expand = c(0.025, 0)) +
  annotate(geom="text", x=165, y=-0.5, label="A",color="red",angle = -35)

########################################################################################################################################################
#6)SAVE DATA
########################################################################################################################################################

#SAVE 5 CLUSTERS
#The order is still the same (I have checked it previously) so I can cbind the output and trait data
trait_data_5 <- cbind(trait_data, e.gr_5)
head(trait_data_5)
#change colname
names(trait_data_5)[names(trait_data_5) == "e.gr_5"] <- "Clusters"
#Write csv
#write.csv(trait_data_5, "Data/Csv/imputed_trait_data_hclust_5_clusters_famd.csv") 
write.csv(trait_data_5, "Data/Csv/imputed_trait_data_hclust_5_clusters_forest_data.csv") 

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
newggplot.ggdend <- function (data, segments = TRUE, labels = TRUE, nodes = TRUE, 
                              horiz = FALSE, theme = theme_dendro(), offset_labels = 0, ...) {
  data <- prepare.ggdend(data)
  #angle <- ifelse(horiz, 0, 90)
  #hjust <- ifelse(horiz, 0, 1)
  p <- ggplot()
  if (segments) {
    p <- p + geom_segment(data = data$segments, aes_string(x = "x", y = "y", xend = "xend", yend = "yend", colour = "col", linetype = "lty", size = "lwd"), lineend = "square") + 
      guides(linetype = FALSE, col = FALSE) + scale_colour_identity() + 
      scale_size_identity() + scale_linetype_identity()
  }
  if (nodes) {
    p <- p + geom_point(data = data$nodes, aes_string(x = "x", y = "y", colour = "col", shape = "pch", size = "cex")) + 
      guides(shape = FALSE, col = FALSE, size = FALSE) + 
      scale_shape_identity()
  }
  if (labels) {
    data$labels$cex <- 5 * data$labels$cex
    data$labels$y <- data$labels$y + offset_labels
    p <- p + geom_text(data = data$labels, aes_string(x = "x", y = "y", label = "label", colour = "col", size = "cex", angle = "angle", hjust = "hjust", vjust = "vjust"))#edited
  }
  if (horiz) {
    p <- p + coord_flip() + scale_y_reverse(expand = c(0.2, 0))
  }
  if (!is.null(theme)) {
    p <- p + theme
  }
  p
}

assignInNamespace(x = "ggplot.ggdend", ns = "dendextend", value = newggplot.ggdend)

dend <- as.dendrogram(e.clust_5)


gdend <- dendextend::as.ggdend(dend %>%
                                 set('branches_k_color', k = 5) %>%
                                 set('branches_lwd', 0.6) %>%
                                 set('labels_colors', k = 5) %>%
                                 set('labels_cex', 0.6),
                               theme = theme_minimal(),
                               horiz = TRUE)
gdend$labels$angle <- seq(90, -270, length = nrow(gdend$labels))
gdend$labels$vjust <- cos(gdend$labels$angle * pi) / (180)
gdend$labels$hjust <- sin(gdend$labels$angle * pi) / (180)


ggd1 <- ggplot(gdend)
ggd1 <- ggd1 + theme(panel.grid.major = element_blank(),
                     axis.text = element_blank(),
                     axis.title = element_blank())
ggd1 <- ggd1 + ylim(max(get_branches_heights(dend)), -3)
ggd1
ggd1 + coord_polar(theta = 'x') 
