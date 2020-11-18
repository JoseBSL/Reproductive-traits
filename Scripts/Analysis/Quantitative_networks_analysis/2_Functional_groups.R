
#This script assigns species to functional groups using weighted functional traits

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#load packages
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

require(cluster)
require(NbClust)
require(maptree)
require(FD)
require(bbmle)
require(spdep)
require(RANN)
library(missMDA)
#Plot dendro
library("ggplot2")
library("reshape2")
library("purrr")
library("dplyr")
# let's start with a dendrogram
library("dendextend")
library(data.table)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#READ DATA
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

trait_data <- read.csv("Data/Csv/quantitative_networks_trait_data.csv")
str(trait_data)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#PREPARE DATA
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#select columns of interest
t <- trait_data[c("Order_all","Family_all","Genus_all","Species_all","Breeding_system","IMPUTED_Compatibility","Autonomous_selfing_level",
"Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant", "Flowers_per_inflorescence",
"Floral_unit_width", "Corolla_diameter_mean", "Corolla_length_mean", "STYLE_IMPUTED", "OVULES_IMPUTED", "life_form", "lifespan",
"IMPUTED_plant_height_mean_m")]

#Breeding_system
t$Breeding_system <- as.character(t$Breeding_system)
t$Breeding_system[t$Breeding_system=="androdioecious"] <- "dioecious"
t$Breeding_system[t$Breeding_system=="androgynodioecious"] <- "dioecious"
t$Breeding_system[t$Breeding_system=="andromonoecious"] <- "monoecious"
t$Breeding_system[t$Breeding_system=="gynodioecious"] <- "dioecious"
t$Breeding_system[t$Breeding_system=="gynoecious"] <- "monoecious"
t$Breeding_system[t$Breeding_system=="gynomonoecious"] <- "monoecious"
t$Breeding_system[t$Breeding_system=="gynomonoecious"] <- "monoecious"
t$Breeding_system[t$Breeding_system=="monoecious_dioecious"] <- "monoecious"
t$Breeding_system[t$Breeding_system=="polygamo-dioecious"] <- "dioecious"
t$Breeding_system[t$Breeding_system=="protandrous"] <- "hermaphrodite"
t$Breeding_system[t$Breeding_system=="protogynous"] <- "hermaphrodite"
t$Breeding_system[t$Breeding_system=="subdioecious"] <- "dioecious"
t$Breeding_system[t$Breeding_system=="submonoecious"] <- "monoecious"
#change to capital letters
t$Breeding_system[t$Breeding_system=="monoecious"] <- "Monoecious"
t$Breeding_system[t$Breeding_system=="dioecious"] <- "Dioecious"
t$Breeding_system[t$Breeding_system=="hermaphrodite"] <- "Hermaphrodite"
levels(as.factor(t$Breeding_system))
t$Breeding_system <- as.factor(t$Breeding_system)

#Now 3 levels

#Flower_morphology
t$Flower_morphology <- as.character(t$Flower_morphology)

t$Flower_morphology[t$Flower_morphology=="bowl"] <- "open"
t$Flower_morphology[t$Flower_morphology=="dish"] <- "open"
t$Flower_morphology[t$Flower_morphology=="exposed"] <- "open"
t$Flower_morphology[t$Flower_morphology=="spadix"] <- "spike"
t$Flower_morphology[t$Flower_morphology=="open"] <- "Open"
t$Flower_morphology[t$Flower_morphology=="brush"] <- "Brush"
t$Flower_morphology[t$Flower_morphology=="campanulate"] <- "Campanulate"
t$Flower_morphology[t$Flower_morphology=="capitulum"] <- "Capitulum"
t$Flower_morphology[t$Flower_morphology=="funnelform"] <- "Funnelform"
t$Flower_morphology[t$Flower_morphology=="papilionaceous"] <- "Papilionaceous"
t$Flower_morphology[t$Flower_morphology=="spike"] <- "Spike"
t$Flower_morphology[t$Flower_morphology=="tube"] <- "Tube"
levels(as.factor(t$Flower_morphology))
t$Flower_morphology <- as.factor(t$Flower_morphology)

#Life span
t$lifespan <- as.character(t$lifespan )
t$lifespan[t$lifespan=="annual"] <- "short_lived"
t$lifespan[t$lifespan=="biennial"] <- "short_lived"
t$lifespan[t$lifespan=="short_lived"] <- "Short lived"
t$lifespan[t$lifespan=="perennial"] <- "Perennial"
levels(as.factor(t$lifespan))
t$lifespan <- as.factor(t$lifespan )


#check for NA's in species column
t_1 <- t[!is.na(t$Species_all),]
t_2 <- t_1[!is.na(t_1$Genus_all),]
t_3 <- t_2[!is.na(t_2$Family_all),]
t_4 <- t_3[!is.na(t_3$Order_all),]
str(t_4)
t_4$Flowers_per_inflorescence <- as.integer(t_4$Flowers_per_inflorescence)

#Explore patterns of missing data
missing_data <- unlist(lapply(t_4, function(x) sum(is.na(x))))/nrow(t)
sort(missing_data[missing_data >= 0], decreasing=T)
#Just one variable is above 50%
#Conduct data imputation
t_imputed <- imputeFAMD(t_4, ncp=3,threshold = 1e-06) 

head(t_imputed$completeObs)
#looks that it has been done well
#I'm going to fix the other two columns

t_imputed$completeObs$Family_all <- gsub("Family_all_", "", t_imputed$completeObs$Family_all)
t_imputed$completeObs$Genus_all <- gsub("Genus_all_", "", t_imputed$completeObs$Genus_all)
head(t_imputed$completeObs)

#SAVE DATA
write.csv(t_imputed$completeObs, "Data/Csv/quantitative_networks_imputed_trait_data.csv")

#Convert all columns of dataframe to factors
cols <- colnames(t_imputed$completeObs)
t_imputed$completeObs[cols] <- lapply(t_imputed$completeObs[cols], factor)
str(t_imputed$completeObs)


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#create Gower dissimilarity matrix (with traits weighted)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



#here starts jamies code again
#recall datframe 
t_dat <- t_imputed$completeObs
str(t_dat)

t_dat_no_duplicates <-  t_dat[!duplicated(t_dat$Species_all),]

rownames(t_dat_no_duplicates) <- t_dat_no_duplicates$Species_all
head(t_dat_no_duplicates)
#Remove order, family, genus and species for calculate clustering
t_dat_no_duplicates <- t_dat_no_duplicates[,-c(1:4)]
str(t_dat_no_duplicates)


#scale variables
t_dat_no_duplicates[,c(4,7:13,16)] <- scale(mutate_all(t_dat_no_duplicates[,c(4,7:13,16)], function(x) as.numeric(as.character(x))))
#check if it has been done properly
str(t_dat_no_duplicates)
#calculate gowers distance for all species
e.dist <- gowdis(t_dat_no_duplicates)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#find optimal number of functional groups
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
noclus <- agnes(t_dat_no_duplicates, method="ward")
b <- kgs(noclus,e.dist, maxclust=21)#6 clusters has lowest penalty score
plot(names (b), b, xlab="Number of Clusters", ylab="Penalty score")

noclus <- hclust(e.dist, method="ward.D2")
b <- kgs(noclus,e.dist, maxclust=21)#5 clusters has lowest penalty score
plot(names (b), b, xlab="Number of Clusters", ylab="Penalty score")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#silhouette plot
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pamx <- pam(e.dist, 10)
summary(pamx)
plot(pamx)
library("factoextra")
library("cluster")
pamx <- pam(e.dist, 5,diss = TRUE)
pamx$clustering

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#plot dendrogram
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

e.clust <- hclust(e.dist, method="ward.D2")
plot(e.clust, main = "Cluster dengrogram based on effect traits")
cut.g <- readline("5")
cut.g <- as.integer(cut.g)
e.gr <- cutree(e.clust, k = 5)
e.gr2 <- rect.hclust(e.clust, k = 5, border = "red")

trial <- as.data.frame(e.gr)
trial$spp <- rownames(trial)
trial_1 <- cbind(trial,t_dat_no_duplicates)
t_dat_no_duplicates$clusters <- e.gr
str(t_dat_no_duplicates)
#check number of levels of each cluster
table(as.factor(t_dat_no_duplicates$e.gr))
head(t_dat_no_duplicates)

#merge now with first dataframe with duplicates
library(data.table)
t_dat_no_duplicates <- setDT(t_dat_no_duplicates, keep.rownames = TRUE)[]
#select just species names and clusters to merge 
t_dat_no_duplicates_1 <- t_dat_no_duplicates[,c(1,18)] 
colnames(t_dat_no_duplicates_1)[1] <- "Species_all"
#merge two dataframes
t_dat_trial <- merge(trait_data , t_dat_no_duplicates_1, by="Species_all")
head(t_dat_trial)
#save data
write.csv(t_dat_trial,"Data/Csv/quantitative_networks_imputed_with_clusters.csv")


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Other resources for plotting clusters
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#THERE ARE DIFFERENT WAYS TO COMPUTE IT, TRY SOME
library(cluster)
#here starts jamies code again
#recall datframe 
t_dat <- t_imputed$completeObs
str(t_dat)

t_dat_no_duplicates <-  t_dat[!duplicated(t_dat$Species_all),]

rownames(t_dat_no_duplicates) <- t_dat_no_duplicates$Species_all
head(t_dat_no_duplicates)
#Remove order, family, genus and species for calculate clustering
t_dat_no_duplicates <- t_dat_no_duplicates[,-c(1:4)]
str(t_dat_no_duplicates)


#scale variables
t_dat_no_duplicates[,c(4,7:13,16)] <- scale(mutate_all(t_dat_no_duplicates[,c(4,7:13,16)], function(x) as.numeric(as.character(x))))
#check if it has been done properly
str(t_dat_no_duplicates)
gower.dist <- daisy(t_dat_no_duplicates, metric = c("gower"))
class(gower.dist) 

#following https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995 post
#------------ DIVISIVE CLUSTERING ------------#
divisive.clust <- diana(as.matrix(gower.dist), 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")

divisive.clust$dc

#------------ AGGLOMERATIVE CLUSTERING ------------#
# complete
aggl.clust.c <- hclust(gower.dist, method = "ward.D2")
plot(aggl.clust.c,
     main = "Agglomerative, ward.D2 linkages")


#Code to check clusting in a table format
library(fpc)
cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}
# I am capping the maximum amout of clusters by 7
# I want to choose a reasonable number, based on which I will be able to see basic differences between customer groups as a result
stats.df.divisive <- cstats.table(gower.dist, divisive.clust, 7)
stats.df.divisive

stats.aggl.clust.c <- cstats.table(gower.dist, aggl.clust.c, 7)
stats.aggl.clust.c

#Checking number of clusters
library(ggplot2)
# Elbow
# Divisive clustering
divisive_plot <- ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))), 
                        aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

# Agglomerative clustering,provides a more ambiguous picture
agglomerative_plot <- ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), 
                             aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))



dendro <- as.dendrogram(aggl.clust.c)
dendro.col <- dendro %>%
  set("branches_k_color", k = 5, value =   c("black", "darkslategray",  "darkcyan", "cyan3", "gold3")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.1)
ggd1 <- as.ggdend(dendro.col)
ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 7")

# Radial plot looks less cluttered (and cooler)
ggplot(ggd1, labels = T) + 
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")

#other way to determine number of clusters
library("Rtsne")

tsne_obj$N
tsne_obj <- Rtsne(gower.dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(t_dat_no_duplicates$e.gr),
         name = t_dat_no_duplicates$e.gr)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


#https://rpubs.com/marwahsi/tnse

tsne <- Rtsne(gower.dist, is_distance = TRUE)
colors = rainbow(length(unique(t_dat_no_duplicates$e.gr)))
names(colors) = unique(t_dat_no_duplicates$e.gr)
par(mgp=c(2.5,1,0))
plot(tsne$Y, t="n", main="tSNE", xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
points(tsne$Y, labels=t_dat_no_duplicates$e.gr, col=colors[t_dat_no_duplicates$e.gr])

tsne_plot <- function(perpl=30,iterations=500,learning=200){
  set.seed(1) # for reproducibility
  tsne <-Rtsne(gower.dist, is_distance = TRUE,dims = 2, perplexity=perpl, verbose=TRUE, max_iter=iterations, eta=learning) 
  plot(tsne$Y, t='n', main = print(paste0("perplexity = ",perpl, ", max_iter = ",iterations, ", learning rate = ",learning)), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=1, "cex.lab"=1.5)
  text(tsne$Y, labels=t_dat_no_duplicates$e.gr, col=colors[t_dat_no_duplicates$e.gr])
}
perplexity_values <- c(2,5,30,50,100)
sapply(perplexity_values,function(i){tsne_plot(perpl=i)})
learning_values <- c(20,200,2000)
sapply(learning_values,function(i){tsne_plot(learning=i)})


