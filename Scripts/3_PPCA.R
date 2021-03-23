########################################################################################################################################################
#SCRIPT TO CALCULATE THE PCA

#1) LOAD DATA 

########################################################################################################################################################
#LOAD LIBRARIES
library(phytools)
library(ape) #for phylogenetic distance
library(dplyr) #data processing
library(rtrees) #for phylogenetic distancelibrary(MASS)
library(reshape2)
library(viridis) #COLOUR GGPLOT
library(MASS)
library(ggplot2)
library(broman) #crayon colours
########################################################################################################################################################
#1) LOAD DATA
########################################################################################################################################################
#read data with missing values filled by data imputation
dat <- read.csv("Data/Csv/all_species_imputed_trait_data_forest_data.csv", row.names = "X")
dat_1 <- read.csv("Data/Csv/imputed_trait_data_hclust_5_clusters_forest_data.csv", row.names = "X") 

dat$Clusters <- dat_1$Clusters
########################################################################################################################################################
#2) Tidy up data to get phylo distance and conduct PCA
########################################################################################################################################################
########################################################################################################################################################
#remove not found species, cannot do PCA with unequal numbers of rows
cols.num <- c("Family_all","Genus_all","Species_all")
dat[cols.num] <- sapply(dat[cols.num],as.character)
dat$Species_all <- gsub("Species_all_", "", dat$Species_all)
dat <- dat[!dat$Species_all == "Diospyros seychellarum", ]
dat <- dat[!dat$Species_all == "Memecylon eleagni", ]
dat <- dat[!dat$Species_all == "Ocotea laevigata", ]
dat <- dat[!dat$Species_all == "Soulamea terminaloides", ]
########################################################################################################################################################
#3) REMOVE OUTLIERS, OUT OF 2.5-97.5 RANGE
########################################################################################################################################################
dat_cleaning <- dat[,c(2,3,4,8,11,14,16,17,20,22)]

dat_cleaning_1 <- dat_cleaning %>%
  filter(between(Flowers_per_plant, quantile(Flowers_per_plant, 0.025), quantile(Flowers_per_plant, 0.975)))

dat_cleaning_2 <- dat_cleaning_1 %>%
  filter(between(Corolla_diameter_mean, quantile(Corolla_diameter_mean, 0.025), quantile(Corolla_diameter_mean, 0.975)))

dat_cleaning_3 <- dat_cleaning_2 %>%
  filter(between(STYLE_IMPUTED, quantile(STYLE_IMPUTED, 0.025), quantile(STYLE_IMPUTED, 0.975)))

dat_cleaning_4 <- dat_cleaning_3 %>%
  filter(between(OVULES_IMPUTED, quantile(OVULES_IMPUTED, 0.025), quantile(OVULES_IMPUTED, 0.975)))

dat_cleaning_5 <- dat_cleaning_4 %>%
  filter(between(IMPUTED_plant_height_mean_m, quantile(IMPUTED_plant_height_mean_m, 0.025), quantile(IMPUTED_plant_height_mean_m, 0.975)))

#dat_cleaning_6 <- dat_cleaning_5 %>%
# filter(between(Autonomous_selfing_level_fruit_set, quantile(Autonomous_selfing_level_fruit_set, 0.025), quantile(Autonomous_selfing_level_fruit_set, 0.975)))


#LOG all columns, seems neccesary to standardize skewed data


dat_cleaning_5[,c(4:9)] <- log(dat_cleaning_5[,c(4:9)]+1)
dat_cleaning_5[,c(4:9)] <- scale(dat_cleaning_5[,c(4:9)], center = T, scale = T)


final_d <- dat_cleaning_5[,c(4:9)]


########################################################################################################################################################
#4) GET PHYLO
########################################################################################################################################################
#calculate phylo 
phylo <- as.data.frame(cbind(dat_cleaning_5$Family_all, dat_cleaning_5$Genus_all, dat_cleaning_5$Species_all))
colnames(phylo) <-  c("family", "genus", "species")
#Select unique cases
#phylo_2 <- phylo[!duplicated(phylo$species),]
phylo_2 <- tibble(phylo)
#get phylo
phylo_output <- get_tree(sp_list = phylo_2, tree = tree_plant_otl, taxon = "plant")
str(phylo_output)
#Convert phylogenetic tree into matrix
A_5 <- vcv.phylo(phylo_output)
#Standardize to max value 1
A_5 <- A_5/max(A_5)
#Unify column names; remove underscore and remove asterik
rownames(A_5) <- gsub("\\*", "", rownames(A_5))
colnames(A_5) <- gsub("\\*", "", colnames(A_5))
colnames(A_5) <- gsub("_", " ", colnames(A_5))
rownames(A_5) <- gsub("_", " ", rownames(A_5))


########################################################################################################################################################
#4) CALCULATE PPCA
########################################################################################################################################################
#set same rownames
final_d <-  dat_cleaning_5[,c(4:9)]

rownames(final_d) <- dat_cleaning_5$Species_all
#fix species names
rownames(final_d) <- gsub(" ", "_", rownames(final_d))

#Output saved not RUN
#phyl_pca_famd_1 <- phyl.pca(phylo_output, final_d,method="lambda",mode="cov")
phyl_pca_forest_1 <- phyl.pca(phylo_output, final_d,method="lambda",mode="cov")


####
#SAVE PHYLO PCA OUTPUT
####
#saveRDS(phyl_pca_famd, "Data/RData/phyl_pca_famd.rds")
saveRDS(phyl_pca_forest_1, "Data/RData/phyl_pca_forest.rds")
#SAVE ALSO DATA TO PLOT IT IN RMD file
#saveRDS(dat_cleaning_5, "Data/RData/data_all_species_for_rmd_plot_ppca_CLUSTERS.rds")

####
#READ DATA
####
phyl_pca_famd <- readRDS("Data/RData/phyl_pca_famd.rds")
phyl_pca_forest <- readRDS("Data/RData/phyl_pca_forest.rds")


#CALL the output PC for simplicity
PC <- phyl_pca_forest
#CHECK CONTENT
#EIGENVALUES
PC$Eval
#PC score (POINTS)
PC$S
#PC loadings (ARROWS)
PC$L


  
########################################################################################################################################################
#4) PLOT PPCA
########################################################################################################################################################

#####
#MASTER FUNCTION TO PLOT 
#####
#seems massive but in reality is a scatterplot with arrows
#it has inside how to compute kernel density but do not think I'll use it
#point density seems to do the job

PCbiplot <- function(PC, x="PC1", y="PC2") {
  # PC being a prcomp object
  data <- data.frame(PC$S)
  plot <- ggplot(data, aes_string(x=x, y=y)) #generate plot
  dat <- data.frame(x = data[,x], y = data[,y])
  
  #######
  #DENSITY FUNCTION
  #######
  get_density <- function(x, y, ...) {
    dens <- MASS::kde2d(x, y, ...)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
  }
  
  dat$density <- get_density(dat$x, dat$y, h = c(2, 2), n = 1000) #obtain density

  plot <- plot + geom_point(data=dat, aes(-x, -y),size=0.65)+scale_color_manual(values=c("#fc2847", "#1cac78")) #+ #scale_color_viridis_c(option = "A", direction = 1, limits = c(min(dat$density), max(dat$density)))+
 # plot <- plot +geom_point(data=dat, aes(-x, -y, colour = density),size=0.65,shape = 1,colour = "black",alpha=0.8)
  
  ########
  #ADD ARROWS 
  ########
  datapc <- data.frame(PC$L) #CREATE DATAFRAME WITH LOADINGS
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .5 * mult * (get(x)),
                      v2 = .5 * mult * (get(y))
  )
  # add arrows
  plot <- plot + geom_segment(data=datapc,linejoin="round", lineend="round",aes(x=0, y=0, xend=-v1, yend=-v2),size=1, arrow=arrow(length=unit(0.5,"cm")), alpha=1, color="brown4")
  
  #Add axis with perctentage
  percentage <- round(diag(PC$Eval) / sum(PC$Eval) * 100, 2) #calculate percentage
  
  plot <- plot + xlab(paste("PC1 ", "(",(percentage[1]),")","%", sep = "")) #XLAB
  plot <- plot + ylab(paste("PC2 ", "(",(percentage[2]),"%",")", sep = "")) #YLAB
  plot <- plot + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                       panel.border = element_rect(linetype = "solid", colour = "black", size=1))
  
  
  #ADD THE OTHER DIRECTION OF THE SEGMENT BECAUSE LOOKS COOL
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2),size=0.6, arrow=arrow(length=unit(0,"cm")),linetype=2, alpha=0.8, color="black")
  
  #ADD LABELS
  rownames(PC$L) <- c("Selfing", "Flower Number", "Flower size", "Style Length", "Ovule Number", "Plant Height" )
  
  PCAloadings <- data.frame(Variables = rownames(PC$L), PC$L)
  plot <- plot + annotate("text", x = -(PCAloadings$PC1*c(3.5,4,4,4,4,4)), y = -(PCAloadings$PC2*c(3,4,4,4,4,4)),
                          label = PCAloadings$Variables, color="black",size=5)
  
  #CHANGE THEME
  
  plot <- plot + theme_bw()
  
  #CALCULATE KERNELS
  #mv.kde <- kde2d(data[,1], data[,2], n = 400)
  #dx <- diff(mv.kde$x[1:2])  # lifted from emdbook::HPDregionplot()
  #dy <- diff(mv.kde$y[1:2])
  #sz <- sort(mv.kde$z)
  #c1 <- cumsum(sz) * dx * dy
  
  # specify desired contour levels:
  #prob <- c(0.5)
  #prob_1 <- c(0.90)
  # plot:
  #dimnames(mv.kde$z) <- list(mv.kde$x,mv.kde$y)
  #dc <- melt(mv.kde$z)
  
  #dc$prob <- approx(sz,1-c1,dc$value)$y
  
  #plot <- plot + geom_contour(data=dc, aes(x=-Var1,y=-Var2,z=prob),colour="brown4",breaks=prob)
  # plot:
  #dimnames(mv.kde$z) <- list(mv.kde$x,mv.kde$y)
  #dc <- melt(mv.kde$z)
  
  #dc$prob_1 <- approx(sz,1-c1,dc$value)$y
  #plot <- plot + geom_contour(data=dc, aes(x=-Var1,y=-Var2,z=prob_1),colour="black",breaks=prob_1)
  
  
  plot
  
}


PCbiplot(PC)
