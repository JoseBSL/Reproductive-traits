########################################################################################################################################################
#SCRIPT TO CALCULATE THE PCA

#1) LOAD DATA 

########################################################################################################################################################
#LOAD LIBRARIES
library(phytools)
library(ape) #for phylogenetic distance
library(dplyr) #data processing
library(rtrees) #for phylogenetic distance
library(DHARMa)
library(brms)
library(cmdstanr)
library(ggplot2)
library(ape)
library(phylobase)
library(adephylo)
library(ggpubr)
library(kgc)
library(MASS)
library(reshape2)
library(viridis)
########################################################################################################################################################
#1) LOAD DATA
########################################################################################################################################################
#read data with missing values filled by data imputation
dat <- read.csv("Data/Csv/all_species_imputed_trait_data.csv", row.names = "X")
########################################################################################################################################################
#2) Calculate phylogenetic distance
########################################################################################################################################################
########################################################################################################################################################
#remove not found species, cannot do PCA with unequal numbers of rows
dat <- dat[-c(414,480,482,634,705,1139),]
cols.num <- c("Family_all","Genus_all","Species_all")
dat[cols.num] <- sapply(dat[cols.num],as.character)
dat$Species_all <- gsub("Species_all_", "", dat$Species_all)
dat <- dat[!dat$Species_all == "Diospyros seychellarum", ]
dat <- dat[!dat$Species_all == "Memecylon eleagni", ]
dat <- dat[!dat$Species_all == "Ocotea laevigata", ]
dat <- dat[!dat$Species_all == "Soulamea terminaloides", ]

#prepare dataframe to calculate tree
phylo <- as.data.frame(cbind(dat$Family_all, dat$Genus_all, dat$Species_all))
colnames(phylo) <-  c("family", "genus", "species")

#Select unique cases
phylo_1 <- phylo[!duplicated(phylo$species),]
phylo_2 <- tibble(phylo_1)
str(phylo_2)

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
#3) Calculate PCA
########################################################################################################################################################

#dat_scaled <- scale(dat[,c(11,13,14,16,17,20)], center = T, scale = T)
dat_scaled <- scale(dat[,c(8,11,14,16,17,20)], center = T, scale = T)
head(dat_scaled)
colnames(dat_scaled) <- c("Selfing", "Flower N.", "Flower width", "Style length", "Ovule N.", "Plant height")
#fix species names
dat$Species_all <- gsub(" ", "_", dat$Species_all)
#set rownames
rownames(dat_scaled) <- dat$Species_all


#Calculate phylo d object
d.4d <- phylo4d(phylo_output, dat_scaled)
#condict phylo pca to phylo d object
output.4d <- ppca(d.4d,scale=FALSE,scannf=FALSE,nfposi=1,nfnega=1, method="Abouheif")

#check pca eigenvalues
p <- barplot(output.4d$eig,main='pPCA eigenvalues',cex.main=1.8)
text(p,par("usr")[3],
     lab=rownames(output.4d$c1), 
     srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)

#Calculate contribution to principal components
dotchart(output.4d$c1[,1],lab=rownames(output.4d$c1),main="Global principal 1")
dotchart(output.4d$c1[,2],lab=rownames(output.4d$c1),main="Global principal 2")

#check phylo structure
#abouheif.moran(d.4d)
#following this paper https://doi.org/10.1007/978-1-4939-8850-1_13

#Calculate %  for adding to axis
percentage <- round(output.4d$eig / sum(output.4d$eig) * 100, 2)
percentage <- diag(as.matrix(percentage))
percentage <- paste0(names(percentage), " (", percentage, "%)")

#####
#CODE ADAPTED FROM https://stackoverflow.com/questions/6578355/plotting-pca-biplot-with-ggplot2
######


########################################################################################################################################################
#4) PLOT PCA MANUALLY WITH GGPLOT
#############################################################################################################################################################
####
#CODE ADAPTED FROM https://stackoverflow.com/questions/6578355/plotting-pca-biplot-with-ggplot2
####

#CALL OUTPUT OF PCA PC
PC <- output.4d
#set call names
colnames(PC$c1) <- c("PC1", "PC2")

#function for colouring dots
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

#function for plotting
#the function is a bit long, can be done in less seteps
#several stackoverflow questions have been useful to edit this

PCbiplot <- function(PC, x="PC1", y="PC6") {
  # PC being a prcomp object
  data <- data.frame(PC$li)
  plot <- ggplot(data, aes_string(x=x, y=y)) 
  colnames(PC$c1) <- c("PC1", "PC6")
  datapc <- data.frame(PC$c1)
  percentage <- round(PC$eig / sum(PC$eig) * 100, 2)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .5 * mult * (get(x)),
                      v2 = .5 * mult * (get(y))
  )
 # df <- data.frame(x = data$PC1, y = data$PC6,
  #                 d = densCols(data$PC1, data$PC6, colramp = colorRampPalette(rev(rainbow(5, end = 4/6)))))
  dat <- data.frame(x = data[,x], y = data[,y])
  dat$density <- get_density(dat$x, dat$y, h = c(2, 2), n = 1000)
  plot <- plot + geom_point(data=dat, aes(-x, -y, color = density)) + scale_color_viridis()
  #plot <- plot + geom_point(data=df, aes(x=-x, y=-y, col = d), size = I(1.3), alpha=I(1)) +  scale_color_identity() 
  plot <- plot + geom_segment(data=datapc,linejoin="round", lineend="round",aes(x=0, y=0, xend=-v1, yend=-v2),size=0.8, arrow=arrow(length=unit(0.5,"cm")), alpha=1, color="black")
  plot <- plot + xlab(paste("PC1 ", "(",(percentage[1]),")","%", sep = "")) +theme_bw()
  plot <- plot + ylab(paste("PC2 ", "(",(percentage[2]),"%",")", sep = ""))
  plot <- plot + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                  panel.border = element_rect(linetype = "solid", colour = "black", size=1))
  
  PCAloadings <- data.frame(Variables = rownames(PC$c1), PC$c1)
  
  plot <- plot + annotate("text", x = -(PCAloadings$PC1*c(5,7,6,5,12,5)), y = -(PCAloadings$PC6*c(5,7,5,5,5,5)),
                          label = PCAloadings$Variables, color="black",size=3)
  
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2),size=0.4, arrow=arrow(length=unit(0,"cm")),linetype=2, alpha=0.8, color="black")
  
  #CALCULATE KERNELS
  mv.kde <- kde2d(data[,1], data[,2], n = 200)
  dx <- diff(mv.kde$x[1:2])  # lifted from emdbook::HPDregionplot()
  dy <- diff(mv.kde$y[1:2])
  sz <- sort(mv.kde$z)
  c1 <- cumsum(sz) * dx * dy
  
  # specify desired contour levels:
  prob <- c(0.5)
  prob_1 <- c(0.75)
  # plot:
  dimnames(mv.kde$z) <- list(mv.kde$x,mv.kde$y)
  dc <- melt(mv.kde$z)

  dc$prob <- approx(sz,1-c1,dc$value)$y
  
  plot <- plot + geom_contour(data=dc, aes(x=-Var1,y=-Var2,z=prob),colour="brown4",breaks=prob)
  # plot:
  dimnames(mv.kde$z) <- list(mv.kde$x,mv.kde$y)
  dc <- melt(mv.kde$z)
  
  dc$prob_1 <- approx(sz,1-c1,dc$value)$y
  plot <- plot + geom_contour(data=dc, aes(x=-Var1,y=-Var2,z=prob_1),colour="black",breaks=prob_1)
  
  plot
  
}

#PLOT PHYLO PCA
PCbiplot(PC)
