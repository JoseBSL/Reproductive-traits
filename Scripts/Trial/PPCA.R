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

dat$Species_all <- gsub(" ", "_", dat$Species_all)

rownames(dat_scaled) <- dat$Species_all

#This works fine no try the oither method. However, not any clue how to plot it well at the moment

library(ape)
library(phylobase)
library(adephylo)
d.4d <- phylo4d(phylo_output, dat_scaled)
output.4d <- ppca(d.4d,scale=FALSE,scannf=FALSE,nfposi=1,nfnega=1, method="Abouheif")

#
p <- barplot(output.4d$eig,main='pPCA eigenvalues',cex.main=1.8)
text(p,par("usr")[3],
     lab=rownames(output.4d$c1), 
     srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)



#Contribution to principal components
dotchart(output.4d$c1[,1],lab=rownames(output.4d$c1),main="Global principal 1")
dotchart(output.4d$c1[,2],lab=rownames(output.4d$c1),main="Global principal 2")

#check phylo structure
abouheif.moran(d.4d)
#following this paper https://doi.org/10.1007/978-1-4939-8850-1_13

#Calculate %  
percentage <- round(output.4d$eig / sum(output.4d$eig) * 100, 2)
percentage <- diag(as.matrix(percentage))
percentage <- paste0(names(percentage), " (", percentage, "%)")


#####
#CODE ADAPTED FROM https://stackoverflow.com/questions/6578355/plotting-pca-biplot-with-ggplot2
######


library(ggpubr)

colnames(PC$c1) <- c("PC1", "PC2")

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
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  plot <- plot + geom_point(data=data, aes(x=PC1, y=PC6), size = 1.6, alpha=1,pch=21, colour="black",fill="orange") + theme_bw()
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2),size=1, arrow=arrow(length=unit(0.5,"cm")), alpha=0.9, color="brown4")
  plot <- plot + xlab(paste("PC1 ", "(",(percentage[1]),")","%", sep = ""))
  plot <- plot + ylab(paste("PC2 ", "(",(percentage[2]),"%",")", sep = ""))
  plot <- plot + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                  panel.border = element_rect(linetype = "solid", colour = "black", size=1))
  
  PCAloadings <- data.frame(Variables = rownames(PC$c1), PC$c1)
  
  
  plot <- plot + annotate("text", x = (PCAloadings$PC1*7), y = (PCAloadings$PC6*7),
                          label = PCAloadings$Variables)
  
  plot

}

PCbiplot(PC)
