########################################################################################################################################################
#SCRIPT TO CALCULATE THE PHLOGENETIC INFORMED PRINCIPAL COMPONENT ANALYSIS
########################################################################################################################################################
#LOAD LIBRARIES
library(phytools) #ppca
library(ape) #for phylogenetic distance
library(dplyr) #data processing
library(rtrees) #for phylogenetic distancelibrary(MASS)
library(reshape2) #data processing
library(MASS) #I think I used it for the kernel density of the plotting function; no longer used but I leave in case its handy later on
library(ggplot2) #plotting
library(broman) #crayon colours
library(magick) #add images
########################################################################################################################################################
#1) LOAD DATA
########################################################################################################################################################
#read data with missing values filled by data imputation
dat <- read.csv("Data/Csv/nectar_pollen_subset_imputed_trait_data.csv", row.names = "X")
nrow(dat)
########################################################################################################################################################
#2) Tidy up data to get phylo distance and conduct PCA
########################################################################################################################################################
########################################################################################################################################################
#remove not found species, cannot do PCA with unequal numbers of rows
cols.num <- c("Family_all","Genus_all","Species_all")
dat[cols.num] <- sapply(dat[cols.num],as.character)
dat$Species_all <- gsub("Species_all_", "", dat$Species_all)
#dat <- dat[!dat$Species_all == "Diospyros seychellarum", ]
#dat <- dat[!dat$Species_all == "Memecylon eleagni", ]
#dat <- dat[!dat$Species_all == "Ocotea laevigata", ]
#dat <- dat[!dat$Species_all == "Soulamea terminaloides", ]
########################################################################################################################################################
#3) REMOVE OUTLIERS, OUT OF 2.5-97.5 RANGE WHICH HELPS IMPUTATION PROCESS. SEE ARTICLE FOR REF.
########################################################################################################################################################
dat_cleaning <- dat[,c(2,3,4,8,11,12,14,15,18,20,23)]
#CHECK LEVELS
str(dat)
str(dat_cleaning)



dat_cleaning_1 <- dat_cleaning %>%
  filter(between(Flowers_per_plant, quantile(Flowers_per_plant, 0.025), quantile(Flowers_per_plant, 0.975)))

dat_cleaning_2 <- dat_cleaning_1 %>%
  filter(between(Corolla_diameter_mean, quantile(Corolla_diameter_mean, 0.025), quantile(Corolla_diameter_mean, 0.975)))

dat_cleaning_3 <- dat_cleaning_2 %>%
  filter(between(Style_length, quantile(Style_length, 0.025), quantile(Style_length, 0.975)))

dat_cleaning_4 <- dat_cleaning_3 %>%
  filter(between(Ovule_number, quantile(Ovule_number, 0.025), quantile(Ovule_number, 0.975)))

dat_cleaning_5 <- dat_cleaning_4 %>%
  filter(between(Plant_height_mean_m, quantile(Plant_height_mean_m, 0.025), quantile(Plant_height_mean_m, 0.975)))

dat_cleaning_6 <- dat_cleaning_5 %>%
  filter(between(Nectar_ul, quantile(Nectar_ul, 0.025), quantile(Nectar_ul, 0.975)))


dat_cleaning_7 <- dat_cleaning_6 %>%
  filter(between(Pollen_per_flower, quantile(Pollen_per_flower, 0.025), quantile(Pollen_per_flower, 0.975)))

#LOG TRANSFORM AND SCALE DATA
#CHECK LEVELS
str(dat_cleaning_7)
dat_cleaning_7[,c(4:11)] <- log(dat_cleaning_7[,c(4:11)]+1)
dat_cleaning_7[,c(4:11)] <- scale(dat_cleaning_7[,c(4:11)], center = T, scale = T)


final_d <- dat_cleaning_7[,c(4:11)]
########################################################################################################################################################
#4) GET PHYLO
########################################################################################################################################################
#calculate phylo 
phylo <- as.data.frame(cbind(dat_cleaning_7$Family_all, dat_cleaning_7$Genus_all, dat_cleaning_7$Species_all))
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
final_d <-  dat_cleaning_7[,c(4:11)]

rownames(final_d) <- dat_cleaning_7$Species_all
#fix species names
rownames(final_d) <- gsub(" ", "_", rownames(final_d))

#Output saved not RUN
phyl_pca_forest_nectar_pollen <- phyl.pca(phylo_output, final_d,method="lambda",mode="cov")






####
#SAVE PHYLO PCA OUTPUT
####
saveRDS(phyl_pca_forest_nectar_pollen, "Data/RData/phyl_pca_forest_nectar_pollen.rds")
#SAVE ALSO DATA TO PLOT IT IN RMD file
saveRDS(dat_cleaning_7, "Data/RData/data_all_species_PPCA_nectar_pollen.rds")

####
#READ DATA
####
phyl_pca_forest_nectar_pollen <- readRDS("Data/RData/phyl_pca_forest_nectar_pollen.rds")
dat_cleaning_7 <- readRDS("Data/RData/data_all_species_PPCA_nectar_pollen.rds")


#CALL the output PC for simplicity
PC <- phyl_pca_forest_nectar_pollen
#CHECK CONTENT
#EIGENVALUES
PC$Eval
#PC score (POINTS)
PC$S
#PC loadings (ARROWS)
PC$L

nrow(PC$S)

percentage <- round(diag(PC$Eval) / sum(PC$Eval) * 100, 2) #calculate percentage

sum(percentage[1]+percentage[2])
########################################################################################################################################################
#4) PLOT PPCA
########################################################################################################################################################

#####
#MASTER FUNCTION TO PLOT 
#####

nrow(PC$S)


all_spp <- function(PC, x="PC1", y="PC2") {
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

  
  plot <- plot+stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',bins=8) + 
    scale_fill_continuous(low="green",high="red",guide=FALSE,lim=c(0.01,0.082),breaks = c(0.02, 0.035, 0.082), labels = c("Low", "Medium", "High")) + theme(legend.position = "none")+guides(fill = guide_legend(override.aes = list(alpha = 0.6),title="Kernel density"))+
    scale_alpha(guide = 'none')
  
  
  plot <- plot + geom_point(data=dat, aes(x, y),size=0.95, color="black")
  
  ########
  #ADD ARROWS 
  ########
  datapc <- data.frame(PC$L) #CREATE DATAFRAME WITH LOADINGS
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  # add arrows
  plot <- plot + geom_segment(data=datapc,linejoin="round", lineend="round",aes(x=0, y=0, xend=v1, yend=v2),size=1.8, arrow=arrow(length=unit(0.5,"cm")), alpha=0.8, colour=c("black"))
  
  
  #ADD THE OTHER DIRECTION OF THE SEGMENT BECAUSE LOOKS COOL
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=-v1, yend=-v2),size=1.6, arrow=arrow(length=unit(0,"cm")),linetype=2, alpha=0.5, colour=c("black"))
  
  #Add axis with perctentage
  percentage <- round(diag(PC$Eval) / sum(PC$Eval) * 100, 2) #calculate percentage
  
  plot <- plot + xlab(paste("PC1 ", "(",(percentage[1]),"%",")", sep = "")) #XLAB
  plot <- plot + ylab(paste("PC2 ", "(",(percentage[2]),"%",")", sep = "")) #YLAB
  
  
  #CHANGE THEME
  
  plot <- plot + theme_bw() 
  
  #ADD LABELS
  rownames(PC$L) <- c("Selfing", "Flower number", "Flower Size", "Style length", "Ovule number", "Plant height", "Nectar","Pollen" )
  
  PCAloadings <- data.frame(Variables = rownames(PC$L), PC$L)
  plot <- plot + annotate("text", x = (PCAloadings$PC1*c(4.6,4.95,5.5,6.25,6.3,6.15,6,6)), y = (PCAloadings$PC2*c(4.693,2.4,3,6.2,5,5.5,6,5)+c(0,0,0,0,0.4,0,0,0)),
                          label = PCAloadings$Variables, color="black",fontface =2,size=4)
  
  plot <- plot + theme_ms() +ylim(-4,4) + xlim(-4,4) +  theme(legend.position = c(0.095, 0.11)) +ggtitle("") 
  
  
  
  plot
  
}

all_spp(PC)
