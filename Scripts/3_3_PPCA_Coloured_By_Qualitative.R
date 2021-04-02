########################################################################################################################################################
#SCRIPT TO CALCULATE THE PHLOGENETIC INFORMED PRINCIPAL COMPONENT ANALYSIS
#Same as script 3 but added qualitative variables to colour the location of the trait space
########################################################################################################################################################
#LOAD LIBRARIES
library(phytools) #ppca
library(ape) #for phylogenetic distance
library(dplyr) #data processing
library(rtrees) #for phylogenetic distancelibrary(MASS)
library(reshape2) #data processing
library(viridis) #COLOUR GGPLOT
library(MASS) #I think I used it for the kernel density of the plotting function; no longer used but I leave in case its handy later on
library(ggplot2) #plotting
library(broman) #crayon colours
########################################################################################################################################################
#1) LOAD DATA
########################################################################################################################################################
#read data with missing values filled by data imputation
dat <- read.csv("Data/Csv/all_species_imputed_trait_data_forest_data.csv", row.names = "X")
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
#3) REMOVE OUTLIERS, OUT OF 2.5-97.5 RANGE
########################################################################################################################################################

dat_cleaning <- dat[,c("Family_all", "Genus_all", "Species_all", "Breeding_system", "Compatibility_system", "Autonomous_selfing_level",
                       "Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant","Corolla_diameter_mean",
                       "Style_length","Ovule_number","life_form", "lifespan", "Plant_height_mean_m", "Nectar_presence_absence")]


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



#ALL VALUES ARE BETWEEN 0 AND 100, THEREFORE WE DO NOT DISCARD OUTLIERS FOR THIS VARIABLE
#dat_cleaning_6 <- dat_cleaning_5 %>%
# filter(between(Nectar_ul, quantile(Nectar_ul, 0.025), quantile(Nectar_ul, 0.975)))


#LOG TRANSFORM AND SCALE DATA
#CHECK LEVELS
str(dat_cleaning_5)

dat_cleaning_5[,c("Autonomous_selfing_level_fruit_set","Flowers_per_plant",
                  "Corolla_diameter_mean","Style_length","Ovule_number","Plant_height_mean_m")] <- log(dat_cleaning_5[,c("Autonomous_selfing_level_fruit_set","Flowers_per_plant",
              "Corolla_diameter_mean","Style_length","Ovule_number","Plant_height_mean_m" )]+1)
dat_cleaning_5[,c("Autonomous_selfing_level_fruit_set","Flowers_per_plant",
                  "Corolla_diameter_mean","Style_length","Ovule_number","Plant_height_mean_m")] <- scale(dat_cleaning_5[,c("Autonomous_selfing_level_fruit_set","Flowers_per_plant",
                  "Corolla_diameter_mean","Style_length","Ovule_number","Plant_height_mean_m")], center = T, scale = T)


final_d <- dat_cleaning_5[,c("Autonomous_selfing_level_fruit_set","Flowers_per_plant",
                             "Corolla_diameter_mean","Style_length","Ovule_number","Plant_height_mean_m")]


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
final_d <-  dat_cleaning_5[,c("Autonomous_selfing_level_fruit_set","Flowers_per_plant",
                              "Corolla_diameter_mean","Style_length","Ovule_number","Plant_height_mean_m")]

rownames(final_d) <- dat_cleaning_5$Species_all
#fix species names
rownames(final_d) <- gsub(" ", "_", rownames(final_d))

#Output saved not RUN
#Alredy calculated in script 3
#phyl_pca_forest <- phyl.pca(phylo_output, final_d,method="lambda",mode="cov")

####
#SAVE PHYLO PCA OUTPUT
####
#saveRDS(phyl_pca_forest, "Data/RData/Data_for_plotting_qualitative_pca_vars.rds")
saveRDS(dat_cleaning_5, "Data/RData/data_all_species_for_rmd_plot_ppca_QUALITATIVE.rds")
####
#READ DATA
####
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

###############
#COMPATIBILITY#
###############

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
  dat$Compatibility <- dat_cleaning_5$IMPUTED_Compatibility
  dat$Compatibility <- as.character(dat$Compatibility)
  dat$Compatibility[dat$Compatibility=="dioecious"] <- "Dioecious"
  dat$Compatibility[dat$Compatibility=="monoecious"] <- "Monoecious"
  dat$Compatibility[dat$Compatibility=="partially_self_compatible"] <- "Partially self compatible"
  dat$Compatibility[dat$Compatibility=="self_compatible"] <- "Self compatible"
  dat$Compatibility[dat$Compatibility=="self_incompatible"] <- "Self incompatible"
  
  plot <- plot + geom_point(data=dat, aes(-x, -y, colour=Compatibility),size=0.65)+scale_color_manual(values=c("#fc2847", "#1cac78","blue","black","orange")) #+ #scale_color_viridis_c(option = "A", direction = 1, limits = c(min(dat$density), max(dat$density)))+
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
  
  
  plot
  
}

PCbiplot(PC)

#####################################################################################################################################################################
#####################################################################################################################################################################
#Life form#
#####################################################################################################################################################################
#####################################################################################################################################################################
dat


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
  dat$Life_form <- dat_cleaning_5$life_form
  dat$Life_form <- as.character(dat$Life_form)
  dat$Life_form[dat$Life_form=="herb"] <- "Herb"
  dat$Life_form[dat$Life_form=="shrub"] <- "Shrub"
  dat$Life_form[dat$Life_form=="tree"] <- "Tree"
  dat$Life_form[dat$Life_form=="Vine"] <- "Vine"
  
  plot <- plot + geom_point(data=dat, aes(-x, -y, colour=Life_form),size=0.65)+scale_color_manual(values=c("#fc2847", "#1cac78","blue","black","orange")) #+ #scale_color_viridis_c(option = "A", direction = 1, limits = c(min(dat$density), max(dat$density)))+
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
  
  
  plot
  
}

PCbiplot(PC)


#####################################################################################################################################################################
#####################################################################################################################################################################
#Life span#
#####################################################################################################################################################################
#####################################################################################################################################################################


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
    dat$lifespan <- dat_cleaning_5$lifespan
  plot <- plot + geom_point(data=dat, aes(-x, -y, colour=lifespan),size=0.65)+scale_color_manual(values=c("#fc2847", "#1cac78","blue","black","orange")) #+ #scale_color_viridis_c(option = "A", direction = 1, limits = c(min(dat$density), max(dat$density)))+
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
  
  
  plot
  
}

PCbiplot(PC)



#####################################################################################################################################################################
#####################################################################################################################################################################
#Flower Symmetry#
#####################################################################################################################################################################
#####################################################################################################################################################################


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
  dat$`Flower symmetry` <- dat_cleaning_5$Flower_symmetry
  dat$`Flower symmetry` <- as.character(dat$`Flower symmetry`)
  dat$`Flower symmetry`[dat$`Flower symmetry`=="actinomorphic"] <- "Actinomorphic"
  dat$`Flower symmetry`[dat$`Flower symmetry`=="zygomorphic"] <- "Zygomorphic"
  
  plot <- plot + geom_point(data=dat, aes(-x, -y, colour=`Flower symmetry`),size=0.65)+scale_color_manual(values=c("#fc2847", "#1cac78","blue","black","orange")) #+ #scale_color_viridis_c(option = "A", direction = 1, limits = c(min(dat$density), max(dat$density)))+
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
  
  
  plot
  
}

PCbiplot(PC)


#####################################################################################################################################################################
#####################################################################################################################################################################
#Flower Shape#
#####################################################################################################################################################################
#####################################################################################################################################################################


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
  dat$`Flower shape` <- dat_cleaning_5$Flower_morphology
  dat$`Flower shape` <- as.character(dat$`Flower shape`)
  dat$`Flower shape`[dat$`Flower shape`=="Funnelform"] <- "Campanulate"
  dat$`Flower shape`[dat$`Flower shape`=="Spike"] <- "Brush"
  
  plot <- plot + geom_point(data=dat, aes(-x, -y, colour=`Flower shape`),size=0.65)+scale_color_manual(values=c("#fc2847", "#1cac78","blue","black","orange","yellow", "brown4", "grey")) #+ #scale_color_viridis_c(option = "A", direction = 1, limits = c(min(dat$density), max(dat$density)))+
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
  
  
  plot
  
}

PCbiplot(PC)



#####################################################################################################################################################################
#####################################################################################################################################################################
#Breeding system#
#####################################################################################################################################################################
#####################################################################################################################################################################


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
  dat$`Breeding system` <- dat_cleaning_5$Breeding_system
  dat$`Breeding system` <- as.character(dat$`Breeding system`)
  dat$`Breeding system`[dat$`Breeding system`==""] <- ""
  dat$`Breeding system`[dat$`Breeding system`==""] <- ""
  
  plot <- plot + geom_point(data=dat, aes(-x, -y, colour=`Breeding system`),size=0.65)+scale_color_manual(values=c("#fc2847", "#1cac78","blue","black","orange","yellow", "brown4", "grey")) #+ #scale_color_viridis_c(option = "A", direction = 1, limits = c(min(dat$density), max(dat$density)))+
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
  
  
  plot
  
}

PCbiplot(PC)




