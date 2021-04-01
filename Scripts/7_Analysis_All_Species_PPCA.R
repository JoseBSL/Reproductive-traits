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
library(tidyverse)

theme_ms <- function(base_size=12, base_family="Helvetica") {
  (theme_bw(base_size = base_size, base_family = base_family)+
     theme(text=element_text(color="black"),
           axis.title=element_text( size = rel(1)),
           axis.text=element_text(size = rel(0.8), color = "black"),
           legend.title=element_text(face="bold"),
           legend.text=element_text(),
           legend.background=element_rect(fill="transparent"),
           legend.key.size = unit(0.5, 'lines'),
           panel.border=element_rect(color="black",size=1),
           panel.grid.minor.x =element_blank(),
           panel.grid.minor.y= element_blank(),
           panel.grid.major= element_blank()
     ))
}


########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#SCRIPT FOR ANALYSIS (VISITATION~FUNCTIONAL GROUP*GUILD) ##HCLUST## FAMD IMPUTATION METHOD

#1)LOAD DATA 

#2)PHYLOGENETIC DISTANCE OF THE SPECIES -Add them as covariable-

#3)SAVE MODEL

#4)PLOT MODEL 


########################################################################################################################################################

#LOAD LIBRARIES
library(ape) #for phylogenetic distance
library(dplyr) #data processing
library(rtrees) #for phylogenetic distance
library(DHARMa)
library(brms)
library(cmdstanr)
library(ggplot2)
library(emmeans)
library(tidybayes)

theme_ms <- function(base_size=12, base_family="Helvetica") {
  (theme_bw(base_size = base_size, base_family = base_family)+
     theme(text=element_text(color="black"),
           axis.title=element_text( size = rel(1.1)),
           axis.text=element_text(size = rel(1), color = "black"),
           legend.title=element_text(face="bold"),
           legend.text=element_text(),
           legend.background=element_rect(fill="transparent"),
           legend.key.size = unit(0.9, 'lines'),
           panel.border=element_rect(color="black",size=1),
           panel.grid.minor.x =element_blank(),
           panel.grid.minor.y= element_blank(),
           panel.grid.major= element_blank()
     ))
}
########################################################################################################################################################
#1) READ DATA
########################################################################################################################################################
setwd("~/R_Projects/Reproductive Traits")
d_5 <- read.csv("Data/Csv/quantitative_networks_Z_scores_with_traits_and_5_clusters_hclust_forest_data.csv", row.names = 1)
levels(factor(d_5$guild))
#I'm going to run the model just with the main poll. functional groups
#exclude other insects level
dat <- subset(d_5, guild!="Other_insects" & guild!="Lizards" & guild!="Birds")
levels(factor(dat$guild))
dat$guild <- as.character(dat$guild)
dat$guild[dat$guild=="Bee"] <- "Bees"
dat$guild <- factor(dat$guild, levels = c("Bees","Coleoptera", "Lepidoptera", "Non-bee-Hymenoptera",
                                          "Non-syrphids-diptera", "Syrphids"))


#LOAD GLOBAL PCA 
phyl_pca_forest <- readRDS("Data/RData/phyl_pca_forest.rds")
phyl_pca_forest$L
#Convert to data.frame
PCA_DATA <- as.data.frame(phyl_pca_forest$S)
#Convert rownames to colnames
library(tidyverse)
PCA_DATA_1 <-PCA_DATA %>% rownames_to_column( var = "Species_all")
#Remove underscore of species names
PCA_DATA_1$Species_all <-  gsub("_", " ", PCA_DATA_1$Species_all)


dat <- merge(dat, PCA_DATA_1, by="Species_all")

########################################################################################################################################################
#2) PHYLOGENETIC DISTANCE OF THE SPECIES
########################################################################################################################################################
#5 CLUSTERS
#Prepare species, genus anD_5 family for calculating phylogenetic distance
dat$Family_all  <- as.character(dat$Family_all)
dat$Genus_all   <- as.character(dat$Genus_all)
dat$Species_all <- as.character(dat$Species_all)


#prepare dataframe to calculate tree
phylo_5 <- as.data.frame(cbind(dat$Family_all, dat$Genus_all, dat$Species_all))
colnames(phylo_5) <-  c("family", "genus", "species")

#Select unique cases
phylo_5_1 <- phylo_5[!duplicated(phylo_5$species),]
phylo_5_2 <- tibble(phylo_5_1)
str(phylo_5_2)

phylo_5_3 <- get_tree(sp_list = phylo_5_2, tree = tree_plant_otl, taxon = "plant")

#Convert phylogenetic tree into matrix
A_5 <- vcv.phylo(phylo_5_3)
#Standardize to max value 1
A_5 <- A_5/max(A_5)
#Unify column names; remove underscore and remove asterik
rownames(A_5) <- gsub("\\*", "", rownames(A_5))
colnames(A_5) <- gsub("\\*", "", colnames(A_5))
colnames(A_5) <- gsub("_", " ", colnames(A_5))
rownames(A_5) <- gsub("_", " ", rownames(A_5))

#Add phylo column to dataset
dat$phylo
dat$phylo <- dat$Species_all
str(dat)

dat$Clusters <- as.character(dat$Clusters)
dat$Clusters[dat$Clusters=="1"] <- "A"
dat$Clusters[dat$Clusters=="2"] <- "B"
dat$Clusters[dat$Clusters=="3"] <- "C"
dat$Clusters[dat$Clusters=="4"] <- "D"
dat$Clusters[dat$Clusters=="5"] <- "E"

dat$Clusters <- as.factor(dat$Clusters)
levels(as.factor(dat$Clusters))
levels(factor(dat$guild))

########################################################################################################################################################
#3.1)ANALYSIS
########################################################################################################################################################

#5 clusters hclust
model_1_subset_spp_4pcs <- brm((Interaction-1) ~ PC1*guild+PC2*guild+PC3*guild+PC4*guild+ (1|Id) + (1|gr(phylo, cov = A)),
                            data = dat, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                            sample_prior = TRUE, warmup = 500, iter = 2000,
                            control = list(adapt_delta = 0.99))


#Save model output 

setwd("~/Dropbox/PhD/R") #DROPBOX, files too large for github
saveRDS(model_1_subset_spp_4pcs, "model_1_subset_spp_4pcs.RDS")
saveRDS(dat, "data_for_model_1_subset_spp_4pcs.RDS")

#Plot nicely PC1
ce_pc1 <- conditional_effects(model_1_subset_spp_4pcs, effects = "PC1:guild",points=T) 

p1 <- ggplot(ce_pc1[[1]], aes(x = PC1, y = (estimate__+1), group=guild, colour=guild)) + geom_point(data = dat,
  aes(x = PC1, y = Interaction),size = 0.75, alpha=0.5) + geom_line(size=0.8) + 
  ylim(0,quantile(dat_analysis$Interaction, 0.95)) + ylab("Number of visits")+
  theme_ms() + theme(legend.position = "none") + scale_color_manual(name="Functional groups",values=c("orange", "black", "limegreen","#E7298A", "cyan4","blueviolet"))

#Plot nicely PC2
ce_pc2 <- conditional_effects(model_1_subset_spp_4pcs, effects = "PC2:guild",points=T) 

p2 <- ggplot(ce_pc2[[1]], aes(x = PC2, y = (estimate__+1), group=guild, colour=guild)) + geom_point(data = dat,
  aes(x = PC2, y = Interaction),size = 0.75, alpha=0.5) + geom_line(size=0.8) + 
  ylim(0,quantile(dat_analysis$Interaction, 0.95)) + ylab("")+
  theme_ms() + theme(legend.position = "none") + scale_color_manual(name="Functional groups",values=c("orange", "black", "limegreen","#E7298A", "cyan4","blueviolet"))


#Plot nicely PC3
ce_pc3 <- conditional_effects(model_1_subset_spp_4pcs, effects = "PC3:guild",points=T) 

p3 <- ggplot(ce_pc3[[1]], aes(x = PC3, y = (estimate__+1), group=guild, colour=guild)) + geom_point(data = dat,
   aes(x = PC3, y = Interaction),size = 0.75, alpha=0.5) + geom_line(size=0.8) + 
  ylim(0,quantile(dat_analysis$Interaction, 0.95)) + ylab("")+
  theme_ms() + theme(legend.position = "none") + scale_color_manual(name="Functional groups",values=c("orange", "black", "limegreen","#E7298A", "cyan4","blueviolet"))


#Plot nicely PC4
ce_pc4 <- conditional_effects(model_1_subset_spp_4pcs, effects = "PC4:guild",points=T) 

p4 <- ggplot(ce_pc4[[1]], aes(x = PC4, y = (estimate__+1), group=guild, colour=guild)) + geom_point(data = dat,
  aes(x = PC4, y = Interaction),size = 1.25, alpha=0.75) + geom_line(size=0.8) + 
  ylim(0,quantile(dat_analysis$Interaction, 0.95)) + ylab("Number of visits")+
  theme_ms() + + scale_color_manual(values=c("cyan4", "orange", "limegreen","#E7298A", "black","blueviolet"))



library(patchwork)

combined <- p1 + p2 + p3 & theme(legend.position = "right")
combined + plot_layout(guides = "collect")


#####PLT PPCA
PC <- phyl_pca_forest
#CHECK CONTENT
#EIGENVALUES
PC$Eval
#PC score (POINTS)
PC$S
#PC loadings (ARROWS)
PC$L

nrow(PC$S)

percentage <- round(diag(PC$Eval) / sum(PC$Eval) * 100, 2) #calculate percentage


########################################################################################################################################################
#4) PLOT PPCA
########################################################################################################################################################

#####
#MASTER FUNCTION TO PLOT 
#####
#seems massive but in reality is a scatterplot with arrows
#it has inside how to compute kernel density but do not think I'll use it
#point density seems to do the job

PCbiplot <- function(PC, x="PC1", y="PC3") {
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
  plot <- plot + ylab(paste("PC3 ", "(",(percentage[3]),"%",")", sep = "")) #YLAB
  plot <- plot + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                       panel.border = element_rect(linetype = "solid", colour = "black", size=1))
  
  
  #ADD THE OTHER DIRECTION OF THE SEGMENT BECAUSE LOOKS COOL
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2),size=0.6, arrow=arrow(length=unit(0,"cm")),linetype=2, alpha=0.8, color="black")
  
  #ADD LABELS
  rownames(PC$L) <- c("Selfing", "Flower Number", "Flower size", "Style Length", "Ovule Number", "Plant Height" )
  
  PCAloadings <- data.frame(Variables = rownames(PC$L), PC$L)
  plot <- plot + annotate("text", x = -(PCAloadings$PC1*c(3.5,4,4,4,4,4)), y = -(PCAloadings$PC3*c(3,4,4,4,4,4)),
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

                       