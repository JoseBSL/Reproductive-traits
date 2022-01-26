########################################################################################################################################################
#SCRIPT FOR DATA IMPUTATION FOR THE SUBSET OF QUANTITATIVE DATA OF NECTAR
########################################################################################################################################################
#1) READ TRAIT DATA

#2) CLEAN DATA AND RENAME LEVELS

#3) EXPLORE PATTERNS OF MISSING DATA

#4) CALCULATE PHYLOGENETIC DISTANCE TO CORRECT WITH EIGENVALUES IN THE IMPUTATION

#5) ICLUDE EIGENVALUES IN RAWDATA TO IMPROVE IMPUTATION OUTPUT

#6) IMPUTE DATA

#7) SAVE DATA
########################################################################################################################################################
#LOAD LIBRARIES
library(readxl) #read excel file (trait data)
library(dplyr) #data manipulation
library(missMDA) #For missing values
library(FactoMineR) #Produce pca biplot with individuals and variables
library(factoextra)
library(ggpubr) 
library(missForest) #random forst imputation
library(rtrees) #for phylogenetic distancelibrary(MASS)
library(ape)
library(PVR) #calculate eigen vectors and add them as a column in the imputation, seems to improve output
library(visdat) #VISUALIZE MISSING DATA
library(phytools) #ppca
library(ggplot2)
########################################################################################################################################################
#1) READ TRAIT DATA
########################################################################################################################################################
#load data
trait_data <- read_excel("Data/Trait_data_raw/Trait_data_final.xlsx",na = "NA")
########################################################################################################################################################
#2) A) CLEAN DATA AND B) RENAME LEVELS FOR IMPUTATION 
########################################################################################################################################################
#####
#A) CLEAN DATA
#####
#select just filled rows
trait_data_1 <- trait_data[1:1712,]

#filter data, select species with flower level info and capitulum
trait_filtered <- filter(trait_data_1, Info_level == "flower" |  Info_level == "capitulum")
levels(as.factor(trait_filtered$Info_level)) #checking levels
#remove NA's and duplicated species
trait_filtered$Species_all[trait_filtered$Species_all=="NA"]<-NA
trait_filtered_1 <- trait_filtered[!is.na(trait_filtered$Species_all),]
trait_filtered_2 <- trait_filtered_1[!duplicated(trait_filtered_1$Species_all),]

str(trait_filtered_2)
#select columns of interest
t <- trait_filtered_2[c("Species_geonet","Order_all","Family_all","Genus_all","Species_all","Breeding_system","Compatibility_system","Autonomous_selfing_level",
                        "Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant",  "Floral_unit_width",
                        "Corolla_diameter_mean", "Corolla_length_mean", "Style_length", "Ovule_number", "life_form", "lifespan",
                        "Plant_height_mean_m","Nectar_presence_absence","Nectar_ul","Nectar_mg","Nectar_concentration","Pollen_per_flower")]

########################################################################################################################################################
#2 DATA PREPARATION
########################################################################################################################################################
#RENAME LEVELS 
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

#use ifelse base r does not work with these NA'S Even with work around. base r does not like the na option of read excel
t$Autonomous_selfing_level_fruit_set <- ifelse(t$Autonomous_selfing_level %in% c("high") & is.na(t$Autonomous_selfing_level_fruit_set), 88, t$Autonomous_selfing_level_fruit_set)
t$Autonomous_selfing_level_fruit_set <- ifelse(t$Autonomous_selfing_level %in% c("medium") & is.na(t$Autonomous_selfing_level_fruit_set), 50.5, t$Autonomous_selfing_level_fruit_set)
t$Autonomous_selfing_level_fruit_set <- ifelse(t$Autonomous_selfing_level %in% c("low") & is.na(t$Autonomous_selfing_level_fruit_set), 13, t$Autonomous_selfing_level_fruit_set)
t$Autonomous_selfing_level_fruit_set <- ifelse(t$Autonomous_selfing_level %in% c("none") & is.na(t$Autonomous_selfing_level_fruit_set), 0, t$Autonomous_selfing_level_fruit_set)

#Now we just select presence/absence of nectar
#select columns of interest
t <- t[c("Species_geonet","Order_all","Family_all","Genus_all","Species_all","Breeding_system","Compatibility_system","Autonomous_selfing_level",
         "Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant", "Floral_unit_width",
         "Corolla_diameter_mean", "Corolla_length_mean", "Style_length", "Ovule_number", "life_form", "lifespan",
         "Plant_height_mean_m","Nectar_presence_absence", "Nectar_ul","Nectar_mg","Nectar_concentration","Pollen_per_flower")]

########################################################################################################################################################
#3) EXPLORE PATTERNS OF MISSING DATA
########################################################################################################################################################
#check missing data now
missing_data <- unlist(lapply(t, function(x) sum(is.na(x))))/nrow(t)*100
sort(missing_data[missing_data >= 0], decreasing=T)

#seems ok to impute, just 36% percent of missing data in this column now.

########################################################################################################################################################
#4) CALCULATE PHYLOGENETIC DISTANCE TO CORRECT WITH EIGENVALUES IN THE IMPUTATION
########################################################################################################################################################
#Convert to factor 
cols <- c("Order_all", "Family_all", "Genus_all", "Species_all", "Compatibility_system", "Autonomous_selfing_level", "Flower_morphology",
          "Flower_symmetry", "life_form", "lifespan","Nectar_presence_absence")
t[cols] <- lapply(t[cols], factor)  ## as.factor() could also be used

#remove not found species that do not run with get tree
cols.num <- c("Family_all","Genus_all","Species_all")
t[cols.num] <- sapply(t[cols.num],as.character)
t$Species_all <- gsub("Species_all_", "", t$Species_all)

t <- t[!t$Species_all == "Pinus luchuensis", ]   # remove gymnosperm species

#remove species that are not until species level
#ALL THE SPECIES WITH SP. ARE DELETD
t$Species_geonet <- gsub("M_PL_.*","",t$Species_geonet) #subsitute partial string for nothing
t$Species_geonet <- gsub(" $","", t$Species_geonet, perl=T) #remove trailing spaces
t$Species_geonet <-  sub('sp.1$', 'sp.', t$Species_geonet)#PREPARE ALL SP TO SP.
t$Species_geonet <- sub('sp.2$', 'sp.', t$Species_geonet)#PREPARE ALL SP TO SP.
t$Species_geonet <- sub('sp$', 'sp.', t$Species_geonet) #PREPARE ALL SP TO SP.
t$Species_geonet <- sub("sp.$", "DELETE", t$Species_geonet) #change all sp. for DELETE
t <- t[- grep("DELETE",  t$Species_geonet),] #remove species with "DELETE"
#CHECK LEVELS
levels(factor(t$Species_all))

#Make these NA's as NA
t$Species_all[t$Species_all=="NA"] <- NA
t$Family_all[t$Family_all=="NA"] <- NA
t$Genus_all[t$Genus_all=="NA"] <- NA
#remove NA's
t <- t[!is.na(t$Family_all),]
t <- t[!is.na(t$Species_all),]
t <- t[!is.na(t$Genus_all),]
#Explore missing data
#check missing data now
missing_data <- unlist(lapply(t, function(x) sum(is.na(x))))/nrow(t)*100
sort(missing_data[missing_data >= 0], decreasing=T)

#Subset species that have info for at least one of these floral traits (pollen or nectar)
t_nectar_pollen <- t[  !is.na(t$Pollen_per_flower)| !is.na(t$Nectar_ul),]
nrow(t_nectar_pollen)

str(t_nectar_pollen)


#Subset columns that are used in the PPCA and do complete cases
dat_cleaning <- t_nectar_pollen[c("Family_all","Genus_all","Species_all", "Autonomous_selfing_level_fruit_set", 
                      "Flowers_per_plant","Corolla_diameter_mean","Style_length","Ovule_number", "Plant_height_mean_m", 
                      "Nectar_ul", "Pollen_per_flower")]

dat_cleaning <- dat_cleaning[complete.cases(dat_cleaning),]


#CHECK LEVELS
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
saveRDS(phyl_pca_forest_nectar_pollen, "Data/RData/phyl_pca_forest_nectar_pollen_complete_cases.rds")
#SAVE ALSO DATA TO PLOT IT IN RMD file
saveRDS(dat_cleaning_7, "Data/RData/data_all_species_PPCA_nectar_pollen_complete_cases.rds")

####
#READ DATA
####
#phyl_pca_forest_nectar_pollen <- readRDS("Data/RData/phyl_pca_forest_nectar_pollen.rds")
#dat_cleaning_7 <- readRDS("Data/RData/data_all_species_PPCA_nectar_pollen.rds")


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
  
  plot <- plot + theme_bw() +ylim(-4,4) + xlim(-4,4) +  theme(legend.position = c(0.095, 0.11)) +ggtitle("") 
  
  
  
  plot
  
}

all_spp(PC)

