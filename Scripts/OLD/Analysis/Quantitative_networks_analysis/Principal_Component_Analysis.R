###################################################################################
####
# PRINCIPAL COMPONENT ANALYSIS (PCA)
####
###################################################################################

#LOAD LIBRARIES
library(readxl) #read excel file (trait data)
library(dplyr) #data manipulation
library(missMDA) #For missing values
library(FactoMineR) #Produce pca biplot with individuals and variables
library(factoextra)
library(ggpubr)

#LOAD DATA
trait_data <- read_excel("Data/Trait_data_raw/Trait_data_final.xlsx",na = "NA")
#select just filled rows
trait_data_1 <- trait_data[1:1701,]
#filter data, select species with flower level info and capitulum
trait_filtered <- filter(trait_data_1, Info_level == "flower" |  Info_level == "capitulum")
levels(as.factor(trait_filtered$Info_level))
#select columns of interest
traits <- trait_filtered %>% select(Order_all,Family_all,Genus_all,Species_all,Breeding_system,IMPUTED_Compatibility,Autonomous_selfing_level,Autonomous_selfing_level_data_type,Autonomous_selfing_level_fruit_set,Flower_morphology,Flower_symmetry,Flowers_per_plant,Flowers_per_inflorescence,Floral_unit_width,Corolla_diameter_mean,Corolla_length_mean,STYLE_IMPUTED,OVULES_IMPUTED,life_form,lifespan,IMPUTED_plant_height_mean_m)
#Remove duplicated species
t <- traits[!duplicated(traits$Species_all), ]
#check structure of the data
str(t)
#remove cases where the species is NA 
t <- t[!is.na(t$Species_all), ]

str(t)
t_trial <- t[,c(5,6,7,9:21)]
str(t_trial)
cols <- c( "Breeding_system","IMPUTED_Compatibility", "Autonomous_selfing_level","Flower_morphology","Flower_symmetry", "life_form", "lifespan")

t_trial[cols] <- lapply(t_trial[cols], factor)  ## as.factor() could also be used

colnames(t_trial) <- c("Breeding system","Compatibility system","Selfing_level", "Quantitative selfing","Flower morphology","Flower symmetry",  "Flower/plant", "Flower/inflorescence", "Inflorescence width", "Flower width",  "Flower length",  "Style length", "Ovule number", "Life form","Life span", "Plant height")

str(t_trial$`Life form`)
t_trial$Selfing_level<- as.factor(t_trial$Selfing_level)
res.impute <- imputeFAMD(t_trial, ncp=3,threshold = 1e-06) 


res.impute$completeObs$Selfing_level <- factor(res.impute$completeObs$Selfing_level, levels = c("high", "medium", "low", "none"))

#from http://www.zhuoyao.net/2019/11/08/principal-component-methods-in-r-practical-guide/
fviz_pca_biplot(prcomp(res.impute$completeObs[-c(414,480,482,634,705,1139),c(4,7,8,9,10,12,13,16)], scale=T), label="var",
                geom.ind = "point",alpha.ind = 1, col.ind=res.impute$completeObs[-c(414,480,482,634,705,1139),c(3)], 
                pointshape = 19, pointsize = 1,
                palette = c("#FDE725FF","#31688EFF","#440154FF", "#35B779FF"),
                # Variables
                col.var = "black",title="",  arrowsize = 0.6,alpha.var=0.7)+xlim(-7, 10) + ylim (-8,8)+border()+labs(color="Selfing")



res.impute$completeObs$Selfing_level <- as.character(res.impute$completeObs$Selfing_level)
res.impute$completeObs$Selfing_level[res.impute$completeObs$Selfing_level=="high"] <- "selfer"
res.impute$completeObs$Selfing_level[res.impute$completeObs$Selfing_level=="medium"] <- "selfer"
res.impute$completeObs$Selfing_level[res.impute$completeObs$Selfing_level=="low"] <- "non_selfer"
res.impute$completeObs$Selfing_level[res.impute$completeObs$Selfing_level=="none"] <- "non_selfer"
res.impute$completeObs$Selfing_level <- as.factor(res.impute$completeObs$Selfing_level)


#from http://www.zhuoyao.net/2019/11/08/principal-component-methods-in-r-practical-guide/
fviz_pca_biplot(prcomp(res.impute$completeObs[-c(414,480,482,634,705,1139),c(4,7,8,9,10,12,13,16)], scale=T), label="var",
                geom.ind = "point",alpha.ind = 1, col.ind=res.impute$completeObs[-c(414,480,482,634,705,1139),c(3)], 
                pointshape = 19, pointsize = 1,
                palette = c("#35B779FF","#FDE725FF"),
                # Variables
                col.var = "black",title="",  arrowsize = 0.6,alpha.var=0.7)+xlim(-7, 10) + ylim (-8,8)+border()+labs(color="Selfing")


#I have prepare all in a Rmd file with the pca coloured by the different qualitative variables

#PREPARE NOW PANE OF PLOTS WITH THE INDIVIDUALS COLOURED BY QUALITATIVE VARIABLES

fviz_pca_ind(prcomp(res.impute$completeObs[-c(414,480,482,634,705,1139),c(4,7,8,9,10,12,13,16)], scale=T), label="none",
                geom.ind = "point",alpha.ind = 1, col.ind=res.impute$completeObs[-c(414,480,482,634,705,1139),c(3)], 
                pointshape = 19, pointsize = 1,axes.linetype=NA,
                palette = c("#35B779FF","#FDE725FF"))+xlim(-7, 10) + ylim (-8,8)+border()+labs(color="Selfing")+
  theme(strip.text.x = element_blank(),strip.background = element_rect(colour="white", fill="white"),legend.position=c(0.1,0.8),legend.title=element_text(size=8),legend.key.size = unit(0.7,"line"))+
  guides(color = guide_legend(override.aes = list(size=1.5)))


