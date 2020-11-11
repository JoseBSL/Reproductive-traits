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
#check data
head(trait_data)
#select just filled rows
trait_data_1 <- trait_data[1:1701,]
#check data structure
str(trait_data_1)
#filter data, select species with flower level info and capitulum
trait_filtered <- filter(trait_data_1, Info_level == "flower" |  Info_level == "capitulum")
levels(as.factor(trait_filtered$Info_level))
#check data structure
str(trait_filtered)

#select columns of interest
traits <- trait_filtered %>% select(Order_all,Family_all,Genus_all,Species_all,Breeding_system,IMPUTED_Compatibility,Autonomous_selfing_level,
                          Autonomous_selfing_level_data_type,Autonomous_selfing_level_fruit_set,Flower_morphology,Flower_symmetry,Flowers_per_plant,Flowers_per_inflorescence,
                          Floral_unit_width,Corolla_diameter_mean,Corolla_length_mean,STYLE_IMPUTED,OVULES_IMPUTED,life_form,lifespan,IMPUTED_plant_height_mean_m)


#Remove duplicated species
t <- traits[!duplicated(traits$Species_all), ]
#check structure of the data
str(t)
#remove cases where the species is NA 
t <- t[!is.na(t$Species_all), ]


#HOW TO HANDLE MISSING VALUES WITH MCA/PCA
#I'm going to follow the procedure explained in factominer webpage
#http://factominer.free.fr/missMDA/index.html

#ALSO http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/115-famd-factor-analysis-of-mixed-data-in-r-essentials/

str(t)
t_trial <- t[,c(7,9,12:18,21)]
str(t_trial)
cols <- c( "Autonomous_selfing_level")

t_trial[cols] <- lapply(t_trial[cols], factor)  ## as.factor() could also be used

colnames(t_trial) <- c("Autonomous_selfing_level", "Quantitative selfing", "Flower/plant", "Flower/inflorescence", "Floral width", "Flower width",
                       "Flower length", "Style length", "Ovule number", "Plant height")

str(t_trial$Autonomous_selfing_level)

res.impute <- imputeFAMD(t_trial, ncp=3,threshold = 1e-06) 
res.impute$completeObs$Autonomous_selfing_level <- factor(res.impute$completeObs$Autonomous_selfing_level, levels = c("high", "medium", "low", "none"))

#from http://www.zhuoyao.net/2019/11/08/principal-component-methods-in-r-practical-guide/
fviz_pca_biplot(prcomp(res.impute$completeObs[-c(414,480,482,634,705,1139),c(2:10)], scale=T), label="none",
                # Individuals
                geom.ind = "point",alpha.ind = 0.9, 
                fill.ind = res.impute$completeObs[-c(414,480,482,634,705,1139),1], col.ind = "black",
                pointshape = 21, pointsize = 1.5,
                palette = c("#FC4E07","#00AFBB", '#800080',"#FFA500"	),
                # Variables
                col.var = "black",title="",  arrowsize = 0.7,alpha.var=0.8,legend.title = list(fill = "Selfing", color = "Contrib",
                alpha = "Contrib",repel = TRUE))+xlim(-3, 10) + ylim (-15, 10)+annotate(geom="text", x=0.45, y=-6.4, label="Plant height")+
  annotate(geom="text", x=-2, y=5, label="Quantitative selfing")+annotate(geom="text", x=-2, y=5, label="Quantitative selfing")+
  annotate(geom="text", x=-1.2, y=-6.4, label="Flowers/plant")+annotate(geom="text", x=4.4, y=-4.4, label="Inflorescence width")+ 
  annotate(geom="text", x=-2, y=-3, label="Flowers/inflorescence")+ annotate(geom="text", x=2.5, y=1.5, label="Ovule number")+ 
  annotate(geom="text", x=8, y=1, label="Flower length")+ annotate(geom="text", x=7.7, y=0.2, label="Flower width")+ 
  annotate(geom="text", x=7.2, y=-0.3, label="Style length")+border()+rremove("grid")



#Now I'm going to create two broad categories of selfers and non-selfers, it may help visualization
#the selfers are going to be the ones that have hoigh and medium selfing and the non-selfers none and low

res.impute <- imputeFAMD(t_trial, ncp=3,threshold = 1e-06) 
res.impute$completeObs$Autonomous_selfing_level <- as.character(res.impute$completeObs$Autonomous_selfing_level)
res.impute$completeObs$Autonomous_selfing_level[res.impute$completeObs$Autonomous_selfing_level=="high"] <- "selfer"
res.impute$completeObs$Autonomous_selfing_level[res.impute$completeObs$Autonomous_selfing_level=="medium"] <- "selfer"
res.impute$completeObs$Autonomous_selfing_level[res.impute$completeObs$Autonomous_selfing_level=="low"] <- "non_selfer"
res.impute$completeObs$Autonomous_selfing_level[res.impute$completeObs$Autonomous_selfing_level=="none"] <- "non_selfer"
res.impute$completeObs$Autonomous_selfing_level <- factor(res.impute$completeObs$Autonomous_selfing_level, levels = c("selfer", "non_selfer", "low"))

#from http://www.zhuoyao.net/2019/11/08/principal-component-methods-in-r-practical-guide/
fviz_pca_biplot(prcomp(res.impute$completeObs[-c(414,480,482,634,705,1139),c(2:10)], scale=T), label="none",habillage = res.impute$completeObs[-c(414,480,482,634,705,1139),1],
                # Individuals
                geom.ind = "point",alpha.ind = 0.8, 
                fill.ind = res.impute$completeObs[-c(414,480,482,634,705,1139),1], col.ind = "black",
                pointshape = 21, pointsize = 1.5,
                palette = c("#FFA500","#00AFBB"	),addEllipses = TRUE,
                # Variables
                col.var = "black",title="",  arrowsize = 0.7,alpha.var=0.8,legend.title = list(fill = "Selfing", repel = TRUE))+
  xlim(-10, 10) + ylim (-15, 10)+annotate(geom="text", x=0.45, y=-6.4, label="Plant height")+
  annotate(geom="text", x=-2, y=5, label="Quantitative selfing")+annotate(geom="text", x=-2, y=5, label="Quantitative selfing")+
  annotate(geom="text", x=-1.2, y=-6.4, label="Flowers/plant")+annotate(geom="text", x=4.4, y=-4.4, label="Inflorescence width")+ 
  annotate(geom="text", x=-2, y=-3, label="Flowers/inflorescence")+ annotate(geom="text", x=2.5, y=1.5, label="Ovule number")+ 
  annotate(geom="text", x=8, y=1, label="Flower length")+ annotate(geom="text", x=7.7, y=0.2, label="Flower width")+ 
  annotate(geom="text", x=7.2, y=-0.3, label="Style length")+border()+rremove("grid")



#Now removing quantitative selfing

str(t)
t_trial <- t[,c(7,12:18,21)]
str(t_trial)
cols <- c( "Autonomous_selfing_level")

t_trial[cols] <- lapply(t_trial[cols], factor)  ## as.factor() could also be used

colnames(t_trial) <- c("Autonomous_selfing_level",  "Flower/plant", "Flower/inflorescence", "Floral width", "Flower width",
                       "Flower length", "Style length", "Ovule number", "Plant height")

str(t_trial$Autonomous_selfing_level)

res.impute <- imputeFAMD(t_trial, ncp=3,threshold = 1e-06) 
res.impute$completeObs$Autonomous_selfing_level <- factor(res.impute$completeObs$Autonomous_selfing_level, levels = c("high", "medium", "low", "none"))

#from http://www.zhuoyao.net/2019/11/08/principal-component-methods-in-r-practical-guide/
fviz_pca_biplot(prcomp(res.impute$completeObs[-c(414,480,482,634,705,1139),c(2:9)], scale=T), label="none",
                # Individuals
                geom.ind = "point",alpha.ind = 0.9, 
                fill.ind = res.impute$completeObs[-c(414,480,482,634,705,1139),1], col.ind = "black",
                pointshape = 21, pointsize = 1.5,
                palette = c("#FC4E07","#00AFBB", '#800080',"#FFA500"	),
                # Variables
                col.var = "black",title="",  arrowsize = 0.7,alpha.var=0.8,legend.title = list(fill = "Selfing", color = "Contrib",
                                                                                               alpha = "Contrib",repel = TRUE))+xlim(-3, 10) + ylim (-15, 10)+annotate(geom="text", x=0.45, y=-6.4, label="Plant height")+
  annotate(geom="text", x=-2, y=5, label="Quantitative selfing")+annotate(geom="text", x=-2, y=5, label="Quantitative selfing")+
  annotate(geom="text", x=-1.2, y=-6.4, label="Flowers/plant")+annotate(geom="text", x=4.4, y=-4.4, label="Inflorescence width")+ 
  annotate(geom="text", x=-2, y=-3, label="Flowers/inflorescence")+ annotate(geom="text", x=2.5, y=1.5, label="Ovule number")+ 
  annotate(geom="text", x=8, y=1, label="Flower length")+ annotate(geom="text", x=7.7, y=0.2, label="Flower width")+ 
  annotate(geom="text", x=7.2, y=-0.3, label="Style length")+border()+rremove("grid")


#Now colour by flower shape


str(t)
t_trial <- t[,c(9,10,12:18,21)]
str(t_trial)
cols <- c( "Autonomous_selfing_level")

t_trial[cols] <- lapply(t_trial[cols], factor)  ## as.factor() could also be used

colnames(t_trial) <- c("Quantitative selfing", "Flower morphology", "Flower/plant", "Flower/inflorescence", "Floral width", "Flower width",
                       "Flower length", "Style length", "Ovule number", "Plant height")

str(t_trial$`Flower morphology`)
t_trial$`Flower morphology` <- as.factor(t_trial$`Flower morphology`)
res.impute <- imputeFAMD(t_trial, ncp=3,threshold = 1e-06) 
#res.impute$completeObs$Autonomous_selfing_level <- factor(res.impute$completeObs$Autonomous_selfing_level, levels = c("high", "medium", "low", "none"))
res.impute$completeObs$`Flower morphology` <- as.factor(res.impute$completeObs$`Flower morphology`)
levels(res.impute$completeObs$`Flower morphology`)

res.impute$completeObs$`Flower morphology`<- as.character(res.impute$completeObs$`Flower morphology`)
res.impute$completeObs$`Flower morphology`[res.impute$completeObs$`Flower morphology`=="bowl"]<- "open"
res.impute$completeObs$`Flower morphology`[res.impute$completeObs$`Flower morphology`=="dish"]<- "open"
res.impute$completeObs$`Flower morphology`[res.impute$completeObs$`Flower morphology`=="exposed"]<- "open"
res.impute$completeObs$`Flower morphology`[res.impute$completeObs$`Flower morphology`=="spadix"]<- "spike"

res.impute$completeObs$`Flower morphology` <- as.factor(res.impute$completeObs$`Flower morphology`)
library("viridis")        

gsub("IMPUTED_Compatibility_", "", res.impute$completeObs$IMPUTED_Compatibility)

#from http://www.zhuoyao.net/2019/11/08/principal-component-methods-in-r-practical-guide/
fviz_pca_biplot(prcomp(res.impute$completeObs[-c(414,480,482,634,705,1139),c(1,3:10)], scale=T), label="var",
                # Individuals
                geom.ind = "point",alpha.ind = 0.9, 
                fill.ind = res.impute$completeObs[-c(414,480,482,634,705,1139),c(2)], col.ind = "black",
                pointshape = 21, pointsize = 1.5,
                palette = "viridis",
                # Variables
                col.var = "black",title="",  arrowsize = 0.7,alpha.var=0.8,legend.title = list(fill = "Selfing"))+xlim(-3, 10) + ylim (-15, 10)
  
  annotate(geom="text", x=0.45, y=-6.4, label="Plant height")+
  annotate(geom="text", x=-2, y=5, label="Quantitative selfing")+annotate(geom="text", x=-2, y=5, label="Quantitative selfing")+
  annotate(geom="text", x=-1.2, y=-6.4, label="Flowers/plant")+annotate(geom="text", x=4.4, y=-4.4, label="Inflorescence width")+ 
  annotate(geom="text", x=-2, y=-3, label="Flowers/inflorescence")+ annotate(geom="text", x=2.5, y=1.5, label="Ovule number")+ 
  annotate(geom="text", x=8, y=1, label="Flower length")+ annotate(geom="text", x=7.7, y=0.2, label="Flower width")+ 
  annotate(geom="text", x=7.2, y=-0.3, label="Style length")+border()+rremove("grid")

  
  
#Now colour by life form
  
str(t)
t_trial <- t[,c(5,6,7,9:21)]
str(t_trial)
cols <- c( "Breeding_system","IMPUTED_Compatibility", "Autonomous_selfing_level","Flower_morphology","Flower_symmetry", "life_form", "lifespan")

t_trial[cols] <- lapply(t_trial[cols], factor)  ## as.factor() could also be used

#colnames(t_trial) <- c("Breeding_system","IMPUTED_Compatibility","Quantitative selfing",  "Flower/plant", "Flower/inflorescence", "Inflorescence width", "Flower width",
 #                      "Flower length", "Style length", "Ovule number", "Life form", "Plant height")

str(t_trial$`Life form`)
t_trial$life_form<- as.factor(t_trial$life_form)
res.impute <- imputeFAMD(t_trial, ncp=3,threshold = 1e-06) 

res.impute$completeObs$life_form


fviz_pca_biplot(prcomp(res.impute$completeObs[-c(414,480,482,634,705,1139),c(4,7,8,9,10,11,12,13,16)], scale=T), label="var",
                # Individuals
                geom.ind = "point",alpha.ind = 1, col.ind=res.impute$completeObs[-c(414,480,482,634,705,1139),c(14)], 
                pointshape = 19, pointsize = 1,
                palette = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"),
                # Variables
                col.var = "black",title="",  arrowsize = 0.6,alpha.var=0.7,legend.title = list(fill = "Life form"))+xlim(-10, 10) + ylim (-15, 10)+border()

annotate(geom="text", x=0.45, y=-6.4, label="Plant height")+
  annotate(geom="text", x=-2, y=5, label="Quantitative selfing")+annotate(geom="text", x=-2, y=5, label="Quantitative selfing")+
  annotate(geom="text", x=-1.2, y=-6.4, label="Flowers/plant")+annotate(geom="text", x=4.4, y=-4.4, label="Inflorescence width")+ 
  annotate(geom="text", x=-2, y=-3, label="Flowers/inflorescence")+ annotate(geom="text", x=2.5, y=1.5, label="Ovule number")+ 
  annotate(geom="text", x=8, y=1, label="Flower length")+ annotate(geom="text", x=7.7, y=0.2, label="Flower width")+ 
  annotate(geom="text", x=7.2, y=-0.3, label="Style length")+border()+rremove("grid")

a <- viridis(3)
rev(a)
