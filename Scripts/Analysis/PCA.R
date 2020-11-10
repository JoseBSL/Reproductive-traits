###################################################################################
####
# MULTIPLE CORRESPONDANCE ANALYSIS (MCA)
####
###################################################################################

#LOAD LIBRARIES
library(readxl)
library(dplyr)
library(missMDA)


#LOAD DATA
trait_data <- read_excel("Data/Trait_data_raw/Trait_data_final.xlsx",na = "NA")

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


#HOW TO HANDLE MISSING VALUES WITH MCA
#I'm going to follow the procedure explained in factominer webpage
#http://factominer.free.fr/missMDA/index.html

#ALSO http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/115-famd-factor-analysis-of-mixed-data-in-r-essentials/

t_trial <- t[,c(5,6,7,9,10:21)]
str(t_trial)
cols <- c("Breeding_system", "IMPUTED_Compatibility", "Autonomous_selfing_level", "Flower_morphology", "Flower_symmetry","life_form", "lifespan")
t_trial[cols] <- lapply(t_trial[cols], factor)  ## as.factor() could also be used


res.impute <- imputeFAMD(t_trial, ncp=3,threshold = 1e-06) 

#checking  
res.afdm <- FAMD(t_trial,tab.disj=res.impute$tab.disj) 

#QUANTITATIVE VARIABLES

fviz_screeplot(res.afdm)
fviz_famd_var(res.afdm, repel = TRUE)
fviz_contrib(res.afdm, "var", axes = 1)
quanti.var <- get_famd_var(res.afdm, "quanti.var")
quanti.var 
fviz_famd_var(res.afdm, "quanti.var", repel = TRUE,
              col.var = "black")



fviz_famd_var(res.afdm, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)


# Color by cos2 values: quality on the factor map
fviz_famd_var(res.afdm, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)


#QUALITATIVE VARIABLES
fviz_famd_var(res.afdm, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

#BY INDV
ind <- get_famd_ind(res.afdm)
ind
fviz_famd_ind(res.afdm, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)


fviz_mfa_ind(res.afdm,  addEllipses = TRUE, ellipse.type = "confidence", repel = TRUE ) 


#Lets try to select just one categorical variable
str(t)
t_trial <- t[,c(7,9,12:18,21)]
str(t_trial)
cols <- c( "Autonomous_selfing_level")
t_trial[cols] <- lapply(t_trial[cols], factor)  ## as.factor() could also be used
res.impute <- imputeFAMD(t_trial, ncp=3,threshold = 1e-06) 
str(res.impute)
#Conducting factor analysis on mixed data
res.afdm <- FAMD(res.impute$completeObs) 

plot(res.afdm$quanti.var)

fviz_famd_var(res.afdm, "quanti.var", repel = TRUE,
              col.var = "black")

fviz_famd_ind(res.afdm,label = "var", geom=c("point"),arrows=T, habillage = "Autonomous_selfing_level", palette = c("#00AFBB", "#FC4E07", "#FFA500",	'#800080'),
              repel = TRUE,alpha.ind = 0.7)+xlim(-5, 10) + ylim (-10, 10)

fviz_famd_var(res.afdm, "quanti.var", geom=c("point"), col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#FC4E07", "#FFA500",	'#800080'),
              repel = TRUE)

fviz_screeplot(res.afdm)
fviz_ellipses(res.afdm, 1:2, geom = "point")
fviz_ellipses(res.afdm, c("Autonomous_selfing_level"), repel = TRUE)

fviz_famd_biplot(res.afdm)
fviz_pca_ind(res.afdm)
fviz_pca_biplot(res.afdm)
fviz_famd_var(res.afdm,choice=c("quanti.var"),geom=c("point"),repel = TRUE,palette = c("#00AFBB"))

get_pca_ind()
data(decathlon2)
decathlon.active <- decathlon2[1:23, 1:10]
res.pca <- prcomp(decathlon.active, scale = TRUE)
fviz_pca_biplot(res.pca)
PCA(completed_sleep$completeObs)

fviz_pca_biplot(res.afdm$quanti.var$coord)

#### CLEAN CODE!

#CALCULATE EVERYTHING WITH NORMAL PCA WITHOUT QUALITATIVE VARIABLES AND THE COLOUR BY THEM
library(missMDA)
str(t)

t_trial <- t[,c(7,9,12:18,21)]
str(t_trial)
cols <- c( "Autonomous_selfing_level")
t_trial[cols] <- lapply(t_trial[cols], factor)  ## as.factor() could also be used

colnames(t_trial) <- c("Autonomous_selfing_level", "Quantitative selfing", "Flower/plant", "Flower/inflorescence", "Floral width", "Flower width",
                       "Flower length", "Style length", "Ovule number", "Plant height")

t_trial$Autonomous_selfing_level <- factor(t_trial$Autonomous_selfing_level, levels = c("none", "low", "medium", "high"))


res.impute <- imputeFAMD(t_trial, ncp=3,threshold = 1e-06) 
fviz_pca_biplot(prcomp(res.impute$completeObs[,c(2:10)], scale=T), geom.ind = "point",label = "var",fill.ind = res.impute$completeObs$Autonomous_selfing_level, 
                  palette = "jco",  addEllipses = TRUE, gradient.cols = "RdYlBu")


#from http://www.zhuoyao.net/2019/11/08/principal-component-methods-in-r-practical-guide/
fviz_pca_biplot(prcomp(res.impute$completeObs[-c(414,480,482,634,705,1139),c(2:10)], scale=T), label="none",
                # Individuals
                geom.ind = "point",alpha.ind = 0.5,
                fill.ind = res.impute$completeObs[-c(414,480,482,634,705,1139),1], col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = c("#FC4E07","#00AFBB", '#800080',"#FFA500"	),
                # Variables
                 col.var = "black",title="",
              
                legend.title = list(fill = "Selfing", color = "Contrib",
                                    alpha = "Contrib",repel = TRUE)
)+xlim(-3, 10) + ylim (-15, 10)+annotate(geom="text", x=0.45, y=-6.4, label="Plant height")+annotate(geom="text", x=-2, y=5, label="Quantitative selfing")+
  annotate(geom="text", x=-2, y=5, label="Quantitative selfing")+annotate(geom="text", x=-1.2, y=-6.4, label="Flowers/plant")+
  annotate(geom="text", x=4.4, y=-4.4, label="Inflorescence width")+ annotate(geom="text", x=-2, y=-3, label="Flowers/inflorescence")+
  annotate(geom="text", x=2.5, y=1.5, label="Ovule number")+annotate(geom="text", x=8, y=1, label="Flower length")+
  annotate(geom="text", x=7.7, y=0.2, label="Flower width")+annotate(geom="text", x=7.2, y=-0.3, label="Style length")


fviz_pca_biplot(prcomp(res.impute$completeObs[-c(414,482,634,705,1139),c(2:10)], scale=T))

            
fviz_pca_biplot(prcomp(res.impute$completeObs[-c(414,480,482,634,705,1139),c(2:10)], scale=T), 
                # Individuals
                geom.ind = "point",alpha.ind = 0.9,
                fill.ind = res.impute$completeObs[-c(414,480,482,634,705,1139),1], col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = c('#800080',"#00AFBB", "#FC4E07", "#FFA500","#00AFBB"),
                # Variables
                col.var = "black",title="",
                
                legend.title = list(fill = "Selfing", color = "Contrib",
                                    alpha = "Contrib",repel = TRUE)
)+xlim(-3, 10) + ylim (-15, 10)+annotate(geom="text", x=0.45, y=-6.4, label="Plant height")+annotate(geom="text", x=-2, y=5, label="Quantitative selfing")+
  annotate(geom="text", x=-2, y=5, label="Quantitative selfing")+annotate(geom="text", x=-1, y=-6, label="Quantitative selfing")
