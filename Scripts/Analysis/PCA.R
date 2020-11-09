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


