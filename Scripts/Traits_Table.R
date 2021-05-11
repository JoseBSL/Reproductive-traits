
########################################################################################################################################################
#SCRIPT TO PREPARE TABLE WITH TRAITS
########################################################################################################################################################

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
library(naniar)
library(VIM)

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
trait_data_1 <- trait_data[1:1712,] #It may be a more elegant way but this does the job, 1712 rows of data

str(trait_data_1)
#filter data, select species with flower level info and capitulum
trait_filtered <- filter(trait_data_1, Info_level == "flower" |  Info_level == "capitulum")
levels(as.factor(trait_filtered$Info_level)) #checking levels
#remove NA's and duplicated species|Some spp are repeated once the spp names are standardize with taxize
trait_filtered$Species_all[trait_filtered$Species_all=="NA"]<-NA 
trait_filtered_1 <- trait_filtered[!is.na(trait_filtered$Species_all),]
trait_filtered_2 <- trait_filtered_1[!duplicated(trait_filtered_1$Species_all),]
str(trait_filtered_2)

#Select columns to work with
t <- trait_filtered_2[c("Species_geonet","Family_all","Genus_all","Species_all","Breeding_system","Compatibility_system",
                        "Autonomous_selfing_level","Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", 
                        "Flowers_per_plant", "Floral_unit_width", "Corolla_diameter_mean", "Corolla_length_mean","Style_length", "Ovule_number", 
                        "life_form", "lifespan","Plant_height_mean_m","Nectar_presence_absence","Nectar_ul")]


####################################################################################################
#2) CLEAN DATA. I REPEAT THE SAME PROCEDURE OF THE DATA IMPUTATION TO GET THE SAME NUMBER OF SPECIES
####################################################################################################
#Before data imputation I need to calculate the phylo of the species
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

nrow(t)

#Select just columns with traits
t <- t[c("Breeding_system","Compatibility_system",
         "Autonomous_selfing_level","Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", 
         "Flowers_per_plant", "Floral_unit_width", "Corolla_diameter_mean", "Corolla_length_mean","Style_length", "Ovule_number", 
         "life_form", "lifespan","Plant_height_mean_m","Nectar_presence_absence","Nectar_ul")]

str(t)


missing.values <- t %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

n_of_records <- subset(missing.values, isna=="FALSE")

#Try to generate table of traits

library(kableExtra)
dt <- mtcars[1:5, 1:6]

colnames(dt)[2] <-"mpg"

kbl(dt) %>%
  kable_paper() %>%
  add_header_above(c(" ", "Group 1" = 2, "Group 2" = 2, "Group 7" = 2)) %>%
  add_header_above(c(" ", "Group 4" = 4, "Group 5" = 2)) %>%
  add_header_above(c(" ", "Group 6" = 6))
str(dt)


Trait <- c("Plant height (m)", " ", "Flower width (mm)", "Flower length (mm)", "Inflorescence width (mm)", "Style length (mm)", "Ovule number/flower",
           "Flowers per plant", "Nectar $(\\mu l)$", "Nectar (mg)", "Nectar concentration (%)", "Pollen grains per flower", "Autonomous selfing (fruit set)")

Trait1 <- c("Lifepan", "Life form", "Flower shape", "Flower symmetry", "Nectar", "Autonomous selfing", "Compatibility system", "Breeding system", " ", " ", " ", " ", " ")

T.cat <- c("Short-lived, perennial", "Herb, shrub, tree", "Brush, campanulate, capitulum, open, papilionaceous, tube", "Actinomorphic, zygomorphic",
           "Presence/absence", "None, low, medium, high", "Incompatible, partially compatible, compatible", "Hermaphrodite, monoecious, dioecious"," ", " ", " ", " ", " ")

Type <- c("Vegetative", "Vegetative", "Floral", "Floral", "Floral", "Floral", "Floral", "Floral","Floral","Floral","Floral","Floral","Reproductive")

Type_1 <- c("Vegetative", "Vegetative", "Floral", "Floral", "Floral", "Floral", "Reproductive", "Reproductive","Reproductive","","","","")


dat <- data.frame(Type, Trait,Type_1, Trait1, T.cat)


colnames(dat) <- c("Trait Type","Traits","Trait Type", "Traits", "Categories")



kbl(dat) %>%
  kable_paper() %>%
  add_header_above(c("Quantitative traits" = 2,"Qualitative traits" = 3)) %>%  column_spec(c(1,3), bold = T,width="2em") %>%
  collapse_rows(columns = c(1,3), valign = "middle") 



kable(dat) %>%
  kable_styling( full_width = F) %>%
  column_spec(3:5, bold = T, color = "grey", background = "#D3D3D3")



library(tidyverse)
library(kableExtra)

set.seed(121)
Dat <- data.frame(
  ID1 = sample(c("AAA", "BBB", "CCC","DDD"), 100, replace = T),
  ID2 = sample(c("Cat", "Dog", "Bird"), 100, replace = T),
  First = rnorm(100),
  Two = sample.int(100)) 

ExTbl <- Dat %>%
  group_by(ID1, ID2) %>%
  summarize(One = mean(First),
            Max = max(Two)) %>%
  arrange(ID1) 

ind_end <- cumsum(rle(as.character(ExTbl$ID1))$lengths)
ind_start <- c(1, ind_end[-length(ind_end)] + 1)
pos <- purrr::map2(ind_start, ind_end, seq)
pos <- unlist(pos[1:length(pos) %% 2 != 0])

kable(ExTbl) %>%
  kable_styling(c("bordered"), full_width = F) %>%
  column_spec(c(1,2), background = "#EEEEEE")


library(dplyr)

 mtcars[1:10, 1:2] %>%
 mutate(
 car = row.names(.),
 mpg = cell_spec(mpg, "html", color = ifelse(mpg > 20, "red", "blue")),
 cyl = cell_spec(cyl, "html", color = "white", align = "c", angle = 45,
 background = factor(cyl, c(4, 6, 8),
 c("#666666", "#999999", "#BBBBBB")))
 ) %>%
 select(car, mpg, cyl) %>%
 kbl(format = "html", escape = F) %>%
 kable_styling("striped", full_width = F)
 
 cs_dt <- mtcars[1:10, 1:2]
 cs_dt$car = row.names(cs_dt)
 row.names(cs_dt) <- NULL
 cs_dt$mpg = cell_spec(cs_dt$mpg, color = ifelse(cs_dt$mpg > 20, "red", "blue"))
 cs_dt$cyl = cell_spec(
   cs_dt$cyl, color = "white", align = "c", angle = 45,
   background = factor(cs_dt$cyl, c(4, 6, 8), c("#666666", "#999999", "#BBBBBB")))
 cs_dt <- cs_dt[c("car", "mpg", "cyl")]
 kbl(cs_dt, booktabs = T, escape = F) %>%
   kable_paper("striped", full_width = F)
 
 

 
 