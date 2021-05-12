########################################################################################################################################################
#TRAIT TABLE
########################################################################################################################################################
#LOAD LIBRARIES
library(readxl) #read excel file (trait data)
library(dplyr) #data manipulation
library(visdat) #VISUALIZE MISSING DATA
library(naniar)
library(tidyverse)
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
t <- trait_filtered_2[c("Species_geonet","Order_all","Family_all","Genus_all","Species_all","Breeding_system","Compatibility_system",
                        "Autonomous_selfing_level","Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", 
                        "Flowers_per_plant","Floral_unit_width", "Corolla_diameter_mean", "Corolla_length_mean","Style_length", "Ovule_number", 
                        "life_form", "lifespan","Plant_height_mean_m","Nectar_presence_absence","Nectar_ul","Nectar_mg","Nectar_concentration","Pollen_per_flower")]

########################################################################################################################################################
#####
#B) RENAME LEVELS FOR IMPUTATION
#####
#Breeding_system| For the purpose of this study we are not going to consider other complex breeding systems and just focus on 
#Hermaphroditism, dioecy and monoecy, the closest levels are grouped within each category
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
#Change to capital letters| There are already levels with lower case
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

########################################################################################################################################################
#3) EXPLORE PATTERNS OF MISSING DATA
########################################################################################################################################################
#Explore patterns of missing data
missing_data <- unlist(lapply(t, function(x) sum(is.na(x))))/nrow(t)
sort(missing_data[missing_data >= 0], decreasing=T)
#nectar values all above 50% except presence/absence
#then, just one cololumn with 68% missing data, then one with 35% and then 20% and 10%. The rest under 5%.
#it is recommended to remove columns over 50% but we are particulary interested in having numeric levels of quantitative selfing 
#we are going to work just with presence/absence of nectar
#we will perform further analysis just with a subset with the quantitative values o nectar (very interesting data and diffciult to obtain)


#We convert cualitative data to quantitative to reduce missing data on this column and hence conduct the imputation 
#use ifelse base r does not work with these NA'S Even with work around. base r does not like the na option of read excel
t$Autonomous_selfing_level_fruit_set <- ifelse(t$Autonomous_selfing_level %in% c("high") & is.na(t$Autonomous_selfing_level_fruit_set), 88, t$Autonomous_selfing_level_fruit_set)
t$Autonomous_selfing_level_fruit_set <- ifelse(t$Autonomous_selfing_level %in% c("medium") & is.na(t$Autonomous_selfing_level_fruit_set), 50.5, t$Autonomous_selfing_level_fruit_set)
t$Autonomous_selfing_level_fruit_set <- ifelse(t$Autonomous_selfing_level %in% c("low") & is.na(t$Autonomous_selfing_level_fruit_set), 13, t$Autonomous_selfing_level_fruit_set)
t$Autonomous_selfing_level_fruit_set <- ifelse(t$Autonomous_selfing_level %in% c("none") & is.na(t$Autonomous_selfing_level_fruit_set), 0, t$Autonomous_selfing_level_fruit_set)

#t$Nectar_ul adding it here just to see how many cells 
#Now we just select presence/absence of nectar
#select columns of interest
t <- t[c("Species_geonet","Order_all","Family_all","Genus_all","Species_all","Breeding_system","Compatibility_system","Autonomous_selfing_level",
         "Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant", "Floral_unit_width",
         "Corolla_diameter_mean", "Corolla_length_mean", "Style_length", "Ovule_number", "life_form", "lifespan",
         "Plant_height_mean_m","Nectar_presence_absence","Nectar_ul","Nectar_mg","Nectar_concentration","Pollen_per_flower")]


#check missing data now
missing_data <- unlist(lapply(t, function(x) sum(is.na(x))))/nrow(t)*100
sort(missing_data[missing_data >= 0], decreasing=T)


missing_data <- unlist(lapply(t, function(x) nrow(t)-sum(is.na(x))))
sum(missing_data)

#Number of missing values on the datset
#vis_miss(t[,c(6:20)])
#9.9% percentage of missing values]

#Checking number of filled cells/ just selecting variables used in the manuscript; nectar ul should be accounted too and it has been considered but
#not added to run this code

########################################################################################################################################################
#4) CALCULATE PHYLOGENETIC DISTANCE TO CORRECT WITH EIGENVALUES IN THE IMPUTATION
########################################################################################################################################################
#Convert to factor 
cols <- c("Order_all", "Family_all", "Genus_all", "Species_all", "Compatibility_system", "Autonomous_selfing_level", "Flower_morphology",
          "Flower_symmetry", "life_form", "lifespan","Nectar_presence_absence")
t[cols] <- lapply(t[cols], factor)  ## as.factor() could also be used
str(t)


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


Trait <- c("Plant height (m)", " ", "Flower width (mm)", "Flower length (mm)", "Inflorescence width (mm)", "Style length (mm)", "Ovule number/flower",
           "Flowers per plant", "Nectar $(\\mu l)$", "Nectar (mg)", "Nectar concentration (%)", "Pollen grains per flower", "Autonomous selfing (fruit set)")

Trait1 <- c("Lifepan", "Life form", "Flower shape", "Flower symmetry", "Nectar", "Autonomous selfing", "Compatibility system", "Breeding system", " ", " ", " ", " ", " ")

T.cat <- c("Short-lived, perennial", "Herb, shrub, tree", "Brush, campanulate, capitulum, open, papilionaceous, tube", "Actinomorphic, zygomorphic",
           "Presence/absence", "None, low, medium, high", "Incompatible, partially compatible, compatible", "Hermaphrodite, monoecious, dioecious"," ", " ", " ", " ", " ")

Type <- c("Vegetative", "Vegetative", "Floral", "Floral", "Floral", "Floral", "Floral", "Floral","Floral","Floral","Floral","Floral","Reproductive")

Type_1 <- c("Vegetative", "Vegetative", "Floral", "Floral", "Floral", "Floral", "Reproductive", "Reproductive","Reproductive","","","","")


levels(factor(n_of_records$key))

Number_of_records_quantitative <- c(n_of_records$num.isna[n_of_records$key=="Plant_height_mean_m"], " ", n_of_records$num.isna[n_of_records$key=="Corolla_diameter_mean"],
  n_of_records$num.isna[n_of_records$key=="Corolla_length_mean"], n_of_records$num.isna[n_of_records$key=="Floral_unit_width"],
  n_of_records$num.isna[n_of_records$key=="Style_length"], n_of_records$num.isna[n_of_records$key=="Ovule_number"],
  n_of_records$num.isna[n_of_records$key=="Flowers_per_plant"], n_of_records$num.isna[n_of_records$key=="Nectar_ul"],
  n_of_records$num.isna[n_of_records$key=="Nectar_mg"], n_of_records$num.isna[n_of_records$key=="Nectar_concentration"], n_of_records$num.isna[n_of_records$key=="Pollen_per_flower"],
  n_of_records$num.isna[n_of_records$key=="Autonomous_selfing_level_fruit_set"]) 


Number_of_records_categorical <- c(n_of_records$num.isna[n_of_records$key=="lifespan"], n_of_records$num.isna[n_of_records$key=="life_form"],
                                   n_of_records$num.isna[n_of_records$key=="Flower_morphology"], n_of_records$num.isna[n_of_records$key=="Flower_symmetry"],
                                   n_of_records$num.isna[n_of_records$key=="Nectar_presence_absence"], n_of_records$num.isna[n_of_records$key=="Autonomous_selfing_level"],
                                   n_of_records$num.isna[n_of_records$key=="Compatibility_system"], n_of_records$num.isna[n_of_records$key=="Breeding_system"],
                                   rep("",5))

dat <- data.frame(Type, Trait,Number_of_records_quantitative,Type_1, Trait1, T.cat,Number_of_records_categorical)


colnames(dat) <- c("Trait Type","Traits", "Number of records","Trait Type", "Traits", "Categories", "Number of records")



kbl(dat) %>%
  kable_paper() %>%
  add_header_above(c("Quantitative traits" = 3,"Qualitative traits" = 4)) %>%  column_spec(c(1,4), bold = T,width="2em") %>%
  collapse_rows(columns = c(1,3), valign = "middle") 

