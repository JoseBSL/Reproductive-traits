########################################################################################################################################################
#CORRELATION BETWEEN QUALITATIVE VARIABLES AND PC's
########################################################################################################################################################
#NOTES
#I did not include the qualitative variables when I prepare the data for the PPCA
#So I have to recalculate the ata here with the qualitative variables
########################################################################################################################################################
#LOAD LIBRARIES
library(emmeans)
########################################################################################################################################################
#LOAD DATA
phyl_pca_forest <- readRDS(phyl_pca_forest, "Data/RData/phyl_pca_forest.rds")
dat <- read.csv("Data/Csv/all_species_imputed_trait_data_forest_data.csv", row.names = "X")
########################################################################################################################################################
#remove not found species, cannot do PCA with unequal numbers of rows
cols.num <- c("Family_all","Genus_all","Species_all")
dat[cols.num] <- sapply(dat[cols.num],as.character)
dat$Species_all <- gsub("Species_all_", "", dat$Species_all)
########################################################################################################################################################
#3) REMOVE OUTLIERS, OUT OF 2.5-97.5 RANGE WHICH HELPS IMPUTATION PROCESS. SEE ARTICLE FOR REF.
########################################################################################################################################################
dat_cleaning <- dat[,]
#CHECK LEVELS
str(dat)

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


final_d <- dat_cleaning_5[,]

pca_data <- as.data.frame(phyl_pca_forest$S)

d <- cbind(final_d, pca_data)

#Check dist of the pcs's
hist(d$PC1)
hist(d$PC2)
hist(d$PC3)


#Prepare qualitative variables
d$Compatibility_system[d$Compatibility_system=="dioecious"] <- "Unisexual flowers"
d$Compatibility_system[d$Compatibility_system=="monoecious"] <- "Unisexual flowers"
d$Compatibility_system[d$Compatibility_system=="partially_self_compatible"] <- "Partially self compatible"
d$Compatibility_system[d$Compatibility_system=="self_compatible"] <- "Self compatible"
d$Compatibility_system[d$Compatibility_system=="self_incompatible"] <- "Self incompatible"
d$Compatibility_system <- factor(d$Compatibility_system, levels = c("Self compatible", "Partially self compatible", "Self incompatible", "Unisexual flowers"))

d$life_form[d$life_form=="herb"] <- "Herb"
d$life_form[d$life_form=="shrub"] <- "Shrub"
d$life_form[d$life_form=="tree"] <- "Tree"
d$life_form[d$life_form=="vine"] <- "Shrub"

d$Flower_symmetry[d$Flower_symmetry=="actinomorphic"] <- "Actinomorphic"
d$Flower_symmetry[d$Flower_symmetry=="zygomorphic"] <- "Zygomorphic"

d$Flower_symmetry[d$Flower_symmetry=="Funnelform"] <- "Campanulate"
d$Flower_symmetry[d$Flower_symmetry=="Spike"] <- "Brush"

#The other qualitative are ok

#All principal components are normally distributed
#Convert to factor before running model
d[sapply(d, is.character)] <- lapply(d[sapply(d, is.character)], as.factor)

#Run model
model1 <- lm(PC1 ~ Breeding_system + Compatibility_system + lifespan + life_form + Flower_morphology + Flower_symmetry + Nectar_presence_absence, d)

#Check summary
summary(model1)
#Run Anova to test for associations
anova(model1)

#Compare differences of the association between groups
#1ST BREEDING SYSTEM
model_breeding <- emmeans(model1, "Breeding_system")
pairs(model_breeding)

#Compatibility
model_com <- emmeans(model1, "Compatibility_system")
pairs(model_com)

#life_form
model_life_form <- emmeans(model1, "life_form")
pairs(model_life_form)

#Flower symmetry
model_sym <- emmeans(model1, "Flower_symmetry")
pairs(model_sym)

#Flower morphology
model_mor <- emmeans(model1, "Flower_morphology")
pairs(model_mor)

#Nectar_presence_absence
model_nec <- emmeans(model1, "Nectar_presence_absence")
pairs(model_nec)

#Nectar_presence_absence
model_lifespan <- emmeans(model1, "lifespan")
pairs(model_lifespan)

########################################################################################################################################################
#Run model
model2 <- lm(PC2 ~ Breeding_system + Compatibility_system + lifespan + life_form + Flower_morphology + Flower_symmetry + Nectar_presence_absence, d)
#Check summary
summary(model2)
#Run Anova to test for associations
anova(model2)

model_breeding <- emmeans(model2, "Breeding_system")
pairs(model_breeding)

#Compatibility
model_com <- emmeans(model2, "Compatibility_system")
pairs(model_com)

#life_form
model_life_form <- emmeans(model2, "life_form")
pairs(model_life_form)

#Flower symmetry
model_sym <- emmeans(model2, "Flower_symmetry")
pairs(model_sym)

#Flower morphology
model_mor <- emmeans(model2, "Flower_morphology")
pairs(model_mor)

#Nectar_presence_absence
model_nec <- emmeans(model2, "Nectar_presence_absence")
pairs(model_nec)

#Nectar_presence_absence
model_lifespan <- emmeans(model2, "lifespan")
pairs(model_lifespan)

########################################################################################################################################################
#Run model
model3 <- lm(PC3 ~ Breeding_system + Compatibility_system + lifespan + life_form + Flower_morphology + Flower_symmetry + Nectar_presence_absence, d)
#Check summary
summary(model3)
#Run Anova to test for associations
anova(model3)

model_breeding <- emmeans(model3, "Breeding_system")
pairs(model_breeding)

#Compatibility
model_com <- emmeans(model3, "Compatibility_system")
pairs(model_com)

#life_form
model_life_form <- emmeans(model3, "life_form")
pairs(model_life_form)

#Flower symmetry
model_sym <- emmeans(model3, "Flower_symmetry")
pairs(model_sym)

#Flower morphology
model_mor <- emmeans(model3, "Flower_morphology")
pairs(model_mor)

#Nectar_presence_absence
model_nec <- emmeans(model3, "Nectar_presence_absence")
pairs(model_nec)

#Nectar_presence_absence
model_lifespan <- emmeans(model3, "lifespan")
pairs(model_lifespan)


phyl_pca_forest$Evec
