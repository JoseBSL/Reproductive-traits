########################################################################################################################################################
#SCRIPT TO PREPARE DATA FOR ANALYSIS 1) VISITS~ POLL. FUNCTIONAL GROUP*PRINCIPAL COMPONENTS
########################################################################################################################################################
#LOAD LIBRARIES
library(data.table)
########################################################################################################################################################
#LOAD DATA
########################################################################################################################################################
long_d <- read.csv("Data/Csv/long_format_quantitative_networks.csv", row.names = 1) #quantitative network data|weighted by frequency of visits per plant species
dat_cleaning <- read.csv("Data/Csv/all_species_imputed_trait_data_forest_data.csv") #data for PPCA
########################################################################################################################################################
#PREPARE DATA
########################################################################################################################################################
#Select data with interaction greater than 0
long_d_1 <- long_d[long_d$Interaction>0,]

#Remove other orders/guilds that are not these ones
long_d_2 <- long_d_1[!is.na(long_d_1$guild),] #I do it by guild because just these 6 guilds are named in this column

#check levels
levels(factor(long_d_2$guild)) #9 DIFFERENT GUILDS|After I'll select the main fucntional poll. groups for analysis


#set 1st col name
colnames(dat_cleaning)[1] <- "Plant_species"

datos <- merge(long_d_1, dat_cleaning, by.x = "Plant_species",  by.y = "Plant_species")


datos <- subset(datos, guild!="Other_insects" & guild!="Lizards" & guild!="Birds") #Select just the main 6 functional groups
levels(factor(datos$guild))
datos$guild <- as.character(datos$guild)
datos$guild[datos$guild=="Bee"] <- "Bees" #Adding an S to bee(S)
datos$guild <- factor(datos$guild, levels = c("Bees","Coleoptera", "Lepidoptera", "Non-bee-Hymenoptera",
                                              "Non-syrphids-diptera", "Syrphids"))



m1 <- lm(Interaction~ guild*Nectar_presence_absence, data=datos)
summary(m1)


library(jtools)
jtools::effect_plot(m1, pred = Nectar_presence_absence, modx = guild)


effect_plot(m1, pred = Interaction, modx= Nectar_presence_absence, interval = TRUE)

library(sjPlot)
library(sjmisc)
library(ggplot2)
m1 <- lm(Interaction~ guild*Nectar_presence_absence, data=datos)
summary(m1)

plot_model(m1, type = "pred", terms = c("Nectar_presence_absence", "guild"))


m1 <- lm(Interaction~ guild*Flower_symmetry, data=datos)
summary(m1)

plot_model(m1, type = "pred", terms = c("Flower_symmetry", "guild"))



m1 <- lm(Interaction~ guild*Plant_height_mean_m, data=datos)
summary(m1)

plot_model(m1, type = "pred", terms = c("Corolla_length_mean", "guild"))


m1 <- lm(Interaction~ guild*Autonomous_selfing_level, data=datos)
summary(m1)

plot_model(m1, type = "pred", terms = c("Autonomous_selfing_level", "guild"))





datos$Family_all  <- as.character(datos$Family_all)
datos$Genus_all   <- as.character(datos$Genus_all)
datos$Plant_species <- as.character(datos$Plant_species)

datos$Family_all[datos$Plant_species=="Beta maritima"] <- "Amaranthaceae"


#prepare dataframe to calculate tree
phylo_5 <- as.data.frame(cbind(datos$Family_all, datos$Genus_all, datos$Plant_species))
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
datos$phylo
datos$phylo <- datos$Plant_species

#prepare random factor
datos$System <- datos$Id

datos$System[grepl("peralta_2006", datos$System)] <- "peralta_2006"
datos$System[grepl("small_1976", datos$System)] <- "small_1976"
datos$System[grepl("arroyo_correa_new_zealand", datos$System)] <- "arroyo_correa_2019"
datos$System[grepl("fang_2008", datos$System)] <- "fang_2008"
datos$System[grepl("kaiser-bunbury_2017", datos$System)] <- "kaiser-bunbury_2017"
datos$System[grepl("inouye_1988", datos$System)] <- "inouye_1988"
datos$System[grepl("kaiser-bunbury_2010", datos$System)] <- "kaiser-bunbury_2010"
datos$System[grepl("kaiser-bunbury_2011", datos$System)] <- "kaiser-bunbury_2011"
datos$System[grepl("burkle_usa_2013", datos$System)] <- "burkle_2013"
datos$System[grepl("dicks_2002", datos$System)] <- "dicks_2002"
datos$System[grepl("dupont_2009", datos$System)] <- "dupont_2009"
datos$System[grepl("bartomeus_spain_2008_medca", datos$System)] <- "bartomeus_2008"
datos$System[grepl("bartomeus_spain_2008_batca", datos$System)] <- "bartomeus_2008"
datos$System[grepl("bartomeus_spain_2008_selop", datos$System)] <- "bartomeus_2008"
datos$System[grepl("bartomeus_spain_2008_miqop", datos$System)] <- "bartomeus_2008"
datos$System[grepl("bartomeus_spain_2008_fraop", datos$System)] <- "bartomeus_2008"
datos$System[grepl("lundgren_2005", datos$System)] <- "lundgren_2005"
datos$System[grepl("olesen_2002_mauritius", datos$System)] <- "olesen_2002_mauritius"
datos$System[grepl("olesen_2002_azores", datos$System)] <- "olesen_2002_azores"
datos$System[grepl("bartomeus_spain_2008", datos$System)] <- "bartomeus_spain_2008"
datos$System[grepl("bundgaard_2003_denmark", datos$System)] <- "bundgaard_2003"
datos$System[grepl("elberling_sweeden_1999", datos$System)] <- "elberling_1999"




levels(factor(datos$Breeding_system)) # ok
datos$Breeding_system[datos$Breeding_system=="Dioecious"] <- "Unisexual flowers"
datos$Breeding_system[datos$Breeding_system=="Monoecious"] <- "Unisexual flowers"
datos$Breeding_system[datos$Breeding_system=="Hermaphrodite"] <- "Hermaphrodite"
levels(factor(datos$Compatibility_system)) #Fix

levels(factor(datos$)) # ok
levels(factor(datos$)) #Fix



datos$Compatibility_system[datos$Compatibility_system=="dioecious"] <- "Unisexual flowers"
datos$Compatibility_system[datos$Compatibility_system=="monoecious"] <- "Unisexual flowers"
datos$Compatibility_system[datos$Compatibility_system=="self_incompatible"] <- "Self incompatible"
datos$Compatibility_system[datos$Compatibility_system=="self_compatible"] <- "Self compatible"
datos$Compatibility_system[datos$Compatibility_system=="partially_self_compatible"] <- "Partially self compatible"
#Check again
levels(factor(datos$Compatibility_system)) #NOW OK
levels(factor(datos$Autonomous_selfing_level)) 
datos$Autonomous_selfing_level[datos$Autonomous_selfing_level=="high"] <- "High-Medium"
datos$Autonomous_selfing_level[datos$Autonomous_selfing_level=="medium"] <- "High-Medium"
datos$Autonomous_selfing_level[datos$Autonomous_selfing_level=="low"] <- "Low-None"
datos$Autonomous_selfing_level[datos$Autonomous_selfing_level=="none"] <- "Low-None"
levels(factor(datos$Autonomous_selfing_level)) #Ok 
levels(factor(datos$Flower_morphology)) 
datos$Flower_morphology[datos$Flower_morphology=="Funnelform"] <- "Campanulate"
datos$Flower_morphology[datos$Flower_morphology=="Spike"] <- "Brush"
levels(factor(datos$Flower_morphology)) #OK
levels(factor(datos$Flower_symmetry)) 
datos$Flower_symmetry[datos$Flower_symmetry=="actinomorphic"] <- "Actinomorphic"
datos$Flower_symmetry[datos$Flower_symmetry=="zygomorphic"] <- "Zygomorphic"
levels(factor(datos$Flower_symmetry)) #OK
levels(factor(datos$life_form)) 
datos$life_form[datos$life_form=="herb"] <- "Herb"
datos$life_form[datos$life_form=="shrub"] <- "Shrub"
datos$life_form[datos$life_form=="tree"] <- "Tree"
datos$life_form[datos$life_form=="vine"] <- "Shrub"
levels(factor(datos$life_form)) #OK
levels(factor(datos$lifespan)) #OK
levels(factor(datos$Nectar_presence_absence))
datos$Nectar_presence_absence[datos$Nectar_presence_absence=="yes"] <- "Yes"
datos$Nectar_presence_absence[datos$Nectar_presence_absence=="no"] <- "No"
levels(factor(datos$Nectar_presence_absence)) #OK




library(brms)

detach("package:jtools", unload=TRUE)


ggplot(datos, aes(Nectar_presence_absence, Interaction, color=guild)) + geom_boxplot()

#check number of levels
datos %>% 
  group_by(Breeding_system) %>%
  summarise(no_rows = length(guild))



hist(datos$Autonomous_selfing_level_fruit_set)

#MODEL
model <- brm((Interaction-1) ~ Nectar_presence_absence*guild + life_form*guild +  Compatibility_system*guild + Breeding_system*guild + (1|System/Id) + (1|gr(phylo, cov = A)),
                    data = datos, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                    sample_prior = TRUE, warmup = 500, iter = 2000,
                    control = list(adapt_delta = 0.99))

performance::r2(model)

summary(model)
conditional_effects(model, effects = "Nectar_presence_absence:guild")
conditional_effects(model, effects = "life_form:guild")
conditional_effects(model, effects = "Compatibility_system:guild")

pp_check(model) + xlim(-5,20)

plot(m, points=TRUE)


datos_bees <- datos[datos$guild=="Bees",]

nrow(datos_bees)
#check number of levels
datos_bees %>% 
  group_by(Flower_morphology) %>%
  summarise(no_rows = length(Flower_symmetry))

str(datos_bees)




#prepare dataframe to calculate tree
phylo_5 <- as.data.frame(cbind(datos_bees$Family_all, datos_bees$Genus_all, datos_bees$Plant_species))
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
datos_bees$phylo
datos_bees$phylo <- datos_bees$Plant_species

str(datos_bees)

datos_bees[,c(12, 15:19,22)] <- data.frame(log(datos_bees[,c(12, 15:19,22)]+1))

datos_bees[,c(12, 15:19,22)] <- data.frame(scale(datos_bees[,c(12, 15:19,22)], center = T, scale = T))




model_bees <- brm((Interaction-1) ~ Nectar_presence_absence + life_form +  Compatibility_system + Breeding_system + Flower_symmetry + Flower_morphology +
                    Autonomous_selfing_level_fruit_set + Flowers_per_plant + Corolla_diameter_mean + Style_length + Ovule_number + Plant_height_mean_m + (1|System/Id) + (1|gr(phylo, cov = A)),
             data = datos_bees, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
             sample_prior = TRUE, warmup = 500, iter = 2000,
             control = list(adapt_delta = 0.99))

conditional_effects(model_bees)



performance::r2(model_bees)



d <- all_long_poll_names[all_long_poll_names$Interaction>0,]
nrow(d)

datos <- merge(d, dat_cleaning, by.x = "Plant_species",  by.y = "Plant_species")

m1 <- lm(Interaction~ guild*Compatibility_system, data=datos)
summary(m1)

plot_model(m1, type = "pred", terms = c("Compatibility_system", "guild"))

model <- brm((Interaction-1) ~ Nectar_presence_absence + life_form +  Compatibility_system + Breeding_system + Flower_symmetry + Flower_morphology +
                    Autonomous_selfing_level_fruit_set + Flowers_per_plant + Corolla_diameter_mean + Style_length + Ovule_number + Plant_height_mean_m + (1|System/Id) + (1|gr(phylo, cov = A)),
                  data = datos, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                  sample_prior = TRUE, warmup = 500, iter = 2000,
                  control = list(adapt_delta = 0.99))




model_qualitative <- brm((Interaction-1) ~ Nectar_presence_absence + life_form +  Compatibility_system + Breeding_system + Flower_symmetry + Flower_morphology +
               Autonomous_selfing_level_fruit_set + Flowers_per_plant + Corolla_diameter_mean + Style_length + Ovule_number + Plant_height_mean_m + (1|System/Id) + (1|gr(phylo, cov = A)),
             data = datos, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
             sample_prior = TRUE, warmup = 500, iter = 2000,
             control = list(adapt_delta = 0.99))