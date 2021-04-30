########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
##SCRIPT FOR ANALYSIS 
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

#HERE I TRY TO SELECT THE MAIN EXPLANATORY TRAITS FOR EACH TRAIT 

#LOAD LIBRARIES
library(data.table) # operate with df
library(bipartite) #calculate metrics
library(dplyr) #data manipulation 
library(stringr) #remove string
library(brms)
library(projpred)
library(dplyr)
library(ggplot2)
library(bayesplot)
library(ape)
library(rtrees)
library(cowplot)
library(emmeans)
library(tidybayes)
library(multcomp)
########################################################################################################################################################
#1) LOAD NETWORK DATA
########################################################################################################################################################
#Set working directory to read files
setwd("~/R_Projects/Reproductive traits/Data/Data_networks_quantitative") 

#read csv files
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, function(i){read.csv(i,  row.names = 1)})
# Add id to dataframe
names(my_data) <- stringr::str_replace(my_files, pattern = ".csv", replacement = "")

########################################################################################################################################################
#2) CALCULATE METRICS 
########################################################################################################################################################

#Function to sum visits per row and convert NA'S to 0's in the case there is any
rs_NA <- function(x){
  z <- as.data.frame(x)
  z[is.na(z)] <- 0  #convert NA'S to 0's
  z <- as.data.frame(rowSums(z)) #sum rows
  z <- setDT(z, keep.rownames = TRUE)[] 
  colnames(z) <- c("Species","Visits")
  return(z)
}


#Calculate all metrics at once with bipartite
met <- function(x){
  Visits_Sum <- rs_NA(x) #previous function to sum rows 
  #degree
  degree <- specieslevel(x, index="degree", level="lower")
  #normalise degree
  n_degree <- specieslevel(x, index="normalised degree", level="lower")
  #specialization
  d_Plant <- specieslevel(x, index="d", level="lower")   # specialization from BLUTHGEN 2006
  #closeness
  closeness <- specieslevel(x, index="closeness", level="lower")
  #betweenness
  betweenness <- specieslevel(x, index="betweenness", level="lower")
  #combine metrics in a unique data frame
  metrics <- cbind(Visits_Sum,n_degree,d_Plant, closeness, betweenness, degree) #degree, n_degree, d, closeness, betweenness)
  return(metrics)
}

#workaround to remove singletones
i <- NULL
data <- NULL
metrics_list <- list()
for (i in names(my_data)){
  metrics_list[[i]] <- my_data[[i]][apply(my_data[[i]][,-1], 1, function(x) !all(x<2)),]
}

#calculate network metrics with for loop
i <- NULL
data <- NULL
metrics_list_1 <- list()
for (i in names(metrics_list)){
  metrics_list_1[[i]] <- met(metrics_list[[i]])
}

#add id as a row name
all_list <- lapply(seq_along(metrics_list_1),function(x) cbind(metrics_list_1[[x]], 
                                                               unique.id=str_replace(my_files[x], pattern = ".csv", replacement = "")))

#Now merge all the data frames 
all_df <- bind_rows(all_list)

#check
#all_df_unique_sp <- all_df[!duplicated(all_df$Species),]

#remove species that are not until species level
#ALL THE SPECIES WITH SP. ARE DELETD
all_df$Species <-  sub('sp.1$', 'sp.', all_df$Species)
all_df$Species <- sub('sp.2$', 'sp.', all_df$Species)
all_df$Species <- sub('sp$', 'sp.', all_df$Species)
all_df$Species <- sub("sp.$", "DELETE", all_df$Species)

all_df <- all_df[- grep("DELETE",  all_df$Species),] #remove species with "DELETE"

colnames(all_df)[1] <- "Species_all"

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

#SET NORMAL WORKING DIRECTORY
setwd("~/R_Projects/Reproductive traits") 

dat <-  read.csv("Data/Csv/nectar_subset_imputed_trait_data.csv", row.names = "X")

#The correct species names are the row names, convert to 1st col to merge
#convert row to col
setDT(dat, keep.rownames = TRUE)[]
#set a colname
colnames(dat)[1] <- "Plants"
#merge
datitos <- merge(all_df, dat, by.x =  "Species_all", by.y= "Plants")




#New colname with log of visits

datitos_1 <- datitos
#FIRST QUALITATIVE VARIABLES
#Check levels before running models and perform cross validation
levels(factor(datitos_1$Breeding_system)) # ok
levels(factor(datitos_1$Compatibility_system)) #Fix
datitos_1$Compatibility_system[datitos_1$Compatibility_system=="dioecious"] <- "Unisexual flowers"
datitos_1$Compatibility_system[datitos_1$Compatibility_system=="monoecious"] <- "Unisexual flowers"
datitos_1$Compatibility_system[datitos_1$Compatibility_system=="self_incompatible"] <- "Self incompatible"
datitos_1$Compatibility_system[datitos_1$Compatibility_system=="self_compatible"] <- "Self compatible"
datitos_1$Compatibility_system[datitos_1$Compatibility_system=="partially_self_compatible"] <- "Partially self compatible"
#Check again
levels(factor(datitos_1$Compatibility_system)) #NOW OK
levels(factor(datitos_1$Autonomous_selfing_level)) 
datitos_1$Autonomous_selfing_level[datitos_1$Autonomous_selfing_level=="high"] <- "High-Medium"
datitos_1$Autonomous_selfing_level[datitos_1$Autonomous_selfing_level=="medium"] <- "High-Medium"
datitos_1$Autonomous_selfing_level[datitos_1$Autonomous_selfing_level=="low"] <- "Low-None"
datitos_1$Autonomous_selfing_level[datitos_1$Autonomous_selfing_level=="none"] <- "Low-None"
levels(factor(datitos_1$Autonomous_selfing_level)) #Ok 
levels(factor(datitos_1$Flower_morphology)) 
datitos_1$Flower_morphology[datitos_1$Flower_morphology=="Funnelform"] <- "Campanulate"
datitos_1$Flower_morphology[datitos_1$Flower_morphology=="Spike"] <- "Brush"
levels(factor(datitos_1$Flower_morphology)) #OK
levels(factor(datitos_1$Flower_symmetry)) 
datitos_1$Flower_symmetry[datitos_1$Flower_symmetry=="actinomorphic"] <- "Actinomorphic"
datitos_1$Flower_symmetry[datitos_1$Flower_symmetry=="zygomorphic"] <- "Zygomorphic"
levels(factor(datitos_1$Flower_symmetry)) #OK
levels(factor(datitos_1$life_form)) 
datitos_1$life_form[datitos_1$life_form=="herb"] <- "Herb"
datitos_1$life_form[datitos_1$life_form=="shrub"] <- "Shrub"
datitos_1$life_form[datitos_1$life_form=="tree"] <- "Tree"
datitos_1$life_form[datitos_1$life_form=="vine"] <- "Shrub"
levels(factor(datitos_1$life_form)) #OK
levels(factor(datitos_1$lifespan)) #OK
levels(factor(datitos_1$Nectar_presence_absence))
datitos_1$Nectar_presence_absence[datitos_1$Nectar_presence_absence=="yes"] <- "Yes"
datitos_1$Nectar_presence_absence[datitos_1$Nectar_presence_absence=="no"] <- "No"
levels(factor(datitos_1$Nectar_presence_absence)) #OK

#Now quantitative variables

str(datitos_1)

#Standardize data 
datitos_1[,c(18, 21:25,28,30)] <- data.frame(log(datitos_1[,c(18, 21:25,28,30)]+1))

datitos_1[,c(18, 21:25,28,30)] <- data.frame(scale(datitos_1[,c(18, 21:25,28,30)], center = T, scale = T))



df <- datitos_1 %>% mutate_if(is.character,as.factor)


df$System <- df$unique.id

df$System <- as.character(df$System)

df$System[grepl("peralta_2006", df$System)] <- "peralta_2006"
df$System[grepl("small_1976", df$System)] <- "small_1976"
df$System[grepl("arroyo_correa_new_zealand", df$System)] <- "arroyo_correa_2019"
df$System[grepl("fang_2008", df$System)] <- "fang_2008"
df$System[grepl("kaiser-bunbury_2017", df$System)] <- "kaiser-bunbury_2017"
df$System[grepl("inouye_1988", df$System)] <- "inouye_1988"
df$System[grepl("kaiser-bunbury_2010", df$System)] <- "kaiser-bunbury_2010"
df$System[grepl("kaiser-bunbury_2011", df$System)] <- "kaiser-bunbury_2011"
df$System[grepl("burkle_usa_2013", v$System)] <- "burkle_2013"
df$System[grepl("dicks_2002", df$System)] <- "dicks_2002"
df$System[grepl("dupont_2009", df$System)] <- "dupont_2009"
df$System[grepl("bartomeus_spain_2008_medca", df$System)] <- "bartomeus_2008a"
df$System[grepl("bartomeus_spain_2008_batca", df$System)] <- "bartomeus_2008b"
df$System[grepl("bartomeus_spain_2008_selop", df$System)] <- "bartomeus_2008c"
df$System[grepl("bartomeus_spain_2008_miqop", df$System)] <- "bartomeus_2008d"
df$System[grepl("bartomeus_spain_2008_fraop", df$System)] <- "bartomeus_2008e"
df$System[grepl("lundgren_2005", df$System)] <- "lundgren_2005"
df$System[grepl("olesen_2002_mauritius", df$System)] <- "olesen_2002_mauritius"
df$System[grepl("olesen_2002_azores", df$System)] <- "olesen_2002_azores"
df$System[grepl("bartomeus_spain_2008", df$System)] <- "bartomeus_spain_2008"
df$System[grepl("bundgaard_2003_denmark", df$System)] <- "bundgaard_2003"
df$System[grepl("elberling_sweeden_1999", df$System)] <- "elberling_1999"


df$System <- as.factor(df$System)

########################################################################################################################################################
#CALCULATE PHYLO
########################################################################################################################################################

#Prepare species, genus anD_5 family for calculating phylogenetic distance
df$Family_all  <- as.character(df$Family_all)
df$Genus_all   <- as.character(df$Genus_all)
df$Species_all.y <- as.character(df$Species_all.y)

#prepare dataframe to calculate tree
phylo_5 <- as.data.frame(cbind(df$Family_all, df$Genus_all, df$Species_all.y))
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
df$phylo
df$phylo <- df$Species_all.y
########################################################################################################################################################

visits_nectar <-  brm(Visits-1 ~ Nectar_ul + (1|System/unique.id) +(1|gr(phylo, cov = A)),
              data = df, data2 = list(A = A_5), family  = zero_inflated_negbinomial(), cores = 4,chains = 4, 
              sample_prior = TRUE, warmup = 1000, iter = 3000,
              control = list(adapt_delta = 0.99)) 

conditional_effects(visits_nectar)

df$d <- ifelse(df$d > 1, 1, df$d)


d_nectar <-  brm(d ~ Nectar_ul + (1|System/unique.id) +(1|gr(phylo, cov = A)),
              data = df, data2 = list(A = A_5), family  = zero_one_inflated_beta(), cores = 4,chains = 4, 
              sample_prior = TRUE, warmup = 1000, iter = 3000,
              control = list(adapt_delta = 0.99)) 

performance::r2(normalised.degree_nectar)

plot(conditional_effects(normalised.degree_nectar), points=T)



normalised.degree_nectar <-  brm(normalised.degree ~ Nectar_ul + (1|System/unique.id) +(1|gr(phylo, cov = A)),
                 data = df, data2 = list(A = A_5), family  = zero_one_inflated_beta(), cores = 4,chains = 4, 
                 sample_prior = TRUE, warmup = 1000, iter = 3000,
                 control = list(adapt_delta = 0.99)) 
