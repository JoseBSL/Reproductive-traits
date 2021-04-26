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

dat <- read.csv("Data/Csv/all_species_imputed_trait_data_forest_data.csv", row.names = "X")

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

#datitos_1[,c(5, 8:12, 15)] <- log(datitos_1[,c(5, 8:12, 15)]+1)


datitos_1[,c(18, 21:25,28)] <- data.frame(log(datitos_1[,c(18, 21:25,28)]+1))


datitos_1[,c(18, 21:25,28)] <- data.frame(scale(datitos_1[,c(18, 21:25,28)], center = T, scale = T))



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

fit_v <-  brm(Visits-1 ~ Breeding_system + Compatibility_system + Autonomous_selfing_level  + Flower_morphology + 
              Flower_symmetry + Flowers_per_plant + Corolla_diameter_mean  + Style_length + Ovule_number + life_form + lifespan + 
              Plant_height_mean_m + Nectar_presence_absence + (1|System/unique.id) +(1|gr(phylo, cov = A)),
            data = df, data2 = list(A = A_5), family  = zero_inflated_negbinomial(), cores = 4,chains = 4, 
            sample_prior = TRUE, warmup = 1000, iter = 3000,
            control = list(adapt_delta = 0.99)) 

pp_check(fit_v)

performance::r2(fit_v)

conditional_effects(fit_v)

################################################################################################################################################################
#QUALITATIVE VARIABLES
################################################################################################################################################################
####BREEDING####

breeding <- fit_v %>%
  emmeans( ~ Breeding_system, transform = "response" ) %>%
  gather_emmeans_draws() 

p1 <- ggplot(breeding, aes(x = Breeding_system, y = .value, fill=Breeding_system)) +
  stat_eye() +
  theme_light() + ylab("Sum of visits") +scale_fill_manual(values=c("forestgreen","orange", "grey"))+
  theme(legend.position = "none") + xlab("Breeding system") +theme(text = element_text(size=10))
  
####COMPATIBILITY####

Compatibility<- fit_v %>%
  emmeans( ~ Compatibility_system, transform = "response" ) %>%
  gather_emmeans_draws() 

p2 <- ggplot(Compatibility, aes(x = Compatibility_system, y = .value, fill=Compatibility_system)) +
  stat_eye() +
  theme_light() + ylab("Sum of visits") +scale_fill_manual(values=c("cyan4", "deepskyblue2","orange","GREY"))+
  theme(legend.position = "none") + xlab("Breeding system")+theme(text = element_text(size=10))


####SELFING####

selfing <- fit_v %>%
  emmeans( ~ Autonomous_selfing_level, transform = "response") %>%
  gather_emmeans_draws() 

p3 <- ggplot(selfing, aes(x = Autonomous_selfing_level, y = .value, fill=Autonomous_selfing_level)) +
  stat_eye() +
  theme_light() + ylab("Sum of visits") +scale_fill_manual(values=c("cyan4", "GREY"))+
  theme(legend.position = "none") + xlab("Autonomous selfing")+theme(text = element_text(size=10))


####FLOWER SHAPE####

f_shape <- fit_v %>%
  emmeans( ~ Flower_morphology, transform = "response" ) %>%
  gather_emmeans_draws()
  
p4 <- ggplot(f_shape, aes(x = Flower_morphology, y = .value, fill=Flower_morphology)) +
  stat_eye() +
  theme_light() + ylab("Sum of visits") +scale_fill_manual(values=c("blue","cyan4", "red","forestgreen","purple","gold"))+
  theme(legend.position = "none") + xlab("Flower shape")+theme(text = element_text(size=10))

####FLOWER SYMMETRY####

f_symmetry <- fit_v %>%
  emmeans( ~ Flower_symmetry, transform = "response" ) %>%
  gather_emmeans_draws()

p5 <- ggplot(f_symmetry, aes(x = Flower_symmetry, y = .value, fill=Flower_symmetry)) +
  stat_eye() +
  theme_light() + ylab("Sum of visits") +scale_fill_manual(values=c("sienna2","purple"))+
  theme(legend.position = "none") + xlab("Flower symmetry")+theme(text = element_text(size=10))

####LIFEFORM####

lifeform <- fit_v %>%
  emmeans( ~ life_form, transform = "response" ) %>%
  gather_emmeans_draws() 

p6 <- ggplot(lifeform, aes(x = life_form, y = .value, fill=life_form)) +
  stat_eye() +
  theme_light() + ylab("Sum of visits") +scale_fill_manual(values=c("forestgreen", "darkorange2","grey"))+
  theme(legend.position = "none") + xlab("Life form")+theme(text = element_text(size=10))

####LIFESPAN####

lifespan <- fit_v %>%
  emmeans( ~ lifespan, transform = "response" ) %>%
  gather_emmeans_draws() 

p7 <- ggplot(lifespan, aes(x = lifespan, y = .value, fill=lifespan)) +
  stat_eye() +
  theme_light() + ylab("Sum of visits") +scale_fill_manual(values=c("sienna4","forestgreen"))+
  theme(legend.position = "none") + xlab("Lifespan")+theme(text = element_text(size=10))

####NECTAR####

nectar <- fit_v %>%
  emmeans( ~ Nectar_presence_absence, transform = "response") %>%
  gather_emmeans_draws() 

p8 <- ggplot(nectar, aes(x = Nectar_presence_absence, y = .value, fill=Nectar_presence_absence)) +
  stat_eye() +
  theme_light() + ylab("Sum of visits") +scale_fill_manual(values=c("grey","gold"))+
  theme(legend.position = "none") + xlab("Nectar")+theme(text = element_text(size=10))


p_quali <- plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,ncol = 2)


plot_grid(p1,p3,p5,p6,p7,p8,ncol = 2)


################################################################################################################################################################
#QUANTITATIVE VARIABLES VARIABLES
################################################################################################################################################################


ce_1 <- conditional_effects(fit_v, effects = "Flowers_per_plant",points=T) 
colnames(ce_1[[1]])[3] <- "Interaction"

 pp1 <- ggplot(ce_1[[1]], aes(x = Flowers_per_plant, y = (estimate__+1))) + geom_point(data = df,aes(x = Flowers_per_plant, y = Visits),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(df$Visits, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Number of visits")

 ce_2 <- conditional_effects(fit_v, effects = "Corolla_diameter_mean",points=T) 
 colnames(ce_2[[1]])[3] <- "Interaction"
 
 pp2 <- ggplot(ce_2[[1]], aes(x = Corolla_diameter_mean, y = (estimate__+1))) + geom_point(data = df,aes(x = Corolla_diameter_mean, y = Visits),
 size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(df$Visits, 0.95)) +theme_ms()+
 geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Number of visits")
 
 ce_3 <- conditional_effects(fit_v, effects = "Style_length",points=T) 
 colnames(ce_3[[1]])[3] <- "Interaction"
 
 pp3 <- ggplot(ce_3[[1]], aes(x = Style_length, y = (estimate__+1))) + geom_point(data = df,aes(x = Style_length, y = Visits),
   size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(df$Visits, 0.95)) +theme_ms()+
   geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Number of visits")
 
 ce_4 <- conditional_effects(fit_v, effects = "Ovule_number",points=T) 
 colnames(ce_4[[1]])[3] <- "Interaction"
 
 pp4 <- ggplot(ce_4[[1]], aes(x = Ovule_number, y = (estimate__+1))) + geom_point(data = df,aes(x = Ovule_number, y = Visits),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(df$Visits, 0.95)) +theme_ms()+
   geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Number of visits")
 
 
 ce_5 <- conditional_effects(fit_v, effects = "Plant_height_mean_m",points=T) 
 colnames(ce_5[[1]])[3] <- "Interaction"
 
 pp5 <- ggplot(ce_5[[1]], aes(x = Plant_height_mean_m, y = (estimate__+1))) + geom_point(data = df,aes(x = Plant_height_mean_m, y = Visits),
 size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(df$Visits, 0.95)) +theme_ms()+
 geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Number of visits")
 
 
p_quanti <- plot_grid(pp1,pp2,pp3,pp4,pp5, ncol=2)
 
plot_grid(p_quali,p_quanti)
 
 
################################################################################################################################################################

df$d <- ifelse(df$d > 1, 1, df$d)


fit_d <-  brm(d ~ Breeding_system + Compatibility_system + Autonomous_selfing_level  + Flower_morphology + 
                Flower_symmetry + Flowers_per_plant + Corolla_diameter_mean  + Style_length + Ovule_number + life_form + lifespan + 
                Plant_height_mean_m + Nectar_presence_absence + (1|System/unique.id) +(1|gr(phylo, cov = A)),
              data = df, data2 = list(A = A_5), family  = zero_one_inflated_beta(), cores = 4,chains = 4, 
              sample_prior = TRUE, warmup = 1000, iter = 3000,
              control = list(adapt_delta = 0.99)) 


pp_check(fit_d)

performance::r2(fit_d)

conditional_effects(fit_d)

################################################################################################################################################################
#QUALITATIVE VARIABLES
################################################################################################################################################################
####BREEDING####

breeding_d <- fit_d %>%
  emmeans( ~ Breeding_system, transform = "response" ) %>%
  gather_emmeans_draws() 

d1 <- ggplot(breeding_d, aes(x = Breeding_system, y = .value, fill=Breeding_system)) +
  stat_eye() +
  theme_light() + ylab("Specialization (d')") +scale_fill_manual(values=c("forestgreen","orange", "grey"))+
  theme(legend.position = "none") + xlab("Breeding system")+theme(text = element_text(size=10))

####COMPATIBILITY####

Compatibility_d <- fit_d %>%
  emmeans( ~ Compatibility_system, transform = "response" ) %>%
  gather_emmeans_draws() 

d2 <- ggplot(Compatibility_d, aes(x = Compatibility_system, y = .value, fill=Compatibility_system)) +
  stat_eye() +
  theme_light() + ylab("Specialization (d')") +scale_fill_manual(values=c("cyan4", "deepskyblue2","orange","GREY"))+
  theme(legend.position = "none") + xlab("Breeding system")+theme(text = element_text(size=10))


####SELFING####

selfing_d <- fit_d %>%
  emmeans( ~ Autonomous_selfing_level, transform = "response") %>%
  gather_emmeans_draws() 

d3 <- ggplot(selfing_d, aes(x = Autonomous_selfing_level, y = .value, fill=Autonomous_selfing_level)) +
  stat_eye() +
  theme_light() + ylab("Specialization (d')") +scale_fill_manual(values=c("cyan4", "GREY"))+
  theme(legend.position = "none") + xlab("Autonomous selfing")+theme(text = element_text(size=10))


####FLOWER SHAPE####

f_shape_d <- fit_d %>%
  emmeans( ~ Flower_morphology, transform = "response" ) %>%
  gather_emmeans_draws()

d4 <- ggplot(f_shape_d, aes(x = Flower_morphology, y = .value, fill=Flower_morphology)) +
  stat_eye() +
  theme_light() + ylab("Specialization (d')") +scale_fill_manual(values=c("blue","cyan4", "red","forestgreen","purple","gold"))+
  theme(legend.position = "none") + xlab("Flower shape")+theme(text = element_text(size=10))


####FLOWER SYMMETRY####

f_symmetry_d <- fit_d %>%
  emmeans( ~ Flower_symmetry, transform = "response" ) %>%
  gather_emmeans_draws()

d5 <- ggplot(f_symmetry_d, aes(x = Flower_symmetry, y = .value, fill=Flower_symmetry)) +
  stat_eye() +
  theme_light() + ylab("Specialization (d')") +scale_fill_manual(values=c("sienna2","purple"))+
  theme(legend.position = "none") + xlab("Flower symmetry")+theme(text = element_text(size=10))


####LIFEFORM####

lifeform_d <- fit_d %>%
  emmeans( ~ life_form, transform = "response" ) %>%
  gather_emmeans_draws() 

d6 <- ggplot(lifeform_d, aes(x = life_form, y = .value, fill=life_form)) +
  stat_eye() +
  theme_light() + ylab("Specialization (d')") +scale_fill_manual(values=c("forestgreen", "darkorange2","grey"))+
  theme(legend.position = "none") + xlab("Life form")+theme(text = element_text(size=10))

####LIFESPAN####

lifespan_d <- fit_d %>%
  emmeans( ~ lifespan, transform = "response" ) %>%
  gather_emmeans_draws() 

d7 <- ggplot(lifespan_d, aes(x = lifespan, y = .value, fill=lifespan)) +
  stat_eye() +
  theme_light() + ylab("Specialization (d')") +scale_fill_manual(values=c("sienna4","forestgreen"))+
  theme(legend.position = "none") + xlab("Lifespan")+theme(text = element_text(size=10))

####NECTAR####

nectar_d <- fit_d %>%
  emmeans( ~ Nectar_presence_absence, transform = "response") %>%
  gather_emmeans_draws() 

d8 <- ggplot(nectar_d, aes(x = Nectar_presence_absence, y = .value, fill=Nectar_presence_absence)) +
  stat_eye() +
  theme_light() + ylab("Specialization (d')") +scale_fill_manual(values=c("grey","gold"))+
  theme(legend.position = "none") + xlab("Nectar")+theme(text = element_text(size=10))

#Plot all
d_quali <- plot_grid(d1,d2,d3,d4,d5,d6,d7,d8,ncol = 2)


################################################################################################################################################################
#QUANTITATIVE VARIABLES
################################################################################################################################################################


ce_1 <- conditional_effects(fit_d, effects = "Flowers_per_plant",points=T) 
colnames(ce_1[[1]])[3] <- "Interaction"

dd1 <- ggplot(ce_1[[1]], aes(x = Flowers_per_plant, y = (estimate__))) + geom_point(data = df,aes(x = Flowers_per_plant, y = d),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2)  +  theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Specialization (d')")

ce_2 <- conditional_effects(fit_d, effects = "Corolla_diameter_mean",points=T) 
colnames(ce_2[[1]])[3] <- "Interaction"

dd2 <- ggplot(ce_2[[1]], aes(x = Corolla_diameter_mean, y = (estimate__))) + geom_point(data = df,aes(x = Corolla_diameter_mean, y = d),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Specialization (d')")

ce_3 <- conditional_effects(fit_d, effects = "Style_length",points=T) 
colnames(ce_3[[1]])[3] <- "Interaction"

dd3 <- ggplot(ce_3[[1]], aes(x = Style_length, y = (estimate__))) + geom_point(data = df,aes(x = Style_length, y = d),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2)  + theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Specialization (d')")

ce_4 <- conditional_effects(fit_d, effects = "Ovule_number",points=T) 
colnames(ce_4[[1]])[3] <- "Interaction"

dd4 <- ggplot(ce_4[[1]], aes(x = Ovule_number, y = (estimate__))) + geom_point(data = df,aes(x = Ovule_number, y = d),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2)  + theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Specialization (d')")


ce_5 <- conditional_effects(fit_d, effects = "Plant_height_mean_m",points=T) 

dd5 <- ggplot(ce_5[[1]], aes(x = Plant_height_mean_m, y = (estimate__))) + geom_point(data = df,aes(x = Plant_height_mean_m, y = d),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2)  + theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Specialization (d')")


d_quanti <- plot_grid(dd1,dd2,dd3,dd4,dd5, ncol=2)

plot_grid(d_quali, d_quanti)

################################################################################################################################################################


fit_nd <-  brm(normalised.degree ~ Breeding_system + Compatibility_system + Autonomous_selfing_level  + Flower_morphology + 
                Flower_symmetry + Flowers_per_plant + Corolla_diameter_mean  + Style_length + Ovule_number + life_form + lifespan + 
                Plant_height_mean_m + Nectar_presence_absence + (1|System/unique.id) +(1|gr(phylo, cov = A)),
              data = df, data2 = list(A = A_5), family  = weibull(), cores = 4,chains = 4, 
              sample_prior = TRUE, warmup = 1000, iter = 3000,
              control = list(adapt_delta = 0.99)) 


pp_check(fit_nd)

performance::r2(fit_nd)

conditional_effects(fit_nd)
################################################################################################################################################################
#QUALITATIVE VARIABLES
################################################################################################################################################################
####BREEDING####

breeding_nd <- fit_nd %>%
  emmeans( ~ Breeding_system, transform = "response" ) %>%
  gather_emmeans_draws() 

nd1 <- ggplot(breeding_nd, aes(x = Breeding_system, y = .value, fill=Breeding_system)) +
  stat_eye() +
  theme_light() + ylab("Normalize degree") +scale_fill_manual(values=c("forestgreen","orange", "grey"))+
  theme(legend.position = "none") + xlab("Breeding system")+theme(text = element_text(size=10))

####COMPATIBILITY####

Compatibility_nd <- fit_nd %>%
  emmeans( ~ Compatibility_system, transform = "response" ) %>%
  gather_emmeans_draws() 

nd2 <- ggplot(Compatibility_nd, aes(x = Compatibility_system, y = .value, fill=Compatibility_system)) +
  stat_eye() +
  theme_light() + ylab("Normalize degree") +scale_fill_manual(values=c("cyan4", "deepskyblue2","orange","GREY"))+
  theme(legend.position = "none") + xlab("Breeding system")+theme(text = element_text(size=10))


####SELFING####

selfing_nd <- fit_nd %>%
  emmeans( ~ Autonomous_selfing_level, transform = "response") %>%
  gather_emmeans_draws() 

nd3 <- ggplot(selfing_nd, aes(x = Autonomous_selfing_level, y = .value, fill=Autonomous_selfing_level)) +
  stat_eye() +
  theme_light() + ylab("Normalize degree") +scale_fill_manual(values=c("cyan4", "GREY"))+
  theme(legend.position = "none") + xlab("Autonomous selfing")+theme(text = element_text(size=10))


####FLOWER SHAPE####

f_shape_nd <- fit_nd %>%
  emmeans( ~ Flower_morphology, transform = "response" ) %>%
  gather_emmeans_draws()

nd4 <- ggplot(f_shape_nd, aes(x = Flower_morphology, y = .value, fill=Flower_morphology)) +
  stat_eye() +
  theme_light() + ylab("Normalize degree") +scale_fill_manual(values=c("blue","cyan4", "red","forestgreen","purple","gold"))+
  theme(legend.position = "none") + xlab("Flower shape")+theme(text = element_text(size=10))


####FLOWER SYMMETRY####

f_symmetry_nd <- fit_nd %>%
  emmeans( ~ Flower_symmetry, transform = "response" ) %>%
  gather_emmeans_draws()

nd5 <- ggplot(f_symmetry_nd, aes(x = Flower_symmetry, y = .value, fill=Flower_symmetry)) +
  stat_eye() +
  theme_light() + ylab("Normalize degree") +scale_fill_manual(values=c("sienna2","purple"))+
  theme(legend.position = "none") + xlab("Flower symmetry")+theme(text = element_text(size=10))


####LIFEFORM####

lifeform_nd <- fit_nd %>%
  emmeans( ~ life_form, transform = "response" ) %>%
  gather_emmeans_draws() 

nd6 <- ggplot(lifeform_nd, aes(x = life_form, y = .value, fill=life_form)) +
  stat_eye() +
  theme_light() + ylab("Normalize degree") +scale_fill_manual(values=c("forestgreen", "darkorange2","grey"))+
  theme(legend.position = "none") + xlab("Life form")+theme(text = element_text(size=10))

####LIFESPAN####

lifespan_nd <- fit_nd %>%
  emmeans( ~ lifespan, transform = "response" ) %>%
  gather_emmeans_draws() 

nd7 <- ggplot(lifespan_nd, aes(x = lifespan, y = .value, fill=lifespan)) +
  stat_eye() +
  theme_light() + ylab("Normalize degree") +scale_fill_manual(values=c("sienna4","forestgreen"))+
  theme(legend.position = "none") + xlab("Lifespan")+theme(text = element_text(size=10))

####NECTAR####

nectar_nd <- fit_nd %>%
  emmeans( ~ Nectar_presence_absence, transform = "response") %>%
  gather_emmeans_draws() 

nd8 <- ggplot(nectar_nd, aes(x = Nectar_presence_absence, y = .value, fill=Nectar_presence_absence)) +
  stat_eye() +
  theme_light() + ylab("Normalize degree") +scale_fill_manual(values=c("grey","gold"))+
  theme(legend.position = "none") + xlab("Nectar")+theme(text = element_text(size=10))

#Plot all
nd_quali <- plot_grid(nd1,nd2,nd3,nd4,nd5,nd6,nd7,nd8,ncol = 2)

################################################################################################################################################################
#QUANTITATIVE VARIABLES
################################################################################################################################################################
ce_1 <- conditional_effects(fit_nd, effects = "Flowers_per_plant",points=T) 
colnames(ce_1[[1]])[3] <- "Interaction"

ndd1 <- ggplot(ce_1[[1]], aes(x = Flowers_per_plant, y = (estimate__))) + geom_point(data = df,aes(x = Flowers_per_plant, y = normalised.degree),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2)  +  theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Normalize degree")

ce_2 <- conditional_effects(fit_nd, effects = "Corolla_diameter_mean",points=T) 
colnames(ce_2[[1]])[3] <- "Interaction"

ndd2 <- ggplot(ce_2[[1]], aes(x = Corolla_diameter_mean, y = (estimate__))) + geom_point(data = df,aes(x = Corolla_diameter_mean, y = normalised.degree),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Normalize degree")

ce_3 <- conditional_effects(fit_nd, effects = "Style_length",points=T) 
colnames(ce_3[[1]])[3] <- "Interaction"

ndd3 <- ggplot(ce_3[[1]], aes(x = Style_length, y = (estimate__))) + geom_point(data = df,aes(x = Style_length, y = normalised.degree),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2)  + theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Normalize degree")

ce_4 <- conditional_effects(fit_nd, effects = "Ovule_number",points=T) 
colnames(ce_4[[1]])[3] <- "Interaction"

ndd4 <- ggplot(ce_4[[1]], aes(x = Ovule_number, y = (estimate__))) + geom_point(data = df,aes(x = Ovule_number, y = normalised.degree),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2)  + theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Normalize degree")


ce_5 <- conditional_effects(fit_nd, effects = "Plant_height_mean_m",points=T) 
colnames(ce_5[[1]])[3] <- "Interaction"

ndd5 <- ggplot(ce_5[[1]], aes(x = Plant_height_mean_m, y = (estimate__))) + geom_point(data = df,aes(x = Plant_height_mean_m, y = normalised.degree),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2)  + theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Normalize degree")


nd_quanti <-  plot_grid(ndd1,ndd2,ndd3,ndd4,ndd5,ncol=2)


plot_grid(nd_quali, nd_quanti)

