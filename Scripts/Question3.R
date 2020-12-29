########################################################################################################################################################
#SCRIPT FOR DATA ANALYSIS *3RD RESEARCH QUESTION OF THE PAPER*
########################################################################################################################################################
####SUMMARY OF WHAT IS DONE HERE###

#1) LOAD DATA FOR ANALYSIS

#2) MODEL SELCTION
########################################################################################################################################################
#LOAD LIBRARIES
library(dplyr)
library(rtrees) #for phylogenetic distance
library(phytools)
library(corrplot)
library(brms)
library(cmdstanr)
library(ggplot2)
########################################################################################################################################################
#1) LOAD NETWORK DATA
########################################################################################################################################################
data <- read.csv("Data/Csv/metric_analysis_data_3rd_question.csv", row.names = 1)
head(data)
str(data)
########################################################################################################################################################
#2) TEST PHYLO SIGNAL OF METRICS
########################################################################################################################################################

#VISITS
#aggregate data per species and calculate mean visits and meand d for each species
phylo_data <- reshape2::dcast(Species + Genus + Family ~ "Visits", value.var = "Visits", fun.aggregate = mean, data = data, na.rm= TRUE)
#prepare dataframe to calculate tree
phylo <- as.data.frame(cbind(phylo_data$Family, phylo_data$Genus, phylo_data$Species))
colnames(phylo) <-  c("family", "genus", "species")
#calculate phylo for all the spp of the dataframe
phylo_tree<- get_tree(sp_list = phylo, tree = tree_plant_otl, taxon = "plant")
#remove underscore
phylo_tree$tip.label <- gsub("_", " ", phylo_tree$tip.label)

#Visits
Visits <-as.data.frame(phylo_data[,c("Visits")])
rownames(Visits) <- phylo_data[,c("Species")]
colnames(Visits)[1] <- "Visits"
Visits <- as.matrix((Visits))[,1]
Visits_signal <- phylosig(tree=phylo_tree,x=Visits,method="lambda",test=TRUE)
plot(Visits_signal)
#Phylogenetic signal lambda : 0.107258 
#logL(lambda) : -3140.15 
#LR(lambda=0) : 5.34073 
#P-value (based on LR test) : 0.0208327 
#SPECIALIZATION
phylo_data <- reshape2::dcast(Species + Genus + Family ~ "d", value.var = "d", fun.aggregate = mean, data = data, na.rm= TRUE)
d <-as.data.frame(phylo_data[,c("d")])
rownames(d) <- phylo_data[,c("Species")]
colnames(d)[1] <- "d"
d <- as.matrix((d))[,1]
d_signal <- phylosig(tree=phylo_tree,x=d,method="lambda",test=TRUE)
plot(d_signal)
#Phylogenetic signal lambda : 0.0273954 
#logL(lambda) : -25.1493 
#LR(lambda=0) : 0.919629 
#P-value (based on LR test) : 0.337572 

#The phylogenetic signal of visits and specialization is very low and therefore we are not going to correct for the phylogeney in the brms

########################################################################################################################################################
#3) MODEL SELECTION SAME MODELS BRMS
########################################################################################################################################################

#subset numeric variables of interest
data_numeric <- data[,c("Selfing_quantitative", "Flower_number", "Flower_inflorescence", "Floral_unit_width", "Flower_width",
                        "Flower_length", "Style_length", "Ovule_number", "Plant_height")]

str(data_numeric)
str(data)
#check for correlations between data
corrplot(cor(data_numeric))

#Prepare data
#create a second dataframe 
data_1 <- data
#Scale all numerical variables 
data_1 <- data_1 %>%
  mutate_if(is.numeric, scale)
#add raw data of visits and d
data_1$Visits <- data$Visits
data_1$d <- data$d
str(data_1)
#Convert characters to factors
data_1 <- mutate_if(data_1, is.character, as.factor)


#Plant height represents life form and life span, due to their correlation

#Flower width, ovule number, style length



#MODEL 1
# Plant_height + Flower_width + Flower_symmetry + Breeding_system + Compatibility + Flower_shape
model1 <- brm((Visits-1) ~ Plant_height + Flower_width + Flower_symmetry + Breeding_system + Compatibility + Flower_shape + (1|Id),
                                      data = data_1, family  = zero_inflated_negbinomial(), cores = 4,chains = 4,save_all_pars=T,
                                      sample_prior = TRUE, warmup = 500, iter = 1500,
                                      control = list(adapt_delta = 0.99)) 
pp_check(model1) +xlim(-50,200)+ylim(0,0.1)
#nice fit
conditional_effects(model1,points=T) 
performance::r2_bayes(model1)
#r2 of almost 0


pplot <- plot(model1, "areas", prob = 0.95, prob_outer = 1)
pplot + geom_vline(xintercept = 0)


varsel2 <- varsel(model1)




#MODEL 2
# Plant_height + Flower_width + Flower_symmetry + Breeding_system + Compatibility 
model2 <- brm((Visits-1) ~ Plant_height + Flower_width + Flower_symmetry + Breeding_system + Compatibility  + (1|Id),
              data = data_1, family  = zero_inflated_negbinomial(), cores = 4,chains = 4, save_all_pars=T,
              sample_prior = TRUE, warmup = 500, iter = 1500,
              control = list(adapt_delta = 0.99)) 
pp_check(model2) +xlim(-50,200)+ylim(0,0.1)
#nice fit
conditional_effects(model2,points=T) 
performance::r2_bayes(model2)

#MODEL 3
# Plant_height + Flower_width + Flower_symmetry + Breeding_system  
model3 <- brm((Visits-1) ~ Plant_height * Flower_width  + (1|Id),
              data = data_1, family  = zero_inflated_negbinomial(), cores = 4,chains = 4, 
              sample_prior = TRUE, warmup = 500, iter = 1500,
              control = list(adapt_delta = 0.99)) 
pp_check(model3) +xlim(-50,200)+ylim(0,0.1)
conditional_effects(model3,points=T) 

performance::r2_bayes(model3)

#MODEL 4
# Plant_height + Flower_width + Flower_symmetry + Breeding_system +C ompatibility
model4 <- brm((Visits-1) ~ Plant_height + Flower_width  + (1|Id),
              data = data_1, family  = zero_inflated_negbinomial(), cores = 4,chains = 4, 
              sample_prior = TRUE, warmup = 500, iter = 1500,
              control = list(adapt_delta = 0.99)) 
pp_average()



#MODEL 4
#Model 4: Breeding_system + Plant_height* + Compatibility
model4 <- brm((Visits-1) ~  Breeding_system*Ovule_number + Plant_height + Compatibility + (1|Id),
              data = data_1, family  = zero_inflated_negbinomial(), cores = 4,chains = 4, 
              sample_prior = TRUE, warmup = 500, iter = 1500,
              control = list(adapt_delta = 0.99)) 
pp_check(model34) +xlim(-50,200)+ylim(0,0.1)
conditional_effects(model4,points=T) 

performance::r2_bayes(model4)

#MODEL 5
#Model 5: Breeding_system + Selfing_quantitative + Plant_height
waic1 <- loo(model1,moment_match = TRUE)
waic2 <- loo(model2,moment_match = TRUE)

loo_compare(waic1, waic2)

