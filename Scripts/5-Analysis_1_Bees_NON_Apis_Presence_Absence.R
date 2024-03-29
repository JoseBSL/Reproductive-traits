########################################################################################################################################################
# SCRIPT FOR ANALYSIS 1 VISITS~ POLL. FUNCTIONAL GROUP*PRINCIPAL COMPONENTS
########################################################################################################################################################
#LOAD LIBRARIES
library(phytools)
library(ape) #for phylogenetic distance
library(dplyr) #data processing
library(rtrees) #for phylogenetic distancelibrary(MASS)
library(reshape2)
library(viridis) #COLOUR GGPLOT
library(MASS)
library(ggplot2)
library(broman) #crayon colours
library(tidyverse)
library(brms)
library(DHARMa)
library(cmdstanr)
library(emmeans)
library(tidybayes)
#LOAD THEME FOR PLOTTING
theme_ms <- function(base_size=12, base_family="Helvetica") {
  (theme_bw(base_size = base_size, base_family = base_family)+
     theme(text=element_text(color="black"),
           axis.title=element_text( size = rel(1.3)),
           axis.text=element_text(size = rel(1), color = "black"),
           legend.title=element_text(face="bold"),
           legend.text=element_text(),
           legend.background=element_rect(fill="transparent"),
           legend.key.size = unit(0.9, 'lines'),
           panel.border=element_rect(color="black",size=1),
           panel.grid.minor.x =element_blank(),
           panel.grid.minor.y= element_blank(),
           panel.grid.major= element_blank()
     ))
}
########################################################################################################################################################
#1) READ DATA
########################################################################################################################################################
setwd("~/R_Projects/Reproductive Traits")
d_a <- readRDS("Data/RData/data_analysis_1_non_apis_presence_absence.rds") #ALL SPP
########################################################################################################################################################
#PREPARE POLL. FUNCTIONAL GROUPS FOR MODELLING
########################################################################################################################################################

#Check levels
levels(as.factor(d_a$guild))
#Select just bees
d_a_1 <- subset(d_a, guild=="Bee" ) #Select just the main functional groups
d_a_1$guild <- as.character(d_a_1$guild)
d_a_1$guild[d_a_1$guild=="Bee"] <- "Bees" #Adding an S to bee(S)
d_a_1$guild <- factor(d_a_1$guild, levels = c("Bees"))


#check number of levels
d_a_1 %>% 
  group_by(guild) %>%
  summarise(no_rows = length(guild))

dat_analysis <- d_a_1
########################################################################################################################################################
#CALCULATE PHYLO FOR THE MODEL
########################################################################################################################################################
#Prepare species, genus anD_5 family for calculating phylogenetic distance
dat_analysis$Family_all  <- as.character(dat_analysis$Family_all)
dat_analysis$Genus_all   <- as.character(dat_analysis$Genus_all)
dat_analysis$Species_all <- as.character(dat_analysis$Species_all)

#prepare dataframe to calculate tree
phylo_5 <- as.data.frame(cbind(dat_analysis$Family_all, dat_analysis$Genus_all, dat_analysis$Species_all))
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
dat_analysis$phylo
dat_analysis$phylo <- dat_analysis$Species_all
str(dat_analysis)

#Create column of system for nested random effects

dat_analysis$System <- dat_analysis$Id

dat_analysis$System[grepl("peralta_2006", dat_analysis$System)] <- "peralta_2006"
dat_analysis$System[grepl("small_1976", dat_analysis$System)] <- "small_1976"
dat_analysis$System[grepl("arroyo_correa_new_zealand", dat_analysis$System)] <- "arroyo_correa_2019"
dat_analysis$System[grepl("fang_2008", dat_analysis$System)] <- "fang_2008"
dat_analysis$System[grepl("kaiser-bunbury_2017", dat_analysis$System)] <- "kaiser-bunbury_2017"
dat_analysis$System[grepl("inouye_1988", dat_analysis$System)] <- "inouye_1988"
dat_analysis$System[grepl("kaiser-bunbury_2010", dat_analysis$System)] <- "kaiser-bunbury_2010"
dat_analysis$System[grepl("kaiser-bunbury_2011", dat_analysis$System)] <- "kaiser-bunbury_2011"
dat_analysis$System[grepl("burkle_usa_2013", dat_analysis$System)] <- "burkle_2013"
dat_analysis$System[grepl("dicks_2002", dat_analysis$System)] <- "dicks_2002"
dat_analysis$System[grepl("dupont_2009", dat_analysis$System)] <- "dupont_2009"
dat_analysis$System[grepl("bartomeus_spain_2008_medca", dat_analysis$System)] <- "bartomeus_2008"
dat_analysis$System[grepl("bartomeus_spain_2008_batca", dat_analysis$System)] <- "bartomeus_2008"
dat_analysis$System[grepl("bartomeus_spain_2008_selop", dat_analysis$System)] <- "bartomeus_2008"
dat_analysis$System[grepl("bartomeus_spain_2008_miqop", dat_analysis$System)] <- "bartomeus_2008"
dat_analysis$System[grepl("bartomeus_spain_2008_fraop", dat_analysis$System)] <- "bartomeus_2008"
dat_analysis$System[grepl("lundgren_2005", dat_analysis$System)] <- "lundgren_2005"
dat_analysis$System[grepl("olesen_2002_mauritius", dat_analysis$System)] <- "olesen_2002_mauritius"
dat_analysis$System[grepl("olesen_2002_azores", dat_analysis$System)] <- "olesen_2002_azores"
dat_analysis$System[grepl("bartomeus_spain_2008", dat_analysis$System)] <- "bartomeus_spain_2008"
dat_analysis$System[grepl("bundgaard_2003_denmark", dat_analysis$System)] <- "bundgaard_2003"
dat_analysis$System[grepl("elberling_sweeden_1999", dat_analysis$System)] <- "elberling_1999"



########################################################################################################################################################
#RUN MODEL
########################################################################################################################################################
str(dat_analysis)
dat_analysis$Interaction[dat_analysis$Interaction>0] <-1
nrow(dat_analysis)


analysis_1_bee_non_apis <- brm(Interaction ~ PC1 + PC2 + PC3 +(1|System/Id) + (1|gr(phylo, cov = A)),
                           data = dat_analysis, family  = bernoulli(),data2 = list(A = A_5), cores = 4,chains = 4, 
                           sample_prior = TRUE, warmup = 500, iter = 2000,
                           control = list(adapt_delta = 0.99))

ce_pc1 <- conditional_effects(analysis_1_bee_non_apis, effects = "PC1",points=T) 

p1 <- ggplot(ce_pc1[[1]], aes(x = -PC1, y = (estimate__))) + geom_point(data = dat_analysis,
aes(x = -PC1, y = Interaction),size = 0.75, alpha=0.5) + geom_line(size=0.8)  + ylab("Absence/presence")+ xlab("PC1")+
  theme_ms() + theme(legend.position = "none") + scale_color_manual(name="Functional groups",values=c("orange", "black", "limegreen","#E7298A", "cyan4","blueviolet"))

ce_pc2 <- conditional_effects(analysis_1_bee_non_apis, effects = "PC2",points=T) 

p2 <- ggplot(ce_pc2[[1]], aes(x = -PC2, y = (estimate__))) + geom_point(data = dat_analysis,
aes(x = -PC2, y = Interaction),size = 0.75, alpha=0.5) + geom_line(size=0.8)  + ylab("Absence/presence")+ xlab("PC2")+
  theme_ms() + theme(legend.position = "none") + scale_color_manual(name="Functional groups",values=c("orange", "black", "limegreen","#E7298A", "cyan4","blueviolet"))

ce_pc3 <- conditional_effects(analysis_1_bee_non_apis, effects = "PC3",points=T) 

p3 <- ggplot(ce_pc3[[1]], aes(x = -PC3, y = (estimate__))) + geom_point(data = dat_analysis,
 aes(x = -PC3, y = Interaction),size = 0.75, alpha=0.5) + geom_line(size=0.8)  + ylab("Absence/presence")+ xlab("PC2")+
  theme_ms() + theme(legend.position = "none") + scale_color_manual(name="Functional groups",values=c("orange", "black", "limegreen","#E7298A", "cyan4","blueviolet"))

library(patchwork)

p1 + p2 + p3

#Save data
#Save output
setwd("~/Dropbox/PhD/R/Chapter_2") #DROPBOX, files too large for github
saveRDS(analysis_1_bee_non_apis, "results_analysis_1_bee_non_apis_presence_absence.rds")
saveRDS(dat_analysis, "dat_analysis_results_analysis_1_bee_non_apis_presence_absence.rds")
