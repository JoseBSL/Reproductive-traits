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
d_a <- readRDS("Data/RData/data_analysis_1.rds") #ALL SPP
########################################################################################################################################################
#PREPARE POLL. FUNCTIONAL GROUPS FOR MODELLING
########################################################################################################################################################
d_a_1 <- subset(d_a, guild!="Other_insects" & guild!="Lizards" & guild!="Birds") #Select just the main 6 functional groups
levels(factor(d_a_1$guild))
d_a_1$guild <- as.character(d_a_1$guild)
d_a_1$guild[d_a_1$guild=="Bee"] <- "Bees" #Adding an S to bee(S)
d_a_1$guild <- factor(d_a_1$guild, levels = c("Bees","Coleoptera", "Lepidoptera", "Non-bee-Hymenoptera",
                                          "Non-syrphids-diptera", "Syrphids"))

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
########################################################################################################################################################
#RUN MODEL
########################################################################################################################################################

analysis_1 <- brm((Interaction-1) ~ PC1*guild + PC2*guild + PC3*guild +(1|Id) + (1|gr(phylo, cov = A)),
                  data = dat_analysis, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                  sample_prior = TRUE, warmup = 500, iter = 2000,
                  control = list(adapt_delta = 0.99))



marginal_effects(analysis_1, effects = "PC1:guild")
pp_check(analysis_1) +xlim(-50,200)+ylim(0,0.1)
pp_check(analysis_1, type='violin_grouped',group="Clusters")+ylim(-4,4)
pp_check(analysis_1, type='violin_grouped',group="guild")+ylim(-4,4)

#Save data
saveRDS(analysis_1, "Data/RData/results_analysis_1.rds")
saveRDS(dat_analysis, "Data/RData/dat_analysis_results_analysis_1.rds")
########################################################################################################################################################
#PLOT
########################################################################################################################################################
#Plot nicely PC1
analysis_1 <- readRDS("Data/RData/results_analysis_1.rds")
dat_analysis <- readRDS("Data/RData/dat_analysis_results_analysis_1.rds")

ce_pc1 <- conditional_effects(analysis_1, effects = "PC1:guild",points=T) 

p1 <- ggplot(ce_pc1[[1]], aes(x = PC1, y = (estimate__+1), group=guild, colour=guild)) + geom_point(data = dat_analysis,
   aes(x = PC1, y = Interaction),size = 0.75, alpha=0.5) + geom_line(size=0.8) + 
  ylim(0,quantile(dat_analysis$Interaction, 0.95)) + ylab("Number of visits")+
  theme_ms() + theme(legend.position = "none") + scale_color_manual(name="Functional groups",values=c("orange", "black", "limegreen","#E7298A", "cyan4","blueviolet"))
#Plot nicely PC2
ce_pc2 <- conditional_effects(analysis_1, effects = "PC2:guild",points=T) 

p2 <- ggplot(ce_pc2[[1]], aes(x = PC2, y = (estimate__+1), group=guild, colour=guild)) + geom_point(data = dat_analysis,
  aes(x = PC2, y = Interaction),size = 0.75, alpha=0.5) + geom_line(size=0.8) + 
  ylim(0,quantile(dat_analysis$Interaction, 0.95)) + ylab("Number of visits")+
  theme_ms() + theme(legend.position = "none") + scale_color_manual(name="Functional groups",values=c("orange", "black", "limegreen","#E7298A", "cyan4","blueviolet"))

#Plot nicely PC3
ce_pc3 <- conditional_effects(analysis_1, effects = "PC3:guild",points=T) 

p3 <- ggplot(ce_pc3[[1]], aes(x = PC3, y = (estimate__+1), group=guild, colour=guild)) + geom_point(data = dat_analysis,
  aes(x = PC3, y = Interaction),size = 0.75, alpha=0.5) + geom_line(size=0.8) + 
  ylim(0,quantile(dat_analysis$Interaction, 0.95)) + ylab("Number of visits")+
  theme_ms() + theme(legend.position = "none") + scale_color_manual(name="Functional groups",values=c("orange", "black", "limegreen","#E7298A", "cyan4","blueviolet"))


