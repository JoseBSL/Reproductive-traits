########################################################################################################################################################
#SCRIPT FOR ANALYSIS (Z-SCORES~FUNCTIONAL GROUP*GUILD) ##HCLUST##

#1)LOAD DATA 

#2)PHYLOGENETIC DISTANCE OF THE SPECIES -Add them as covariable-

#3)SAVE MODEL

#4)PLOT OUTPUT NICELY
########################################################################################################################################################

#LOAD LIBRARIES
library(ape) #for phylogenetic distance
library(dplyr) #data processing
library(rtrees) #for phylogenetic distance
library(DHARMa)
library(brms)
library(cmdstanr)
library(ggplot2)

########################################################################################################################################################
#1) READ DATA
########################################################################################################################################################

d_5 <- read.csv("Data/Csv/quantitative_networks_Z_scores_with_traits_and_5_clusters_hclust.csv", row.names = 1)

########################################################################################################################################################
#2) PHYLOGENETIC DISTANCE OF THE SPECIES
########################################################################################################################################################

#5 CLUSTERS
#Prepare species, genus anD_5 family for calculating phylogenetic distance
d_5$Family_all <- as.character(d_5$Family_all)
d_5$Genus_all <- as.character(d_5$Genus_all)
d_5$Species_all <- as.character(d_5$Species_all)

#These species are not present for analysis in the matrix, remove or model cannot have phylo as covariable
d_5$Species_all[d_5$Species_all=="Diospyros seychellarum"] <- NA
d_5$Species_all[d_5$Species_all=="Memecylon eleagni"] <- NA
d_5$Species_all[d_5$Species_all=="Ocotea laevigata"] <- NA
d_5$Species_all[d_5$Species_all=="Soulamea terminaloides"] <- NA

#Make these NA's as NA
d_5$Species_all[d_5$Species_all=="NA"] <- NA
d_5$Family_all[d_5$Family_all=="NA"] <- NA
d_5$Genus_all[d_5$Genus_all=="NA"] <- NA
#remove NA's
d_5_1 <- d_5[!is.na(d_5$Family_all),]
d_5_1 <- d_5[!is.na(d_5$Species_all),]
d_5_1 <- d_5[!is.na(d_5$Genus_all),]

#prepare dataframe to calculate tree
phylo_5 <- as.data.frame(cbind(d_5_1$Family_all, d_5_1$Genus_all, d_5_1$Species_all))
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
d_5_1$phylo
d_5_1$phylo <- d_5_1$Species_all
str(d_5_1)

d_5_1$Clusters <- as.character(d_5_1$Clusters)
d_5_1$Clusters[d_5_1$Clusters=="1"] <- "A"
d_5_1$Clusters[d_5_1$Clusters=="2"] <- "B"
d_5_1$Clusters[d_5_1$Clusters=="3"] <- "C"
d_5_1$Clusters[d_5_1$Clusters=="4"] <- "D"
d_5_1$Clusters[d_5_1$Clusters=="5"] <- "E"

d_5_1$Clusters <- as.factor(d_5_1$Clusters)
levels(as.factor(d_5_1$Clusters))

#save data
#write.csv(d_5_1, "Data/Csv/d_5_1_brms_all_5_clusters.csv")

########################################################################################################################################################
#3)ANALYSIS
########################################################################################################################################################
#STUDENT
#5 clusters hclust
m_5_clust_stu_hclust <- brm(Z_scores ~ guild*Clusters + (1|Id) + (1|gr(phylo, cov = A)),
                            data = d_5_1, family  = student(),data2 = list(A = A_5), cores = 4,chains = 4, 
                            sample_prior = TRUE, warmup = 500, iter = 1500,
                            control = list(adapt_delta = 0.99)) 


marginal_effects(m_5_clust_stu_hclust, effects = "Clusters:guild")
pp_check(m_5_clust_stu_hclust) +xlim(-10,10)+ylim(0,3)
pp_check(m_5_clust_stu_hclust, type='violin_grouped',group="Clusters")+ylim(-4,4)
pp_check(m_5_clust_stu_hclust, type='violin_grouped',group="guild")+ylim(-4,4)

#SAVE MODEL
setwd("~/Dropbox/PhD/R") #DROPBOX, files too large for github
saveRDS(m_5_clust_stu_hclust, "m_5_clust_stu_hclust.RDS")
m_5_clust_stu_hclust <- readRDS("m_5_clust_stu_hclust.RDS")


#TRYING OTHER MODEL
#5 clusters hclust
m_5_clust_zero_neg_hclust <- brm((Interaction-1) ~ guild*Clusters + (1|Id) + (1|gr(phylo, cov = A)),
                            data = d_5_1, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                            sample_prior = TRUE, warmup = 500, iter = 1500,
                            control = list(adapt_delta = 0.99)) 

marginal_effects(m_5_clust_zero_neg_hclust, effects = "Clusters:guild")
pp_check(m_5_clust_zero_neg_hclust) +xlim(-50,200)+ylim(0,0.1)
pp_check(m_5_clust_zero_neg_hclust, type='violin_grouped',group="Clusters")+ylim(-4,4)
pp_check(m_5_clust_zero_neg_hclust, type='violin_grouped',group="guild")+ylim(-4,4)
saveRDS(m_5_clust_zero_neg_hclust, "m_5_clust_zero_neg_hclust")

pp_check(m_5_clust_zero_neg_hclust) +xlim(-50,200)+ylim(0,0.1)
marginal_effects(m_5_clust_zero_neg_hclust, effects = "Clusters:guild")
ce <- conditional_effects(m_5_clust_zero_neg_hclust, effects = "Clusters:guild",points=T) 


########################################################################################################################################################
#)4)SAVE MODEL
########################################################################################################################################################
#SAVE MODEL 1
setwd("~/Dropbox/PhD/R") #DROPBOX, files too large for github
saveRDS(m_5_clust_stu_hclust, "m_5_clust_stu_hclust.RDS")
#SAVE MODEL 2
setwd("~/Dropbox/PhD/R") #DROPBOX, files too large for github
saveRDS(m_5_clust_zero_neg_hclust, "m_5_clust_zero_neg_hclust.RDS")
########################################################################################################################################################
#5)PLOT OUTPUT 
########################################################################################################################################################
#read model 1
m_5_clust_stu_hclust <- readRDS("m_5_clust_stu_hclust.RDS")
#read model 2
m_5_clust_zero_neg_hclust <- readRDS("m_5_clust_zero_neg_hclust_1.RDS")

#############
#PLOT MODEL 1
#############
#Z-SCORES
ce <- conditional_effects(m_5_clust_stu_hclust, effects = "Clusters:guild",points=T) 
#change colnames in model output to use same aesthetics
#if not gg seems to don't like it
colnames(ce[[1]])[3] <- "a"
colnames(ce[[1]])[9] <- "Z_scores"

ggplot(ce[[1]], aes(x = Clusters, y = Z_scores, colour = as.factor(guild), group = 
  as.factor(guild))) +
  geom_point(size = 1, position = position_dodge(width = 0.4), alpha=1) +
  theme_bw()+ ylab("Standardize Nº of visits (Z-scores)") + xlab("Plant reproductive groups")+
  geom_point(data = d_5_1,aes(x = Clusters, y = Z_scores),size = 1, position = position_dodge(width = 0.4), alpha=0.15)+
  geom_errorbar(data=ce[[1]],mapping=aes(x=Clusters, ymin=lower__, ymax=upper__,colour = as.factor(guild), group = 
  as.factor(guild)), width=.4, position = position_dodge(width = 0.4)) +ylim(-0.8,2.5)
  
#############
#PLOT MODEL 2
#############
#Z-SCORES
ce_1 <- conditional_effects(m_5_clust_zero_neg_hclust, effects = "Clusters:guild",points=T) 
#change colnames in model output to use same aesthetics
#if not gg seems to don't like it
colnames(ce_1[[1]])[4] <- "a"
colnames(ce_1[[1]])[10] <- "Interaction"
ce_1[[1]][10]<- ce_1[[1]][10]+1
#Order levels
ce_1[[1]]$guild <- factor(ce_1[[1]]$guild, levels = c("Bee","Non-bee-Hymenoptera","Syrphids","Non-syrphids-diptera","Lepidoptera","Coleoptera"))
d_5_1$guild <- factor(d_5_1$guild, levels = c("Bee","Non-bee-Hymenoptera","Syrphids","Non-syrphids-diptera","Lepidoptera","Coleoptera"))


ggplot(ce_1[[1]], aes(x = Clusters, y = Interaction, colour = as.factor(guild), group = 
  as.factor(guild))) +
  geom_point(size = 1.2, position = position_dodge(width = 0.8), alpha=1) +
  theme_bw()+ ylab("Nº of visits per plant taxa") + xlab("Plant reproductive groups")+
  geom_point(data = d_5_1,aes(x = Clusters, y = (Interaction+1)),size = 1.2, position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2), alpha=0.3)+
  geom_errorbar(data=ce_1[[1]],mapping=aes(x=Clusters, ymin=lower__, ymax=upper__,colour = as.factor(guild), group = 
  as.factor(guild)), width=.6,alpha=0.8, size = 0.9,position = position_dodge(width = 0.8)) +ylim(0,200)+
  scale_color_manual("Floral visitors guilds",values=c("#E69F00","#D55E00", "#287DAB", "#009E73", "#A7473A",  "black"))
  
position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.2)
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

