########################################################################################################################################################
#SCRIPT FOR ANALYSIS (VISITATION~FUNCTIONAL GROUP*GUILD) ##HCLUST## FAMD IMPUTATION METHOD

#1)LOAD DATA 

#2)PHYLOGENETIC DISTANCE OF THE SPECIES -Add them as covariable-

#3)SAVE MODEL

#4)PLOT MODEL 


########################################################################################################################################################

#LOAD LIBRARIES
library(ape) #for phylogenetic distance
library(dplyr) #data processing
library(rtrees) #for phylogenetic distance
library(DHARMa)
library(brms)
library(cmdstanr)
library(ggplot2)
library(emmeans)
library(tidybayes)

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
d_5 <- read.csv("Data/Csv/quantitative_networks_Z_scores_with_traits_and_5_clusters_hclust_forest_data.csv", row.names = 1)
levels(factor(d_5$guild))
#I'm going to run the model just with the main poll. functional groups
#exclude other insects level
dat <- subset(d_5, guild!="Other_insects" & guild!="Lizards" & guild!="Birds")
levels(factor(dat$guild))
dat$guild <- as.character(dat$guild)
dat$guild[dat$guild=="Bee"] <- "Bees"
dat$guild <- factor(dat$guild, levels = c("Bees","Coleoptera", "Lepidoptera", "Non-bee-Hymenoptera",
                                          "Non-syrphids-diptera", "Syrphids"))

########################################################################################################################################################
#2) PHYLOGENETIC DISTANCE OF THE SPECIES
########################################################################################################################################################
#5 CLUSTERS
#Prepare species, genus anD_5 family for calculating phylogenetic distance
dat$Family_all  <- as.character(dat$Family_all)
dat$Genus_all   <- as.character(dat$Genus_all)
dat$Species_all <- as.character(dat$Species_all)


#prepare dataframe to calculate tree
phylo_5 <- as.data.frame(cbind(dat$Family_all, dat$Genus_all, dat$Species_all))
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
dat$phylo
dat$phylo <- dat$Species_all
str(dat)

dat$Clusters <- as.character(dat$Clusters)
dat$Clusters[dat$Clusters=="1"] <- "A"
dat$Clusters[dat$Clusters=="2"] <- "B"
dat$Clusters[dat$Clusters=="3"] <- "C"
dat$Clusters[dat$Clusters=="4"] <- "D"
dat$Clusters[dat$Clusters=="5"] <- "E"

dat$Clusters <- as.factor(dat$Clusters)
levels(as.factor(dat$Clusters))
levels(factor(dat$guild))

########################################################################################################################################################
#3)ANALYSIS
########################################################################################################################################################

#5 clusters hclust
model_forest_data <- brm((Interaction-1) ~ guild*Clusters + (1|Id) + (1|gr(phylo, cov = A)),
                            data = dat, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                            sample_prior = TRUE, warmup = 500, iter = 2000,
                            control = list(adapt_delta = 0.99))


#MODEL FIT
pp_check(m_5_clust_zero_neg_hclust_famd) +xlim(-50,200)+ylim(0,0.1)
pp_check(m_5_clust_zero_neg_hclust_famd, type='violin_grouped',group="Clusters")+ylim(-4,4)
pp_check(m_5_clust_zero_neg_hclust_famd, type='violin_grouped',group="guild")+ylim(-4,4)

#CHECK BAYES R2 
performance::r2_bayes(model_forest_data)
bayes_R2(model_forest_data)
loo_R2(model_forest_data)

#CHECK MODEL OUTPUT
marginal_effects(model_forest_data, effects = "Clusters:guild")

########################################################################################################################################################
#4) POSTHOC COMPARISON
########################################################################################################################################################


result <- model_forest_data %>%
  emmeans( ~ Clusters) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws()


posthoc_plot <- ggplot(result, aes(x = .value, y = contrast,  fill=contrast)) +
  stat_eye() +theme_ms() +theme(legend.position = "none")+geom_vline(xintercept=0,linetype="dashed") + ylab("Pairwise comparisons")+
  xlab("Marginal mean difference")


########################################################################################################################################################
#5)SAVE MODEL
########################################################################################################################################################
#SAVE MODEL 1
setwd("~/Dropbox/PhD/R") #DROPBOX, files too large for github
saveRDS(model_forest_data, "model_forest_data.RDS")
saveRDS(dat, "data_model.RDS")
saveRDS(result, "result_Tukey_model.RDS")

########################################################################################################################################################
#6)PLOT OUTPUT VISITATION DATA
########################################################################################################################################################
#read model 2
setwd("~/Dropbox/PhD/R") #DROPBOX, files too large for github

model_forest_data <- readRDS("model_forest_data.RDS")

dat <- readRDS("data_model.RDS")

#cond effect model visitation data
ce_1 <- conditional_effects(model_forest_data, effects = "Clusters:guild",points=T) 
#change colnames in model output to use same aesthetics
#if not gg seems to don't like it
colnames(ce_1[[1]])[4] <- "a"
colnames(ce_1[[1]])[10] <- "Interaction"
ce_1[[1]][10]<- ce_1[[1]][10]+1
ce_1[[1]][12]<- ce_1[[1]][12]+1
ce_1[[1]][13]<- ce_1[[1]][13]+1

#Order levels
ce_1[[1]]$guild <- as.character(ce_1[[1]]$guild)
ce_1[[1]]$guild <- factor(ce_1[[1]]$guild, levels = c("Bees","Non-bee-Hymenoptera","Syrphids","Non-syrphids-diptera","Lepidoptera","Coleoptera"))
dat$guild <- as.character(dat$guild)
dat$guild <- factor(dat$guild, levels = c("Bees","Non-bee-Hymenoptera","Syrphids","Non-syrphids-diptera","Lepidoptera","Coleoptera"))

#plot model
model_plot <- ggplot(ce_1[[1]], aes(x = Clusters, y = Interaction, colour = as.factor(guild), group = 
  as.factor(guild))) +
  geom_point(size = 2.4, position = position_dodge(width = 0.68), alpha=1) +
  theme_ms()+ ylab("Nº of visits per plant taxa") + xlab("Plant functional groups")+
  geom_point(data = dat,aes(x = Clusters, y = (Interaction),group = 
  as.factor(guild)),size = 1.2,shape=21, position = position_jitterdodge(dodge.width = 0.68, jitter.width = 0.25), alpha=0.11)+
  geom_errorbar(data=ce_1[[1]],mapping=aes(x=Clusters, ymin=lower__, ymax=upper__,colour = as.factor(guild), group = 
  as.factor(guild)), width=0.6,alpha=1, size = 1,position = position_dodge(width = 0.68)) +ylim(0,quantile(dat$Interaction, 0.95))+
  scale_color_manual("Floral visitors guilds",values=c("#E69F00","#D55E00", "#287DAB", "#009E73", "#A7473A",  "black","grey"))
  
#Values over the percentile 95 were omitted for visualization purposes

#Combine posthoc and model plot

library(patchwork)

model_plot + posthoc_plot


########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
