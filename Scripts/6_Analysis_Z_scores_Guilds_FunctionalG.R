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


#TRYING OTHER MODEL
#5 clusters hclust
m_5_clust_neg_hclust <- brm(Interaction ~ guild*Clusters + (1|Id) + (1|gr(phylo, cov = A)),
                            data = d_5_1, family  = negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                            sample_prior = TRUE, warmup = 500, iter = 1500,
                            control = list(adapt_delta = 0.99)) 

marginal_effects(m_5_clust_neg_hclust, effects = "Clusters:guild")
pp_check(m_5_clust_neg_hclust) +xlim(-50,200)+ylim(0,0.1)
pp_check(m_5_clust_neg_hclust, type='violin_grouped',group="Clusters")+ylim(-4,4)
pp_check(m_5_clust_neg_hclust, type='violin_grouped',group="guild")+ylim(-4,4)
saveRDS(m_5_clust_neg_hclust, "m_5_clust_neg_hclust")






########################################################################################################################################################
#)4)SAVE MODEL
########################################################################################################################################################
#SAVE MODEL
setwd("~/Dropbox/PhD/R") #DROPBOX, files too large for github
saveRDS(m_5_clust_stu_hclust, "m_5_clust_stu_hclust.RDS")

########################################################################################################################################################
#5)PLOT OUTPUT NICELY
########################################################################################################################################################
summary(m1)
pp_check(m1) + xlim(-200,200)+ylim(0,0.2)
ce <- conditional_effects(m_5_clust_stu_hclust, effects = "Clusters:guild",points=T) 
levels(ce[[1]]$guild)
#PLOT OUTPUT
cat_plot(m1, pred = cyl, modx = fwd)

colnames(ce[[1]])[3] <- "a"
colnames(ce[[1]])[9] <- "Z_scores"


ggplot(d_5_1, aes(x = Clusters, y = Z_scores, colour = as.factor(guild), group = 
                  as.factor(guild))) +
  geom_point(size = 1, position = position_dodge(width = 0.4), alpha=0.2) +
  theme_bw()+ ylab("Standardize visits (Z-scores)") + xlab("Plant reproductive groups")+
  geom_point(data=ce[[1]],group=as.factor(ce[[1]]$guild))

ce[]


ggplot(ce[[1]], aes(x = Clusters, y = Z_scores, colour = as.factor(guild), group = 
                    as.factor(guild))) +
  geom_point(size = 1, position = position_dodge(width = 0.4), alpha=1) +
  theme_bw()+ ylab("Standardize visits (Z-scores)") + xlab("Plant reproductive groups")+
  geom_point(data = d_5_1,aes(x = Clusters, y = Z_scores),size = 1, position = position_dodge(width = 0.4), alpha=0.15)+
  geom_errorbar(data=ce[[1]],mapping=aes(x=Clusters, ymin=lower__, ymax=upper__,colour = as.factor(guild), group = 
  as.factor(guild)), width=.4, position = position_dodge(width = 0.4))+ylim(-0.8,1)
  
conditions <- data.frame(zAge = c(-1, 0, 1))


conditional_effects(
  m_5_clust_stu_hclust, "Clusters:guild",
  select_points = 0.1,points = TRUE)
plot(ce, points = TRUE)
# }



more_than_500 <- d_5_1[d_5_1$Interaction>500, ]

levels(more_than_500$Id)


hist(d_5_1$Interaction)



mean(d_5_1$Z_scores[d_5_1$Id=="11_8_kaiser-bunbury_2017_seychelles_trois_feres.csv"])
mean(d_5_1$Z_scores[d_5_1$Id=="16_1_burkle_usa_2013.csv"])

$››‹colnames(ce[[1]])

+
  geom_errorbar(aes(ymax = val + SD, ymin = val - SD), position = 
                  position_dodge(width = 0.2), width = 0.2) +
  labs(y = "Mean # of explorations (+/- SD", colour = "pp")
             


ggplot(data=ce[[1]], aes(x = Clusters, y = Z_scores,color = ordered(guild))) +
  geom_point(data = d_5_1,aes(x = Clusters, y = Z_scores,color = ordered(guild))) + 
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2") + theme_bw() +
  geom_errorbar(data=p1.2.1[[1]],mapping=aes(x=autonomous_selfing_level, ymin=lower__, ymax=upper__), width=.1, color="black")+
  geom_point(data=p1.2.1[[1]], mapping=aes(x=autonomous_selfing_level, y=estimate__), color="black") + ylab("Visits") + xlab("Selfing level")+
  theme(legend.position = "none")



########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

