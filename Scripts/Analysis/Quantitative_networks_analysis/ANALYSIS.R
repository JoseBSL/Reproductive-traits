#ANALYSIS OF NETWORK METRICS WITH PLANT FUNCTIONAL GROUPS AND GUILD

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#load packages
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(brms) #bayesian models
library(ape)
library(dplyr)
library(rtrees)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#load data
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

qn <- read.csv("Data/Csv/quantitative_networks_imputed_with_clusters.csv")
qn_1 <- qn[,-c(1,3,29)]
str(qn_1)
qn_1$Species_all <- as.character(qn_1$Species_all)
qn_1$Genus_all <- as.character(qn_1$Genus_all)
qn_1$Family_all <- as.character(qn_1$Family_all)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#calculate phylogenetic distance
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Prepare species, genus and family for calculating phylogenetic distance
phylo <- as.data.frame(cbind(qn_1$Family_all, qn_1$Genus_all, qn_1$Species_all))
colnames(phylo) <-  c("family", "genus", "species")

#Select unique cases
phylo_1 <- phylo[!duplicated(phylo$species),]
phylo_2 <- tibble(phylo_1)
phylo_3 <- get_tree(sp_list = phylo_2, tree = tree_plant_otl, taxon = "plant")

#Convert phylogenetic tree into matrix
A <- vcv.phylo(phylo_3)

#Standardize to max value 1
A <- A/max(A)

#Unify column names; remove underscore and remove asterik
rownames(A) <- gsub("\\*", "", rownames(A))
colnames(A) <- gsub("\\*", "", colnames(A))
colnames(A) <- gsub("_", " ", colnames(A))
rownames(A) <- gsub("_", " ", rownames(A))

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#analysis
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(phyr)
#model
library(ggplot2)
ggplot(qn_1, aes(x=log(Corolla_diameter_mean), y=Z_scores,group=Order_all)) +
  geom_point(aes( color=Order_all))

str(qn_1)



m1 <- pglmm(Z_scores ~ clusters+(1 | phylo) +(1 | Id),
data = qn_1,cov_ranef = list(sp = A),bayes = FALSE,REML = TRUE)


summary(m1)


pglmm.plot.re(x = m1, show.image = FALSE, show.sim.image = TRUE, 
              add.tree.sp = TRUE, colorkey = FALSE, useAbs = FALSE)








#check data distribution
hist(qn_1$Z_scores)

min(qn_1$Z_scores)
#move distribution to the right in order to have minimum value equal to one
qn_1$postive_z_scores <- qn_1$Z_scores + abs(min(qn_1$Z_scores))+1


qn_1$clusters <- as.character(qn_1$clusters)
str(qn_1)
(qn_1$guild)


qn_1$clusters <- as.character(qn_1$clusters)
qn_1$clusters[qn_1$clusters=="1"] <- "A"
qn_1$clusters[qn_1$clusters=="2"] <- "B"
qn_1$clusters[qn_1$clusters=="3"] <- "C"
qn_1$clusters[qn_1$clusters=="4"] <- "D"
qn_1$clusters[qn_1$clusters=="5"] <- "E"
qn_1$clusters <- as.factor(qn_1$clusters)



qn_1$Interaction

model_visits_1 <- brm(log(Interaction+1)~ clusters*guild + (1|Id) + (1|gr(phylo, cov = A)),
                      data = qn_1, family  = lognormal(),data2 = list(A = A), cores = 4,
                      sample_prior = TRUE, warmup = 500, iter = 1500,save_all_pars=T,
                      control = list(adapt_delta = 0.99)) 


pp_check(model_visits_1) 
marginal_effects(model_visits_1, effects = "clusters:guild")








model1 <- brm(postive_z_scores ~ clusters*guild + (1|Id) + (1|gr(phylo, cov = A)),
          data = qn_1, family  = lognormal(),data2 = list(A = A), cores = 4,
          sample_prior = TRUE, warmup = 500, iter = 1500,save_all_pars=T,
          control = list(adapt_delta = 0.99)) 

marginal_effects(model1, effects = "clusters:guild")


summary(model1)
pp_check(model1) 
c_e <- conditional_effects(model1)
p <- plot(c_e, points=T,plot = FALSE)[[1]]
performance::r2_bayes(model1)
plot(c_e, points=T,plot = FALSE)[[1]]




qn_1$postive_z_scores <- qn_1$Z_scores + abs(min(qn_1$Z_scores))+1

model2 <- brm(postive_z_scores ~ clusters*guild + (1|Id) + (1|gr(phylo, cov = A)),
              data = qn_1,  brmsfamily("gamma"),data2 = list(A = A), cores = 4,
              sample_prior = TRUE, warmup = 500, iter = 1500,save_all_pars=T,
              control = list(adapt_delta = 0.99)) 


marginal_effects(model2, effects = "clusters:guild")

summary(model2)
pp_check(model2) 
c_e <- conditional_effects(model2)
p <- plot(c_e, points=T,plot = FALSE)[[1]]
performance::r2_bayes(model2)
plot(c_e, points=T,plot = FALSE)[[1]]

hist(qn_1$Z_scores)

model3 <- brm(Z_scores ~ clusters*guild + (1|Id) + (1|gr(phylo, cov = A)),
              data = qn_1,  brmsfamily("exgaussian"),data2 = list(A = A), cores = 4,
              sample_prior = TRUE, warmup = 500, iter = 1500,save_all_pars=T,
              control = list(adapt_delta = 0.99)) 


marginal_effects(model3, effects = "clusters:guild")

summary(model3)
pp_check(model3) 
c_e <- conditional_effects(model3)
p <- plot(c_e, points=T,plot = FALSE)[[1]]
performance::r2_bayes(model2)
plot(c_e, points=T,plot = FALSE)[[1]]
hist(qn_1$Interaction)
model4 <- brm(Interaction ~ clusters*guild + (1|Id) + (1|gr(phylo, cov = A)),
              data = qn_1,  brmsfamily("negbinomial"),data2 = list(A = A), cores = 4,
              sample_prior = TRUE, warmup = 500, iter = 1500,save_all_pars=T,
              control = list(adapt_delta = 0.99)) 


marginal_effects(model4, effects = "clusters:guild")
library(ggplot2)
summary(model4)
pp_check(model4) + xlim(-10,40)
c_e <- conditional_effects(model4)
p <- plot(c_e, points=T,plot = FALSE)[[1]]
performance::r2_bayes(model2)
plot(c_e, points=T,plot = FALSE)[[1]]

hist(log(qn_1$Interaction))

model5 <- brm(log(Interaction) ~ clusters*guild + (1|Id) + (1|gr(phylo, cov = A)),
              data = qn_1,  brmsfamily("skew_normal"),data2 = list(A = A), cores = 4,
              sample_prior = TRUE, warmup = 500, iter = 1500,save_all_pars=T,
              control = list(adapt_delta = 0.99)) 

pp_check(model5) + xlim(-10,40)
summary(model5)
marginal_effects(model5, effects = "clusters:guild")



model6 <- brm(log(Interaction) ~ clusters*guild + (1|Id) + (1|gr(phylo, cov = A)),
              data = qn_1,  brmsfamily("gaussian"),data2 = list(A = A), cores = 4,
              sample_prior = TRUE, warmup = 500, iter = 1500,save_all_pars=T,
              control = list(adapt_delta = 0.99)) 

hist(qn_1$Interaction)
boxplot(qn_1$Interaction)
qn_1$Interaction[qn_1$Interaction>100] <- 50
qn_1$Interaction[qn_1$Interaction>200] <- 200


str(qn_1)
qn_1$clusters <- as.factor(qn_1$clusters)
summary(lm(qn_1$Z_scores~qn_1$clusters*qn_1$guild))
hist(qn_1$Z_scores)

interceptonlymodel <- brm(Z_scores ~ clusters * guild+ (1|Id),brmsfamily(family = "student"),  data = qn_1, warmup = 200, iter = 1000, cores = 4, chains = 4, seed = 123) #to run the model

summary(interceptonlymodel)


summary(interceptonlymodel)
pp_check(interceptonlymodel) 
c_e <- conditional_effects(interceptonlymodel)
p1 <- plot(c_e, points=T,plot = FALSE)[[1]]
bayes_R2(interceptonlymodel)