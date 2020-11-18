#ANALYSIS OF NETWORK METRICS WITH PLANT FUNCTIONAL GROUPS AND GUILD

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#load packages
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(brms) 

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#load data
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

qn <- read.csv("Data/Csv/quantitative_networks_imputed_with_clusters.csv")
qn_1 <- qn[,-c(1,3,29)]


library(lme4)

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