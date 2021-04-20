########################################################################################################################################################
# SCRIPT FOR ANALYSIS 1 VISITS/SPECIALIZATION ~ PRINCIPAL COMPONENTS
########################################################################################################################################################
#Load libraries
library(brms) #modelling
library(cmdstanr) #modelling
library(cowplot) #panel of plots
library(ggplot2) #plotting
library(patchwork)

# Theme for publication
theme_ms <- function(base_size=12, base_family="Helvetica") {
  (theme_bw(base_size = base_size, base_family = base_family)+
     theme(text=element_text(color="black"),
           axis.title=element_text( size = rel(1.3)),
           axis.text=element_text(size = rel(1.5), color = "black"),
           legend.title=element_text(face="bold"),
           legend.text=element_text(),
           legend.background=element_rect(fill="transparent"),
           legend.key.size = unit(0.4, 'lines'),
           panel.border=element_rect(color="black",size=1),
           panel.grid.minor.x =element_blank(),
           panel.grid.minor.y= element_blank(),
           panel.grid.major= element_blank()
     ))
}
########################################################################################################################################################
#LOAD DATA
########################################################################################################################################################
data_analysis_2 <- readRDS("Data/RData/data_analysis_2.rds") 
colnames(data_analysis_2)[11] <- "Id"
########################################################################################################################################################
#MODEL VISITS ~ PRINCIPAL COMPONENTS
########################################################################################################################################################
Visits_PCs <- brm((Visits-1) ~ PC1 + PC2 + PC3 + (1|Id),
                  data = data_analysis_2, family  = zero_inflated_negbinomial(), cores = 4,chains = 4, 
                  sample_prior = TRUE, warmup = 500, iter = 2000,
                  control = list(adapt_delta = 0.99)) 

#looks ok
pp_check(Visits_PCs) +xlim(-50,200)+ylim(0,0.1)
#summary
sumary(Visits_PCs)

#Save output
setwd("~/Dropbox/PhD/R/Chapter_2") #DROPBOX, files too large for github
saveRDS(Visits_PCs, "results_analysis_2_Visits.rds") 
#read
Visits_PCs <- readRDS("results_analysis_2_Visits.rds") #load model

ce_1 <- conditional_effects(Visits_PCs, effects = "PC1",points=T) 
colnames(ce_1[[1]])[3] <- "Interaction"

Visits_PC1 <- ggplot(ce_1[[1]], aes(x = -PC1, y = (estimate__+1))) + geom_point(data = data_analysis_2,aes(x = -PC1, y = Visits),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(data_analysis_2$Visits, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Number of visits")

ce_2 <- conditional_effects(Visits_PCs, effects = "PC2",points=T) 
colnames(ce_2[[1]])[3] <- "Interaction"

Visits_PC2 <- ggplot(ce_2[[1]], aes(x = -PC2, y = (estimate__+1))) + geom_point(data = data_analysis_2,aes(x = -PC2, y = Visits),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(data_analysis_2$Visits, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Number of visits")

ce_3 <- conditional_effects(Visits_PCs, effects = "PC3",points=T) 
colnames(ce_3[[1]])[3] <- "Interaction"

Visits_PC3 <- ggplot(ce_3[[1]], aes(x = -PC3, y = (estimate__+1))) + geom_point(data = data_analysis_2,aes(x = -PC3, y = Visits),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(data_analysis_2$Visits, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Number of visits")

cowplot::plot_grid(Visits_PC1,Visits_PC2,Visits_PC3)
########################################################################################################################################################

data_analysis_2$d <- ifelse(data_analysis_2$d > 1, 1, data_analysis_2$d)

Specialization_PCs <- brm(d ~ PC1 + PC2 + PC3 + (1|Id),
                          data = data_analysis_2, family  = zero_one_inflated_beta(), cores = 4,chains = 4, 
                          sample_prior = TRUE, warmup = 500, iter = 2000,
                          control = list(adapt_delta = 0.99)) 


summary(Specialization_PCs)

#Save output
setwd("~/Dropbox/PhD/R/Chapter_2") #DROPBOX, files too large for github
saveRDS(Specialization_PCs, "results_analysis_2_Specialization_PCs.rds") 
#read
Specialization_PCs <- readRDS("results_analysis_2_Specialization_PCs.rds") #load model

#DO PPCHECK HERE

ce_1 <- conditional_effects(Specialization_PCs, effects = "PC1",points=T) 
colnames(ce_1[[1]])[3] <- "Interaction"

Specialization_PC1 <- ggplot(ce_1[[1]], aes(x = -PC1, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC1, y = d),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(data_analysis_2$d, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Specialization (d')")


ce_2 <- conditional_effects(Specialization_PCs, effects = "PC2",points=T) 
colnames(ce_2[[1]])[3] <- "Interaction"

Specialization_PC2 <- ggplot(ce_2[[1]], aes(x = -PC2, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC2, y = d),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(data_analysis_2$d, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Specialization (d')")

ce_3 <- conditional_effects(Specialization_PCs, effects = "PC3",points=T) 
colnames(ce_3[[1]])[3] <- "Interaction"

Specialization_PC3 <- ggplot(ce_3[[1]], aes(x = -PC3, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC3, y = d),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(data_analysis_2$d, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Specialization (d')")

cowplot::plot_grid(Specialization_PC1, Specialization_PC2, Specialization_PC3)


########################################################################################################################################################
#degree
########################################################################################################################################################

hist(data_analysis_2$degree)

degree <- brm(degree ~ PC1 + PC2 + PC3 + (1|Id),
                             data = data_analysis_2, family  = zero_inflated_negbinomial(), cores = 4,chains = 4, 
                             sample_prior = TRUE, warmup = 500, iter = 2000,
                             control = list(adapt_delta = 0.99)) 

pp_check(degree)

summary(degree)

conditional_effects(degree, effects = "PC1",points=T) 
conditional_effects(degree, effects = "PC2",points=T) 
conditional_effects(degree, effects = "PC3",points=T) 

#Save output
setwd("~/Dropbox/PhD/R/Chapter_2") #DROPBOX, files too large for github
saveRDS(degree, "results_analysis_2_degree.rds") 
#read
degree <- readRDS("results_analysis_2_degree.rds") #load model


ce_1 <- conditional_effects(degree, effects = "PC1",points=T) 

degree_PC1 <- ggplot(ce_1[[1]], aes(x = -PC1, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC1, y = degree),
 size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2)  +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("degree")

ce_2 <- conditional_effects(degree, effects = "PC2",points=T) 

degree_PC2 <- ggplot(ce_2[[1]], aes(x = -PC2, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC2, y = degree),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("degree")

ce_3 <- conditional_effects(degree, effects = "PC3",points=T) 

degree_PC3 <- ggplot(ce_3[[1]], aes(x = -PC3, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC3, y = degree),
 size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("degree")

degree_PC1 + degree_PC2 + degree_PC3


########################################################################################################################################################
#normalised.degree
########################################################################################################################################################

normalised.degree_PCs <- brm(normalised.degree ~ PC1 + PC2 + PC3 + (1|Id),
                             data = data_analysis_2, family  = weibull(), cores = 4,chains = 4, 
                             sample_prior = TRUE, warmup = 500, iter = 2000,
                             control = list(adapt_delta = 0.99)) 


pp_check(normalised.degree_PCs) +xlim(-1,1.5)

conditional_effects(normalised.degree_PCs, effects = "PC1",points=T) 
conditional_effects(normalised.degree_PCs, effects = "PC2",points=T) 
conditional_effects(normalised.degree_PCs, effects = "PC3",points=T) 


#Save output
setwd("~/Dropbox/PhD/R/Chapter_2") #DROPBOX, files too large for github
saveRDS(normalised.degree_PCs, "results_analysis_2_normalised.degree_PCs.rds") 
#read
normalised.degree_PCs <- readRDS("results_analysis_2_normalised.degree_PCs.rds") #load model

ce_1 <- conditional_effects(normalised.degree_PCs, effects = "PC1",points=T) 

normalised.degree_PC1 <- ggplot(ce_1[[1]], aes(x = -PC1, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC1, y = normalised.degree),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2)  +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("normalised.degree")

ce_2 <- conditional_effects(normalised.degree_PCs, effects = "PC2",points=T) 

normalised.degree_PC2 <- ggplot(ce_2[[1]], aes(x = -PC2, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC2, y = normalised.degree),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("normalised.degree")

ce_3 <- conditional_effects(normalised.degree_PCs, effects = "PC3",points=T) 

normalised.degree_PC3 <- ggplot(ce_3[[1]], aes(x = -PC3, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC3, y = normalised.degree),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(data_analysis_2$normalised.degree, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("normalised.degree")

normalised.degree_PC1 + normalised.degree_PC2 + normalised.degree_PC3

########################################################################################################################################################
##betweenness
########################################################################################################################################################

data_analysis_2$betweenness <- ifelse(data_analysis_2$betweenness > 1, 1, data_analysis_2$betweenness)

betweenness <- brm(betweenness ~ PC1 + PC2 + PC3 + (1|Id),
              data = data_analysis_2, family  = zero_one_inflated_beta(), cores = 4,chains = 4, 
              sample_prior = TRUE, warmup = 500, iter = 2000,
              control = list(adapt_delta = 0.99)) 

summary(betweenness)

pp_check(betweenness)


ce_1 <- conditional_effects(betweenness, effects = "PC1",points=T) 


#Save output
setwd("~/Dropbox/PhD/R/Chapter_2") #DROPBOX, files too large for github
saveRDS(betweenness, "results_analysis_2_betweenness.rds") 
#read
betweenness <- readRDS("results_analysis_2_betweenness.rds") #load model


betweenness_PC1 <- ggplot(ce_1[[1]], aes(x = -PC1, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC1, y = betweenness),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2)  +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("betweenness")

ce_2 <- conditional_effects(betweenness, effects = "PC2",points=T) 

betweenness_PC2 <- ggplot(ce_2[[1]], aes(x = -PC2, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC2, y = betweenness),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("betweenness")

ce_3 <- conditional_effects(betweenness, effects = "PC3",points=T) 

betweenness_PC3 <- ggplot(ce_3[[1]], aes(x = -PC3, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC3, y = betweenness),
   size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("betweenness")

betweenness_PC1 + betweenness_PC2 + betweenness_PC3

########################################################################################################################################################
##weighted betweenness
########################################################################################################################################################

data_analysis_2$weighted.betweenness <- ifelse(data_analysis_2$weighted.betweenness > 1, 1, data_analysis_2$weighted.betweenness)

weighted.betweenness <- brm(weighted.betweenness ~ PC1 + PC2 + PC3 + (1|Id),
                   data = data_analysis_2, family  = zero_one_inflated_beta(), cores = 4,chains = 4, 
                   sample_prior = TRUE, warmup = 500, iter = 2000,
                   control = list(adapt_delta = 0.99)) 

summary(weighted.betweenness)

pp_check(weighted.betweenness)

#Save output
setwd("~/Dropbox/PhD/R/Chapter_2") #DROPBOX, files too large for github
saveRDS(weighted.betweenness, "results_analysis_2_weighted.betweenness.rds") 
#read
weighted.betweenness <- readRDS("results_analysis_2_weighted.betweenness.rds") #load model


ce_1 <- conditional_effects(weighted.betweenness, effects = "PC1",points=T) 

weighted.betweenness_PC1 <- ggplot(ce_1[[1]], aes(x = -PC1, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC1, y = weighted.betweenness),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2)  +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("weighted.betweenness")

ce_2 <- conditional_effects(weighted.betweenness, effects = "PC2",points=T) 

weighted.betweenness_PC2 <- ggplot(ce_2[[1]], aes(x = -PC2, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC2, y = weighted.betweenness),
   size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("weighted.betweenness")

ce_3 <- conditional_effects(weighted.betweenness, effects = "PC3",points=T) 

weighted.betweenness_PC3 <- ggplot(ce_3[[1]], aes(x = -PC3, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC3, y = weighted.betweenness),
   size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("weighted.betweenness")

weighted.betweenness_PC1 + weighted.betweenness_PC2 + weighted.betweenness_PC3
########################################################################################################################################################
##PDI
########################################################################################################################################################

data_analysis_2$PDI <- ifelse(data_analysis_2$PDI > 1, 1, data_analysis_2$PDI)

PDI <- brm(PDI ~ PC1 + PC2 + PC3 + (1|Id),
                   data = data_analysis_2, family  = zero_one_inflated_beta(), cores = 4,chains = 4, 
                   sample_prior = TRUE, warmup = 500, iter = 2000,
                   control = list(adapt_delta = 0.99)) 

pp_check(PDI)

summary(PDI)

#Save output
setwd("~/Dropbox/PhD/R/Chapter_2") #DROPBOX, files too large for github
saveRDS(PDI, "results_analysis_2_PDI.rds") 
#read
PDI <- readRDS("results_analysis_2_PDI.rds") #load model


ce_1 <- conditional_effects(PDI, effects = "PC1",points=T) 

PDI_PC1 <- ggplot(ce_1[[1]], aes(x = -PC1, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC1, y = PDI),
   size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2)  +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("PDI")

ce_2 <- conditional_effects(PDI, effects = "PC2",points=T) 

PDI_PC2 <- ggplot(ce_2[[1]], aes(x = -PC2, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC2, y = PDI),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("PDI")

ce_3 <- conditional_effects(PDI, effects = "PC3",points=T) 

PDI_PC3 <- ggplot(ce_3[[1]], aes(x = -PC3, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC3, y = PDI),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("PDI")

PDI_PC1 + PDI_PC2 + PDI_PC3

########################################################################################################################################################
##CLOSENESS
########################################################################################################################################################

closeness <- brm(closeness ~ PC1 + PC2 + PC3 + (1|Id),
           data = data_analysis_2, family  = zero_one_inflated_beta(), cores = 4,chains = 4, 
           sample_prior = TRUE, warmup = 500, iter = 2000,
           control = list(adapt_delta = 0.99)) 

pp_check(closeness)

summary(closeness)


ce_1 <- conditional_effects(closeness, effects = "PC1",points=T) 

#Save output
setwd("~/Dropbox/PhD/R/Chapter_2") #DROPBOX, files too large for github
saveRDS(PDI, "results_analysis_2_closeness.rds") 
#read
closeness <- readRDS("results_analysis_2_closeness.rds") #load model


ce_1 <- conditional_effects(closeness, effects = "PC1",points=T) 

closeness_PC1 <- ggplot(ce_1[[1]], aes(x = -PC1, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC1, y = closeness),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2)  +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("closeness")

ce_2 <- conditional_effects(closeness, effects = "PC2",points=T) 

closeness_PC2 <- ggplot(ce_2[[1]], aes(x = -PC2, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC2, y = closeness),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("closeness")

ce_3 <- conditional_effects(closeness, effects = "PC3",points=T) 

closeness_PC3 <- ggplot(ce_3[[1]], aes(x = -PC3, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC3, y = closeness),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("closeness")


closeness_PC1 + closeness_PC2 + closeness_PC3

########################################################################################################################################################
##WEIGHTED CLOSENESS
########################################################################################################################################################

#weighted.closeness <- brm(weighted.closeness ~ PC1 + PC2 + PC3 + (1|Id),
                # data = data_analysis_2, family  = zero_one_inflated_beta(), cores = 4,chains = 4, 
                # sample_prior = TRUE, warmup = 500, iter = 2000,
                # control = list(adapt_delta = 0.99)) 

pp_check(weighted.closeness)

summary(weighted.closeness)

#Save output
setwd("~/Dropbox/PhD/R/Chapter_2") #DROPBOX, files too large for github
saveRDS(PDI, "results_analysis_2_weighted.closeness.rds") 
#read
weighted.closeness <- readRDS("results_analysis_2_weighted.closeness.rds") #load model


ce_1 <- conditional_effects(weighted.closeness, effects = "PC1",points=T) 

weighted.closeness_PC1 <- ggplot(ce_1[[1]], aes(x = -PC1, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC1, y = weighted.closeness),
 size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2)  +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("weighted.closeness")

ce_2 <- conditional_effects(weighted.closeness, effects = "PC2",points=T) 

weighted.closeness_PC2 <- ggplot(ce_2[[1]], aes(x = -PC2, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC2, y = weighted.closeness),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("weighted.closeness")

ce_3 <- conditional_effects(weighted.closeness, effects = "PC3",points=T) 

weighted.closeness_PC3 <- ggplot(ce_3[[1]], aes(x = -PC3, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC3, y = weighted.closeness),
  size = 1, alpha=0.9) + geom_line(colour="darkblue",size=1.2) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("weighted.closeness")


weighted.closeness_PC1 + weighted.closeness_PC2 + weighted.closeness_PC3

########################################################################################################################################################
#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL
########################################################################################################################################################

Visits_PC1 + Visits_PC2 + Visits_PC3 + degree_PC1 + degree_PC2 + degree_PC3+ normalised.degree_PC1 + normalised.degree_PC2 + normalised.degree_PC3 +
  Specialization_PC1 + Specialization_PC2+ Specialization_PC3+  PDI_PC1 + PDI_PC2 + PDI_PC3+   plot_layout(ncol = 3)
  
  
  
betweenness_PC1 + betweenness_PC2 + betweenness_PC3 +
PDI_PC1 + PDI_PC2 + PDI_PC3 + closeness_PC1 + closeness_PC2 + closeness_PC3 +
  plot_layout(ncol = 3)


